/*
    FiraDisk -- File-disk/RAM-disk virtual disk driver
    Copyright (C) 2009-2011  Panot Joonkhiaw

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
// busstart.c

#include "firadisk.h"
#include <stdlib.h>
#include <mountmgr.h>

// Note: All FiraDiskBusStart* function run with IRQL=PASSIVE_LEVEL

typedef char check_drive_map_slot_size[(sizeof(GRUB4DOS_DRIVE_MAP_SLOT)==24)?1:-1];

/*
NTSTATUS DiskDeviceInterfaceChangeNotificationCallback ( IN PVOID NotificationStructure, IN PVOID Context )
{
	PDEVICE_INTERFACE_CHANGE_NOTIFICATION pnotif = (PDEVICE_INTERFACE_CHANGE_NOTIFICATION)NotificationStructure;
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)Context;
	DBGPrint2(("FiraDisk: DeviceInterfaceChangeNotification Disk %s %wZ\n",
		IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_ARRIVAL) ? "Arrival" :
		IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_REMOVAL) ? "Removal" : "?",
		pnotif->SymbolicLinkName
		));
	return STATUS_SUCCESS;
}
*/
/*
NTSTATUS VolumeDeviceInterfaceChangeNotificationCallback ( IN PVOID NotificationStructure, IN PVOID Context )
{
	PDEVICE_INTERFACE_CHANGE_NOTIFICATION pnotif = (PDEVICE_INTERFACE_CHANGE_NOTIFICATION)NotificationStructure;
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)Context;
	UCHAR arrival = IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_ARRIVAL);
	UCHAR removal = IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_REMOVAL);
	DBGPrint2(("FiraDisk: DeviceInterfaceChangeNotification Volume %s %wZ\n",
		arrival? "Arrival" : removal? "Removal" : "?",
		pnotif->SymbolicLinkName
		));
	if (arrival && fdoext->haveofflinedrive)
		IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
	return STATUS_SUCCESS;
}
*/
NTSTATUS MountedDeviceInterfaceChangeNotificationCallback ( IN PVOID NotificationStructure, IN PVOID Context )
{
	PDEVICE_INTERFACE_CHANGE_NOTIFICATION pnotif = (PDEVICE_INTERFACE_CHANGE_NOTIFICATION)NotificationStructure;
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)Context;
	UCHAR arrival = IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_ARRIVAL);
	UCHAR removal = IsEqualGUID(&pnotif->Event,&GUID_DEVICE_INTERFACE_REMOVAL);
	DBGPrint2(("FiraDisk: DeviceInterfaceChangeNotification Mounted %s %wZ\n",
		arrival? "Arrival" : removal? "Removal" : "?",
		pnotif->SymbolicLinkName
		));
	if (arrival && fdoext->haveofflinedrive)
		IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
	return STATUS_SUCCESS;
}

void FiraDiskBusStartCreateRAMDrive (PFIRADISK_FDO_EXT fdoext, HANDLE hkDetected)
{
	NTSTATUS status;
	ULONG ramdrivenumber;
	WCHAR namebuffer[FIRADISKMAXKEYPATH];
	UNICODE_STRING usname = { 0, CCSIZEOF(namebuffer), namebuffer };
	for (ramdrivenumber = 0; ; ++ramdrivenumber)
	{
		DISK_GEOMETRY geometry;
		FIRADISK_RAM_DRIVE_PARAMETERS ramdriveparam;
		UCHAR removable;
		{
			typedef struct _KVPI {
				KEY_VALUE_PARTIAL_INFORMATION kvpi;
				UCHAR buffer[sizeof(FIRADISK_RAM_DRIVE_PARAMETERS)];
			} KVPI;
			KVPI data;
			ULONG reslen = 0;
			RtlUnicodeStringPrintf(&usname,wsfValueNameDetectedRAMDrive,ramdrivenumber);
			RtlZeroMemory(&data,sizeof(data));
			status = ZwQueryValueKey ( hkDetected, &usname, KeyValuePartialInformation, &data, sizeof(data), &reslen );
			if (!NT_SUCCESS(status)) 
				break;
			if (data.kvpi.Type==REG_BINARY && data.kvpi.DataLength==sizeof(FIRADISK_RAM_DRIVE_PARAMETERS))
				RtlCopyMemory(&ramdriveparam, &data.kvpi.Data, sizeof(FIRADISK_RAM_DRIVE_PARAMETERS));
			else
				break;
		}
		// check data
		removable=0;
		if ( ramdriveparam.sclass==FiraDiskSClassFloppy )
		{ //FloppyDisk is currently not well tested
			removable = 1;
			//ramdriveparam.sclass = FiraDiskSClassDisk;
		} else if ( ramdriveparam.sclass==FiraDiskSClassCDROM )	{
			removable = 1;
		} else if ( ramdriveparam.sclass==FiraDiskSClassDisk ) {
		} else {
			ramdriveparam.sclass = FiraDiskSClassUnknown;
		}
		// add device
		DBGPrint0(("FiraDisk:  Create RAM drive class=%d addr=0x%I64x len=%I64d bps=%d spt=%d tpc=%d\n",
			ramdriveparam.sclass, ramdriveparam.address, ramdriveparam.length,
			1<<ramdriveparam.sectorsizelog, ramdriveparam.sectorpertrack, ramdriveparam.trackpercylinder));
		geometry.MediaType = removable? RemovableMedia: FixedMedia;
		geometry.BytesPerSector = 1<<ramdriveparam.sectorsizelog;
		geometry.SectorsPerTrack = ramdriveparam.sectorpertrack;
		geometry.TracksPerCylinder = ramdriveparam.trackpercylinder;
		geometry.Cylinders.QuadPart = 0;
		FiraDiskCreatePhysicalMemDisk(fdoext->common.devobj, ramdriveparam.sclass, 
			ramdriveparam.length, ramdriveparam.address, NULL, &geometry, FALSE, 0);
	}
}

UINT32 WstrToUint32(PWSTR pws)
{
	UINT32 u = 0;
	PWSTR p;
	if (pws[0]==L'0' && (pws[1]|32)==L'x') {
		// base16 integer
		for (p = pws+2; *p; ++p) {
			USHORT d = *p;
			d = (USHORT)((*p)-L'0');
			// '0'..'9' d is 0..9 
			// 'A'..'F' d is 17..22. d-7 is 10..15.
			// 'a'..'f' d id 49..54. d-7 is 32+10..32+15.
			if (d >= 10) {  
				d = (d-7) & ~(USHORT)32;
				// for 'A'..'F' and 'a'..'f', d is now 10..15 and d-10 is 0..5
				if ((USHORT)(d-10)>5) 
					break;
			}
			u = (u<<4)+d;
		}
	} else {
		// base10 integer
		for (p = pws; ((USHORT)((*p)-L'0'))<10; ++p) 
			u = u*10+((USHORT)(*p-L'0'));
	}
	return u;
}

UINT64 WstrToUint64(PWSTR pws)
{
	UINT64 u = 0;
	PWSTR p;
	if (pws[0]==L'0' && ((pws[1])|32)==L'x') {
		// base16 integer
		for (p = pws+2; *p; ++p) {
			USHORT d = *p;
			d = (USHORT)((*p)-L'0');
			// '0'..'9' d is 0..9 
			// 'A'..'F' d is 17..22. d-7 is 10..15.
			// 'a'..'f' d id 49..54. d-7 is 32+10..32+15.
			if (d >= 10) {  
				d = (d-7) & ~(USHORT)32;
				// for 'A'..'F' and 'a'..'f', d is now 10..15 and d-10 is 0..5
				if ((USHORT)(d-10)>5) 
					break;
			}
			u = (u<<4)+d;
		}
	} else {
		// base10 integer
		for (p = pws; ((USHORT)((*p)-L'0'))<10; ++p) 
			u = u*10+((USHORT)(*p-L'0'));
	}
	return u;
}

void FiraDiskBusStartParseOptions (PFIRADISK_FDO_EXT fdoext, PWSTR pwsOptions )
{
	PWSTR pws1,pws1next,pws1end;
	int ndrive = 0;
	DBGPrint0(("FiraDisk:  Options %ls\n",pwsOptions));
	// parse
	for (pws1=pwsOptions; *pws1; pws1=pws1next)
	{	// loop for each drive separated with ';'
		PWSTR pws2,pws2next,pws2end;
		UCHAR driveclass;
		DISK_GEOMETRY geo;
		UCHAR imgtype;
		UCHAR imgpreload;
		UCHAR imgcopyonwrite;
		UCHAR imgreadonly;
		UCHAR isbootdrive = 0;
		int npart;
		UINT64 imgsize;
		UINT64 imgoffset;
		//ULONG mapdrivenumber;
		UNICODE_STRING uspath;
		UNICODE_STRING usload;
		DBGPrint0(("FiraDisk:  drv %u\n", pws1-pwsOptions));
		driveclass = 0;
		npart = 0;
		imgtype = 0;
		imgpreload = 0;
		imgcopyonwrite = 0;
		imgreadonly = 0;
		imgsize = 0;
		imgoffset = 0;
		//mapdrivenumber = 0;
		RtlInitEmptyUnicodeString(&uspath, NULL, 0);
		RtlInitEmptyUnicodeString(&usload, NULL, 0);
		for (pws1end=pws1; *pws1end && (*pws1end != L';'); ++pws1end) {}
		for (pws1next=pws1end; *pws1next == L';'; ++pws1next) {}
		*pws1end = L'\0';
		for (pws2=pws1; *pws2; pws2=pws2next)
		{	// loop for each drive options separated with ','
			PWSTR pws2data;
			for (pws2end=pws2; *pws2end && (*pws2end != L','); ++pws2end) {}
			for (pws2next=pws2end; *pws2next == L','; ++pws2next) {}
			*pws2end = L'\0';
			// split variable=data pair
			for (pws2data=pws2; *pws2data && (*pws2data != L'='); ++pws2data) {}
			if (*pws2data==L'=') { *pws2data=L'\0'; pws2data++; }
			DBGPrint0(("FiraDisk:   var %u data %u\n", pws2-pws1, pws2data-pws2));
			// check for variable name
			if        (_wcsicmp(pws2,L"disk")==0) {
				driveclass = FiraDiskSClassDisk;
				RtlZeroMemory(&geo, sizeof(geo));
			} else if (_wcsicmp(pws2,L"cdrom")==0) {
				driveclass = FiraDiskSClassCDROM;
				RtlZeroMemory(&geo, sizeof(geo));
			} else if (_wcsicmp(pws2,L"floppy")==0) {
				driveclass = FiraDiskSClassFloppy;
				RtlZeroMemory(&geo, sizeof(geo));
				geo.TracksPerCylinder = 2;
				geo.SectorsPerTrack = 18;
			} else if (_wcsicmp(pws2,L"file")==0) {
				imgtype = FiraDiskImageFile;
				RtlInitUnicodeString(&uspath,pws2data);
			} else if (_wcsicmp(pws2,L"vmem")==0) {
				imgtype = FiraDiskImageVirtualMemory;
				RtlInitUnicodeString(&uspath,pws2data);
			} else if (_wcsicmp(pws2,L"physicalmemory")==0) {
				imgtype = FiraDiskImagePhysicalMemory;
				RtlInitUnicodeString(&uspath,pws2data);
			//} else if (_wcsicmp(pws2,L"map-drive-number")==0) {
			//	ULONG tmp = WstrToUint32(pws2data);
			//	if (tmp<0x100) mapdrivenumber = tmp+0x100;
			} else if (_wcsicmp(pws2,L"size")==0) {
				imgsize = WstrToUint64(pws2data);
			} else if (_wcsicmp(pws2,L"offset")==0) {
				imgoffset = WstrToUint64(pws2data);
			} else if (_wcsicmp(pws2,L"heads")==0) {
				geo.TracksPerCylinder = WstrToUint32(pws2data);
			} else if (_wcsicmp(pws2,L"sectors-per-track")==0 || _wcsicmp(pws2,L"spt")==0) {
				geo.SectorsPerTrack = WstrToUint32(pws2data);
//			} else if (_wcsicmp(pws2,L"load")==0) {
//				imgpreload = 1;
//			} else if (_wcsicmp(pws2,L"cow")==0) {
//				imgcopyonwrite = 1;
			} else if (_wcsicmp(pws2,L"boot")==0) {
				isbootdrive = 1;
			} else if (_wcsicmp(pws2,L"ro")==0) {
				imgreadonly = 1;
			}
		}
		// create drive if paramenters is valid
		DBGPrint0(("FiraDisk:  params %u %u %I64u %I64u\n", driveclass, imgtype, imgsize, imgoffset));
		if (driveclass && imgtype)
		{
			switch (imgtype)
			{
			case FiraDiskImageFile:
				FiraDiskCreateFileDisk(fdoext->common.devobj, driveclass, imgsize, imgoffset, &uspath, &geo, FALSE, isbootdrive);
				break;
			case FiraDiskImageVirtualMemory:
				FiraDiskCreateVirtualMemDisk(fdoext->common.devobj, driveclass, imgsize, imgoffset, &uspath, &geo, FALSE, isbootdrive);
				break;
#ifdef _M_IX86
			case FiraDiskImagePhysicalMemory:
				// Valid only for 32-bit version.
				// Don't allow address below 4GB
				if (0x100000000<=imgoffset && 0!=imgsize) {
					FiraDiskCreatePhysicalMemDisk(fdoext->common.devobj, driveclass, imgsize, imgoffset, &uspath, &geo, isbootdrive, FALSE);
				} else {
					DBGPrint0(("FiraDisk:  invalid offset or size\n"));
				}
				break;
#endif
			}
		}
	}
}

void FiraDiskBusStartReadFiraDiskOptions (PFIRADISK_FDO_EXT fdoext, HANDLE hkFiraDisk)
{
	NTSTATUS status;
	ULONG buffersize, resultlen=0;	
	struct { KEY_VALUE_PARTIAL_INFORMATION kvpi; UCHAR moredata[255]; } mybuffer;
	PVOID allocatedbuffer = NULL;
	PKEY_VALUE_PARTIAL_INFORMATION pkvpi;
	PWSTR pwsOptions;
	pkvpi = &mybuffer.kvpi; buffersize = sizeof(mybuffer);
	// value StartOptions
	status = ZwQueryValueKey ( hkFiraDisk, (PUNICODE_STRING)&usVNStartOptions, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
	if (status==STATUS_BUFFER_OVERFLOW || status==STATUS_BUFFER_TOO_SMALL) 
	{
		// get larger buffer and try again
		buffersize = resultlen;
		allocatedbuffer = ExAllocatePoolWithTag(PagedPool, buffersize, FIRADISK_POOL_TAG);
		if (allocatedbuffer) 
		{
			pkvpi = (PKEY_VALUE_PARTIAL_INFORMATION)allocatedbuffer;
			status = ZwQueryValueKey ( hkFiraDisk, (PUNICODE_STRING)&usVNStartOptions, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
		}
	}
	if (!NT_SUCCESS(status))
	{
		DBGPrint0(("FiraDisk:  Read Options status=0x%X\n",status));
	}
	else
	{
		// test validity
		pwsOptions = (PWSTR)(pkvpi->Data);
		if (!NT_SUCCESS(RtlStringCbLengthW(pwsOptions, pkvpi->DataLength, NULL))) {
			DBGPrint0(("FiraDisk:  Invalid Options data\n"));
		} else {
			// valid string
			FiraDiskBusStartParseOptions(fdoext, pwsOptions);
		}
	}
	// cleanup
	if (allocatedbuffer) 
	{
		ExFreePoolWithTag(allocatedbuffer, FIRADISK_POOL_TAG);
	}
}

void FiraDiskBusStartReadSystemStartOptions (PFIRADISK_FDO_EXT fdoext, HANDLE hkControl)
{
	NTSTATUS status;
	ULONG buffersize, resultlen=0;	
	struct { KEY_VALUE_PARTIAL_INFORMATION kvpi; UCHAR moredata[255]; } mybuffer;
	PVOID allocatedbuffer = NULL;
	PKEY_VALUE_PARTIAL_INFORMATION pkvpi;
	pkvpi = &mybuffer.kvpi; buffersize = sizeof(mybuffer);
	status = ZwQueryValueKey ( hkControl, (PUNICODE_STRING)&usVNSystemStartOptions, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
	if (status==STATUS_BUFFER_OVERFLOW || status==STATUS_BUFFER_TOO_SMALL) 
	{
		// get larger buffer and try again
		buffersize = resultlen;
		allocatedbuffer = ExAllocatePoolWithTag(PagedPool, buffersize, FIRADISK_POOL_TAG);
		if (allocatedbuffer) 
		{
			pkvpi = (PKEY_VALUE_PARTIAL_INFORMATION)allocatedbuffer;
			status = ZwQueryValueKey ( hkControl, (PUNICODE_STRING)&usVNSystemStartOptions, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
		}
	}
	if (!NT_SUCCESS(status))
	{
		DBGPrint0(("FiraDisk:  Read SystemStartOptions failed status=0x%X\n",status));
	}
	else
	{
		// test validity
		PWSTR pwssysoption = (PWSTR)(pkvpi->Data);
		if (!NT_SUCCESS(RtlStringCbLengthW(pwssysoption, pkvpi->DataLength, NULL))) {
			DBGPrint0(("FiraDisk:  Invalid SystemStartOptions data\n"));
		} else {
			// valid string
			PWSTR pws,pwsnext;
			unsigned int len = wcslen(wsFiraDiskOptionPrefix);
			//DBGPrint0(("FiraDisk:  SystemStartOptions %ls\n",pwssysoption));
			// parse
			for (pws=pwssysoption; *pws; pws=pwsnext)
			{
				PWSTR pwsend;
				for (pwsend=pws; *pwsend > L' '; ++pwsend) {}
				for (pwsnext=pwsend; *pwsnext &&  (*pwsnext <= L' '); ++pwsnext) {}
				*pwsend = L'\0';
				if (_wcsnicmp(pws, wsFiraDiskOptionPrefix, len)
					==0)
				{
					PWSTR pwsv=pws+len;
					FiraDiskBusStartParseOptions(fdoext, pwsv);
				}
			}
		}
	}
	// cleanup
	if (allocatedbuffer) ExFreePoolWithTag(allocatedbuffer, FIRADISK_POOL_TAG);
}

NTSTATUS FiraDiskBusStartDetectDrives ( IN PDEVICE_OBJECT fdo )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	NTSTATUS status;
	WCHAR namebuffer[FIRADISKMAXKEYPATH];
	UNICODE_STRING objname = { 0, CCSIZEOF(namebuffer), namebuffer };
	OBJECT_ATTRIBUTES obja;
	DBGPrint0(("FiraDisk: BusStart\n"));
	// open key \Registry\Machine\SYSTEM\CurrentControlSet\Control
	{
		HANDLE hkControl = NULL;
		RtlUnicodeStringCopyString(&objname, objPathRegMachine);
		RtlUnicodeStringCatString(&objname, keyPathSysCurControl);
		InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
		status = ZwOpenKey(&hkControl, KEY_QUERY_VALUE|KEY_CREATE_SUB_KEY, &obja);
		if (!NT_SUCCESS(status)) {
			DBGPrint0(("FiraDisk: failed open key %wZ status=0x%X\n",&objname,status));
		} else {
			HANDLE hkFiraDisk = NULL;
			// create/open FiraDisk subkey
			RtlUnicodeStringCopyString(&objname, keyNameFiraDisk);
			InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,hkControl,NULL);
			status = ZwCreateKey(&hkFiraDisk, KEY_QUERY_VALUE|KEY_CREATE_SUB_KEY, &obja, 0, NULL, 0, NULL);
			if (!NT_SUCCESS(status)) {
				DBGPrint0(("FiraDisk: failed create key FiraDisk status=0x%X\n",status));
			} else {
				FiraDiskBusStartReadFiraDiskOptions(fdoext,hkFiraDisk);
				{
					HANDLE hkData = NULL;
					ULONG dispos = 0;
					// create/open key Control\FiraDisk\VolatileData
					RtlUnicodeStringCopyString(&objname, keyNameVolatileData);
					InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,hkFiraDisk,NULL);
					status = ZwCreateKey(&hkData, KEY_QUERY_VALUE|KEY_SET_VALUE, &obja, 0, NULL, REG_OPTION_VOLATILE, &dispos);
					if (!NT_SUCCESS(status)) {
						DBGPrint0(("FiraDisk: failed create key VolatileData status=0x%X\n",status));
					} else {
						// Create RAM drives
						if (!configDisableDetectedRAMDrives)
							FiraDiskBusStartCreateRAMDrive(fdoext,hkData);
						// Create Non-RAM drives
						FiraDiskBusStartReadFiraDiskOptions(fdoext,hkData);
						ZwClose(hkData);
					}
				}			
				ZwClose(hkFiraDisk);
			}
			// Read SystemStartOptions
			FiraDiskBusStartReadSystemStartOptions(fdoext,hkControl);
			ZwClose(hkControl);
		}
	}
	// Read services\firadisk\Parameters StartOptions
	{
		HANDLE hkServiceFiraDiskParameters = NULL;
		RtlUnicodeStringCopy(&objname, &driverRegistryPath);
		RtlUnicodeStringCatString(&objname, wsBkSlash);
		RtlUnicodeStringCatString(&objname, keyNameParameters);
		InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
		status = ZwOpenKey(&hkServiceFiraDiskParameters, KEY_QUERY_VALUE, &obja);
		if (!NT_SUCCESS(status)) {
			DBGPrint0(("FiraDisk: failed open key %wZ status=0x%X\n",&objname,status));
		} else {
			FiraDiskBusStartReadFiraDiskOptions(fdoext,hkServiceFiraDiskParameters);
			ZwClose(hkServiceFiraDiskParameters);
		}
	}
	// start all created drive
	/*{
		PLIST_ENTRY phead = &fdoext->childlist;
		PLIST_ENTRY pentry;
		// activate offline drives
		for (pentry=phead->Flink; pentry != phead; pentry=pentry->Flink)
		{
			PFIRADISK_CHILD_EXT childpdoext = CONTAINING_RECORD(pentry, FIRADISK_CHILD_EXT, listentry);
			if (childpdoext->drivestatus == FiraDiskDriveStatusOffline)
				FiraDiskChildActivateDrive(childpdoext->common.devobj, NULL);
		}
	}*/
	return STATUS_SUCCESS;
}

NTSTATUS FiraDiskBusStart ( IN PDEVICE_OBJECT fdo )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	NTSTATUS status;
	DBGPrint1(("FiraDisk: BusStart\n"));
	if (!fdoext->detectdisk)
	{
		fdoext->detectdisk = 1;
		status = FiraDiskBusStartDetectDrives(fdo);
	}
	fdoext->common.started = TRUE;
	fdoext->common.pausestate = 0;
	fdoext->common.removestate = 0;

	/*
	if (!fdoext->notifentry1)
	IoRegisterPlugPlayNotification(EventCategoryDeviceInterfaceChange,
		PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES,(PVOID)&GUID_DEVINTERFACE_DISK,
		fdo->DriverObject, DiskDeviceInterfaceChangeNotificationCallback, fdoext, &fdoext->notifentry1);
	if (!fdoext->notifentry2)
	IoRegisterPlugPlayNotification(EventCategoryDeviceInterfaceChange,
		PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES,(PVOID)&GUID_DEVINTERFACE_VOLUME,
		fdo->DriverObject, VolumeDeviceInterfaceChangeNotificationCallback, fdoext, &fdoext->notifentry2);
	*/
	if (!fdoext->notifentry3)
	IoRegisterPlugPlayNotification(EventCategoryDeviceInterfaceChange,
		PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES,(PVOID)&MOUNTDEV_MOUNTED_DEVICE_GUID,
		fdo->DriverObject, MountedDeviceInterfaceChangeNotificationCallback, fdoext, &fdoext->notifentry3);

	DBGPrint1(("FiraDisk: BusStart end\n"));
	return STATUS_SUCCESS;
}
