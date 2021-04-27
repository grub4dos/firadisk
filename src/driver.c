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
// driver.c
// Driver entry point

#include "firadisk.h"

#define INITGUID
#include <guiddef.h>
#include <ntddstor.h>
#include <mountmgr.h>
#include "firaguid.h"
#include "ver.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text (INIT, DriverEntry)
#endif

// global variable
FIRADISK_DEV_CLASS FiraDiskFDOClass = {
	FiraDiskFDODispatchPnP, 
	FiraDiskFDODispatchPower, 
	FiraDiskFDODispatchDeviceControl, 
	FiraDiskFDODispatchSystemControl,
	FiraDiskFDODispatchCreateClose, 
	FiraDiskFDODispatchCreateClose, 
	FiraDiskFDODispatchReadWrite, 
	FiraDiskFDODispatchReadWrite,
	FiraDiskFDODispatchInternalDeviceControl, 
	FiraDiskFDOStartIo
};

FIRADISK_DEV_CLASS FiraDiskChildClassSCSI = {
	FiraDiskChildDispatchPnP, 
	FiraDiskChildDispatchPower, 
	FiraDiskChildDispatchDeviceControl, 
	FiraDiskChildDispatchSystemControl,
	FiraDiskChildDispatchCreateClose, 
	FiraDiskChildDispatchCreateClose, 
	FiraDiskChildDispatchReadWrite, 
	FiraDiskChildDispatchReadWrite,
	FiraDiskChildDispatchSCSI,
	FiraDiskChildStartIo
};

// shared strings
const WCHAR wsBkSlash[] = L"\\";
const WCHAR objPathRegMachine[] = L"\\Registry\\Machine\\";
const WCHAR keyPathSysCurControl[] = L"System\\CurrentControlSet\\Control";
const WCHAR keyNameFiraDisk[] = L"FiraDisk";
const WCHAR keyNameVolatileData[] = L"VolatileData";
const WCHAR keyNameParameters[] = L"Parameters";
const WCHAR wsfValueNameDetectedRAMDrive[] = L"RAMDrive%d";
const WCHAR wsfValueNameDetectedDiskMap[] = L"DiskMap%d";
const WCHAR wsfValueNameDetectedCDMap[] = L"CDMap%d";
const WCHAR wsFiraDiskOptionPrefix[] = L"FIRADISK=";

// strings used in this file
DECLA_CASTRING(asFiraDiskSection,"[FiraDisk]");
DECLA_CASTRING(asStartOptions,"StartOptions");
DECLA_CUSTRING(usVNDisableDetectGrub4dos,   L"DisableDetectGrub4dos"   );
DECLA_CUSTRING(usVNDisableDetectMemdisk,    L"DisableDetectMemdisk"    );
DECLA_CUSTRING(usVNDisableDetectedRAMDrives,L"DisableDetectedRAMDrives");

UNICODE_STRING driverRegistryPath;
LIST_ENTRY buslisthead;
FAST_MUTEX buslistlock;
UCHAR afterboot = 0;
ULONG copymem_sse2_threshold = 4096;
#ifndef _AMD64_
UCHAR sse2_available = 0;
#endif
ULONG configDisableDetectGrub4dos    = 0;
ULONG configDisableDetectMemdisk     = 0;
ULONG configDisableDetectedRAMDrives = 0;

GRUB4DOS_DRIVE_MAP_SLOT drive_map_table[8];

// function declaration

DRIVER_REINITIALIZE FiraDiskBootDriverReinitialize;
DRIVER_REINITIALIZE FiraDiskDriverReinitialize;

/*
void dumpmemory(PVOID src, ULONG size)
{
	PUCHAR p, pe; LONG n=0;
	for (p=(PUCHAR)src, pe=p+size; p<pe; ++p, ++n)
	{
		DBGPrint0(( "%02x%c",*p,((n+1)&31)?' ':'\n' ));
	}
	DBGPrint0(( "\n" ));
}
*/

/*NTSTATUS FiraDiskTestCreateFile(PWSTR wsfilename, UINT64 length)
{
	NTSTATUS status;
	HANDLE hFile = NULL;
	UNICODE_STRING ufilename;
	OBJECT_ATTRIBUTES obja;
	IO_STATUS_BLOCK iosb;
	LARGE_INTEGER size;
	RtlInitUnicodeString(&ufilename, wsfilename);
	InitializeObjectAttributes(&obja,&ufilename,
		OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
	size.QuadPart = length;
	DBGPrint0(("FiraDisk: TestCreateFile %ls %I64d\n",wsfilename,length));
	status = ZwCreateFile(&hFile, GENERIC_READ|GENERIC_WRITE, &obja, &iosb, &size, 
		FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN_IF, FILE_NON_DIRECTORY_FILE, NULL, 0);
	if (!NT_SUCCESS(status))
	{  // failed
		DBGPrint0(("FiraDisk:  failed create file status=0x%x\n",status));
	} else { //success
		DBGPrint0(("FiraDisk:  open file success\n"));
		{
			FILE_STANDARD_INFORMATION fileinfo;
			status = ZwQueryInformationFile(hFile, &iosb, &fileinfo, sizeof(FILE_STANDARD_INFORMATION), FileStandardInformation);
			if (NT_SUCCESS(status)) {
				DBGPrint0(("FiraDisk:   file size=%I64d\n",fileinfo.EndOfFile));
			}
		}
		ZwClose(hFile);
	}
	return status;
}*/
/*
NTSTATUS FiraDiskTestReadFile(PWSTR wsfilename, UINT64 length)
{
	NTSTATUS status;
	HANDLE hFile = NULL;
	UNICODE_STRING ufilename;
	OBJECT_ATTRIBUTES obja;
	IO_STATUS_BLOCK iosb;
	LARGE_INTEGER size;
	RtlInitUnicodeString(&ufilename, wsfilename);
	InitializeObjectAttributes(&obja,&ufilename,
		OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
	size.QuadPart = length;
	DBGPrint0(("FiraDisk: TestCreateFile %ls %I64d\n",wsfilename,length));
	status = ZwCreateFile(&hFile, GENERIC_READ|GENERIC_WRITE, &obja, &iosb, &size, 
		FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN, FILE_NON_DIRECTORY_FILE, NULL, 0);
	if (!NT_SUCCESS(status))
	{  // failed
		DBGPrint0(("FiraDisk:  failed create file status=0x%x\n",status));
	} else { //success
		DBGPrint0(("FiraDisk:  open file success\n"));
		{
			FILE_STANDARD_INFORMATION fileinfo;
			status = ZwQueryInformationFile(hFile, &iosb, &fileinfo, sizeof(FILE_STANDARD_INFORMATION), FileStandardInformation);
			if (NT_SUCCESS(status)) {
				DBGPrint0(("FiraDisk:   file size=%I64d\n",fileinfo.EndOfFile));
			}
		}
		{
			UCHAR buffer[512];
			LARGE_INTEGER offset;
			offset.QuadPart =0;
			iosb.Information = 0;
			status = ZwReadFile	(hFile, NULL, NULL, NULL, &iosb, buffer, (ULONG)length,
					&offset, NULL);
			DBGPrint0(("FiraDisk:   read file  status=0x%X, length=%d\n",status,(ULONG)iosb.Information));
			if ((ULONG)iosb.Information)
				dumpmemory(buffer,(ULONG)iosb.Information);
		}
		ZwClose(hFile);
	}
	return status;
}
*/

NTSTATUS FiraDiskFindAdapter(PDRIVER_OBJECT DriverObject)
{
	NTSTATUS status;
	if (!IsListEmpty(&buslisthead))	{
		// Bus device is already discovered.
		return STATUS_SUCCESS;
	}
	else
	{
		ULONG PnP = 0;
		WCHAR namebuffer[FIRADISKMAXKEYPATH];
		UNICODE_STRING objname = { 0, CCSIZEOF(namebuffer), namebuffer };
		OBJECT_ATTRIBUTES obja;
		HANDLE hkServiceFiraDiskParameters = NULL;
		ULONG resultlen=0;	
		RtlUnicodeStringCopy(&objname, &driverRegistryPath);
		RtlUnicodeStringCatString(&objname, wsBkSlash);
		RtlUnicodeStringCatString(&objname, keyNameParameters);
		InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
		status = ZwCreateKey(&hkServiceFiraDiskParameters, KEY_QUERY_VALUE|KEY_SET_VALUE, &obja, 0, NULL, REG_OPTION_NON_VOLATILE, NULL);
		if (!NT_SUCCESS(status))
			hkServiceFiraDiskParameters = NULL;
		if (hkServiceFiraDiskParameters)
		{
			struct { KEY_VALUE_PARTIAL_INFORMATION kvpi; UCHAR moredata[4]; } mybuffer;
			status = ZwQueryValueKey ( hkServiceFiraDiskParameters, (PUNICODE_STRING)&usVNPnP, KeyValuePartialInformation, &mybuffer.kvpi, sizeof(mybuffer), &resultlen );
			if (NT_SUCCESS(status))
			{
				if (mybuffer.kvpi.Type==REG_DWORD)
					PnP = *(PULONG)&mybuffer.kvpi.Data;
			}
		}
		if (PnP) // Skip detection of legacy device.
		{
			status = STATUS_SUCCESS;
		}
		else // Allow detection of legacy device.
		{	
			PDEVICE_OBJECT buspdo = NULL;
			DBGPrint1(("FiraDisk: FindAdapter Calling IoReportDetectedDevice\n"));
			status = IoReportDetectedDevice(DriverObject, Internal, -1, -1, NULL, NULL, FALSE, &buspdo);
			DBGPrint1(("FiraDisk: FindAdapter IoReportDetectedDevice status=0x%X\n",status));
			if (!NT_SUCCESS(status)) {
			} else {
				PDEVICE_OBJECT fdo=NULL;
				ULONG data = 1;
				ZwSetValueKey(hkServiceFiraDiskParameters, (PUNICODE_STRING)&usVNPnP, 0, REG_DWORD, &data, sizeof(ULONG));
				status = FiraDiskFDOAddDevice(DriverObject, buspdo, &fdo);
				if (NT_SUCCESS(status))
				{
					FiraDiskBusStart (fdo);
				}
			}
			DBGPrint0(("FiraDisk: FindAdapter end\n"));
		}
		if (hkServiceFiraDiskParameters)
		{
			ZwClose(hkServiceFiraDiskParameters);
		}
	}
	return status;
}

VOID FiraDiskBootDriverReinitialize (__in struct _DRIVER_OBJECT *DriverObject, __in_opt PVOID Context, __in ULONG Count )
{
	UCHAR reinitagain = FALSE;
	DBGPrint0(("FiraDisk: BootDriverReinitialize %d ----------\n",Count));
	afterboot = 0;
	//FiraDiskTestCreateFile(L"\\DosDevices\\C:\\FiraDiskBootReinit", 1000);
	//FiraDiskTestReadFile(L"\\GLOBAL??\\PhysicalDrive0", 512);
	//FiraDiskTestReadFile(L"\\DosDevices\\PhysicalDrive0", 512);
	if (!IsListEmpty(&buslisthead)) {
		PFIRADISK_FDO_EXT fdoext = CONTAINING_RECORD(buslisthead.Flink, FIRADISK_FDO_EXT, buslistentry);
		if (fdoext->haveofflinedrive)
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
	}
	if (reinitagain) {
		IoRegisterBootDriverReinitialization(DriverObject, FiraDiskBootDriverReinitialize, Context);
	} else {
//		if (Context) 
//			ExFreePoolWithTag(Context, FIRADISK_POOL_TAG);
	}
}

VOID FiraDiskDriverReinitialize (__in struct _DRIVER_OBJECT *DriverObject, __in_opt PVOID Context, __in ULONG Count )
{
	UCHAR reinitagain = FALSE;
	DBGPrint0(("FiraDisk: DriverReinitialize %d ----------\n",Count));
	afterboot = 1;
	//FiraDiskTestCreateFile(L"\\DosDevices\\C:\\FiraDiskReinit", 1000);
	//FiraDiskTestReadFile(L"\\GLOBAL??\\PhysicalDrive0", 512);
	//FiraDiskTestReadFile(L"\\DosDevices\\PhysicalDrive0", 512);
	//Don't FiraDiskFindAdapter here.
	if (!IsListEmpty(&buslisthead)) {
		PFIRADISK_FDO_EXT fdoext = CONTAINING_RECORD(buslisthead.Flink, FIRADISK_FDO_EXT, buslistentry);
		if (fdoext->haveofflinedrive)
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
	}
	if (reinitagain) {
		IoRegisterDriverReinitialization(DriverObject, FiraDiskDriverReinitialize, Context);
	} else {
//		if (Context) 
//			ExFreePoolWithTag(Context, FIRADISK_POOL_TAG);
	}
}

void FiraDiskReadRAMOptions(PVOID pbase, ULONG length, HANDLE hkData)
{
	PCHAR pa = (PCHAR)pbase;
	PCHAR pz = pa + length;
	PCHAR pb,pn,p;
	ULONG section;
	// Make sure there is at least one NUL character.
	for (p=pa; p<pz && *p; ++p) {}
	if (p==pz) 
	{
		DBGPrint0(("FiraDisk:   RAM Options data is not NUL-terminated.\n"));
		return;
	}
	pz = p;
	section = 0;
	pn = pa;
	while (*pn)
	{
		// skip white space
		for (pb=pn; *pb && (UCHAR)(*pb)<=32; ++pb) {}
		// find EOL
		for (pn=pb; (UCHAR)(*pn)>=32; ++pn) {}
		// replace EOL with NUL and point pn to next line
		if (*pn) *pn++ = '\0'; 
		if (*pb==';') // comment
		{
			// skip this line
		}
		else if (*pb=='[') // begin section
		{
			for (p=pb+1; *p && *p!=']'; ++p) {}
			if (*p==']')
			{
				ANSI_STRING as;
				++p;
				as.Buffer = pb; as.Length = as.MaximumLength = (USHORT)(p-pb); 
				if (RtlEqualString(&as,&asFiraDiskSection,TRUE))
					section = 1; // [FiraDisk]
				else
					section = 0;
			}
		}
		else
		{
			for (p=pb; *p && *p!='='; ++p) {}
			switch(section)
			{
			case 1: // [FiraDisk]
				if (*p=='=')
				{
					ANSI_STRING as;
					as.Buffer = pb; as.Length = as.MaximumLength = (USHORT)(p-pb);
					DBGPrint0(("FiraDisk:    %s\n",pb));
					if (RtlEqualString(&as,&asStartOptions,TRUE))
					{
						UNICODE_STRING us; NTSTATUS status;
						PCHAR pd;
						p++;
						pd = p; // pd point to beginning of data
						for (; *p; ++p) { // replace slash with backslash
							if (*p=='/')
								*p = '\\';
						}
						// p point to EOL NUL character
						as.Buffer = pd; as.Length = as.MaximumLength = (USHORT)(p-pd)+1; // include NUL character
						us.Buffer = 0; us.Length = us.MaximumLength = 0;
						status = RtlAnsiStringToUnicodeString(&us, &as, TRUE);
						if (NT_SUCCESS(status))
						{
							// save data in registry
							ZwSetValueKey(hkData, (PUNICODE_STRING)&usVNStartOptions, 0, REG_SZ, us.Buffer, us.Length);
							RtlFreeUnicodeString(&us);
						}
					}
				}
				break;
			}
		}
	} //while (*pn)
}

void FiraDiskDriverStartDetectRAMDrive (HANDLE hkFiraDisk, HANDLE hkData)
{
	NTSTATUS status;
	PHYSICAL_ADDRESS physaddr;
	SIZE_T viewsize;
	PCHAR physmem;
	physaddr.QuadPart = 0; viewsize = 0xA0000; // real-mode RAM range 0-640KB
	physmem = (PCHAR) MmMapIoSpace(physaddr, viewsize, MmCached);
	if (physmem) {
		WCHAR namebuffer[FIRADISKMAXKEYPATH];
		UNICODE_STRING usname = { 0, CCSIZEOF(namebuffer), namebuffer };
		ULONG ramdrivecount = 0;
		ULONG ramdrivenumber = 0;
		INTRVECT int13vector;
		PCHAR int13entry;
		UCHAR foundhandler = 0;
		FIRADISK_RAM_DRIVE_PARAMETERS ramdriveparam;
		RtlZeroMemory(&ramdriveparam, sizeof(ramdriveparam));
		DBGPrint0(("FiraDisk:  map real-mode memory success\n",physmem));
		//dumpmemory(physmem,0x20*4);
		for (int13vector = 0x9F0E0; int13vector >= 0; int13vector -= 0x1000) 
		{
			DBGPrint0(("FiraDisk:  int13=%08x\n",int13vector));                     
			//dumpmemory(int13entry,0x20);
			if (!RtlEqualMemory(int13entry+3,"$INT13SF",8))
				break;
			if( !configDisableDetectGrub4dos && RtlEqualMemory(int13entry+3+8,"GRUB4DOS",8) )
				// version checking has not been implemented
				// support GRUB4DOS 0.4.4-0.4.5a
				// other version has not been tested
			{
				PGRUB4DOS_DRIVE_MAP_SLOT pdrvmap;
				ULONG i;
				DBGPrint0(("FiraDisk:  GRUB4DOS at %04x:%04x\n",int13vector.segment,int13vector.offset));
				foundhandler = 1;
				// drive map slot starts at offset 0x20 of the same segment as int13 entry
				pdrvmap = (PGRUB4DOS_DRIVE_MAP_SLOT)(physmem+(((UINT32)int13vector.segment << 4) + 0x20));
				RtlCopyMemory(&drive_map_table, pdrvmap, sizeof(drive_map_table));
				for (i=0
					; 
					i<GRUB4DOS_DRIVE_MAP_MAX
					&& ( ((PUINT32)pdrvmap)[0] || ((PUINT32)pdrvmap)[1] || ((PUINT32)pdrvmap)[2]
				      || ((PUINT32)pdrvmap)[3] || ((PUINT32)pdrvmap)[4] || ((PUINT32)pdrvmap)[5] )
					; 
					++i, ++pdrvmap)
				{
					UCHAR sclass; UCHAR sectorsizelog;
					sclass = (pdrvmap->from_cdrom)? FiraDiskSClassCDROM:
						(pdrvmap->from_drive >= 0x80)? FiraDiskSClassDisk:
							FiraDiskSClassFloppy;
					sectorsizelog = (pdrvmap->from_cdrom)? FiraDiskSectorSizeLogCDROM: FiraDiskSectorSizeLogDisk;
					if (pdrvmap->to_drive==0xFF && !pdrvmap->to_cdrom) // map mem drive
					{
						ULONG isoptiondrive = 0;
						RtlZeroMemory(&ramdriveparam,sizeof(ramdriveparam));
						ramdriveparam.sclass = sclass;
						ramdriveparam.address = pdrvmap->start_sector << GRUB4DOSMemSectorSizeLog;
						ramdriveparam.length  = pdrvmap->sector_count << GRUB4DOSMemSectorSizeLog;
						ramdriveparam.sectorsizelog = sectorsizelog;
						ramdriveparam.sectorpertrack = pdrvmap->max_sector;
						ramdriveparam.trackpercylinder = pdrvmap->max_head? pdrvmap->max_head+1: 0;
						if (sclass==FiraDiskSClassFloppy 
							&& ramdriveparam.length 
							&& ramdriveparam.length < (1UL<<20))
						{
							PHYSICAL_ADDRESS physaddr;
							PVOID pdata;
							physaddr.QuadPart = ramdriveparam.address;
							pdata = MmMapIoSpace(physaddr, (SIZE_T)ramdriveparam.length, MmCached);
							if (pdata)
							{
								ANSI_STRING as = { asFiraDiskSection.Length,asFiraDiskSection.Length, (PCHAR)pdata };
								if (RtlEqualString(&as,&asFiraDiskSection,TRUE))
								{ //[FIRADISK]
									isoptiondrive = 1;
									DBGPrint0(("FiraDisk:   Detected RAM options addr=0x%I64x len=%I64d\n",
										ramdriveparam.address, ramdriveparam.length));
									FiraDiskReadRAMOptions(pdata,(ULONG)ramdriveparam.length,hkData);
								}
								MmUnmapIoSpace(pdata, (SIZE_T)ramdriveparam.length);
							}
						}
						if (!isoptiondrive)
						{
							DBGPrint0(("FiraDisk:   Detected RAM drive class=%d addr=0x%I64x len=%I64d bps=%d spt=%d tpc=%d\n",
								ramdriveparam.sclass, ramdriveparam.address, ramdriveparam.length,
								1<<ramdriveparam.sectorsizelog, ramdriveparam.sectorpertrack, ramdriveparam.trackpercylinder));
							RtlUnicodeStringPrintf(&usname,wsfValueNameDetectedRAMDrive,ramdrivenumber);
							status = ZwSetValueKey(hkData, &usname, 0, REG_BINARY, &ramdriveparam, sizeof(ramdriveparam));
							++ramdrivenumber;
						}
					}
					else
					{
						DBGPrint0(("FiraDisk:   Detected map drive 0x%02X->%02X offset=%I64u length=%I64u\n",
							pdrvmap->from_drive,pdrvmap->to_drive,
							pdrvmap->start_sector << (pdrvmap->to_cdrom? FiraDiskSectorSizeLogCDROM: FiraDiskSectorSizeLogDisk),
							pdrvmap->sector_count << (pdrvmap->to_cdrom? FiraDiskSectorSizeLogCDROM: FiraDiskSectorSizeLogDisk)
							));
					}
				}
			} // GRUB4DOS
			else if( !configDisableDetectMemdisk && RtlEqualMemory(int13entry+3+8,"MEMDISK ",8) )
				// support MEMDISK v>=3.85
			{
				ULONG addr_mBFT = *(UNALIGNED PULONG)(int13entry+27);
				if (addr_mBFT + sizeof(MEMDISK_MBFT) <= 0xA0000)
				{
					PMEMDISK_MBFT pmbft = (PMEMDISK_MBFT)(physmem+addr_mBFT);
					DBGPrint0(("FiraDisk:  MEMDISK at %04x:%04x  mBFT at 0x%08X\n",int13vector.segment,int13vector.offset,addr_mBFT));
					foundhandler = 1;
					if (RtlEqualMemory(pmbft,"mBFT",4))
					{
						UCHAR sclass; UCHAR sectorsizelog;
						if (pmbft->mdi.driveno == 0xE0) // CD-ROM
						{
							sclass = FiraDiskSClassCDROM;
							sectorsizelog = 11; 
						}
						else if (pmbft->mdi.driveno & 0x80) // Hard disk
						{
							sclass = FiraDiskSClassDisk;
							sectorsizelog = 9; 
						}
						else // Floppy
						{
							sclass = FiraDiskSClassFloppy;
							sectorsizelog = 9; 
						}
						RtlZeroMemory(&ramdriveparam,sizeof(ramdriveparam));
						ramdriveparam.sclass = sclass;
						ramdriveparam.address = pmbft->mdi.diskbuf;
						ramdriveparam.length  = (UINT64)pmbft->mdi.disksize << sectorsizelog;
						ramdriveparam.sectorsizelog = sectorsizelog;
						ramdriveparam.sectorpertrack = (UCHAR) pmbft->mdi.sectors;
						ramdriveparam.trackpercylinder = (UCHAR) pmbft->mdi.heads;
						DBGPrint0(("FiraDisk:   Detected RAM drive class=%d addr=0x%I64x len=%I64d bps=%d spt=%d tpc=%d\n",
							ramdriveparam.sclass, ramdriveparam.address, ramdriveparam.length,
							1<<ramdriveparam.sectorsizelog, ramdriveparam.sectorpertrack, ramdriveparam.trackpercylinder));
						RtlUnicodeStringPrintf(&usname,wsfValueNameDetectedRAMDrive,ramdrivenumber);
						status = ZwSetValueKey(hkData, &usname, 0, REG_BINARY, &ramdriveparam, sizeof(ramdriveparam));
						++ramdrivenumber;
					}
				}
			} // MEMDISK
		} // for
		if (!foundhandler) 
		{
			DBGPrint0(("FiraDisk:  No known int13 handler found.\n",physmem));
		}
		MmUnmapIoSpace(physmem, viewsize);
	} else {
		DBGPrint0(("FiraDisk:  map real-mode memory failed\n"));
	}				
	return;
}

NTSTATUS FiraDiskDriverStart ( )
{
	NTSTATUS status;
	WCHAR namebuffer[FIRADISKMAXKEYPATH];
	UNICODE_STRING objname = { 0, CCSIZEOF(namebuffer), namebuffer };
	OBJECT_ATTRIBUTES obja;
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
				HANDLE hkData = NULL;
				ULONG dispos = 0;
				// read configuration
				{
					ULONG buffersize, resultlen=0;
					struct { KEY_VALUE_PARTIAL_INFORMATION kvpi; UCHAR moredata[4]; } mybuffer;
					PVOID allocatedbuffer = NULL;
					PKEY_VALUE_PARTIAL_INFORMATION pkvpi;
					pkvpi = &mybuffer.kvpi; buffersize = sizeof(mybuffer);
					status = ZwQueryValueKey ( hkFiraDisk, (PUNICODE_STRING)&usVNDisableDetectGrub4dos, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
					if (NT_SUCCESS(status) && (mybuffer.kvpi.Type==REG_DWORD))
					{
						configDisableDetectGrub4dos = *(PULONG)&mybuffer.kvpi.Data;
					}
					status = ZwQueryValueKey ( hkFiraDisk, (PUNICODE_STRING)&usVNDisableDetectMemdisk, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
					if (NT_SUCCESS(status) && (mybuffer.kvpi.Type==REG_DWORD))
					{
						configDisableDetectMemdisk = *(PULONG)&mybuffer.kvpi.Data;
					}
					status = ZwQueryValueKey ( hkFiraDisk, (PUNICODE_STRING)&usVNDisableDetectedRAMDrives, KeyValuePartialInformation, pkvpi, buffersize, &resultlen );
					if (NT_SUCCESS(status) && (mybuffer.kvpi.Type==REG_DWORD))
					{
						configDisableDetectedRAMDrives = *(PULONG)&mybuffer.kvpi.Data;
					}
				}
				// create/open key Control\FiraDisk\VolatileData
				RtlUnicodeStringCopyString(&objname, keyNameVolatileData);
				InitializeObjectAttributes(&obja,&objname,OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,hkFiraDisk,NULL);
				status = ZwCreateKey(&hkData, KEY_QUERY_VALUE|KEY_SET_VALUE, &obja, 0, NULL, REG_OPTION_VOLATILE, &dispos);
				if (!NT_SUCCESS(status)) {
					DBGPrint0(("FiraDisk: failed create key VolatileData status=0x%X\n",status));
				} else {
					// Detect RAM drive only once per boot
					if (dispos==REG_CREATED_NEW_KEY) {
						FiraDiskDriverStartDetectRAMDrive(hkFiraDisk,hkData);
					}
					ZwClose(hkData);
				}
				ZwClose(hkFiraDisk);
			}
			ZwClose(hkControl);
		}
	}
	return STATUS_SUCCESS;
}

// function definitions
NTSTATUS DriverEntry( IN PDRIVER_OBJECT  DriverObject, IN PUNICODE_STRING  RegistryPath )
{
	DBGPrint0(("FiraDisk: " PROGRAM_VERSION_STRING "\n"));
	DBGPrint0(("FiraDisk: DriverEntry start ----------\n"));
	DBGPrint0(("FiraDisk: RegPath = %wZ\n",RegistryPath));

	// initialize DriverObject
	DriverObject->DriverExtension->AddDevice = FiraDiskAddDevice;
	//DriverObject->DriverStartIo = FiraDiskStartIo; 
	DriverObject->DriverUnload  = FiraDiskUnload; 
	DriverObject->MajorFunction[IRP_MJ_CREATE] = FiraDiskDispatch;
    DriverObject->MajorFunction[IRP_MJ_CLOSE ] = FiraDiskDispatch;
    DriverObject->MajorFunction[IRP_MJ_READ  ] = FiraDiskDispatch;
    DriverObject->MajorFunction[IRP_MJ_WRITE ] = FiraDiskDispatch;
	DriverObject->MajorFunction[IRP_MJ_PNP   ] = FiraDiskDispatch; 
	DriverObject->MajorFunction[IRP_MJ_POWER ] = FiraDiskDispatch; 
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FiraDiskDispatch;
    DriverObject->MajorFunction[IRP_MJ_SYSTEM_CONTROL] = FiraDiskDispatch;
    DriverObject->MajorFunction[IRP_MJ_INTERNAL_DEVICE_CONTROL] = FiraDiskDispatch;

	{
		USHORT size = RegistryPath->Length;
		PVOID pbuffer = ExAllocatePoolWithTag(NonPagedPool, size, FIRADISK_POOL_TAG);
		if (pbuffer) {
			RtlInitEmptyUnicodeString(&driverRegistryPath, (PWSTR)pbuffer, size);
			RtlCopyUnicodeString(&driverRegistryPath, RegistryPath);
		} else {
			RtlInitEmptyUnicodeString(&driverRegistryPath, NULL, 0);
			DBGPrint0(("FiraDisk:  Failed allocate memory\n"));
		}
	}

#ifndef _AMD64_
	if (ExIsProcessorFeaturePresent(PF_XMMI64_INSTRUCTIONS_AVAILABLE))
	{
		sse2_available = 1;
	}
	else 
	{
		copymem_sse2_threshold = 0xFFFFFFFF;
	}
#endif
	
	InitializeListHead(&buslisthead);
	ExInitializeFastMutex(&buslistlock);
	//IoInitializeRemoveLock(&rmlock_driver,0,0,0);

	FiraDiskDriverStart();
	FiraDiskFindAdapter(DriverObject);

	//FiraDiskTestCreateFile(L"\\DosDevices\\C:\\FiraDiskEntry", 1000);
	IoRegisterBootDriverReinitialization(DriverObject, FiraDiskBootDriverReinitialize, NULL);
	IoRegisterDriverReinitialization(DriverObject, FiraDiskDriverReinitialize, NULL);

	DBGPrint0(("FiraDisk: DriverEntry success\n"));
    return STATUS_SUCCESS; 
}

VOID FiraDiskUnload( IN PDRIVER_OBJECT  DriverObject )
{
	DBGPrint0(("FiraDisk: Unload\n"));
	// free driver-wide allocated resource
	if (driverRegistryPath.Buffer) {
		PVOID p = driverRegistryPath.Buffer;
		RtlInitEmptyUnicodeString(&driverRegistryPath, NULL, 0);
		ExFreePoolWithTag(p, FIRADISK_POOL_TAG);
	}
	//IoAcquireRemoveLock(&rmlock_driver,0);
	//IoReleaseRemoveLockAndWait(&rmlock_driver, 0);
	DBGPrint0(("FiraDisk: Unload end ----------\n\n"));
}

NTSTATUS FiraDiskDispatch( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_DEV_CLASS devclass = ((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->devclass;
	if (!devclass)
		return STATUS_INVALID_DEVICE_OBJECT_PARAMETER;	
	switch (IoGetCurrentIrpStackLocation(Irp)->MajorFunction)
	{
		case IRP_MJ_PNP:			return devclass->DispatchPnP    (DeviceObject, Irp);
		case IRP_MJ_POWER:			return devclass->DispatchPower  (DeviceObject, Irp);
		case IRP_MJ_DEVICE_CONTROL:	return devclass->DispatchDeviceControl (DeviceObject, Irp);
		case IRP_MJ_SYSTEM_CONTROL:	return devclass->DispatchSystemControl (DeviceObject, Irp);
		case IRP_MJ_CREATE:			return devclass->DispatchCreate (DeviceObject, Irp);
		case IRP_MJ_CLOSE:			return devclass->DispatchClose  (DeviceObject, Irp);
		case IRP_MJ_READ:			return devclass->DispatchRead   (DeviceObject, Irp);
		case IRP_MJ_WRITE:			return devclass->DispatchWrite  (DeviceObject, Irp);
		case IRP_MJ_INTERNAL_DEVICE_CONTROL:
									return devclass->DispatchInternalDeviceControl (DeviceObject, Irp);
	}
	return STATUS_NOT_SUPPORTED;	
}

VOID FiraDiskStartIo( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_DEV_CLASS devclass = ((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->devclass;
	if (devclass) devclass->StartIo (DeviceObject, Irp);
}

