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
// createdisk.c

#include "firadisk.h"

void FiraDiskChildInitialize(PDEVICE_OBJECT pdo, PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, PDISK_GEOMETRY geometry)
{
	PFIRADISK_CHILD_EXT pdoext = (PFIRADISK_CHILD_EXT)(pdo->DeviceExtension);
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	//pdo->Flags |= DO_BUFFERED_IO;
	//pdoext->common.devclass = (sclass==FiraDiskSClassFloppy)? &FiraDiskChildClassFloppy: &FiraDiskChildClassSCSI;
	pdoext->common.devclass = &FiraDiskChildClassSCSI;
	pdoext->common.devobj = pdo;
	pdoext->common.started = FALSE;
	pdoext->common.pausestate = 0;
	pdoext->common.removestate = 0;
	pdoext->common.pagingpathcount = 0;
	IoInitializeRemoveLock(&pdoext->common.removelock, FIRADISK_POOL_TAG, 0, 0);
	// child device data
	pdoext->copymemory = FiraDiskCopyMemory;
	InitializeListHead(&pdoext->irpqueue_head  ); KeInitializeSpinLock(&pdoext->irpqueue_lock  );
	InitializeListHead(&pdoext->volumelist_head); KeInitializeSpinLock(&pdoext->volumelist_lock);
	KeInitializeEvent(&pdoext->wakethread,NotificationEvent,FALSE);
	pdoext->parent = fdo;
	pdoext->drivenumber = InterlockedIncrement(&fdoext->nextdrivenumber);
	pdoext->claimed = 0;
	pdoext->sclass = sclass;
	pdoext->readonly = FALSE;
	if (geometry) {
		pdoext->geometry = *geometry;
		if (pdoext->geometry.MediaType==0) {
			pdoext->geometry.MediaType = (sclass==FiraDiskSClassCDROM || sclass==FiraDiskSClassFloppy)? RemovableMedia: FixedMedia;
		}
		if (pdoext->geometry.BytesPerSector!=0) {
			char shift = RtlFindMostSignificantBit(pdoext->geometry.BytesPerSector);
			if (((ULONG)1 << shift) == pdoext->geometry.BytesPerSector) {
				pdoext->sectorshift = shift;
			} else {
				DBGPrint0(("Firadisk:  bad sector size %d\n",pdoext->geometry.BytesPerSector));
				pdoext->geometry.BytesPerSector = 0;
			}
		}
		if (pdoext->geometry.BytesPerSector==0) {
			pdoext->sectorshift = (sclass==FiraDiskSClassCDROM)? 11: 9; // 1<<9=512 1<<11=2048
			pdoext->geometry.BytesPerSector = 1 << (pdoext->sectorshift);
		}
	} else {
		pdoext->geometry.MediaType = (sclass==FiraDiskSClassCDROM || sclass==FiraDiskSClassFloppy)? RemovableMedia: FixedMedia;
		pdoext->sectorshift = (sclass==FiraDiskSClassCDROM)? 11: 9; // 1<<9=512 1<<11=2048
		pdoext->geometry.BytesPerSector = 1 << (pdoext->sectorshift);
		pdoext->geometry.SectorsPerTrack = 0;
		pdoext->geometry.TracksPerCylinder = 0;
		pdoext->geometry.Cylinders.QuadPart = 0;
	}
	// divide by sector size, round up
	pdoext->totalsectors = (length+(pdoext->geometry.BytesPerSector-1)) >> (pdoext->sectorshift);
	pdoext->totalbytes = pdoext->totalsectors << (pdoext->sectorshift);
	// image data
	pdoext->imagepart.next = NULL;
	pdoext->imagepart.type = FiraDiskImageZero;
	pdoext->imagepart.readonly = 0;
	pdoext->imagepart.copyonwrite = 0;
	pdoext->imagepart.pfileobj = 0;
	//pdoext->imagepart.offset = 0;
	RtlInitEmptyUnicodeString(&pdoext->imagepart.filename,NULL,0);
	RtlInitEmptyUnicodeString(&pdoext->imagepart.loadfilename,NULL,0);
	pdoext->imagepart.length = pdoext->totalbytes;
	// device thread
	{
		OBJECT_ATTRIBUTES oa;
		HANDLE hThread;
		InitializeObjectAttributes(&oa, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);
		PsCreateSystemThread(&hThread, 0, &oa, NULL, NULL, FiraDiskChildThreadStart, pdoext);
		ObReferenceObjectByHandle(hThread, SYNCHRONIZE, *PsThreadType, KernelMode, (PVOID*)&pdoext->pthreadobj,NULL);
		ZwClose(hThread);
	}
}

NTSTATUS FiraDiskCreateZeroDisk       ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	PFIRADISK_CHILD_EXT pdoext;
	PDEVICE_OBJECT pdo;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDOCreateZeroDisk start\n"));
	status = IoCreateDevice( fdo->DriverObject, sizeof(FIRADISK_CHILD_EXT), NULL /*no name*/,
		FILE_DEVICE_DISK, FILE_DEVICE_SECURE_OPEN, FALSE, &pdo);
	if (!NT_SUCCESS(status)) {
		DBGPrint0(("FiraDisk: FDOCreateZeroDisk IoCreateDevice failed status=0x%X\n",status));
		return status;
	}
	// initialize device data
	FiraDiskChildInitialize(pdo, fdo, sclass, length, geometry);
	pdoext = (PFIRADISK_CHILD_EXT)(pdo->DeviceExtension);
	pdoext->drivestatus = FiraDiskDriveStatusOffline;
	status = STATUS_SUCCESS;
	if (NT_SUCCESS(status))
	{
		// add to list
		ExAcquireFastMutex(&fdoext->fm_childlist);
		InsertTailList(&fdoext->childlist, &pdoext->listentry);
		ExReleaseFastMutex(&fdoext->fm_childlist);
		// success
		if (updatebus) {
			DBGPrint0(("FiraDisk: call IoInvalidateDeviceRelations\n"));
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
		}
		DBGPrint0(("FiraDisk: FDOCreateZeroDisk success\n"));
		return STATUS_SUCCESS;
	}
	IoDeleteDevice(pdo);
	DBGPrint0(("FiraDisk: FDOCreateZroDisk failed status=0x%X\n",status));
	return status;
}

NTSTATUS FiraDiskCreateFileDisk       ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	PFIRADISK_CHILD_EXT pdoext;
	PDEVICE_OBJECT pdo;
	NTSTATUS status;
	//LARGE_INTEGER sectionlength;
	//OBJECT_ATTRIBUTES objattr;
	DBGPrint0(("FiraDisk: FDOCreateFileDisk start\n"));
	status = IoCreateDevice( fdo->DriverObject, sizeof(FIRADISK_CHILD_EXT), NULL /*no name*/,
		FILE_DEVICE_DISK, FILE_AUTOGENERATED_DEVICE_NAME | FILE_DEVICE_SECURE_OPEN, FALSE, &pdo);
	if (!NT_SUCCESS(status)) {
		DBGPrint0(("FiraDisk: FDOCreateFileDisk IoCreateDevice failed status=0x%X\n",status));
		return status;
	}
	// initialize device data
	FiraDiskChildInitialize(pdo, fdo, sclass, length, geometry);
	pdoext = (PFIRADISK_CHILD_EXT)(pdo->DeviceExtension);
	pdoext->isbootdrive = isbootdrive;
	pdoext->imagepart.type = FiraDiskImageFile;
	pdoext->imagepart.parameters.file.hfile = NULL;
	pdoext->imagepart.parameters.file.fileoffset = offset;
	RtlInitEmptyUnicodeString(&pdoext->imagepart.filename, pdoext->imagepart.namebuffer, CBSIZEOF(pdoext->imagepart.namebuffer));
	RtlUnicodeStringCopy(&pdoext->imagepart.filename, filename);
	DBGPrint0(("FiraDisk: FDOCreateFileDisk filename=%wZ\n",&pdoext->imagepart.filename));
	//sectionlength.QuadPart = pdoext->imagepart.length;
	pdoext->drivestatus = FiraDiskDriveStatusOffline;
	status = STATUS_SUCCESS;
	if (NT_SUCCESS(status))
	{
		// add to list
		ExAcquireFastMutex(&fdoext->fm_childlist);
		InsertTailList(&fdoext->childlist, &pdoext->listentry);
		ExReleaseFastMutex(&fdoext->fm_childlist);
		// success
		if (updatebus) {
			DBGPrint0(("FiraDisk: call IoInvalidateDeviceRelations\n"));
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
		}
		DBGPrint0(("FiraDisk: FDOCreateFileDisk success\n"));
		return STATUS_SUCCESS;
	}
	IoDeleteDevice(pdo);
	DBGPrint0(("FiraDisk: FDOCreateFileDisk failed status=0x%X\n",status));
	return status;
}

NTSTATUS FiraDiskCreateVirtualMemDisk ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	PFIRADISK_CHILD_EXT pdoext;
	PDEVICE_OBJECT pdo;
	NTSTATUS status;
	//LARGE_INTEGER sectionlength;
	//OBJECT_ATTRIBUTES objattr;
	DBGPrint0(("FiraDisk: FDOCreateVirtualMemDisk start\n"));
	status = IoCreateDevice( fdo->DriverObject, sizeof(FIRADISK_CHILD_EXT), NULL /*no name*/,
		FILE_DEVICE_DISK, FILE_AUTOGENERATED_DEVICE_NAME | FILE_DEVICE_SECURE_OPEN, FALSE, &pdo);
	if (!NT_SUCCESS(status)) {
		DBGPrint0(("FiraDisk: FDOCreateVirtualMemDisk IoCreateDevice failed status=0x%X\n",status));
		return status;
	}
	// initialize device data
	FiraDiskChildInitialize(pdo, fdo, sclass, length, geometry);
	pdoext = (PFIRADISK_CHILD_EXT)(pdo->DeviceExtension);
	pdoext->isbootdrive = isbootdrive;
	pdoext->imagepart.type = FiraDiskImageVirtualMemory;
	pdoext->imagepart.parameters.virtmem.hsection = NULL;
	pdoext->imagepart.parameters.virtmem.sectionoffset = offset;
	pdoext->imagepart.filename.Buffer = pdoext->imagepart.namebuffer;
	pdoext->imagepart.filename.MaximumLength = CCSIZEOF(pdoext->imagepart.namebuffer);
	pdoext->imagepart.filename.Length = 0;
	pdoext->imagepart.copyonwrite = 0;
	//pdoext->imagepart.unopened_diskoffset = 0;
	/*if (mapdrivenumber)
	{
		unsigned int i;
		for (i=0; i<GRUB4DOS_DRIVE_MAP_MAX; ++i)
		{
			if (drive_map_table[i].from_drive == (UCHAR)mapdrivenumber 
				&& drive_map_table[i].start_sector && drive_map_table[i].sector_count
				&& length <= drive_map_table[i].sector_count << FiraDiskSectorSizeLogDisk)
			{
				pdoext->imagepart.unopened_diskoffset = drive_map_table[i].start_sector << FiraDiskSectorSizeLogDisk;
				if (!length)
					pdoext->imagepart.length = drive_map_table[i].sector_count << FiraDiskSectorSizeLogDisk;
				break;
			}
		}
	}*/
	RtlUnicodeStringCopy(&pdoext->imagepart.filename, filename);
	pdoext->drivestatus = FiraDiskDriveStatusOffline;
	status = STATUS_SUCCESS;
	if (NT_SUCCESS(status))
	{
		// add to list
		ExAcquireFastMutex(&fdoext->fm_childlist);
		InsertTailList(&fdoext->childlist, &pdoext->listentry);
		ExReleaseFastMutex(&fdoext->fm_childlist);
		// success
		if (updatebus) {
			DBGPrint0(("FiraDisk: call IoInvalidateDeviceRelations\n"));
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
		}
		DBGPrint0(("FiraDisk: FDOCreateVirtualMemDisk success\n"));
		return STATUS_SUCCESS;
	}
	IoDeleteDevice(pdo);
	DBGPrint0(("FiraDisk: FDOCreateVirtualMemDisk ZwCreateSection failed status=0x%X\n",status));
	return status;
}

NTSTATUS FiraDiskCreatePhysicalMemDisk( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 physaddr, PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
	PFIRADISK_CHILD_EXT pdoext;
	PDEVICE_OBJECT pdo;
	NTSTATUS status;
	//OBJECT_ATTRIBUTES objattr;
	DBGPrint0(("FiraDisk: FDOCreatePhysicalMemDisk start\n"));
	status = IoCreateDevice( fdo->DriverObject, sizeof(FIRADISK_CHILD_EXT), NULL /*no name*/,
		FILE_DEVICE_DISK, FILE_AUTOGENERATED_DEVICE_NAME | FILE_DEVICE_SECURE_OPEN, FALSE, &pdo);
	if (!NT_SUCCESS(status)) {
		DBGPrint0(("FiraDisk: FDOCreatePhysicalMemDisk IoCreateDevice failed status=0x%X\n",status));
		return status;
	}
	// initialize device data
	FiraDiskChildInitialize(pdo, fdo, sclass, length, geometry);
	pdoext = (PFIRADISK_CHILD_EXT)(pdo->DeviceExtension);
	pdoext->imagepart.type = FiraDiskImagePhysicalMemory;
	pdoext->imagepart.parameters.physmem.physaddr = physaddr;
	pdoext->drivestatus = FiraDiskDriveStatusOffline;
	status = STATUS_SUCCESS;
	if (NT_SUCCESS(status))
	{
		// add to list
		ExAcquireFastMutex(&fdoext->fm_childlist);
		InsertTailList(&fdoext->childlist, &pdoext->listentry);
		ExReleaseFastMutex(&fdoext->fm_childlist);
		// success
		if (updatebus) {
			DBGPrint0(("FiraDisk: call IoInvalidateDeviceRelations\n"));
			IoInvalidateDeviceRelations(fdoext->lowerDO, BusRelations);
		}
		DBGPrint0(("FiraDisk: FDOCreatePhysicalMemDisk success\n"));
		return STATUS_SUCCESS;
	}
	IoDeleteDevice(pdo);
	DBGPrint0(("FiraDisk: FDOCreatePhysicalMemDisk failed status=0x%X\n",status));
	return status;
}