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
// busfd.c
// This file contains routines for functional device object of virtual bus 

#include "firadisk.h"

// AddDevice
// Create functional device object (FDO) for FiraDisk bus device and attach to device stack. 
// Called when PnP Manager found root-enumerated FiraDisk bus device. 

static const WCHAR FiraDiskFDODeviceID[] = L"root\\firadisk\0";
static const WCHAR FiraDiskFDOHwID[] = L"root\\firadisk\0";
//static const WCHAR FiraDiskFDOCompatID[] = L"detected\\firadisk\0";
static const WCHAR FiraDiskFDODeviceDesc[] = L"FiraDisk Virtual Disk Enumerator";

NTSTATUS CallDriverSyncIoCompletion( IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, IN PVOID Context )
{
	PKEVENT pev = (PKEVENT) Context;
	DBGPrint1(("FiraDisk: CallDriverSyncIoCompletion %p %p %p\n", DeviceObject, Irp, Context));
	KeSetEvent(pev, 0, FALSE);
	return STATUS_MORE_PROCESSING_REQUIRED;
}

NTSTATUS CallDriverSync( IN PDEVICE_OBJECT  TargetDeviceObject, IN PIRP  Irp )
{
	KEVENT ev;
	NTSTATUS status;
	DBGCODE(
	PIO_STACK_LOCATION pisl = IoGetNextIrpStackLocation(Irp);
	DBGPrint4(("FiraDisk: CallDriverSync %p %p %08X %08X\n", TargetDeviceObject, Irp,
		pisl->MajorFunction, pisl->MinorFunction));
	);
	KeInitializeEvent(&ev, NotificationEvent, FALSE);
	IoSetCompletionRoutine(Irp, CallDriverSyncIoCompletion, &ev, TRUE,TRUE,TRUE);
	status = IoCallDriver(TargetDeviceObject, Irp);
	if (status == STATUS_PENDING)
	{
		DBGPrint4(("FiraDisk: CallDriverSync waiting\n",status));
		KeWaitForSingleObject(&ev, Executive, KernelMode, FALSE, NULL);
		status = Irp->IoStatus.Status;
	}
	DBGPrint1(("FiraDisk: CallDriverSync end status=%08X\n",status));
	return status;
}

NTSTATUS ForwardIrpSync( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)DeviceObject->DeviceExtension;
	DBGCODE(
	PIO_STACK_LOCATION pisl = IoGetCurrentIrpStackLocation(Irp);
	DBGPrint4(("FiraDisk: ForwardIrpSync %p %p %08X %08X\n",DeviceObject, Irp, 
		pisl->MajorFunction, pisl->MinorFunction));
	);
	IoCopyCurrentIrpStackLocationToNext(Irp);
	return CallDriverSync(fdoext->lowerDO, Irp);
}

NTSTATUS FiraDiskAddDevice( IN PDRIVER_OBJECT  DriverObject, IN PDEVICE_OBJECT  PhysicalDeviceObject )
{
	UCHAR firstbus;
	ExAcquireFastMutex(&buslistlock);
	firstbus = IsListEmpty(&buslisthead);
	ExReleaseFastMutex(&buslistlock);
	if (firstbus)
		return FiraDiskFDOAddDevice( DriverObject, PhysicalDeviceObject, NULL );
	else
		return STATUS_UNSUCCESSFUL;
}

NTSTATUS FiraDiskFDOAddDevice( IN PDRIVER_OBJECT  DriverObject, IN PDEVICE_OBJECT  PhysicalDeviceObject, PDEVICE_OBJECT *pfdo )
{
	PDEVICE_OBJECT fdo;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: AddDevice start\n"));
	status = IoCreateDevice( DriverObject, sizeof(FIRADISK_FDO_EXT), NULL /*no need for device name*/,
		FILE_DEVICE_BUS_EXTENDER, FILE_DEVICE_SECURE_OPEN, FALSE, &fdo);
	if (!NT_SUCCESS(status)) {
		DBGPrint0(("FiraDisk: AddDevice IoCreateDevice failed status=0x%X\n",status));
		if (pfdo) *pfdo = NULL;
		return status;
	} else {
		PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(fdo->DeviceExtension);
		fdoext->common.devclass = &FiraDiskFDOClass;
		fdoext->common.devobj = fdo;
		fdoext->common.started = FALSE;
		fdoext->common.pausestate = 0;
		fdoext->common.removestate = 0;
		fdoext->common.pagingpathcount = 0;
		fdoext->lowerDO = PhysicalDeviceObject;
		IoInitializeRemoveLock(&fdoext->common.removelock, FIRADISK_POOL_TAG, 0, 0);
		InitializeListHead(&fdoext->buslistentry);
		InitializeListHead(&fdoext->childlist);
		InitializeListHead(&fdoext->removedchildlist);
		ExInitializeFastMutex(&fdoext->fm_childlist);
		fdoext->detectdisk = 0;
		fdoext->haveofflinedrive = 0;
		fdoext->nextdrivenumber = 0;
		fdoext->lowerDO = IoAttachDeviceToDeviceStack(fdo, PhysicalDeviceObject);
		if (!NT_SUCCESS(status)) {
			IoDeleteDevice(fdo);
			DBGPrint0(("FiraDisk: AddDevice IoAttachDeviceToDeviceStack failed status=0x%X\n",status));
			if (pfdo) *pfdo = NULL;
			return status;
		}
		// add to buslist
		ExAcquireFastMutex(&buslistlock);
		InsertTailList(&buslisthead, &fdoext->buslistentry);
		ExReleaseFastMutex(&buslistlock);
		// initialize complete
		fdo->Flags &= ~DO_DEVICE_INITIALIZING;
		if (pfdo) *pfdo = fdo;
		DBGPrint0(("FiraDisk: AddDevice success\n"));
		status = STATUS_SUCCESS;
	}
	return status;
}

typedef struct _FIRADISK_FDO_WORKER_CONTEXT {
	PIO_WORKITEM workitem;
	PIRP Irp;
	PIO_STACK_LOCATION iost;
} FIRADISK_FDO_WORKER_CONTEXT, *PFIRADISK_FDO_WORKER_CONTEXT;

VOID FiraDiskFDOInvalidateBusRelationsWorker ( IN PDEVICE_OBJECT  DeviceObject, IN PVOID  Context)
{
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)DeviceObject->DeviceExtension;
	IoInvalidateDeviceRelations(fdoext->lowerDO,BusRelations);
	IoFreeWorkItem((PIO_WORKITEM)Context);
}

// Asynchronous processing of IRP_MN_QUERY_DEVICE_RELATIONS:BusRelations
VOID FiraDiskFDOQueryBusRelationsWorker ( IN PDEVICE_OBJECT  DeviceObject, IN PVOID  Context)
{
	PFIRADISK_FDO_WORKER_CONTEXT pcontext = (PFIRADISK_FDO_WORKER_CONTEXT)Context;
	PIRP Irp = pcontext->Irp;
	PIO_STACK_LOCATION iost = pcontext->iost;
	PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	NTSTATUS status = STATUS_UNSUCCESSFUL;
	UCHAR bootdriveoffline = 0;
	DBGPrint0(("FiraDisk: FDOWorkerPnpQueryBusRelations start\n"));
	// do real work
	// Enumerate all child PDO
	//{
	//	RtlCheckRegistryKey(RTL_REGISTRY_CONTROL, L"FiraDisk\\Data\\Detected");
	//}
	// Mark PDO as inactive for device no longer found
	//  (not applicable)
	// Report currently pressent child device
	{
		LONG childrencount = 0, offlinecount = 0;
		PLIST_ENTRY phead = &fdoext->childlist;
		PLIST_ENTRY pentry;

		// activate offline drives
		for (pentry=phead->Flink; pentry != phead; pentry=pentry->Flink)
		{
			PFIRADISK_CHILD_EXT childpdoext = CONTAINING_RECORD(pentry, FIRADISK_CHILD_EXT, listentry);
			if (childpdoext->drivestatus == FiraDiskDriveStatusOffline)
				FiraDiskChildActivateBootDrive(childpdoext);
		}

		// count drives
		childrencount = 0;
		offlinecount = 0;
		//ExAcquireFastMutex(&fdoext->fm_childlist);
		for (pentry=phead->Flink; pentry != phead; pentry=pentry->Flink)
		{
			PFIRADISK_CHILD_EXT childpdoext = CONTAINING_RECORD(pentry, FIRADISK_CHILD_EXT, listentry);
			if (childpdoext->drivestatus == FiraDiskDriveStatusOnline)
				++childrencount;
			else
			{
				++offlinecount;
				if (childpdoext->isbootdrive) bootdriveoffline = 1;
			}
		}
		//ExReleaseFastMutex(&fdoext->fm_childlist);
		// set flag in fdoext
		fdoext->haveofflinedrive = (offlinecount!=0);
	    DBGPrint0(("FiraDisk:  drive count = %d, offline drive count = %d\n",childrencount,offlinecount));

		if (childrencount == 0) {
			Irp->IoStatus.Status = status = STATUS_SUCCESS; 
		} else {
			PDEVICE_RELATIONS devrtn, devrtnold;
			ULONG size; 
			ULONG totalcount, i;
			devrtnold = (PDEVICE_RELATIONS)(Irp->IoStatus.Information);
			totalcount = childrencount + (devrtnold? devrtnold->Count: 0);
			size = FIELD_OFFSET(DEVICE_RELATIONS, Objects) 
				+ sizeof(PDEVICE_OBJECT) * totalcount;
			devrtn = (PDEVICE_RELATIONS)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
			if (!devrtn) {
				DBGPrint0(("FiraDisk:  ExAllocatePoolWithTag failed.\n"));
				// complete request with error
				Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
			} else {
				devrtn->Count = totalcount;
				i=0;
				if (devrtnold) {
					for(; i < devrtnold->Count; ++i)
						devrtn->Objects[i] = devrtnold->Objects[i];
				}
				//DBGPrint0(("FiraDisk:  call ExAcquireFastMutex\n"));
				//ExAcquireFastMutex(&fdoext->fm_childlist);
				for (pentry=phead->Flink; (pentry != phead)&&(i < totalcount); (pentry=pentry->Flink)) {
					PFIRADISK_CHILD_EXT childpdoext = CONTAINING_RECORD(pentry, FIRADISK_CHILD_EXT, listentry);
					if (childpdoext->drivestatus == FiraDiskDriveStatusOnline) {
						devrtn->Objects[i] = childpdoext->common.devobj;
						//DBGPrint0(("FiraDisk:   call ObReferenceObject\n"));
						ObReferenceObject(devrtn->Objects[i]);
						//DBGPrint0(("FiraDisk:   returned from ObReferenceObject\n"));
						++i;
					}
				}
				//DBGPrint0(("FiraDisk:  call ExReleaseFastMutex\n"));
				//ExReleaseFastMutex(&fdoext->fm_childlist);
				Irp->IoStatus.Information = (ULONG_PTR)devrtn;
				if (devrtnold)
					ExFreePool(devrtnold);
				Irp->IoStatus.Status = status = STATUS_SUCCESS; 
			}
		}
	}
	// cleanup
	//DBGPrint0(("FiraDisk: FDOWorkerPnpQueryBusRelations cleanup\n"));
	IoFreeWorkItem(pcontext->workitem);
	ExFreePoolWithTag(pcontext, FIRADISK_POOL_TAG);
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	if (bootdriveoffline) 
	{
		PIO_WORKITEM pworkitem = IoAllocateWorkItem(DeviceObject);
		if (pworkitem)
		{
			IoQueueWorkItem(pworkitem,FiraDiskFDOInvalidateBusRelationsWorker,DelayedWorkQueue,pworkitem);
		}
	}
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	DBGPrint0(("FiraDisk: FDOWorkerPnpQueryBusRelations end\n"));
}

NTSTATUS FiraDiskFDODispatchPnP( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchPnP\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	case IRP_MN_START_DEVICE:			//Required, bottom-up
		DBGPrint0(("FiraDisk:  START_DEVICE\n"));
		status = ForwardIrpSync(DeviceObject, Irp);
		if (NT_SUCCESS(status))
		{
			Irp->IoStatus.Status = status = FiraDiskBusStart(DeviceObject);
		}
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
		return status;
	case IRP_MN_QUERY_STOP_DEVICE:		//Required, top-down
		DBGPrint0(("FiraDisk:  QUERY_STOP_DEVICE\n"));
		// Win98: Stop for remove, Win2K+: Stop for rebalancing resource. 
		//status = STATUS_SUCCESS;
		fdoext->common.pausestate = 1;
		// more processing to determine if device can be stopped
		// Just fail it for now.
		status = STATUS_UNSUCCESSFUL;
		if (fdoext->common.pagingpathcount)
			status = STATUS_DEVICE_BUSY;
		// If device cannot be stopped
		if (!NT_SUCCESS(status)) {
			Irp->IoStatus.Status = status; 
			IoCompleteRequest(Irp, IO_NO_INCREMENT);
			IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
			return status;
		} else
		// Device can be stopped.
		Irp->IoStatus.Status = STATUS_SUCCESS;
		// Pass the IRP to lower driver.
		break;
	case IRP_MN_STOP_DEVICE:			//Required, top-down
		DBGPrint0(("FiraDisk:  STOP_DEVICE\n"));
		// Win2K+ will not call unless IRP_MN_QUERY_STOP_DEVICE success.
		fdoext->common.pausestate = 2;
		// Pass the IRP to lower driver.
		break;
	case IRP_MN_CANCEL_STOP_DEVICE:		//Required, bottom-up
		DBGPrint0(("FiraDisk:  CANCEL_STOP_DEVICE\n"));
		status = ForwardIrpSync(DeviceObject, Irp);
		fdoext->common.pausestate = 0;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
		return status;
	case IRP_MN_QUERY_REMOVE_DEVICE:	//Required, top-down
		DBGPrint0(("FiraDisk:  QUERY_REMOVE_DEVICE\n"));
		status = STATUS_SUCCESS;
		if (fdoext->common.pagingpathcount)
			status = STATUS_DEVICE_BUSY;
		if (!NT_SUCCESS(status)) {
			// Device cannot be removed
			Irp->IoStatus.Status = status; 
			IoCompleteRequest(Irp, IO_NO_INCREMENT);
			IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
			return status;
		}
		// Device can be removed.
		fdoext->common.removestate = 1;
		Irp->IoStatus.Status = STATUS_SUCCESS;
		// Pass the IRP to lower driver.
		break;
	case IRP_MN_SURPRISE_REMOVAL:		//Required, top-down
		DBGPrint0(("FiraDisk:  SURPRISE_REMOVAL\n"));
		fdoext->common.removestate = 2;
		Irp->IoStatus.Status = STATUS_SUCCESS;
		// Pass the IRP to lower driver.
		break;
	case IRP_MN_REMOVE_DEVICE:			//Required, synchronize
		DBGPrint0(("FiraDisk:  REMOVE_DEVICE\n"));
		fdoext->common.removestate = 3;
		if (fdoext->notifentry1) 
		{
			IoUnregisterPlugPlayNotification(fdoext->notifentry1);
			fdoext->notifentry1 = 0;
		}
		if (fdoext->notifentry2) 
		{
			IoUnregisterPlugPlayNotification(fdoext->notifentry2);
			fdoext->notifentry2 = 0;
		}
		if (fdoext->notifentry3) 
		{
			IoUnregisterPlugPlayNotification(fdoext->notifentry3);
			fdoext->notifentry3 = 0;
		}
		// Remove from bus list
		ExAcquireFastMutex(&buslistlock);
		RemoveEntryList(&fdoext->buslistentry);
		InitializeListHead(&fdoext->buslistentry);
		ExReleaseFastMutex(&buslistlock);
		// Remove all remaining child PDO
		{
			PFAST_MUTEX pfm = &fdoext->fm_childlist;
			PLIST_ENTRY phead = &fdoext->childlist;
			PLIST_ENTRY pentry;
			ExAcquireFastMutex(pfm);
			while ( (pentry = RemoveHeadList(phead)) != phead ) {
				ExReleaseFastMutex(pfm);
				// remove from list, free resource, and delete device object
				{
					PFIRADISK_CHILD_EXT pdoext = CONTAINING_RECORD(pentry, FIRADISK_CHILD_EXT, listentry);
					FiraDiskChildDeleteDevice(pdoext);
				}
				ExAcquireFastMutex(pfm);
			}
			ExReleaseFastMutex(pfm);
		}
		// Continue removal of bus FDO
		IoSkipCurrentIrpStackLocation(Irp);
		status = IoCallDriver(fdoext->lowerDO, Irp);
		// wait all I/O completion
		IoReleaseRemoveLockAndWait(&fdoext->common.removelock, Irp);
		// delete device
		IoDetachDevice(fdoext->lowerDO);
		IoDeleteDevice(DeviceObject);
		DBGPrint0(("FiraDisk:  REMOVE_DEVICE end\n"));
		return status;
	case IRP_MN_CANCEL_REMOVE_DEVICE:	//Required, bottom-up
		DBGPrint0(("FiraDisk:  CANCEL_REMOVE_DEVICE\n"));
		status = ForwardIrpSync(DeviceObject, Irp);
		fdoext->common.pausestate = 0;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
		return status;
	//case IRP_MN_QUERY_CAPABILITIES:			//Optional
	//	break;
	//case IRP_MN_QUERY_PNP_DEVICE_STATE:		//Optional
	//	break;
	case IRP_MN_FILTER_RESOURCE_REQUIREMENTS:	//Optional
	//	DBGPrint0(("FiraDisk:  FILTER_RESOURCE_REQUIREMENTS\n"));
		break;
	case IRP_MN_DEVICE_USAGE_NOTIFICATION:	//Required
		DBGPrint0(("FiraDisk:  DEVICE_USAGE_NOTIFICATION\n"));
		Irp->IoStatus.Status = STATUS_SUCCESS;
		if (iost->Parameters.UsageNotification.InPath){
			IoAdjustPagingPathCount(&fdoext->common.pagingpathcount, TRUE);
		}else{
			IoAdjustPagingPathCount(&fdoext->common.pagingpathcount, FALSE);
		}
		// Pass the IRP to lower driver.
		break;
	case IRP_MN_QUERY_DEVICE_RELATIONS:		//Required, PASSIVE_LEVEL
		DBGPrint0(("FiraDisk:  QUERY_DEVICE_RELATIONS\n"));
		{
			switch (iost->Parameters.QueryDeviceRelations.Type)
			{
			case BusRelations:			//Required
				DBGPrint0(("FiraDisk:   BusRelation\n"));
				// asynchronous operation
				{
					PFIRADISK_FDO_WORKER_CONTEXT pcontext;
					pcontext = (PFIRADISK_FDO_WORKER_CONTEXT) ExAllocatePoolWithTag(NonPagedPool, 
						sizeof (FIRADISK_FDO_WORKER_CONTEXT), FIRADISK_POOL_TAG);
					if (pcontext)
					{
						pcontext->workitem = IoAllocateWorkItem(DeviceObject);
						if (pcontext->workitem)
						{
							pcontext->Irp = Irp;
							pcontext->iost = iost;
							IoQueueWorkItem(pcontext->workitem,FiraDiskFDOQueryBusRelationsWorker,DelayedWorkQueue,pcontext);
							IoMarkIrpPending(Irp);
							return STATUS_PENDING;
						}
						ExFreePoolWithTag(pcontext, FIRADISK_POOL_TAG);
					}
				}
				// error
				Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				IoCompleteRequest(Irp, IO_NO_INCREMENT);
				IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
				return status;
			//case RemovalRelations:		//Optional
			//	break;
			//case EjectionRelations:		//No
			//case TargetDeviceRelation:	//No
			//	;
			}
		}
		break;
	//case IRP_MN_QUERY_RESOURCES:				//No
	//case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:	//No
	//case IRP_MN_QUERY_ID:						//No
	case IRP_MN_QUERY_ID:
		DBGPrint0(("FiraDisk:  QUERY_ID  IdType=%d FDO\n",iost->Parameters.QueryId.IdType));
		{
			switch (iost->Parameters.QueryId.IdType)
			{
				PCWSTR src;
				PWCHAR pws;
				int size;
			case BusQueryDeviceID:
				size = sizeof(FiraDiskFDODeviceID);
				src = FiraDiskFDODeviceID;
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				} else {
					RtlCopyMemory(pws, src, size);				
					DBGPrint0(("FiraDisk:   DeviceID=%ls\n",pws));
					Irp->IoStatus.Information = (ULONG_PTR)pws;
					// success, complete request
					Irp->IoStatus.Status = status = STATUS_SUCCESS;
				}
				IoCompleteRequest(Irp, IO_NO_INCREMENT);
				IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
				return status;
			case BusQueryHardwareIDs:
				size = sizeof(FiraDiskFDOHwID);
				src = FiraDiskFDOHwID;
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				} else {
					RtlCopyMemory(pws, src, size);				
					DBGPrint0(("FiraDisk:   HwID=%ls\n",pws));
					Irp->IoStatus.Information = (ULONG_PTR)pws;
					// success, complete request
					Irp->IoStatus.Status = status = STATUS_SUCCESS;
				}
				IoCompleteRequest(Irp, IO_NO_INCREMENT);
				IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
				return status;
			/*case BusQueryCompatibleIDs:
				size = sizeof(FiraDiskFDOCompatID);
				src = FiraDiskFDOCompatID;
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				} else {
					RtlCopyMemory(pws, src, size);				
					DBGPrint0(("FiraDisk:   CompatID=%ls\n",pws));
					Irp->IoStatus.Information = (ULONG_PTR)pws;
					// success, complete request
					Irp->IoStatus.Status = status = STATUS_SUCCESS;
				}
				IoCompleteRequest(Irp, IO_NO_INCREMENT);
				IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
				return status;*/
			/*case BusQueryInstanceID:	//Optional
				break;*/
			}
		}
		break;	
	case IRP_MN_QUERY_DEVICE_TEXT:			//No
		//DBGPrint0(("FiraDisk:  QUERY_DEVICE_TEXT FDO\n"));
		switch (iost->Parameters.QueryDeviceText.DeviceTextType)
		{
		case DeviceTextDescription:
			switch (iost->Parameters.QueryDeviceText.LocaleId)
			{
				int size;
				PCWSTR src;
				PWCHAR ptext;
			default:
			case 0x00000409:
				src = FiraDiskFDODeviceDesc;
				size = CBSIZEOF(FiraDiskFDODeviceDesc);
				ptext = (PWSTR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!ptext) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				memcpy(ptext, src, size);
				DBGPrint0(("FiraDisk:   DeviceText=%ls\n",ptext));
				Irp->IoStatus.Information = (ULONG_PTR)ptext;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			}
			break;
		//case DeviceTextLocationInformation:
		//	break;
		}
		break;	//case IRP_MN_QUERY_BUS_INFORMATION:		//No
	case IRP_MN_QUERY_INTERFACE:			//Optional
		DBGPrint0(("FiraDisk:  QUERY_INTERFACE\n"));
		break;
	//case IRP_MN_READ_CONFIG:					//No
	//case IRP_MN_WRITE_CONFIG:					//No
	//case IRP_MN_EJECT:						//No
	//case IRP_MN_SET_LOCK:						//No
	//	break;
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskFDODispatchPower( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchPower\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		PoStartNextPowerIrp(Irp);
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	//case IRP_MN_POWER_SEQUENCE:	//Function driver can send this to bus PDO
	//	break;
	case IRP_MN_QUERY_POWER:	//Optional, top-down
		status = STATUS_SUCCESS;
		//if (iost->Parameters.Power.Type==SystemPowerState 
		//	&& iost->Parameters.Power.State==PowerSystemHibernate)
		//	status = STATUS_UNSUCCESSFUL;
		// Fail if unsupported device state
		//if (!NT_SUCCESS(status))
		//{
		//	Irp->IoStatus.Status = status; 
		//  PoStartNextPowerIrp(Irp); // must call this for each IRP_MN_QUERY_POWER, IRP_MN_SET_POWER  in Win2K-2003
		//	IoCompleteRequest(Irp, IO_NO_INCREMENT);
		//	IoReleaseRemoveLock(fdoext->common.removelock, Irp);
		//	return status;
		//}
		// Otherwise, pass IRP down
		PoStartNextPowerIrp(Irp); // must call this for each IRP_MN_QUERY_POWER, IRP_MN_SET_POWER in Win2K-2003
		// then pass IRP down
		break;
	case IRP_MN_SET_POWER:
		status = STATUS_SUCCESS;
		PoStartNextPowerIrp(Irp); // must call this for each IRP_MN_QUERY_POWER, IRP_MN_SET_POWER in Win2K-2003
		// then pass IRP down
		break;
	//case IRP_MN_WAIT_WAKE:
		// Just pass down to lower-PDO
		//break;
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = PoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskFDODispatchDeviceControl( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchDeviceControl\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	switch (iost->Parameters.DeviceIoControl.IoControlCode)
	{
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

// WMI request
NTSTATUS FiraDiskFDODispatchSystemControl( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchSystemControl\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskFDODispatchCreateClose( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchCreateClose\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskFDODispatchReadWrite( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchReadWrite\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	}
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskFDODispatchInternalDeviceControl( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_FDO_EXT fdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: FDODispatchSCSI\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	fdoext = (PFIRADISK_FDO_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	// default -- pass IRP down the I/O stack
	IoSkipCurrentIrpStackLocation(Irp);
	status = IoCallDriver(fdoext->lowerDO, Irp);
	IoReleaseRemoveLock(&fdoext->common.removelock, Irp);
	return status;
}

VOID     FiraDiskFDOStartIo( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	DBGPrint0(("FiraDisk: FDOStartIo\n"));
}
