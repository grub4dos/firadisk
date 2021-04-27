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
// diskpd.c
// This file contains routines for physical devices object of virtual drives

#include "firadisk.h"
#include <stddef.h>
#include <emmintrin.h>
#include <ntddscsi.h>
#include <ntddstor.h>
#include <ntdddisk.h>
#include <mountmgr.h>
#include <scsi.h>

extern ULONG copymem_sse2_threshold;
extern const WCHAR wsBkSlash[];
const WCHAR objPathDosDevices[] = L"\\DosDevices\\";
const WCHAR objPathArcName[] = L"\\ArcName\\";

//WCHAR EmptyString[] = L"";
WCHAR EmptyMultiString[] = L"";
WCHAR FiraDiskDeviceIDDisk[]   = L"FIRADISK\\Disk\0";
WCHAR FiraDiskDeviceIDRemovableDisk[] = L"FIRADISK\\RemovableDisk\0";
WCHAR FiraDiskDeviceIDCdrom[]  = L"FIRADISK\\CDROM\0";
WCHAR FiraDiskDeviceIDFloppy[] = L"FIRADISK\\SFloppy\0";
WCHAR FiraDiskHwIDDisk[]       = L"FIRADISK\\Disk\0GenDisk\0";
WCHAR FiraDiskHwIDCdrom[]      = L"FIRADISK\\CDROM\0GenCdRom\0";
WCHAR FiraDiskHwIDFloppy[]     = L"FIRADISK\\SFloppy\0GenSFloppy\0";
WCHAR FiraDiskCompatIDDisk[]   = L"GenDisk\0";
WCHAR FiraDiskCompatIDCdrom[]  = L"GenCdRom\0";
WCHAR FiraDiskCompatIDFloppy[] = L"GenSFloppy\0";
WCHAR wsTextImageType[4][16] = { L"Virtual", L"RAM", L"VirtualMem", L"File" };
WCHAR wsTextSClass[4][16]   = { L"Drive", L"Disk", L"CD-ROM Drive", L"Floppy Drive" };
WCHAR wsTextRemovableDisk[] = { L"Removable Disk" };

typedef struct _FIRADISK_PORT_WORKER_CONTEXT {
	PIO_WORKITEM workitem;
	PIRP Irp;
	PIO_STACK_LOCATION iost;
} FIRADISK_PORT_WORKER_CONTEXT, *PFIRADISK_PORT_WORKER_CONTEXT;

typedef CONST UCHAR *PCUCHAR;


void FiraDiskChildQueueInsert(PFIRADISK_CHILD_EXT pdoext, PIRP Irp);
PIRP FiraDiskChildQueueRemove(PFIRADISK_CHILD_EXT pdoext);

////////

#ifndef _AMD64_
void FiraDiskCopyMemory (PVOID dst, PVOID src, SIZE_T size)
{
	ULONG x = ((ULONG)(ULONG_PTR)dst | (ULONG)(ULONG_PTR)src | (ULONG)size);
	if ((x&3) == 0)
		__movsd((ULONG*)dst,(const ULONG*)src,size>>2);
	else
	if ((x&1) == 0)
		__movsw((USHORT*)dst,(const USHORT*)src,size>>1);
	else
		__movsb((UCHAR*)dst,(const UCHAR*)src,size);
}
#endif

#ifdef _AMD64_
void FiraDiskCopyMemory (PVOID dst, PVOID src, SIZE_T size)
#else
void FiraDiskCopyMemorySSE2 (PVOID dst, PVOID src, SIZE_T size)
#endif
{
	PUCHAR pcdst = (PUCHAR)dst;
	PCUCHAR pcsrc = (PCUCHAR)src;
	// align destination
	{
		ULONG n;
		n = (-(LONG)(INT_PTR)pcdst)&15;
		if (n > size) n = (ULONG)size;
		__movsb(pcdst,pcsrc, n);
		pcdst += n; pcsrc += n; size -= n;
	}
	// determine appropriate method
	if ( (((ULONG)(ULONG_PTR)pcsrc)&15) == 0
		&& size >= copymem_sse2_threshold)
	{
		// experimental SSE2 memory copy code, probably not fastest.
		register __m128i r0,r1,r2,r3,r4,r5,r6,r7;
		// prefetch first
		{
			PCUCHAR p;
			ULONG n;
			p = pcsrc;
			n = (size>=2048)? 2048/(64*8): (ULONG)size/(64*8);
			do
			{
				_mm_prefetch((PCCHAR)(p+64*0),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*1),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*2),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*3),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*4),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*5),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*6),_MM_HINT_NTA);
				_mm_prefetch((PCCHAR)(p+64*7),_MM_HINT_NTA);
				p += 64*8;
			} while (--n);
		}
		// Copy 8*16 = 128 bytes at a time
		while(size >= 8*16*2)
		{
			_mm_prefetch((PCCHAR)(pcsrc+2048   ),_MM_HINT_NTA);
			_mm_prefetch((PCCHAR)(pcsrc+2048+64),_MM_HINT_NTA);
			_ReadBarrier();
			r0 = _mm_load_si128(((const __m128i*)pcsrc)+0);
			r1 = _mm_load_si128(((const __m128i*)pcsrc)+1);
			r2 = _mm_load_si128(((const __m128i*)pcsrc)+2);
			r3 = _mm_load_si128(((const __m128i*)pcsrc)+3);
			r4 = _mm_load_si128(((const __m128i*)pcsrc)+4);
			r5 = _mm_load_si128(((const __m128i*)pcsrc)+5);
			r6 = _mm_load_si128(((const __m128i*)pcsrc)+6);
			r7 = _mm_load_si128(((const __m128i*)pcsrc)+7);
			_ReadWriteBarrier();
			_mm_stream_si128(((__m128i*)pcdst)+0, r0);
			_mm_stream_si128(((__m128i*)pcdst)+1, r1);
			_mm_stream_si128(((__m128i*)pcdst)+2, r2);
			_mm_stream_si128(((__m128i*)pcdst)+3, r3);
			_mm_stream_si128(((__m128i*)pcdst)+4, r4);
			_mm_stream_si128(((__m128i*)pcdst)+5, r5);
			_mm_stream_si128(((__m128i*)pcdst)+6, r6);
			_mm_stream_si128(((__m128i*)pcdst)+7, r7);
			_WriteBarrier();
			pcsrc += 8*16;
			pcdst += 8*16;
			size  -= 8*16;
		}
		// Last loop without prefetch
		{
			_ReadBarrier();
			r0 = _mm_load_si128(((const __m128i*)pcsrc)+0);
			r1 = _mm_load_si128(((const __m128i*)pcsrc)+1);
			r2 = _mm_load_si128(((const __m128i*)pcsrc)+2);
			r3 = _mm_load_si128(((const __m128i*)pcsrc)+3);
			r4 = _mm_load_si128(((const __m128i*)pcsrc)+4);
			r5 = _mm_load_si128(((const __m128i*)pcsrc)+5);
			r6 = _mm_load_si128(((const __m128i*)pcsrc)+6);
			r7 = _mm_load_si128(((const __m128i*)pcsrc)+7);
			_ReadWriteBarrier();
			_mm_stream_si128(((__m128i*)pcdst)+0, r0);
			_mm_stream_si128(((__m128i*)pcdst)+1, r1);
			_mm_stream_si128(((__m128i*)pcdst)+2, r2);
			_mm_stream_si128(((__m128i*)pcdst)+3, r3);
			_mm_stream_si128(((__m128i*)pcdst)+4, r4);
			_mm_stream_si128(((__m128i*)pcdst)+5, r5);
			_mm_stream_si128(((__m128i*)pcdst)+6, r6);
			_mm_stream_si128(((__m128i*)pcdst)+7, r7);
			_WriteBarrier();
			pcsrc += 8*16;
			pcdst += 8*16;
			size  -= 8*16;
		}
		_mm_mfence();
		// 0-127 bytes remaining
	}
#ifdef _AMD64_
	if ((((ULONG)(ULONG_PTR)pcsrc)&7) == 0)
	{
		SIZE_T n = size>>3;
		__movsq((ULONGLONG*)pcdst,(const ULONGLONG*)pcsrc,n);
		pcdst += n*8; pcsrc += n*8; size &= 7;
	}
	else
#endif
	if ((((ULONG)(ULONG_PTR)pcsrc)&3) == 0)
	{
		SIZE_T n = size>>2;
		__movsd((ULONG*)pcdst,(const ULONG*)pcsrc,n);
		pcdst += n*4; pcsrc += n*4; size &= 3;
	}
	else
	if ((((ULONG)(ULONG_PTR)pcsrc)&1) == 0)
	{
		SIZE_T n = size>>1;
		__movsw((USHORT*)pcdst,(const USHORT*)pcsrc,n);
		pcdst += n*2; pcsrc += n*2; size &= 1;
	}
	// move the remaining few bytes or the whole unaligned request
	__movsb(pcdst,pcsrc,size);
}

__inline wchar_t *wcs_end(wchar_t *pws)
{
	wchar_t *p;
	for (p=pws; *p; ++p) {}
	return p;
}

// this function runs in <=DISPATCH_LEVEL
void FiraDiskChildQueueInsert(PFIRADISK_CHILD_EXT pdoext, PIRP Irp)
{
	DBGPrint3(("FiraDisk:  QueueInsert %p %p\n",pdoext,Irp));
	ExInterlockedInsertTailList(&pdoext->irpqueue_head, &Irp->Tail.Overlay.ListEntry, &pdoext->irpqueue_lock);
	KeSetEvent(&pdoext->wakethread, 1, FALSE);
}

PIRP FiraDiskChildQueueRemove(PFIRADISK_CHILD_EXT pdoext)
{
	PLIST_ENTRY plistentry = ExInterlockedRemoveHeadList(&pdoext->irpqueue_head, &pdoext->irpqueue_lock);
	return plistentry ? 
		CONTAINING_RECORD(plistentry,IRP,Tail.Overlay.ListEntry) :
		(PIRP)NULL;
}

// this function runs in PASSIVE_LEVEL
NTSTATUS FiraDiskOpenFile (PHANDLE pFileHandle, PCUNICODE_STRING volumepath, PUNICODE_STRING pusPath, PUINT64 pLength, UCHAR readonly )
{
	HANDLE hFile = NULL;
	NTSTATUS status;
	IO_STATUS_BLOCK iosb;
	{
		WCHAR pathbuffer[400];
		UNICODE_STRING fileobjpath;
		OBJECT_ATTRIBUTES obja;
		LARGE_INTEGER size;
		int i;
		size.QuadPart = *pLength;
		RtlInitEmptyUnicodeString(&fileobjpath, pathbuffer, sizeof(pathbuffer));
		if (pusPath->Length >= 6*sizeof(WCHAR) 
			&& (pusPath->Buffer[0]|32)==L'f'
			&& (pusPath->Buffer[1]|32)==L'i'
			&& (pusPath->Buffer[2]|32)==L'n'
			&& (pusPath->Buffer[3]|32)==L'd'
			&& (pusPath->Buffer[4]   )==L':'
			&& (pusPath->Buffer[5]   )==L'\\' )
		{
			// use volumepath
			if (volumepath)
			{
				UNICODE_STRING path1;
				path1.Length = pusPath->Length-5*sizeof(WCHAR);
				path1.MaximumLength = pusPath->MaximumLength-5*sizeof(WCHAR);
				path1.Buffer = pusPath->Buffer+5;
				//DBGPrint0(("FiraDisk:  %p\n",volumepath));
				//DBGPrint0(("FiraDisk:   %wZ\n",volumepath));
				RtlUnicodeStringCopy(&fileobjpath, volumepath);
				RtlUnicodeStringCat(&fileobjpath, &path1);
				InitializeObjectAttributes(&obja,&fileobjpath,
					OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
				DBGPrint0(("FiraDisk:  try open file %wZ\n",&fileobjpath));
				status = ZwCreateFile(&hFile, 
					(readonly? GENERIC_READ : GENERIC_READ|GENERIC_WRITE), 
					&obja, &iosb, &size, FILE_ATTRIBUTE_NORMAL, 0, 
					FILE_OPEN, FILE_NON_DIRECTORY_FILE, NULL, 0);
			}
			else
				status = STATUS_UNSUCCESSFUL;
		}
		else
		{
			// try DosDevices
			RtlUnicodeStringCopyString(&fileobjpath, objPathDosDevices);
			RtlUnicodeStringCat(&fileobjpath, pusPath);
			InitializeObjectAttributes(&obja,&fileobjpath,
				OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
			DBGPrint0(("FiraDisk:  try open file %wZ\n",&fileobjpath));
			status = ZwCreateFile(&hFile, 
				(readonly? GENERIC_READ : GENERIC_READ|GENERIC_WRITE), 
				&obja, &iosb, &size, FILE_ATTRIBUTE_NORMAL, 0, 
				(size.QuadPart? FILE_OPEN_IF: FILE_OPEN), 
				FILE_NON_DIRECTORY_FILE, NULL, 0);
			if (!NT_SUCCESS(status) && !(pusPath->Length>=2*sizeof(WCHAR) && pusPath->Buffer[1]==':'))
			{	// try ArcName
				RtlUnicodeStringCopyString(&fileobjpath, objPathArcName);
				RtlUnicodeStringCat(&fileobjpath, pusPath);
				InitializeObjectAttributes(&obja,&fileobjpath,
					OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF,NULL,NULL);
				DBGPrint0(("FiraDisk:  try open file %wZ\n",&fileobjpath));
				status = ZwCreateFile(&hFile,
					(readonly? GENERIC_READ : GENERIC_READ|GENERIC_WRITE), 
					&obja, &iosb, &size, FILE_ATTRIBUTE_NORMAL, 0, 
					FILE_OPEN, FILE_NON_DIRECTORY_FILE, NULL, 0);
			}
		}
	}
	if (!NT_SUCCESS(status))
	{  // failed
		DBGPrint0(("FiraDisk:  failed create file status=0x%X\n",status));
		*pFileHandle = NULL;
		return status;
	} 
	else
	{ //success
		FILE_STANDARD_INFORMATION fileinfo;
		DBGPrint0(("FiraDisk:  open file success\n"));
		status = ZwQueryInformationFile(hFile, &iosb, &fileinfo, sizeof(FILE_STANDARD_INFORMATION), FileStandardInformation);
		if (NT_SUCCESS(status)) {
			DBGPrint0(("FiraDisk:   file size=%I64d\n",fileinfo.EndOfFile.QuadPart));
			if (*pLength==0) {
				// if we don't know part length, use file size, round down to multiple of 4KB
				*pLength = fileinfo.EndOfFile.QuadPart & (-4096LL);
			} else if ((UINT64)(fileinfo.EndOfFile.QuadPart) < *pLength) {
				// if file size < required length, extend file
				FILE_END_OF_FILE_INFORMATION fileeof;
				fileeof.EndOfFile.QuadPart = *pLength;
				ZwSetInformationFile(hFile, &iosb, &fileeof, sizeof(FILE_END_OF_FILE_INFORMATION), FileEndOfFileInformation);
			}
		}
		*pFileHandle = hFile;
		//don't ZwClose(hFile);
		return STATUS_SUCCESS;
	}
}

// this function runs in PASSIVE_LEVEL
NTSTATUS FiraDiskChildOpenMedia( PFIRADISK_CHILD_EXT pdoext, PCUNICODE_STRING volumepath)
{
	NTSTATUS status = STATUS_SUCCESS;
	PFIRADISK_IMAGE_PART ppart;
	DBGPrint0(("FiraDisk:  ChildOpenMedia\n"));
	if (InterlockedCompareExchange(&pdoext->drivestatus, FiraDiskDriveStatusStarting, FiraDiskDriveStatusOffline)
		==FiraDiskDriveStatusOffline)
	{
		ULONG notreadycount = 0;
		DBGPrint0(("FiraDisk:   Starting\n"));
		pdoext->totalbytes = 0;
		for (ppart=&pdoext->imagepart; ppart!=NULL && notreadycount==0; ppart=ppart->next)
		{
			ULONG partready;
			partready = FALSE;
			switch (ppart->type)
			{
			case FiraDiskImageZero:
				DBGPrint0(("FiraDisk:   Zero  length=%I64u\n", ppart->length));
				partready = TRUE;
				break;
			case FiraDiskImagePhysicalMemory:
				DBGPrint0(("FiraDisk:   RAM   length=%I64u addr=0x%I64x\n", ppart->length, ppart->parameters.physmem.physaddr));
				partready = TRUE;
				break;
			case FiraDiskImageFile:
				DBGPrint0(("FiraDisk:   File  length=%I64u offs=%I64u name=%wZ\n",ppart->length, ppart->parameters.file.fileoffset, &ppart->filename));
				if (ppart->parameters.file.hfile!=NULL)
					partready = TRUE;
				else
				{   // try to open file
					HANDLE hFile = NULL;
					if  (ppart->filename.Length!=0) 
					{
						UINT64 filelength = ppart->length? ppart->length + ppart->parameters.file.fileoffset : 0;
						status = FiraDiskOpenFile (&hFile, volumepath, &ppart->filename, &filelength, ppart->readonly);
						if (NT_SUCCESS(status))
						{
							if (!ppart->length) ppart->length = filelength - ppart->parameters.file.fileoffset;
							ObReferenceObjectByHandle(hFile, (ppart->readonly? FILE_READ_DATA: FILE_WRITE_DATA),
								*IoFileObjectType, KernelMode, (PVOID*)&ppart->pfileobj, NULL);
							ppart->parameters.file.hfile = hFile;
							partready = TRUE;
						}
					}
				}
				break;
			case FiraDiskImageVirtualMemory:
				DBGPrint0(("FiraDisk:   VMem  length=%I64u offs=%I64u name=%wZ\n",ppart->length, ppart->parameters.virtmem.sectionoffset, &ppart->filename));
				if (ppart->parameters.virtmem.hsection!=NULL)
					partready = TRUE;
				else
				{
					HANDLE hFile;
					hFile = NULL;
					if  (ppart->filename.Length!=0) 
					{
						UINT64 filelength = ppart->length? ppart->length + ppart->parameters.virtmem.sectionoffset : 0;
						status = FiraDiskOpenFile (&hFile, volumepath, &ppart->filename, &filelength, ppart->readonly);
						if (NT_SUCCESS(status))
						{
							if (!ppart->length) ppart->length = filelength - ppart->parameters.file.fileoffset;
						}
					}
					if (hFile!=NULL || ppart->filename.Length==0) {
						HANDLE hSection = NULL;
						LARGE_INTEGER sectionlength;
						OBJECT_ATTRIBUTES objattr;
						sectionlength.QuadPart = ppart->parameters.virtmem.sectionoffset + ppart->length;
						InitializeObjectAttributes(&objattr, (PUNICODE_STRING)NULL, OBJ_KERNEL_HANDLE|OBJ_CASE_INSENSITIVE|OBJ_OPENIF, NULL, NULL);
						status = ZwCreateSection(&hSection, 
							((ppart->readonly)? SECTION_MAP_READ|SECTION_QUERY :
							  SECTION_MAP_READ|SECTION_MAP_WRITE|SECTION_QUERY), 
							&objattr, &sectionlength, 
							((ppart->readonly)?    PAGE_READONLY : 
							                       PAGE_READWRITE ),
							0x8000000, hFile);
						if (!NT_SUCCESS(status))
						{  // failed
							DBGPrint0(("FiraDisk:  failed create section status=0x%X\n",status));
							++notreadycount;
						}
						else 
						{
							DBGPrint0(("FiraDisk:  create section success\n"));
							if (hFile)
							{
								ObReferenceObjectByHandle(hFile, (ppart->readonly? FILE_READ_DATA: FILE_WRITE_DATA),
									*IoFileObjectType, KernelMode, (PVOID*)&ppart->pfileobj, NULL);
							}
							ppart->parameters.virtmem.hsection = hSection;
							partready = TRUE;
						}
						// hFile is no longer needed.
						if (hFile) {
							ZwClose(hFile); hFile = NULL; 
						}
					}
				}
				break;
			default:
				DBGPrint0(("FiraDisk:   Unknown part type\n"));
			}// end switch ppart->type
			if (partready) {
				pdoext->totalbytes += ppart->length;
			} else {
				++notreadycount;
			}
		}// for ppart
		if (notreadycount==0) {
			pdoext->totalsectors = (pdoext->totalbytes) >> (pdoext->sectorshift);
			if (pdoext->geometry.Cylinders.QuadPart==0)
			{
				ULONG cylsize = (pdoext->geometry.TracksPerCylinder) * (pdoext->geometry.SectorsPerTrack);
				if (cylsize) pdoext->geometry.Cylinders.QuadPart = (pdoext->totalsectors) / cylsize;
			}
			DBGPrint0(("FiraDisk:  Drive started bytes=%I64d sectors=%I64u b/s=%d\n",
				pdoext->totalbytes,pdoext->totalsectors,1<<pdoext->sectorshift));
			InterlockedExchange(&pdoext->drivestatus, FiraDiskDriveStatusOnline);
		} else {
			InterlockedExchange(&pdoext->drivestatus, FiraDiskDriveStatusOffline);
		}
	}
	return (pdoext->drivestatus == FiraDiskDriveStatusOnline)? STATUS_SUCCESS: STATUS_DEVICE_OFF_LINE;
}

NTSTATUS FiraDiskChildActivateBootDrive( PFIRADISK_CHILD_EXT pdoext )
{
	FiraDiskChildOpenMedia(pdoext, NULL);
	if (pdoext->drivestatus==FiraDiskDriveStatusOffline)
	{
		PWSTR psymlinklist = NULL;
		if (NT_SUCCESS(IoGetDeviceInterfaces(&GUID_DEVINTERFACE_VOLUME, NULL, 0, &psymlinklist)))
		{
			PWSTR ps;
			for (ps=psymlinklist; *ps && pdoext->drivestatus==FiraDiskDriveStatusOffline; ps = wcs_end(ps)+1)
			{
				UNICODE_STRING us;
				RtlInitUnicodeString(&us,ps);
				FiraDiskChildOpenMedia(pdoext, &us);
			}
			ExFreePool(psymlinklist);
		}
	}
	if (pdoext->drivestatus == FiraDiskDriveStatusOnline)
	{
		pdoext->common.started = TRUE;
		pdoext->common.pausestate = 0;
		pdoext->common.removestate = 0;
		DBGPrint0(("FiraDisk:  Drive %p online\n",pdoext));
		return STATUS_SUCCESS;
	} else {
		PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)pdoext->parent->DeviceExtension;
		fdoext->haveofflinedrive = TRUE;
		DBGPrint0(("FiraDisk:  Drive %p offline\n",pdoext));
		return STATUS_DEVICE_NOT_READY;
	}
}


// PASSIVE_LEVEL, system thread
void FiraDiskChildDeleteDevice( PFIRADISK_CHILD_EXT pdoext )
{
	PFIRADISK_IMAGE_PART part, nextpart;
	ULONG partnumber;
	// remove from bus device's childlist
	{
		PFIRADISK_FDO_EXT fdoext = (PFIRADISK_FDO_EXT)(pdoext->parent->DeviceExtension);
		ExAcquireFastMutex(&fdoext->fm_childlist);
		RemoveEntryList(&pdoext->listentry);
		InitializeListHead(&pdoext->listentry);
		ExReleaseFastMutex(&fdoext->fm_childlist);
	}
	// stop thread
	pdoext->common.removestate = 3;
	KeSetEvent(&pdoext->wakethread, 0, FALSE);
	// wait all I/O completion and thread termination
	KeWaitForSingleObject(pdoext->pthreadobj, Executive, KernelMode, FALSE, NULL);
	ObDereferenceObject(pdoext->pthreadobj); 
	pdoext->pthreadobj = 0;
	// free all allocated per-device resource
	for (part=&pdoext->imagepart, partnumber=0; part!=NULL; partnumber++)
	{
		if (part->pfileobj) { 
			ObDereferenceObject(part->pfileobj); 
			part->pfileobj = NULL; 
		}
		switch (part->type)
		{
		//case FiraDiskImageZero:
		//case FiraDiskImagePhysicalMemory:
		//	break;
		case FiraDiskImageVirtualMemory:
			if (part->parameters.virtmem.hsection) {
				ZwClose(part->parameters.virtmem.hsection);
				part->parameters.virtmem.hsection = NULL;
			}
			break;
		case FiraDiskImageFile:
			if (part->parameters.file.hfile)
				ZwClose(part->parameters.file.hfile);
				part->parameters.file.hfile = NULL;
			break;
		}
		nextpart = part->next; 
		if (partnumber==0)
			part->next = NULL;
		else
			ExFreePoolWithTag(part, FIRADISK_POOL_TAG);
		part = nextpart;
	}
	// delete device (only once)
	IoDeleteDevice(pdoext->common.devobj);
}

/*
NTSTATUS FiraDiskChildDispatchPrototype( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: ChildDispatchNotSupported\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return STATUS_SUCCESS;
}
*/

/*NTSTATUS FiraDiskChildAsyncStart( IN PDEVICE_OBJECT  DeviceObject, IN OPTIONAL PIRP Irp )
{
	PFIRADISK_PORT_WORKER_CONTEXT pcontext;
	pcontext = (PFIRADISK_PORT_WORKER_CONTEXT) ExAllocatePoolWithTag(NonPagedPool, 
		sizeof (FIRADISK_PORT_WORKER_CONTEXT), FIRADISK_POOL_TAG);
	if (pcontext)
	{
		pcontext->workitem = IoAllocateWorkItem(DeviceObject);
		if (pcontext->workitem)
		{
			if (NT_SUCCESS(IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, pcontext)))
			{
				pcontext->Irp = Irp;
				pcontext->iost = Irp? IoGetCurrentIrpStackLocation(Irp): NULL;
				if (Irp) IoMarkIrpPending(Irp);
				IoQueueWorkItem(pcontext->workitem,FiraDiskChildStartDeviceWorker,DelayedWorkQueue,pcontext);
				return STATUS_PENDING;
			}
		}
		ExFreePoolWithTag(pcontext, FIRADISK_POOL_TAG);
	}
	return STATUS_INSUFFICIENT_RESOURCES;
}*/
/*
DRIVER_NOTIFICATION_CALLBACK_ROUTINE FiraDiskDINCDisk;
DRIVER_NOTIFICATION_CALLBACK_ROUTINE FiraDiskDINCVolume;

NTSTATUS FiraDiskChildDINCDisk ( IN PVOID NotificationStructure, IN PVOID Context )
{
	return STATUS_SUCCESS;
}

NTSTATUS FiraDiskChildDINCVolume ( IN PVOID NotificationStructure, IN PVOID Context )
{
	// queue a worker thread to call firadiskchildstartdrive
	return STATUS_SUCCESS;
}*/

NTSTATUS FiraDiskChildDispatchPnP( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: ChildDispatchPnP\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	case IRP_MN_START_DEVICE:			//Required
		DBGPrint0(("FiraDisk:  START_DEVICE\n"));
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_QUERY_STOP_DEVICE:		//Required
		DBGPrint0(("FiraDisk:  QUERY_STOP_DEVICE\n"));
		// not support resource rebalancing for now
		Irp->IoStatus.Status = status = STATUS_UNSUCCESSFUL;
		break;
	case IRP_MN_STOP_DEVICE:			//Required
		DBGPrint0(("FiraDisk:  STOP_DEVICE\n"));
		// should not be called unless IRP_MN_QUERY_STOP_DEVICE success
		pdoext->common.pausestate = 2;
		// free hardware resource that can be freed
			// not implemented
		// and complete request
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_CANCEL_STOP_DEVICE:		//Required
		DBGPrint0(("FiraDisk:  CANCEL_STOP_DEVICE\n"));
		pdoext->common.pausestate = 0;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_QUERY_REMOVE_DEVICE:	//Required
		DBGPrint0(("FiraDisk:  QUERY_REMOVE_DEVICE\n"));
		status = STATUS_SUCCESS;
		if (pdoext->common.pagingpathcount)
			status = STATUS_DEVICE_BUSY;
		if (!NT_SUCCESS(status)) {
			// Device cannot be removed
			Irp->IoStatus.Status = status; 
			break;
		}
		// Device can be removed.
		pdoext->common.removestate = 1;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_REMOVE_DEVICE:			//Required, PASSIVE_LEVEL
		DBGPrint0(("FiraDisk:  REMOVE_DEVICE removestate=%d\n",pdoext->common.removestate));
		if (pdoext->common.removestate == 0)
		{ // After failed start
		  // Let child device's function driver delete its FDO. 
		  // Do not delete child PDO until the device is physically removed from bus. 
			Irp->IoStatus.Status = status = STATUS_SUCCESS; 
			break; 
		}
		else if (pdoext->common.removestate == 1)
		{ // After QUERY_REMOVE_DEVICE
		  // Let child device's function driver delete its FDO. 
		  // Do not delete child PDO until the device is physically removed from bus. 
			Irp->IoStatus.Status = status = STATUS_SUCCESS; 
			break; 
		}
		else if (pdoext->common.removestate == 2)
		{ // surprise removed
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
			IoCompleteRequest(Irp, IO_NO_INCREMENT);
			// Now, don't access Irp anymore.
			IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
			// wait all I/O completion
			// remove from bus device's childlist
			// free all allocated per-device resource and delete device object
			FiraDiskChildDeleteDevice(pdoext);
			DBGPrint0(("FiraDisk:  REMOVE_DEVICE end\n"));
			return status;
		}
		else if (pdoext->common.removestate == 3)
		{ // already removed. Don't delete device more than once.
			Irp->IoStatus.Status = status = STATUS_NO_SUCH_DEVICE;
			break; 
		}
		break;
	case IRP_MN_CANCEL_REMOVE_DEVICE:	//Required
		DBGPrint0(("FiraDisk:  CANCEL_REMOVE_DEVICE\n"));
		pdoext->common.removestate = 0;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_SURPRISE_REMOVAL:		//Required
		DBGPrint0(("FiraDisk:  SURPRISE_REMOVAL\n"));
		pdoext->common.removestate = 2;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_QUERY_CAPABILITIES:		//Required
		DBGPrint0(("FiraDisk:  QUERY_CAPABILITIES\n"));
		{
			PDEVICE_CAPABILITIES pdc = iost->Parameters.DeviceCapabilities.Capabilities;
			/// pdc->Address = 0xFFFFFFFF; // bus specific
			pdc->RawDeviceOK = 1;
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
		}
		break;
	case IRP_MN_QUERY_PNP_DEVICE_STATE:	//Optional
		DBGPrint0(("FiraDisk:  QUERY_PNP_DEVICE_STATE\n"));
		// Not Supported. Complete without modifying Irp->IoStatus.Information.
		break;
	case IRP_MN_FILTER_RESOURCE_REQUIREMENTS://No
		DBGPrint0(("FiraDisk:  FILTER_RESOURCE_REQUIREMENTS\n"));
		break;
	case IRP_MN_DEVICE_USAGE_NOTIFICATION:	//Required
		DBGPrint0(("FiraDisk:  DEVICE_USAGE_NOTIFICATION\n"));
		if (iost->Parameters.UsageNotification.InPath){
			if (pdoext->imagepart.type==FiraDiskImageVirtualMemory 
				&& pdoext->imagepart.filename.Length==0)
			{
				DBGPrint0(("FiraDisk:  Pagefile cannot be in virtual memory.\n"));
				Irp->IoStatus.Status = status = STATUS_UNSUCCESSFUL;
			}
			else
			if (pdoext->imagepart.type==FiraDiskImageFile)
			{
				DBGPrint0(("FiraDisk:  Pagefile in file is not supported.\n"));
				Irp->IoStatus.Status = status = STATUS_UNSUCCESSFUL;
			}
			else
			{
				IoAdjustPagingPathCount(&pdoext->common.pagingpathcount, TRUE);
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
			}
		}else{
			IoAdjustPagingPathCount(&pdoext->common.pagingpathcount, FALSE);
			Irp->IoStatus.Status = status = STATUS_SUCCESS;		
		}
		break;
	case IRP_MN_QUERY_DEVICE_RELATIONS:		//Required, PASSIVE_LEVEL
		DBGPrint0(("FiraDisk:  QUERY_DEVICE_RELATIONS type=%d\n",iost->Parameters.QueryDeviceRelations.Type));
		{
			PDEVICE_RELATIONS devrtn, devrtnold;
			int size; 
			devrtnold = (PDEVICE_RELATIONS)(Irp->IoStatus.Information);
			switch (iost->Parameters.QueryDeviceRelations.Type)
			{
			//case BusRelations:			//No
			//case RemovalRelations:		//No
			//case EjectionRelations:		//Optional
			case TargetDeviceRelation:	//Required
				DBGPrint4(("FiraDisk:   Target\n"));
				size = FIELD_OFFSET(DEVICE_RELATIONS, Objects)+sizeof(PDEVICE_OBJECT)*0;
				devrtn = (PDEVICE_RELATIONS)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!devrtn) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				devrtn->Count = 1;
				devrtn->Objects[0] = DeviceObject;
				ObReferenceObject(DeviceObject);
				Irp->IoStatus.Information = (ULONG_PTR)devrtn;
				if (devrtnold)
					ExFreePool(devrtnold);
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS; 
				break;
			}
		}
		break;
	case IRP_MN_QUERY_RESOURCES:			//Required, PASSIVE_LEVEL 
		DBGPrint0(("FiraDisk:  QUERY_RESOURCES\n"));
		status = Irp->IoStatus.Status;
		break;
		/*
		if (pdoext->imagepart.type == FiraDiskImagePhysicalMemory)
		{
			PCM_RESOURCE_LIST res;
			SIZE_T size;
			PHYSICAL_ADDRESS addr;
			ULARGE_INTEGER length;
			ULONG i;
			size = sizeof(CM_RESOURCE_LIST)+sizeof(CM_PARTIAL_RESOURCE_DESCRIPTOR);
			res = (PCM_RESOURCE_LIST) ExAllocatePoolWithTag (PagedPool, size, FIRADISK_POOL_TAG);
			if (!res) {
				// complete request with error
				Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				break;
			}
			RtlZeroMemory(res, size);
			res->Count = 1;
			res->List[0].PartialResourceList.Version = 1;
			res->List[0].PartialResourceList.Revision = 1;
			res->List[0].PartialResourceList.Count = 0;
			addr.QuadPart = pdoext->imagepart.parameters.physmem.physaddr;
			length.QuadPart = pdoext->imagepart.length;
			i = 0;
			if (length.HighPart != 0)
			{
				res->List[0].PartialResourceList.PartialDescriptors[i].Type = CmResourceTypeMemoryLarge;
				res->List[0].PartialResourceList.PartialDescriptors[i].ShareDisposition = CmResourceShareDeviceExclusive;
				res->List[0].PartialResourceList.PartialDescriptors[i].Flags = CM_RESOURCE_MEMORY_READ_WRITE;
				res->List[0].PartialResourceList.PartialDescriptors[i].u.Memory64.Start = addr;
				res->List[0].PartialResourceList.PartialDescriptors[i].u.Memory64.Length64 = length.HighPart;
				addr.HighPart += length.HighPart;
				++i;
			}
			if (length.LowPart != 0)
			{
				res->List[0].PartialResourceList.PartialDescriptors[i].Type = CmResourceTypeMemory;
				res->List[0].PartialResourceList.PartialDescriptors[i].ShareDisposition = CmResourceShareDeviceExclusive;
				res->List[0].PartialResourceList.PartialDescriptors[i].Flags = CM_RESOURCE_MEMORY_READ_WRITE;
				res->List[0].PartialResourceList.PartialDescriptors[i].u.Memory.Start = addr;
				res->List[0].PartialResourceList.PartialDescriptors[i].u.Memory.Length = length.LowPart;
				++i;
			}
			res->List[0].PartialResourceList.Count = i;
			Irp->IoStatus.Information = (ULONG_PTR)res;
			
			// success, complete request
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
		}
		break;*/
	case IRP_MN_QUERY_RESOURCE_REQUIREMENTS://Required, PASSIVE_LEVEL 
		DBGPrint0(("FiraDisk:  QUERY_RESOURCE_REQUIREMENTS\n"));
		status = Irp->IoStatus.Status;
		break;
		/*{
			PIO_RESOURCE_REQUIREMENTS_LIST resreq;
			int size = sizeof(IO_RESOURCE_REQUIREMENTS_LIST);
			resreq = (PIO_RESOURCE_REQUIREMENTS_LIST) ExAllocatePoolWithTag (PagedPool, size, FIRADISK_POOL_TAG);
			if (!resreq) {
				// complete request with error
				Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				break;
			}
			RtlZeroMemory(resreq, size);
			resreq->ListSize = sizeof(IO_RESOURCE_REQUIREMENTS_LIST);
			resreq->AlternativeLists = 1;
			resreq->List[0].Version = 1;
			resreq->List[0].Revision = 1;
			resreq->List[0].Count = 0;
			Irp->IoStatus.Information = (ULONG_PTR)resreq;
			//if (pdoext->imagepart.type == FiraDiskImagePhysicalMemory) {
				// fixme:
			//}
			// success, complete request
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
		}*/
		break;
	case IRP_MN_QUERY_ID:					//Required
		DBGPrint0(("FiraDisk:  QUERY_ID  IdType=%d class=%d\n",iost->Parameters.QueryId.IdType,pdoext->sclass));
		{
			switch (iost->Parameters.QueryId.IdType)
			{
				PWCHAR pws, src;
				int size;
			case BusQueryDeviceID:		//Required
				// "FIRADISK\\" + {"Disk","RemovableDisk","CdRom","SFloppy"}
				switch (pdoext->sclass)
				{
				case FiraDiskSClassDisk:
					if (pdoext->geometry.MediaType==RemovableMedia) {
						size = sizeof(FiraDiskDeviceIDRemovableDisk);
						src = FiraDiskDeviceIDRemovableDisk;
					} else {
						size = sizeof(FiraDiskDeviceIDDisk);
						src = FiraDiskDeviceIDDisk;
					}
					break;
				case FiraDiskSClassCDROM:
					size = sizeof(FiraDiskDeviceIDCdrom);
					src = FiraDiskDeviceIDCdrom;
					break;
				case FiraDiskSClassFloppy:
					size = sizeof(FiraDiskDeviceIDFloppy);
					src = FiraDiskDeviceIDFloppy;
					break;
				default:
					size = sizeof(EmptyMultiString);
					src = EmptyMultiString;
					break;
				}
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				RtlCopyMemory(pws, src, size);				
				DBGPrint0(("FiraDisk:   DeviceID=%ls\n",pws));
				Irp->IoStatus.Information = (ULONG_PTR)pws;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			case BusQueryHardwareIDs:	//Optional
				// "GenDisk" "GenCDROM" "GenSFloppy" ""
				switch (pdoext->sclass)
				{
				case FiraDiskSClassDisk:
					size = sizeof(FiraDiskHwIDDisk);
					src = FiraDiskHwIDDisk;
					break;
				case FiraDiskSClassCDROM:
					size = sizeof(FiraDiskHwIDCdrom);
					src = FiraDiskHwIDCdrom;
					break;
				case FiraDiskSClassFloppy:
					size = sizeof(FiraDiskHwIDFloppy);
					src = FiraDiskHwIDFloppy;
					break;
				default:
					size = sizeof(EmptyMultiString);
					src = EmptyMultiString;
					break;
				}
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				RtlCopyMemory(pws, src, size);				
				DBGPrint0(("FiraDisk:   HwID=%ls\n",pws));
				Irp->IoStatus.Information = (ULONG_PTR)pws;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			case BusQueryCompatibleIDs:	//Optional
				// "GenDisk" "GenCDROM" "GenSFloppy" ""
				switch (pdoext->sclass)
				{
				case FiraDiskSClassDisk:
					size = sizeof(FiraDiskCompatIDDisk);
					src = FiraDiskCompatIDDisk;
					break;
				case FiraDiskSClassCDROM:
					size = sizeof(FiraDiskCompatIDCdrom);
					src = FiraDiskCompatIDCdrom;
					break;
				case FiraDiskSClassFloppy:
					size = sizeof(FiraDiskCompatIDFloppy);
					src = FiraDiskCompatIDFloppy;
					break;
				default:
					size = sizeof(EmptyMultiString);
					src = EmptyMultiString;
					break;
				}
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				RtlCopyMemory(pws, src, size);				
				DBGPrint0(("FiraDisk:   CompatID=%ls\n",pws));
				Irp->IoStatus.Information = (ULONG_PTR)pws;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			case BusQueryInstanceID:	//Optional
				// unique ID for device with unique ID
				// bus number for device without unique ID
				size = 20*sizeof(WCHAR);
				pws = (PWCHAR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!pws) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				RtlStringCbPrintfW(pws,size,L"%d.%d",
					pdoext->imagepart.type,pdoext->drivenumber);
				DBGPrint0(("FiraDisk:   InstanceID=%ls\n",pws));
				Irp->IoStatus.Information = (ULONG_PTR)pws;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			}
		}
		break;
	case IRP_MN_QUERY_DEVICE_TEXT:		//Required
		DBGPrint0(("FiraDisk:  QUERY_DEVICE_TEXT\n"));
		switch (iost->Parameters.QueryDeviceText.DeviceTextType)
		{
		case DeviceTextDescription:
			switch (iost->Parameters.QueryDeviceText.LocaleId)
			{
				int size,len1,len2;
				PCWSTR txtimage,txtclass;
				PWSTR ptext;
			default:
			case 0x00000409:
				txtimage = wsTextImageType[(pdoext->imagepart.type<4)? pdoext->imagepart.type: 0];
				txtclass = (pdoext->sclass==FiraDiskSClassDisk && pdoext->geometry.MediaType==RemovableMedia)? wsTextRemovableDisk :
					wsTextSClass[(pdoext->sclass<4)? pdoext->sclass: 0];
				len1 = wcslen(txtimage); len2 = wcslen(txtclass);
				size = (len1+1+len2+1)*sizeof(WCHAR);
				ptext = (PWSTR)ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
				if (!ptext) {
					// complete request with error
					Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
					break;
				}
				memcpy(ptext, txtimage, len1*sizeof(WCHAR));
				ptext[len1] = L' ';
				memcpy(ptext+len1+1, txtclass, (len2+1)*sizeof(WCHAR));
				DBGPrint0(("FiraDisk:   DeviceText=%ls\n",ptext));
				Irp->IoStatus.Information = (ULONG_PTR)ptext;
				// success, complete request
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			}
			break;
		case DeviceTextLocationInformation:
			break;
		}
		break;
	case IRP_MN_QUERY_BUS_INFORMATION:	//Required
		DBGPrint0(("FiraDisk:  QUERY_BUS_INFORMATION\n"));
		{
			int size;
			PPNP_BUS_INFORMATION pbusinfo;
			size = sizeof(PNP_BUS_INFORMATION);
			pbusinfo = (PPNP_BUS_INFORMATION) ExAllocatePoolWithTag(PagedPool,size,FIRADISK_POOL_TAG_PNP);
			if (!pbusinfo) {
				// complete request with error
				Irp->IoStatus.Status = status = STATUS_INSUFFICIENT_RESOURCES;
				break;
			}
			pbusinfo->BusTypeGuid = guid_bustype_firadisk;
			pbusinfo->LegacyBusType = PNPBus;
			pbusinfo->BusNumber = 0;
			Irp->IoStatus.Information = (ULONG_PTR)pbusinfo;
			// success, complete request
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
		}
		break;
	case IRP_MN_QUERY_INTERFACE:		//Required
		DBGPrint0(("FiraDisk:  QUERY_INTERFACE\n"));
		break;
	case IRP_MN_READ_CONFIG:			//Required
		DBGPrint0(("FiraDisk:  READ_CONFIG\n"));
		//status = Irp->IoStatus.Status = STATUS_NOT_SUPPORTED;
		break;
	case IRP_MN_WRITE_CONFIG:			//Required
		DBGPrint0(("FiraDisk:  WRITE_CONFIG\n"));
		//status = Irp->IoStatus.Status = STATUS_NOT_SUPPORTED;
		break;
	case IRP_MN_EJECT:					//Required
		DBGPrint0(("FiraDisk:  EJECT\n"));
		//Irp->IoStatus.Information = 0;
		//Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MN_SET_LOCK:				//Required
		DBGPrint0(("FiraDisk:  SET_LOCK\n"));
		Irp->IoStatus.Information = 0;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;	
		break;
	default:
		DBGPrint0(("FiraDisk:  unknown MinorFunction 0x%x\n",iost->MinorFunction));
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return status;
}


NTSTATUS FiraDiskChildDispatchPower( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint0(("FiraDisk: ChildDispatchPower\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		PoStartNextPowerIrp(Irp);
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MinorFunction)
	{
	case IRP_MN_POWER_SEQUENCE: // Optional
		Irp->IoStatus.Status = status = STATUS_NOT_IMPLEMENTED;
		break;
	case IRP_MN_QUERY_POWER:
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		PoStartNextPowerIrp(Irp); // must call this for each IRP_MN_QUERY_POWER, IRP_MN_SET_POWER in Win2K-2003
		break;
	case IRP_MN_SET_POWER:
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		PoStartNextPowerIrp(Irp); // must call this for each IRP_MN_QUERY_POWER, IRP_MN_SET_POWER in Win2K-2003
		break;
	case IRP_MN_WAIT_WAKE:
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return STATUS_SUCCESS;
}

NTSTATUS FiraDiskChildDispatchDeviceControl( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint4(("FiraDisk: ChildDispatchDeviceControl\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->Parameters.DeviceIoControl.IoControlCode)
	{
	case IOCTL_SCSI_GET_ADDRESS:
		DBGPrint0(("FiraDisk:  IOCTL_SCSI_GET_ADDRESS\n"));
		{
			ULONG length = min(sizeof(SCSI_ADDRESS),iost->Parameters.DeviceIoControl.OutputBufferLength);
			SCSI_ADDRESS scsiaddr;
			scsiaddr.Length = sizeof(SCSI_ADDRESS);
			scsiaddr.PortNumber = 0;
			scsiaddr.PathId = (UCHAR)(pdoext->drivenumber);
			scsiaddr.TargetId = 0;
			scsiaddr.Lun = 0;
			RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer, &scsiaddr, length);
			Irp->IoStatus.Information = length;
			Irp->IoStatus.Status = status = 
				(length==sizeof(SCSI_ADDRESS))? STATUS_SUCCESS: STATUS_DATA_OVERRUN;
		}
		break;
	case IOCTL_STORAGE_QUERY_PROPERTY:
		DBGPrint0(("FiraDisk:  IOCTL_STORAGE_QUERY_PROPERTY\n"));
		if (iost->Parameters.DeviceIoControl.InputBufferLength < sizeof(STORAGE_PROPERTY_QUERY))
		{
			DBGPrint0(("FiraDisk:  invalid parameter\n"));
			Irp->IoStatus.Status = status = STATUS_INVALID_PARAMETER;
		} else {
			PSTORAGE_PROPERTY_QUERY query = (PSTORAGE_PROPERTY_QUERY)(Irp->AssociatedIrp.SystemBuffer);
			PSTORAGE_DESCRIPTOR_HEADER desch = (PSTORAGE_DESCRIPTOR_HEADER)(Irp->AssociatedIrp.SystemBuffer);
			DBGPrint0(("FiraDisk:   propid=%d querytype=%d\n",query->PropertyId,query->QueryType));
			switch (query->PropertyId)
			{
			case StorageDeviceProperty:
				if (query->QueryType==PropertyStandardQuery) 
				{
					STORAGE_DEVICE_DESCRIPTOR devdes = {
						sizeof(STORAGE_DEVICE_DESCRIPTOR),
						sizeof(STORAGE_DEVICE_DESCRIPTOR),
						(pdoext->sclass==FiraDiskSClassCDROM)? READ_ONLY_DIRECT_ACCESS_DEVICE :
						(pdoext->sclass==FiraDiskSClassDisk)? DIRECT_ACCESS_DEVICE:
						(pdoext->sclass==FiraDiskSClassFloppy)? DIRECT_ACCESS_DEVICE:
						DIRECT_ACCESS_DEVICE,
						0, // no modifier
						(pdoext->sclass==FiraDiskSClassCDROM || pdoext->sclass==FiraDiskSClassFloppy),
						FALSE, // command queueing
						0, // vendor id offset
						0, // product id offset
						0, // product revision offset
						0, // serial number offset
						BusTypeVirtual,
						0,
						{0}
					};
					ULONG_PTR length = min(sizeof(STORAGE_DEVICE_DESCRIPTOR),iost->Parameters.DeviceIoControl.OutputBufferLength);
					RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer, &devdes, length);
					Irp->IoStatus.Information = length;
					Irp->IoStatus.Status = status = STATUS_SUCCESS;					
					DBGPrint0(("FiraDisk:   STORAGE_DEVICE_DESCRIPTOR length=%d/%d\n",length,sizeof(STORAGE_DEVICE_DESCRIPTOR)));
				}
				break;
			case StorageAdapterProperty:
				if (query->QueryType==PropertyStandardQuery) 
				{
					STORAGE_ADAPTER_DESCRIPTOR adapdes = {
						sizeof(STORAGE_ADAPTER_DESCRIPTOR),
						sizeof(STORAGE_ADAPTER_DESCRIPTOR),
						FIRADISK_MAX_TRANSFER_LENGTH,
						FIRADISK_MAX_PHYSICAL_PAGES, // max physical pages
						0,       // 0=byte aligned, 7=QWORD align
						TRUE,    // use PIO, require system-space virtual address mapped for data buffer
						FALSE,   // scans down
						FALSE,   // command queueing
						FALSE,   // accelerated tx
						BusTypeVirtual,
						0,       // bus major version
						0        // bus minor version
					};
					ULONG_PTR length = min(sizeof(STORAGE_ADAPTER_DESCRIPTOR),iost->Parameters.DeviceIoControl.OutputBufferLength);
					RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer, &adapdes, length);
					Irp->IoStatus.Information = length;
					Irp->IoStatus.Status = status = STATUS_SUCCESS;					
					DBGPrint0(("FiraDisk:   STORAGE_ADAPTER_DESCRIPTOR length=%d/%d\n",length,sizeof(STORAGE_ADAPTER_DESCRIPTOR)));
				}
				break;
			default:
				Irp->IoStatus.Status = status = STATUS_NOT_IMPLEMENTED;
				break;
			}
		}
		break;
	case IOCTL_DISK_GET_DRIVE_GEOMETRY: // optional
		{
			PDISK_GEOMETRY geo = &pdoext->geometry;
			if (geo->BytesPerSector && geo->SectorsPerTrack && geo->TracksPerCylinder) {
				ULONG length = min(sizeof(DISK_GEOMETRY),iost->Parameters.DeviceIoControl.OutputBufferLength);
				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer, geo, length);
				Irp->IoStatus.Information = length;
				Irp->IoStatus.Status = status = 
					(length==sizeof(DISK_GEOMETRY))? STATUS_SUCCESS : STATUS_BUFFER_TOO_SMALL;
				DBGPrint0(("FiraDisk:  Disk geometry  m=%d b=%u s=%u t=%u c=%I64d\n",
					geo->MediaType,geo->BytesPerSector,geo->SectorsPerTrack,geo->TracksPerCylinder,geo->Cylinders.QuadPart));
			} else {
				Irp->IoStatus.Status = status = STATUS_NOT_SUPPORTED;
				DBGPrint2(("FiraDisk:  No disk geometry\n"));
			}
		}
		break;
	default:
		DBGPrint0(("FiraDisk:  Unknown IoControlCode 0x%x\n",iost->Parameters.DeviceIoControl.IoControlCode));
		Irp->IoStatus.Status = status = STATUS_INVALID_PARAMETER;
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return status;
}

NTSTATUS FiraDiskChildDispatchSystemControl( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	NTSTATUS status;
	DBGPrint0(("FiraDisk: ChildDispatchSystemControl\n"));
	status = Irp->IoStatus.Status;
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return status;
}

NTSTATUS FiraDiskChildDispatchCreateClose( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext;
	PIO_STACK_LOCATION iost;
	NTSTATUS status;
	DBGPrint4(("FiraDisk: ChildDispatchCreateClose\n"));
	if (IoAcquireRemoveLock(&((PFIRADISK_DEV_EXT)(DeviceObject->DeviceExtension))->removelock, Irp) 
		!= STATUS_SUCCESS)
	{
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		DBGPrint0(("FiraDisk:  device removed\n"));
		return status;
	}
	pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	iost = IoGetCurrentIrpStackLocation(Irp);
	status = Irp->IoStatus.Status;
	switch (iost->MajorFunction)
	{
	case IRP_MJ_CREATE:
		DBGPrint3(("FiraDisk:  CREATE\n"));
		Irp->IoStatus.Information = 0;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	case IRP_MJ_CLOSE:
		DBGPrint3(("FiraDisk:  CLOSE\n"));
		Irp->IoStatus.Information = 0;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return STATUS_SUCCESS;
}

NTSTATUS FiraDiskChildDispatchReadWrite( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	NTSTATUS status;
	DBGPrint0(("FiraDisk: ChildDispatchReadWrite\n"));
	status = Irp->IoStatus.Status;
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return status;
}

NTSTATUS FiraDiskChildDispatchSCSI( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	PFIRADISK_CHILD_EXT pdoext = (PFIRADISK_CHILD_EXT)(DeviceObject->DeviceExtension);
	PIO_STACK_LOCATION iost;
	PSCSI_REQUEST_BLOCK Srb;
	NTSTATUS status;
	DBGPrint4(("FiraDisk: ChildDispatchSCSI\n"));
	if (IoAcquireRemoveLock(&pdoext->common.removelock, Irp) != STATUS_SUCCESS)
	{
		DBGPrint0(("FiraDisk:  device removed\n"));
		Irp->IoStatus.Status = status = STATUS_DEVICE_REMOVED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return status;
	}
	iost = IoGetCurrentIrpStackLocation(Irp);
	Srb = iost->Parameters.Scsi.Srb;
	status = Irp->IoStatus.Status;
	if (!Srb) {
		DBGPrint0(("FiraDisk:  DispatchSCSI no Srb\n"));
		Irp->IoStatus.Status = status = STATUS_INVALID_PARAMETER;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
		return status;
	}
	switch (Srb->Function)
	{
	case SRB_FUNCTION_EXECUTE_SCSI:
	case SRB_FUNCTION_IO_CONTROL:
	case SRB_FUNCTION_SHUTDOWN:
	case SRB_FUNCTION_FLUSH:
		DBGPrint3(( "FiraDisk:  SRB_FUNCTION %d (queued)\n",Srb->Function));
		IoMarkIrpPending(Irp);
		FiraDiskChildQueueInsert(pdoext, Irp);
		return STATUS_PENDING;
	case SRB_FUNCTION_CLAIM_DEVICE:
		DBGPrint1(("FiraDisk:  SRB_FUNCTION_CLAIM_DEVICE\n"));
		//if (InterlockedCompareExchange(&pdoext->claimed,1,0))
		//{
		//	ObReferenceObject(DeviceObject);
		//}
			Srb->DataBuffer = DeviceObject;
			Srb->SrbStatus = SRB_STATUS_SUCCESS;
			Irp->IoStatus.Status = status = STATUS_SUCCESS;
		//} else {
		//	Srb->DataBuffer = NULL;
		//	Srb->SrbStatus = SRB_STATUS_BUSY;
		//	Irp->IoStatus.Status = status = STATUS_DEVICE_BUSY;
		//}
		break;
	case SRB_FUNCTION_RELEASE_DEVICE:
		DBGPrint1(("FiraDisk:  SRB_FUNCTION_RELEASE_DEVICE\n"));
		//if (InterlockedCompareExchange(&pdoext->claimed,0,1))
		//{
		//	ObDereferenceObject(DeviceObject);
		//}
		Srb->SrbStatus = SRB_STATUS_SUCCESS;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		break;
	default:
		DBGPrint0(("FiraDisk:  SRB_FUNCTION unknown %d\n",Srb->Function));
		Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
		Irp->IoStatus.Status = status = STATUS_NOT_IMPLEMENTED;
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	return STATUS_SUCCESS;
}

VOID     FiraDiskChildStartIo( IN PDEVICE_OBJECT  DeviceObject, IN PIRP  Irp )
{
	DBGPrint0(("FiraDisk: ChildStartIo\n"));
}

#ifdef _AMD64_
#define FiraDiskChildCopyMemory FiraDiskCopyMemory
#else
#define FiraDiskChildCopyMemory pdoext->copymemory
#endif

NTSTATUS FiraDiskChildSCSIReadWrite( PFIRADISK_CHILD_EXT pdoext, PIRP  Irp )
{
	PIO_STACK_LOCATION iost = IoGetCurrentIrpStackLocation(Irp);
	PSCSI_REQUEST_BLOCK Srb = iost->Parameters.Scsi.Srb;
	PCDB cdb = (PCDB)(Srb->Cdb);
	UCHAR opread = (cdb->AsByte[0]==SCSIOP_READ   || cdb->AsByte[0]==SCSIOP_READ16 );
	//UCHAR opwrite = (cdb->AsByte[0]==SCSIOP_WRITE  || cdb->AsByte[0]==SCSIOP_WRITE16);
	NTSTATUS status = STATUS_UNSUCCESSFUL;
	UINT64 startlba;
	UINT32 sectorcount;
	UINT64 partoffset;
	UINT32 rqlength, txlength;
	PCHAR sysbuffer;
	PFIRADISK_IMAGE_PART ppart;
	DBGPrint4(("FiraDisk:  ChildSCSIReadWrite\n"));
	Irp->IoStatus.Information = 0;

	// get position and length
	if (cdb->AsByte[0]==SCSIOP_READ || cdb->AsByte[0]==SCSIOP_WRITE)
	{
		startlba = RtlUlongByteSwap(*(UNALIGNED PULONG)&cdb->CDB10.LogicalBlockByte0);
		sectorcount = RtlUshortByteSwap(*(UNALIGNED PUSHORT)&cdb->CDB10.TransferBlocksMsb);
	} else {
		startlba = RtlUlonglongByteSwap(*(UNALIGNED PUINT64)&cdb->READ16.LogicalBlock[0]);
		sectorcount = RtlUlongByteSwap(*(UNALIGNED PULONG)&cdb->READ16.TransferLength[0]);
	}
	// check for invalid offset
	if ((INT64)startlba < 0 || startlba > pdoext->totalsectors)
	{
		Irp->IoStatus.Information = Srb->DataTransferLength = 0;
		Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
		Irp->IoStatus.Status = status = STATUS_INVALID_PARAMETER;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
		DBGPrint0(("FiraDisk:  Invalid read/write offset.\n"));
		return status;
	}
	// check for write to read-only disk
	if (!opread && pdoext->readonly)
	{
		Irp->IoStatus.Information = Srb->DataTransferLength = 0;
		Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
		Irp->IoStatus.Status = status = STATUS_MEDIA_WRITE_PROTECTED;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
		DBGPrint0(("FiraDisk:  Cannot write to read-only disk.\n"));
		return status;
	}
	// complete zero-length read/write
	if (sectorcount==0)
	{
		Irp->IoStatus.Information = Srb->DataTransferLength = 0;
		Srb->SrbStatus = SRB_STATUS_SUCCESS;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
		DBGPrint0(("FiraDisk:  Zero-length transfer completed.\n"));
		return status;
	}
	// do real work
	partoffset = startlba << pdoext->sectorshift;
	rqlength = sectorcount << pdoext->sectorshift;
	txlength = 0;
	ppart = &pdoext->imagepart;
	DBGPrint4(("FiraDisk:   sector start=%I64d count=%d  byte offset=0x%I64x rqlength=0x%x\n",startlba,sectorcount,partoffset,rqlength));
	sysbuffer = (PCHAR) MmGetSystemAddressForMdlSafe(Irp->MdlAddress, NormalPagePriority);
	if (sysbuffer)
	{
		PCHAR txbuffer = sysbuffer + ((PCHAR)(Srb->DataBuffer)-(PCHAR)MmGetMdlVirtualAddress(Irp->MdlAddress));
		while ( ppart && rqlength > 0 ) 
		{
			//DBGPrint4(("FiraDisk:   buffer=0x%Ix  ppart=0x%Ix len=%I64x  ofs=0x%I64x len=0x%x\n",txbuffer,ppart,ppart?ppart->length:0,partoffset,rqlength));
			while ( ppart && partoffset >= ppart->length ) 
			{
				partoffset -= ppart->length;
				ppart = ppart->next;
				//DBGPrint4(("FiraDisk:   buffer=0x%Ix  ppart=0x%Ix len=%I64x  ofs=0x%I64x len=0x%x\n",txbuffer,ppart,ppart?ppart->length:0,partoffset,rqlength));
			}
			if (ppart)
			{
				ULONG rqpart = (ULONG) min(rqlength, ppart->length-partoffset);
				ULONG txpart = 0;
				DBGPrint4(("FiraDisk:   buffer=0x%Ix  ppart=0x%Ix len=%I64x  ofs=0x%I64x len=0x%x\n",txbuffer,ppart,ppart->length,partoffset,rqlength));
				switch (ppart->type)
				{
				case FiraDiskImageZero:
					txpart = rqpart;
					if (opread)
						RtlZeroMemory(txbuffer, txpart); // Read zero
					//else
					//	; // Ignore write
					break;
				case FiraDiskImagePhysicalMemory:
					{
						LARGE_INTEGER tximageoffset;
						PCHAR viewbase;
						SIZE_T viewsize = min(rqpart, FIRADISK_VIEW_SIZE);
						tximageoffset.QuadPart = ppart->parameters.physmem.physaddr + partoffset;
						DBGPrint4(("FiraDisk:   map view     physaddr=0x%I64x viewsize=0x%Ix\n",
							tximageoffset.QuadPart,viewsize));
						viewbase = (PCHAR)MmMapIoSpace(tximageoffset, viewsize, MmCached);
						if (viewbase)
						{
							UINT_PTR viewoffset = (UINT_PTR)(ppart->parameters.virtmem.sectionoffset + partoffset - tximageoffset.QuadPart);
							DBGPrint4(("FiraDisk:   map success  sectionoffset=0x%I64x viewsize=0x%Ix viewbase=0x%Ix\n",tximageoffset.QuadPart,viewsize,viewbase));
							txpart = (ULONG) min(rqpart, viewsize-viewoffset);
							if (opread)
								FiraDiskChildCopyMemory( txbuffer, viewbase+viewoffset, txpart );
							else
								FiraDiskChildCopyMemory( viewbase+viewoffset, txbuffer, txpart );
							DBGPrint4(("FiraDisk:   copy data completed.\n"));
							MmUnmapIoSpace(viewbase, viewsize);
						} else {
							DBGPrint0(("FiraDisk:   map failed."));
						}
					}
					break;
				case FiraDiskImageVirtualMemory:
					if (ppart->parameters.virtmem.hsection)
					{
						LARGE_INTEGER tximageoffset;
						PCHAR viewbase;
						SIZE_T viewsize = min(rqpart, FIRADISK_VIEW_SIZE);
						tximageoffset.QuadPart = ppart->parameters.virtmem.sectionoffset + partoffset;
						{
							ULONG alignoffset = tximageoffset.LowPart & (FIRADISK_MAP_ALIGNMENT-1);
							tximageoffset.LowPart -= alignoffset;
							viewsize += alignoffset;
						}
						DBGPrint4(("FiraDisk:   map view     sectionoffset=0x%I64x viewsize=0x%Ix hsection=%Ix\n",
							tximageoffset.QuadPart,viewsize,ppart->parameters.virtmem.hsection));
						viewbase = NULL;
						status = ZwMapViewOfSection( ppart->parameters.virtmem.hsection, ZwCurrentProcess(), (PVOID*)(PCHAR*)&viewbase, 0, 0,
							&tximageoffset, &viewsize, ViewUnmap, MEM_RESERVE,
							((ppart->readonly)?    PAGE_READONLY : PAGE_READWRITE )
							);
						if (NT_SUCCESS(status))
						{
							UINT_PTR viewoffset;
							DBGPrint4(("FiraDisk:   map success  sectionoffset=0x%I64x viewsize=0x%Ix viewbase=0x%Ix\n",tximageoffset.QuadPart,viewsize,viewbase));
							viewoffset = (UINT_PTR)(ppart->parameters.virtmem.sectionoffset + partoffset - tximageoffset.QuadPart);
							txpart = (ULONG) min(rqpart, viewsize-viewoffset);
							if (opread)
								FiraDiskChildCopyMemory( txbuffer, viewbase+viewoffset, txpart );
							else
								FiraDiskChildCopyMemory( viewbase+viewoffset, txbuffer, txpart );
							DBGPrint4(("FiraDisk:   copy data completed.\n"));
							ZwUnmapViewOfSection(ZwCurrentProcess(), viewbase);
						} else {
							DBGPrint0(("FiraDisk:   map failed  status=0x%X\n",status));
						}
					} else {
						status = STATUS_DEVICE_NOT_READY;
						DBGPrint0(("FiraDisk:   read/write hfile is NULL\n"));
					}
					break;
				case FiraDiskImageFile:
					if (ppart->parameters.file.hfile)
					{
						LARGE_INTEGER tximageoffset;
						PVOID buffer = NULL;
						IO_STATUS_BLOCK iosb;
						HANDLE hFile;
						tximageoffset.QuadPart = ppart->parameters.file.fileoffset + partoffset;
						iosb.Information = 0;
						DBGPrint4(("FiraDisk:   File transfer offset=0x%I64x\n",tximageoffset.QuadPart));
						hFile = ppart->parameters.file.hfile;
						status = (opread? ZwReadFile : ZwWriteFile)
							(hFile, NULL, NULL, NULL, &iosb, txbuffer, rqpart,
								&tximageoffset, NULL);
						txpart = (ULONG)iosb.Information;
						if (!NT_SUCCESS(status)) {
							DBGPrint0(("FiraDisk:   ZwXxxFile  status=0x%X\n",status));
						}
					} else {
						status = STATUS_DEVICE_NOT_READY;
						DBGPrint0(("FiraDisk:   read/write hsection is NULL\n"));
					}
				}
				if (txpart == 0) break;
				txlength += txpart;
				txbuffer += txpart;
				rqlength -= txpart;
				partoffset += txpart;
			}
		}
	} else {
		DBGPrint0(("FiraDisk:   map buffer to system address failed\n"));
	}
	if (rqlength == 0) {
		Srb->SrbStatus = SRB_STATUS_SUCCESS;
		status = STATUS_SUCCESS; 
	} else {
		Srb->SrbStatus = (status==STATUS_DEVICE_NOT_READY)? SRB_STATUS_BUSY: SRB_STATUS_INTERNAL_ERROR;
	}
	Irp->IoStatus.Information = Srb->DataTransferLength = txlength; 
	Irp->IoStatus.Status = status; 
	IoCompleteRequest(Irp, 1);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
	DBGPrint4(("FiraDisk:   complete txlength=%d\n",txlength));
	return status;
}

typedef struct _FIRADISK_MAP_VIEW_CONTEXT
{
	LARGE_INTEGER position;
	ULONG_PTR viewsize;
	PUCHAR viewbase;
} FIRADISK_MAP_VIEW_CONTEXT, *PFIRADISK_MAP_VIEW_CONTEXT;

// system thread I/O SCSI
VOID FiraDiskChildSTIOSCSI ( PFIRADISK_CHILD_EXT pdoext, IN PIRP  Irp, PFIRADISK_MAP_VIEW_CONTEXT pmvc )
{
	PIO_STACK_LOCATION iost;
	PSCSI_REQUEST_BLOCK Srb;
	NTSTATUS status;
	DBGPrint4(("FiraDisk: FiraDiskChildSTIOSCSI\n"));
	iost = IoGetCurrentIrpStackLocation(Irp);
	Srb = iost->Parameters.Scsi.Srb;
	status = Irp->IoStatus.Status;
	switch (Srb->Function)
	{
	case SRB_FUNCTION_EXECUTE_SCSI:
		/*if (pdoext->drivestatus != FiraDiskDriveStatusOnline) {
			Srb->SrbStatus = SRB_STATUS_BUSY;
			Irp->IoStatus.Status = status = STATUS_DEVICE_NOT_READY;
			DBGPrint3(("FiraDisk:  EXECUTE_SCSI device not ready\n"));
			FiraDiskChildAsyncStart( pdoext->common.devobj, NULL );
		} else*/ {
			PCDB cdb = (PCDB)(Srb->Cdb);
			UCHAR scsiop = cdb->AsByte[0];
			DBGPrint3(("FiraDisk:  EXECUTE_SCSI 0x%02x\n", scsiop));
			switch(scsiop)
			{
			case SCSIOP_TEST_UNIT_READY:
				Srb->SrbStatus = SRB_STATUS_SUCCESS;
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			case SCSIOP_READ:		DBGPrint4(("FiraDisk:   READ\n")); goto label_readwrite;
			case SCSIOP_WRITE:  	DBGPrint4(("FiraDisk:   WRITE\n")); goto label_readwrite;
			case SCSIOP_READ16: 	DBGPrint4(("FiraDisk:   READ16\n")); goto label_readwrite;
			case SCSIOP_WRITE16:	DBGPrint4(("FiraDisk:   WRITE16\n")); goto label_readwrite;
				label_readwrite:	
				FiraDiskChildSCSIReadWrite(pdoext, Irp);
				return;
			case SCSIOP_VERIFY:		DBGPrint4(("FiraDisk:   VERIFY\n")); goto label_verify;
			case SCSIOP_VERIFY16:	DBGPrint4(("FiraDisk:   VERIFY16\n")); goto label_verify;
				label_verify:
				Srb->SrbStatus = SRB_STATUS_SUCCESS;
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			case SCSIOP_READ_CAPACITY:
				if (Srb->DataTransferLength < sizeof(READ_CAPACITY_DATA)) {
					Irp->IoStatus.Information = Srb->DataTransferLength = 0;
					Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
					Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
					DBGPrint0(("FiraDisk:   READ_CAPACITY data overrun\n"));
				} else {
					UNALIGNED PREAD_CAPACITY_DATA capd = (PREAD_CAPACITY_DATA)(Srb->DataBuffer);
					ULARGE_INTEGER maxlba; maxlba.QuadPart = pdoext->totalsectors-1;
					if (maxlba.HighPart) maxlba.LowPart = 0xffffffff;
					capd->BytesPerBlock = RtlUlongByteSwap(pdoext->geometry.BytesPerSector);
					capd->LogicalBlockAddress = RtlUlongByteSwap(maxlba.LowPart);
					Irp->IoStatus.Information = sizeof(READ_CAPACITY_DATA);
					Srb->SrbStatus = SRB_STATUS_SUCCESS;
					Irp->IoStatus.Status = status = STATUS_SUCCESS;
					DBGPrint3(("FiraDisk:   READ_CAPACITY success maxlba32=%u  b/s=%u\n",maxlba.LowPart,pdoext->geometry.BytesPerSector));
				}
				break;
			case SCSIOP_READ_CAPACITY16:
				if (Srb->DataTransferLength < sizeof(READ_CAPACITY_DATA_EX)) {
					Irp->IoStatus.Information = Srb->DataTransferLength = 0;
					Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
					Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
					DBGPrint0(("FiraDisk:   READ_CAPACITY16 data overrun\n"));
				} else {
					UNALIGNED PREAD_CAPACITY_DATA_EX capdx = (PREAD_CAPACITY_DATA_EX)(Srb->DataBuffer);
					ULARGE_INTEGER maxlba; maxlba.QuadPart = pdoext->totalsectors-1;
					capdx->BytesPerBlock = RtlUlongByteSwap(pdoext->geometry.BytesPerSector);
					capdx->LogicalBlockAddress.QuadPart = RtlUlonglongByteSwap(maxlba.QuadPart);
					Irp->IoStatus.Information = sizeof(READ_CAPACITY_DATA_EX);
					Srb->SrbStatus = SRB_STATUS_SUCCESS;
					Irp->IoStatus.Status = status = STATUS_SUCCESS;
					DBGPrint3(("FiraDisk:   READ_CAPACITY16 success maxlba64=%I64u  b/s=%u\n",maxlba.QuadPart,pdoext->geometry.BytesPerSector));
				}
				break;
			case SCSIOP_MODE_SENSE:
				DBGPrint2(("FiraDisk:   MODE_SENSE  pagecode=%d  pc=%d\n",
					cdb->MODE_SENSE.PageCode,cdb->MODE_SENSE.Pc));
				if (Srb->DataTransferLength < sizeof(MODE_PARAMETER_HEADER)) {
					Irp->IoStatus.Information = Srb->DataTransferLength = 0;
					Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
					Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
					DBGPrint0(("FiraDisk:   MODE_SENSE data overrun\n"));
				} else {
					PUCHAR pb = (PUCHAR)Srb->DataBuffer;
					UNALIGNED PMODE_PARAMETER_HEADER mph = (PMODE_PARAMETER_HEADER)pb; 
					ULONG size = sizeof(MODE_PARAMETER_HEADER);
					switch (cdb->MODE_SENSE.PageCode)
					{
					case MODE_PAGE_FLEXIBILE: 
						{
							UNALIGNED PMODE_FLEXIBLE_DISK_PAGE mfdp = (PMODE_FLEXIBLE_DISK_PAGE)(pb+size); 
							size += offsetof(MODE_FLEXIBLE_DISK_PAGE, StartWritePrecom);
							if (Srb->DataTransferLength < size) {
								DBGPrint0(("FiraDisk:    MODE_PAGE_FLEXIBILE data overrun\n"));
								Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
								Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
								break;
							}
							RtlZeroMemory(Srb->DataBuffer, size);
							mph->ModeDataLength = (UCHAR)(size-1);
							mph->MediumType = 0;
							mph->DeviceSpecificParameter = 0;
							mph->BlockDescriptorLength = 0;
							mfdp->PageCode = MODE_PAGE_FLEXIBILE;
							mfdp->PageLength = offsetof(MODE_FLEXIBLE_DISK_PAGE, StartWritePrecom)-2;
							mfdp->NumberOfHeads = (UCHAR)(pdoext->geometry.TracksPerCylinder);
							mfdp->SectorsPerTrack = (UCHAR)(pdoext->geometry.SectorsPerTrack);
							mfdp->BytesPerSector[0] = (UCHAR)(pdoext->geometry.BytesPerSector >> 8);
							mfdp->BytesPerSector[1] = (UCHAR)(pdoext->geometry.BytesPerSector     );
							mfdp->NumberOfCylinders[0] = (UCHAR)(pdoext->geometry.Cylinders.LowPart >> 8);
							mfdp->NumberOfCylinders[1] = (UCHAR)(pdoext->geometry.Cylinders.LowPart     );
							Irp->IoStatus.Information = Srb->DataTransferLength = size;
							Srb->SrbStatus = SRB_STATUS_SUCCESS;
							Irp->IoStatus.Status = status = STATUS_SUCCESS;
							break;
						}
						break;
					default:
						mph->ModeDataLength = sizeof(MODE_PARAMETER_HEADER);
						mph->MediumType = 0;
						mph->DeviceSpecificParameter = 0;
						mph->BlockDescriptorLength = 0;
						Irp->IoStatus.Information = Srb->DataTransferLength = sizeof(MODE_PARAMETER_HEADER);
						Srb->SrbStatus = SRB_STATUS_SUCCESS;
						Irp->IoStatus.Status = status = STATUS_SUCCESS;
						break;
					}
					DBGPrint3(("FiraDisk:   MODE_SENSE success\n"));
				}
				break;
			case SCSIOP_FORMAT_UNIT:
				Irp->IoStatus.Information = 0;
				Srb->SrbStatus = SRB_STATUS_SUCCESS;
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				DBGPrint3(("FiraDisk:   FORMAT_UNIT success\n"));
			case SCSIOP_MEDIUM_REMOVAL:
				Irp->IoStatus.Information = 0;
				Srb->SrbStatus = SRB_STATUS_SUCCESS;
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				DBGPrint3(("FiraDisk:   MEDIUM_REMOVAL success\n"));
				break;
			case SCSIOP_READ_TOC:
				DBGPrint4(("FiraDisk:   READ_TOC %d %d\n",cdb->READ_TOC.Format,cdb->READ_TOC.StartingTrack));
				{
					CDROM_TOC toc; ULONG length;
					RtlZeroMemory(&toc, sizeof(CDROM_TOC));
					*(PUSHORT)(&toc.Length[0]) = RtlUshortByteSwap(sizeof(CDROM_TOC)-2);
					toc.FirstTrack = 1;
					toc.LastTrack = 1;
					toc.TrackData[0].Control = 4;
					toc.TrackData[0].TrackNumber = 1;
					length = min(sizeof(CDROM_TOC),Srb->DataTransferLength);
					RtlCopyMemory(Srb->DataBuffer, &toc, length);
					Irp->IoStatus.Information = Srb->DataTransferLength = length;
					if (length==sizeof(CDROM_TOC)) {
						Srb->SrbStatus = SRB_STATUS_SUCCESS;
						Irp->IoStatus.Status = status = STATUS_SUCCESS;
					} else {
						Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
						Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
					}
					DBGPrint4(("FiraDisk:   READ_TOC\n"));
				}
				break;
			case SCSIOP_INQUIRY:
				DBGPrint1(("FiraDisk:   INQUERY\n"));
				{
					INQUIRYDATA data; ULONG length;
					RtlZeroMemory(&data, sizeof(INQUIRYDATA));
					data.DeviceType = (pdoext->sclass == FiraDiskSClassCDROM)? OPTICAL_DEVICE : DIRECT_ACCESS_DEVICE;
					data.AdditionalLength = INQUIRYDATABUFFERSIZE - 4;
					length = min(sizeof(INQUIRYDATA),Srb->DataTransferLength);
					RtlCopyMemory(Srb->DataBuffer, &data, length);
					Irp->IoStatus.Information = Srb->DataTransferLength = length;
					if (length==sizeof(INQUIRYDATA)) {
						Srb->SrbStatus = SRB_STATUS_SUCCESS;
						Irp->IoStatus.Status = status = STATUS_SUCCESS;
					} else {
						Srb->SrbStatus = SRB_STATUS_DATA_OVERRUN;
						Irp->IoStatus.Status = status = STATUS_DATA_OVERRUN;
					}
				}
				Srb->SrbStatus = SRB_STATUS_SUCCESS;
				Irp->IoStatus.Status = status = STATUS_SUCCESS;
				break;
			default:
				DBGPrint0(("FiraDisk:   SCSIOP unknown 0x%02x\n",scsiop));
				Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
				Irp->IoStatus.Status = status = STATUS_NOT_IMPLEMENTED;
				break;
			}
		}
		break;
    case SRB_FUNCTION_IO_CONTROL:
		DBGPrint1(("FiraDisk:  SRB_FUNCTION_IO_CONTROL\n"));
		Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
		Irp->IoStatus.Status = status = STATUS_NOT_SUPPORTED;
		break;
	case SRB_FUNCTION_SHUTDOWN:
	case SRB_FUNCTION_FLUSH:
		DBGPrint1(( (Srb->Function==SRB_FUNCTION_FLUSH)? 
			"FiraDisk:  SRB_FUNCTION_FLUSH\n":
			"FiraDisk:  SRB_FUNCTION_SHUTDOWN\n"));
		// wait until all previous SCSI I/O completed before completing this request
			// all previous SCSI I/O already completed before this code run
		// flush underlying file.
		{
			PFIRADISK_IMAGE_PART ppart = &pdoext->imagepart;
			while (ppart)
			{
				if (ppart->pfileobj)
				{
					IO_STATUS_BLOCK iosb;
					KEVENT ke;
					PIRP Irp2;
					PFILE_OBJECT pfileobj = ppart->pfileobj;
					KeInitializeEvent(&ke, NotificationEvent, FALSE);
					Irp2 = IoBuildSynchronousFsdRequest(IRP_MJ_FLUSH_BUFFERS, pfileobj->DeviceObject, NULL, 0, NULL, &ke, &iosb);
					if (Irp2)
					{
						PIO_STACK_LOCATION Irp2Sp = IoGetNextIrpStackLocation(Irp2);
						Irp2Sp->FileObject = pfileobj;
						if (IoCallDriver(pfileobj->DeviceObject,Irp2) == STATUS_PENDING)
						{
							KeWaitForSingleObject(&ke, Executive, KernelMode, FALSE, NULL);
						}
						// Irp2 is automatically freed after IoCompleteRequest is called
					}
				}
				ppart = ppart->next;
			}
		}
		Srb->SrbStatus = SRB_STATUS_SUCCESS;
		Irp->IoStatus.Status = status = STATUS_SUCCESS;
		// complete
		break;
	default:
		DBGPrint0(("FiraDisk:  SRB_FUNCTION unknown %d\n",Srb->Function));
		Srb->SrbStatus = SRB_STATUS_INVALID_REQUEST;
		Irp->IoStatus.Status = status = STATUS_NOT_IMPLEMENTED;
		break;
	}
	// default -- complete request
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
}


// driver created system thread
// this function runs in PASSIVE_LEVEL
VOID FiraDiskChildThreadStart(__in PVOID StartContext)
{
	PFIRADISK_CHILD_EXT pdoext = (PFIRADISK_CHILD_EXT)StartContext;
	DBGPrint0(("FiraDisk: ThreadStart %p\n",pdoext));
	if (IoAcquireRemoveLock(&pdoext->common.removelock, FiraDiskChildThreadStart) == STATUS_SUCCESS)
	{
		UCHAR removed;
		FIRADISK_MAP_VIEW_CONTEXT mapviewcontext;
#ifndef _AMD64_
		KFLOATING_SAVE floatingsave;
		NTSTATUS status_floatingsave = sse2_available? KeSaveFloatingPointState(&floatingsave): STATUS_NOT_SUPPORTED;
		if (NT_SUCCESS(status_floatingsave))
			pdoext->copymemory = FiraDiskCopyMemorySSE2;
#endif
		RtlZeroMemory(&mapviewcontext, sizeof(mapviewcontext));
		removed = 0;
		/*DBGPrint0(("FiraDisk:  ActivateDrive %p\n",pdoext));
		FiraDiskChildActivateDrive(pdoext);
		
		if (pdoext->drivestatus==FiraDiskDriveStatusOffline)
		{
			IoInitializeRemoveLock(&pdoext->volumelist_rmlock,0,0,0);
			if (NT_SUCCESS(IoAcquireRemoveLock(&pdoext->volumelist_rmlock, FiraDiskChildThreadStart)))
			{
				PVOID notifentry = NULL;
				DBGPrint0(("FiraDisk:  Register volume notification %p\n",pdoext));
				if (NT_SUCCESS( IoRegisterPlugPlayNotification ( EventCategoryDeviceInterfaceChange,
						PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES, (PVOID)&GUID_DEVINTERFACE_VOLUME,
						pdoext->common.devobj->DriverObject,
						FiraDiskChildVolumeListNotifCallback, pdoext, &notifentry )
					))
				{
					while (pdoext->common.removestate<=1 && pdoext->drivestatus==FiraDiskDriveStatusOffline)
					{
						PLIST_ENTRY ple; 
						KeWaitForSingleObject(&pdoext->wakethread, Executive, KernelMode, FALSE, NULL);
						KeClearEvent(&pdoext->wakethread);
						while ( (ple = ExInterlockedRemoveHeadList(&pdoext->volumelist_head,&pdoext->volumelist_lock))
								!= NULL )
						{
							PVOLUMELIST_ENTRY pvle = CONTAINING_RECORD(ple,VOLUMELIST_ENTRY,entry);
							//DBGPrint0(("FiraDisk:  Volume : %wZ\n",&pvle->name));
							FiraDiskChildOpenMedia(pdoext, &pvle->name);
							ExFreePoolWithTag(pvle,FIRADISK_POOL_TAG);
						}
						FiraDiskChildOpenMedia(pdoext, NULL);
					}
					IoUnregisterPlugPlayNotification(notifentry);
				}
				IoReleaseRemoveLockAndWait(&pdoext->volumelist_rmlock, FiraDiskChildThreadStart);
				// cleanup unused volumelist entry
				{
					PLIST_ENTRY ple; 
					while ( (ple = ExInterlockedRemoveHeadList(&pdoext->volumelist_head,&pdoext->volumelist_lock))
							!= NULL )
					{
						ExFreePoolWithTag(ple,FIRADISK_POOL_TAG);
					}
				}
			}
		}
		*/
		while (pdoext->common.removestate<=1)
		{
			KeWaitForSingleObject(&pdoext->wakethread, Executive, KernelMode, FALSE, NULL);
			KeClearEvent(&pdoext->wakethread);
			switch (pdoext->common.removestate)
			{
			case 0: // normal working state
			case 1: // query remove
				switch (pdoext->common.pausestate)
				{
				case 0: // normal working state
				case 1: // query stop
					{
						UCHAR queueempty = 0;
						do // while (!queueempty)
						{
							// remove job from queue
							PIRP Irp = FiraDiskChildQueueRemove(pdoext);
							if (Irp)
							{
								switch (IoGetCurrentIrpStackLocation(Irp)->MajorFunction)
								{
									case IRP_MJ_SCSI:
										FiraDiskChildSTIOSCSI(pdoext, Irp, &mapviewcontext);
										break;
									default: // Invalid queued request
										Irp->IoStatus.Status = STATUS_INTERNAL_ERROR;
										IoCompleteRequest(Irp, IO_NO_INCREMENT);
										IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
								}
								KeClearEvent(&pdoext->wakethread); // continue
							}
							else // queue is empty
							{
								queueempty = 1;
							}
						} while (!queueempty);
					}
					break;
				case 2: // stopped
					break;
				}
				break;
			case 2: // surprise removed
			case 3: // removed, being deleted
				//removed = 1; // quit outer loop
				break;
			}
		}
		// Device is removed or being removed.
		{	// Fail all remaining IRP in queue
			UCHAR queueempty = 0;
			do // while (!queueempty);
			{
				// remove job from queue
				PIRP Irp = FiraDiskChildQueueRemove(pdoext);
				if (Irp)
				{	// fail IRP
					Irp->IoStatus.Status = STATUS_DEVICE_REMOVED;
					IoCompleteRequest(Irp, IO_NO_INCREMENT);
					IoReleaseRemoveLock(&pdoext->common.removelock, Irp);
				}
				else
				{
					queueempty = 1;
				}
			} while (!queueempty);
			removed = 1; // quit outer loop
		}
		// wait for other IRP to complete.
		IoReleaseRemoveLockAndWait(&pdoext->common.removelock, FiraDiskChildThreadStart);
#ifndef _AMD64_
		if (NT_SUCCESS(status_floatingsave))
		{
			pdoext->copymemory = FiraDiskCopyMemory;
			KeRestoreFloatingPointState(&floatingsave);
		}
#endif
	}
	PsTerminateSystemThread(0);
}
