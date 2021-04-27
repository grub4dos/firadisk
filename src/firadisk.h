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
// firadisk.h

#include <ntifs.h>
#include <ntddstor.h>
#include <ntdddisk.h>
#include <ntddcdrm.h>
#include <ntddk.h>
//#include <wdm.h>

#if (NTDDI_VERSION < NTDDI_WINXP)
  #define NTSTRSAFE_LIB
#endif

#include <ntstrsafe.h>
#include <wdmguid.h>
#include "firaguid.h"

#define FIRADISK_MAP_ALIGNMENT 0x10000
// 0x10000=64K
#define FIRADISK_VIEW_SIZE 0x400000
// 0x400000=4M
#define FIRADISK_MAX_TRANSFER_LENGTH 0x400000
// 0x400000=4M
#define FIRADISK_MAX_PHYSICAL_PAGES 1024
// 1024*4096=4M
// must be <= (65535 - sizeof(MDL)) / sizeof(ULONG_PTR)
// which is 16376 for 32-bit and 8185 for 64-bit

#ifdef DBG
#define DBGPrint0(xx) (DbgPrint xx )
#define DBGPrint1(xx) (DbgPrint xx )
#define DBGPrint2(xx) (DbgPrint xx )
#define DBGPrint3(xx) 0
#define DBGPrint4(xx) 0
#define DBGCODE(xx)   xx
#else
#define DBGPrint0(xx) 0
#define DBGPrint1(xx) 0
#define DBGPrint2(xx) 0
#define DBGPrint3(xx) 0
#define DBGPrint4(xx) 0
#define DBGCODE(xx)   0
#endif

enum {
    FIRADISK_POOL_TAG_PNP = 'PriF', // FirP
    FIRADISK_POOL_TAG = 'ariF' // Fira
};

//typedef enum _FIRADISK_DISK_MAPTYPE {
//	FIRADISK_DISK_MAP_UNKNOWN = 0,
//	FIRADISK_DISK_MAP_MEMORY,
//	FIRADISK_DISK_MAP_SECTION
//} FIRADISK_DISK_MAPTYPE;

typedef struct FIRADISK_DEV_CLASS {
	PDRIVER_DISPATCH 
		DispatchPnP, 
		DispatchPower, 
		DispatchDeviceControl, 
		DispatchSystemControl,
		DispatchCreate, 
		DispatchClose, 
		DispatchRead, 
		DispatchWrite,
		DispatchInternalDeviceControl;
	PDRIVER_STARTIO 
		StartIo;
} FIRADISK_DEV_CLASS, *PFIRADISK_DEV_CLASS;

enum FIRADISK_IMAGE_PART_TYPE { 
	FiraDiskImageZero=0, 
	FiraDiskImagePhysicalMemory, 
	FiraDiskImageVirtualMemory, 
	FiraDiskImageFile 
};

enum FIRADISK_STORAGE_CLASS { 
	FiraDiskSClassUnknown=0, 
	FiraDiskSClassDisk, 
	FiraDiskSClassCDROM, 
	FiraDiskSClassFloppy, 
};

#define IMAGE_PART_NAME_BUFFER_SIZE 256

typedef struct _FIRADISK_IMAGE_PART *PFIRADISK_IMAGE_PART;
typedef struct _FIRADISK_IMAGE_PART {
	PFIRADISK_IMAGE_PART next;
	UCHAR type,readonly,copyonwrite;
	//UCHAR mediumopened; // 0=not opened, 1=disk opened, 2=file opened
	UNICODE_STRING filename;
	UNICODE_STRING loadfilename;
	UINT64 length;
	UINT64 unopened_fileoffset;
	//UINT64 unopened_diskoffset;
	union _PARAMETERS {
		struct _PHYSMEM {
			UINT64 physaddr;
		} physmem;
		struct _VIRTMEM {
			UINT64 sectionoffset;
			HANDLE hsection;
		} virtmem;
		struct _FILE {
			UINT64 fileoffset;
			HANDLE hfile;
		} file;
	} parameters;
	PFILE_OBJECT pfileobj;
	PVOID pnotification_volume;
	PVOID pnotification_disk;
	WCHAR namebuffer[IMAGE_PART_NAME_BUFFER_SIZE];
} FIRADISK_IMAGE_PART;

#define FiraDiskSectorSizeDisk     512
#define FiraDiskSectorSizeCDROM   2048
#define FiraDiskSectorSizeFloppy   512
#define FiraDiskSectorSizeLogDisk    9
#define FiraDiskSectorSizeLogCDROM  11
#define FiraDiskSectorSizeLogFloppy  9
#define GRUB4DOSMemSectorSizeLog  9

typedef struct _FIRADISK_RAM_DRIVE_PARAMETERS
{
	INT64 address;
	INT64 length;
	UCHAR sectorsizelog;
	UCHAR sectorpertrack;
	UCHAR trackpercylinder;
	UCHAR sclass:4, fakewrite:1, readonly:1;
} FIRADISK_RAM_DRIVE_PARAMETERS, *PFIRADISK_RAM_DRIVE_PARAMETERS;

typedef struct _FIRADISK_DEV_EXT {
	PFIRADISK_DEV_CLASS devclass;
	PDEVICE_OBJECT devobj;
	IO_REMOVE_LOCK removelock; // prevent deletion of device object while handling IRP
	UCHAR started;
	UCHAR pausestate;	// 0=not paused, 1=paused, 2=stopped
	UCHAR removestate;	// 0=not removed, 1=to be removed, 2=surprise removed or physically removed, 3=device objected deleted
	LONG  pagingpathcount;
} FIRADISK_DEV_EXT, *PFIRADISK_DEV_EXT;

enum FIRADISK_DRIVE_STATUS
{
	FiraDiskDriveStatusNotInitialized = 0,
	FiraDiskDriveStatusOffline,
	FiraDiskDriveStatusStarting,
	FiraDiskDriveStatusOnline
};

typedef struct _FIRADISK_FDO_EXT {
	FIRADISK_DEV_EXT common;
	PDEVICE_OBJECT lowerDO;
	LIST_ENTRY buslistentry;
	LIST_ENTRY childlist, removedchildlist;
	FAST_MUTEX fm_childlist;
	UCHAR detectdisk;
	UCHAR haveofflinedrive;
	LONG nextdrivenumber;
	PVOID notifentry1, notifentry2, notifentry3;
} FIRADISK_FDO_EXT, *PFIRADISK_FDO_EXT;

typedef struct _FIRADISK_DRIVE_PARAMETERS 
{
	UCHAR enumtype;
	ULONG enumid;
	UCHAR devicetype;
} FIRADISK_DRIVE_PARAMETERS, *PFIRADISK_DRIVE_PARAMETERS;

typedef struct _LIST_OF_VOLUMES
{
	LIST_ENTRY entry;
	PDEVICE_OBJECT volume_devobj;
} LIST_OF_VOLUME, *PLIST_OF_VOLUMES;

typedef void COPY_MEMORY_ROUTINE(PVOID dst, PVOID src, SIZE_T size);

typedef struct _FIRADISK_CHILD_EXT {
	FIRADISK_DEV_EXT common;
	PDEVICE_OBJECT parent;
	LIST_ENTRY listentry;
	LIST_ENTRY irpqueue_head;
	KSPIN_LOCK irpqueue_lock;
	LIST_ENTRY volumelist_head;
	KSPIN_LOCK volumelist_lock;
	IO_REMOVE_LOCK volumelist_rmlock;
	PIO_WORKITEM pworkitem_activatedrive;
	UNICODE_STRING name;
	PKTHREAD pthreadobj;
	KEVENT wakethread;
	DISK_GEOMETRY geometry;
	COPY_MEMORY_ROUTINE *copymemory;
	UINT64 totalbytes, totalsectors;
	UCHAR sectorshift;
	UCHAR sclass;
	UCHAR readonly;
	UCHAR isbootdrive;
	// drivestatus, drivenumber must be LONG, used with InterlockedXxx
	LONG drivestatus; // 0=not initialized, 1=offline, 2=starting, 3=ready
	LONG drivenumber;
	LONG claimed;
	FIRADISK_IMAGE_PART imagepart;
} FIRADISK_CHILD_EXT, *PFIRADISK_CHILD_EXT;


typedef struct _INTRVECT { 
	UINT16 offset; UINT16 segment; 
} INTRVECT, *PINTRVECT;

#define GRUB4DOS_DRIVE_MAP_MAX 8
typedef struct _GRUB4DOS_DRIVE_MAP_SLOT {
	unsigned char from_drive;
	unsigned char to_drive;			// 0xFF indicates a memdrive
	unsigned char max_head;
	unsigned char max_sector:6,
		disable_lba:1,		// bit 6: disable lba
		read_only:1;		// bit 7: read only 
	unsigned short to_cylinder:13,	// max cylinder of the TO drive
		from_cdrom:1,		// bit 13: FROM drive is CDROM(with big 2048-byte sector)
		to_cdrom:1,			// bit 14:  TO  drive is CDROM(with big 2048-byte sector)
		to_support_lba:1;	// bit 15:  TO  drive support LBA
	unsigned char to_head;			// max head of the TO drive
	unsigned char to_sector:6,		// max sector of the TO drive
		fake_write:1,		// bit 6: fake-write or safe-boot
		in_situ:1;			// bit 7: in-situ
	UINT64 start_sector;
	UINT64 sector_count;
} GRUB4DOS_DRIVE_MAP_SLOT, *PGRUB4DOS_DRIVE_MAP_SLOT;

#pragma pack(push,1)
typedef struct _MEMDISK_MDI
{
	unsigned short length;
	unsigned char  ver_minor, ver_major;
	unsigned long  diskbuf;
	unsigned long  disksize;	
	unsigned char  unused1[18];
	// MDI structuer end
	unsigned char  unused2[6];
	unsigned short cylinder;
	unsigned short heads;
	unsigned long  sectors;
	unsigned char  unused3[8];
	unsigned char  driveno;
} MEMDISK_MDI, *PMEMDISK_MDI;

typedef struct _MEMDISK_MBFT
{
	unsigned long acpisignature;
	unsigned long length;
	unsigned char acpirevision;
	unsigned char checksum;
	unsigned char oemid[6];
	unsigned char oemtableid[8];
	unsigned long oemrevision;
	unsigned long acpiaslcvid;
	unsigned long acpiaslcrev;
	unsigned long safehook_addr;
	MEMDISK_MDI mdi;
} MEMDISK_MBFT, *PMEMDISK_MBFT;
#pragma pack(pop)

#define CBSIZEOF(str) (sizeof(str))
#define CCSIZEOF(str) (sizeof(str)/sizeof(*(str)))

#define FIRADISKMAXKEYPATH 256

#define DECLA_CASTRING(name,string) const ANSI_STRING name = {sizeof(string)-sizeof( CHAR),sizeof(string),string};
#define DECLA_CUSTRING(name,string) const UNICODE_STRING name = {sizeof(string)-sizeof(WCHAR),sizeof(string),string};
#define DECLG_CASTRING(name,string) extern const __declspec(selectany) ANSI_STRING name = {sizeof(string)-sizeof( CHAR),sizeof(string),string};
#define DECLG_CUSTRING(name,string) extern const __declspec(selectany) UNICODE_STRING name = {sizeof(string)-sizeof(WCHAR),sizeof(string),string};

// constant
extern const WCHAR wsBkSlash[];
extern const WCHAR objPathRegMachine[];
extern const WCHAR keyPathSysCurControl[];
extern const WCHAR keyNameFiraDisk[];
extern const WCHAR keyNameVolatileData[];
extern const WCHAR keyNameParameters[];
extern const WCHAR wsfValueNameDetectedRAMDrive[];
extern const WCHAR wsFiraDiskOptionPrefix[];

DECLG_CUSTRING(usVNPnP,L"PnP");
DECLG_CUSTRING(usVNSystemStartOptions, L"SystemStartOptions");
DECLG_CUSTRING(usVNStartOptions, L"StartOptions");

// variables
extern FIRADISK_DEV_CLASS FiraDiskFDOClass;
extern FIRADISK_DEV_CLASS FiraDiskChildClassSCSI;
extern LONG offlinediskcount;
extern LIST_ENTRY buslisthead;
extern FAST_MUTEX buslistlock;
extern UCHAR afterboot;
extern UNICODE_STRING driverRegistryPath;
extern ULONG configDisableDetectGrub4dos   ;
extern ULONG configDisableDetectMemdisk    ;
extern ULONG configDisableDetectedRAMDrives;
extern GRUB4DOS_DRIVE_MAP_SLOT drive_map_table[8];

#ifndef _AMD64_
extern UCHAR sse2_available;
#endif

// function declarations
NTSTATUS DriverEntry( IN PDRIVER_OBJECT  DriverObject, IN PUNICODE_STRING  RegistryPath );
DRIVER_UNLOAD     FiraDiskUnload; 
DRIVER_ADD_DEVICE FiraDiskAddDevice;
DRIVER_DISPATCH   FiraDiskDispatch;
DRIVER_STARTIO    FiraDiskStartIo;

DRIVER_DISPATCH   FiraDiskFDODispatchPnP;
DRIVER_DISPATCH   FiraDiskFDODispatchPower;
DRIVER_DISPATCH   FiraDiskFDODispatchDeviceControl;
DRIVER_DISPATCH   FiraDiskFDODispatchSystemControl;
DRIVER_DISPATCH   FiraDiskFDODispatchCreateClose;
DRIVER_DISPATCH   FiraDiskFDODispatchReadWrite;
DRIVER_DISPATCH   FiraDiskFDODispatchInternalDeviceControl;
DRIVER_STARTIO    FiraDiskFDOStartIo;

DRIVER_DISPATCH   FiraDiskChildDispatchPnP;
DRIVER_DISPATCH   FiraDiskChildDispatchPower;
DRIVER_DISPATCH   FiraDiskChildDispatchDeviceControl;
DRIVER_DISPATCH   FiraDiskChildDispatchSystemControl;
DRIVER_DISPATCH   FiraDiskChildDispatchCreateClose;
DRIVER_DISPATCH   FiraDiskChildDispatchReadWrite;
DRIVER_DISPATCH   FiraDiskChildDispatchSCSI;
DRIVER_DISPATCH   FiraDiskChildDispatchFloppy;
DRIVER_STARTIO    FiraDiskChildStartIo;

NTSTATUS FiraDiskFDOAddDevice( IN PDRIVER_OBJECT  DriverObject, IN PDEVICE_OBJECT  PhysicalDeviceObject, PDEVICE_OBJECT *pfdo );
NTSTATUS FiraDiskBusStart ( IN PDEVICE_OBJECT fdo );
void FiraDiskChildInitialize(PDEVICE_OBJECT pdo, PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, PDISK_GEOMETRY geometry);
void FiraDiskChildDeleteDevice( PFIRADISK_CHILD_EXT pdoext );
NTSTATUS FiraDiskCreateZeroDisk       ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive );
NTSTATUS FiraDiskCreateFileDisk       ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive );
NTSTATUS FiraDiskCreateVirtualMemDisk ( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 offset,   PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive );
NTSTATUS FiraDiskCreatePhysicalMemDisk( IN PDEVICE_OBJECT fdo, UCHAR sclass, UINT64 length, UINT64 physaddr, PUNICODE_STRING filename, PDISK_GEOMETRY geometry, BOOLEAN updatebus, BOOLEAN isbootdrive );
NTSTATUS FiraDiskChildActivateDrive( PFIRADISK_CHILD_EXT pdoext );
NTSTATUS FiraDiskChildActivateBootDrive( PFIRADISK_CHILD_EXT pdoext );

COPY_MEMORY_ROUTINE FiraDiskCopyMemory;
KSTART_ROUTINE FiraDiskChildThreadStart;


