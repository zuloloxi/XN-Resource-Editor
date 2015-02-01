unit NTDDK;

interface

uses
  Windows, SysUtils;
const
  STATUS_SUCCESS = 0;
  STATUS_BUFFER_OVERFLOW = $80000005;
  STATUS_INFO_LENGTH_MISMATCH = $C0000004;

  OBJ_INHERIT             = $00000002;
  OBJ_PERMANENT           = $00000010;
  OBJ_EXCLUSIVE           = $00000020;
  OBJ_CASE_INSENSITIVE    = $00000040;
  OBJ_OPENIF              = $00000080;
  OBJ_OPENLINK            = $00000100;
  OBJ_KERNEL_HANDLE       = $00000200;
  OBJ_VALID_ATTRIBUTES    = $000003F2;

  IRP_MJ_MAXIMUM_FUNCTION = $1b;
  MAXIMUM_VOLUME_LABEL_LENGTH = 32 * sizeof(WideChar);

  TDI_TRANSPORT_ADDRESS_FILE  = 1;
  TDI_CONNECTION_FILE = 2;
  TDI_CONTROL_CHANNEL_FILE = 3;


type
  DEVICE_TYPE = DWORD;

  POOL_TYPE = (
    NonPagedPool,
    PagedPool,
    NonPagedPoolMustSucceed,
    DontUseThisType,
    NonPagedPoolCacheAligned,
    PagedPoolCacheAligned,
    NonPagedPoolCacheAlignedMustS,
    MaxPoolType);

  SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation,
    SystemProcessorInformation,             // obsolete...delete
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemPathInformation,
    SystemProcessInformation,
    SystemCallCountInformation,
    SystemDeviceInformation,
    SystemProcessorPerformanceInformation,
    SystemFlagsInformation,
    SystemCallTimeInformation,
    SystemModuleInformation,
    SystemLocksInformation,
    SystemStackTraceInformation,
    SystemPagedPoolInformation,
    SystemNonPagedPoolInformation,
    SystemHandleInformation,
    SystemObjectInformation,
    SystemPageFileInformation,
    SystemVdmInstemulInformation,
    SystemVdmBopInformation,
    SystemFileCacheInformation,
    SystemPoolTagInformation,
    SystemInterruptInformation,
    SystemDpcBehaviorInformation,
    SystemFullMemoryInformation,
    SystemLoadGdiDriverInformation,
    SystemUnloadGdiDriverInformation,
    SystemTimeAdjustmentInformation,
    SystemSummaryMemoryInformation,
    SystemNextEventIdInformation,
    SystemEventIdsInformation,
    SystemCrashDumpInformation,
    SystemExceptionInformation,
    SystemCrashDumpStateInformation,
    SystemKernelDebuggerInformation,
    SystemContextSwitchInformation,
    SystemRegistryQuotaInformation,
    SystemExtendServiceTableInformation,
    SystemPrioritySeperation,
    SystemPlugPlayBusInformation,
    SystemDockInformation,
    SystemPowerInformation,
    SystemProcessorSpeedInformation,
    SystemCurrentTimeZoneInformation,
    SystemLookasideInformation);

  FILE_INFORMATION_CLASS = (
    FileDirectoryInformation       = 1,
    FileFullDirectoryInformation,   // 2
    FileBothDirectoryInformation,   // 3
    FileBasicInformation,           // 4  wdm
    FileStandardInformation,        // 5  wdm
    FileInternalInformation,        // 6
    FileEaInformation,              // 7
    FileAccessInformation,          // 8
    FileNameInformation,            // 9
    FileRenameInformation,          // 10
    FileLinkInformation,            // 11
    FileNamesInformation,           // 12
    FileDispositionInformation,     // 13
    FilePositionInformation,        // 14 wdm
    FileFullEaInformation,          // 15
    FileModeInformation,            // 16
    FileAlignmentInformation,       // 17
    FileAllInformation,             // 18
    FileAllocationInformation,      // 19
    FileEndOfFileInformation,       // 20 wdm
    FileAlternateNameInformation,   // 21
    FileStreamInformation,          // 22
    FilePipeInformation,            // 23
    FilePipeLocalInformation,       // 24
    FilePipeRemoteInformation,      // 25
    FileMailslotQueryInformation,   // 26
    FileMailslotSetInformation,     // 27
    FileCompressionInformation,     // 28
    FileObjectIdInformation,        // 29
    FileCompletionInformation,      // 30
    FileMoveClusterInformation,     // 31
    FileQuotaInformation,           // 32
    FileReparsePointInformation,    // 33
    FileNetworkOpenInformation,     // 34
    FileAttributeTagInformation,    // 35
    FileTrackingInformation,        // 36
    FileIdBothDirectoryInformation, // 37
    FileIdFullDirectoryInformation, // 38
    FileMaximumInformation);


  KTHREAD               = record end; PKTHREAD               = ^KTHREAD;
  EPROCESS              = record end; PEPROCESS              = ^EPROCESS;
  ETHREAD               = record end; PETHREAD               = ^ETHREAD;
  PEB                   = record end; PPEB                   = ^PEB;
  KINTERRUPT            = record end; PKINTERRUPT            = ^KINTERRUPT;
  IO_TIMER              = record end; PIO_TIMER              = ^IO_TIMER;
  OBJECT_TYPE           = record end; POBJECT_TYPE           = ^OBJECT_TYPE;
  DEVICE_HANDLER_OBJECT = record end; PDEVICE_HANDLER_OBJECT = ^DEVICE_HANDLER_OBJECT;
  BUS_HANDLER           = record end; PBUS_HANDLER           = ^BUS_HANDLER;

  PMDL = ^TMDL;
  TMDL = packed record
    Next : PMDL;
    Size : word;
    mdlFlags : word;
    Process : PEPROCESS;
    MappedSystemVa : Pointer;
    StartVa : Pointer;
    ByteCount : DWORD;
    ByteOffset : DWORD
  end;

  ERESOURCE_THREAD = PDWORD;
  PERESOURCE_THREAD = ^ERESOURCE_THREAD;

  KSPIN_LOCK = PDWORD;
  PKSPIN_LOCK = ^KSPIN_LOCK;

  TOwnerEntry = packed record
    OwnerThread : ERESOURCE_THREAD;
    case Integer of
      0 : (OwnerCount : LongInt);
      1 : (TableSize : DWORD)
  end;
  POwnerEntry = ^TOwnerEntry;

  TDispatcherHeader = packed record
    _Type : Byte;
    _Absolute : Byte;
    Size : Byte;
    Inserted : Byte;
    SignalState : LongInt;
    WaitListHead : TListEntry
  end;

  KEVENT = packed record
    Header : TDispatcherHeader
  end;
  PKEVENT = ^KEVENT;

  KSEMAPHORE  = packed record
    Header : TDispatcherHeader;
    Limit : LongInt
  end;
  PKSEMAPHORE = ^KSEMAPHORE;

  ERESOURCE = packed record
    SystemResourceList : TListEntry;
    OwnerTable : POwnerEntry;
    ActiveCount : SmallInt;
    Flag : Word;
    SharedWaiters : PKSEMAPHORE;
    ExclusiveWaiters : PKEVENT;
    OwnerThreads : array [0..1] of TOwnerEntry;
    ContentionCount : DWORD;
    NumberOfSharedWaiters : Word;
    NumberOfExclusiveWaiters : word;
    a : record case Integer of
      0 : (Address : Pointer);
      1 : (CreatorBacktraceIndex : PDWORD)
    end;
    SpinLock : KSPIN_LOCK
  end;
  PERESOURCE = ^ERESOURCE;

  TUnicodeString = packed record
    Length : Word;
    MaximumLength : Word;
    Buffer : PWideChar
  end;
  PUnicodeString = ^TUnicodeString;

  TClientID = record
    UniqueProcess : THandle;
    UniqueThread : THandle
  end;

  TObjectAttributes = record
    Length : DWORD;
    RootDirectory : THandle;
    ObjectName : PUnicodeString;
    Attributes : DWORD;
    SecurityDescriptor : pointer;        // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService : pointer;  // Points to type SECURITY_QUALITY_OF_SERVICE
  end;

  TZwProcessInfo = packed record
    size : DWORD;
    ThreadCount : DWORD;
    Unknown1 : array [0..47] of Byte;
    ProcesssNameLen : Word;
    Unknown2 : word;
    ProcessName : PWideChar;
    Unknown3 : DWORD;
    PID : DWORD;
    Unknown4 : array [0..63] of byte;
  end;
  PzwProcessInfo = ^TZwProcessInfo;

  TzwThreadInfo = packed record
    unknown1 : array [0..35] of byte;
    ThreadID : DWORD;
    unknown2 : array [0..23] of byte;
  end;
  PzwThreadInfo = ^TzwThreadInfo;

  TzwHandleTableEntry = packed record
    UniqueProcessId : WORD;
    CreatorBackTraceIndex : WORD;
    ObjectTypeIndex : BYTE;
    HandleAttributes : BYTE;
    HandleValue : WORD;
    obj : pointer;
    GrantedAccess : DWORD
  end;

  TzwHandleTable = packed record
    NumberOfHandles : DWORD;
    Handles : array [0..0] of  TzwHandleTableEntry;
  end;
  PzwHandleTable = ^TzwHandleTable;

const
  ObjectNameInformation = 1;
  ObjectTypeInformation = 2;
type
  TObjectNameInformation = packed record
    Name : TUnicodeString;
    NameBuffer : array [0..0] of WideChar;
  end;
  PObjectNameInformation = ^TObjectNameInformation;


  TFileNameInformation = packed record
    FileNameLength : DWORD;
    Name : array [0..0] of WideChar
  end;
  PFileNameInformation = ^TFileNameInformation;

  TSystemObjectInformation = packed record
    NextEntryOffset : DWORD;
    Obj : Pointer;
    CreatorUniqueProcess : THandle;
    CreatorBackTraceIndex : WORD;
    Flags : WORD;
    PointerCount : LongInt;
    HandleCount : LongInt;
    PagedPoolCharge : DWORD;
    NonPagedPoolCharge : DWORD;
    ExclusiveProcessId : THandle;
    SecurityDescriptor : pointer;
    NameInfo : TObjectNameInformation;
  end;
  PSystemObjectInformation = ^TSystemObjectInformation;

  TSystemObjectTypeInformation = packed record
    NextEntryOffset : LongInt;
    NumberOfObjects : LongInt;
    NumberOfHandles : LongInt;
    TypeIndex : LongInt;
    InvalidAttributes : LongInt;
    GenericMapping : TGenericMapping;
    ValidAccessMask : LongInt;
    PoolType : LongInt;
    SecurityRequired : word;
    WaitableObject : word;
    TypeName : TUnicodeString
  end;
  PSystemObjectTypeInformation = ^TSystemObjectTypeInformation;

  TIOStatusBlock = record case Integer of
    0 : (Status : DWORD);
    1 : (_Pointer : Pointer; Information : DWORD)
  end;
  PIOStatusBlock = ^TIOStatusBlock;

  TSystemHandleTableEntryInfo = packed record
    UniqueProcessId : word;
    CreatorBackTraceIndex : word;
    ObjectTypeIndex : byte;
    HandleAttributes : byte;  // 0x01 = PROTECT_FROM_CLOSE, 0x02 = INHERIT
    HandleValue : word;
    Obj : pointer;
    GrantedAccess : ACCESS_MASK;
  end;
  PSystemHandleTableEntryInfo = ^TSystemHandleTableEntryInfo;

  TSystemHandleInformation = packed record
    NumberOfHandles : ULONG;
    Handles : array [0..0] of TSystemHandleTableEntryInfo
  end;
  PSystemHandleInformation = ^TSystemHandleInformation;

  TFileBasicInformation = packed record
    CreationTime : TLargeInteger;
    LastAccessTime : TLargeInteger;
    LastWriteTime : TLargeInteger;
    ChangeTime : TLargeInteger;
    FileAttributes : DWORD;
  end;
  PFileBasicInformation = ^TFileBasicInformation;

  TFileStandardInformation = packed record
    AllocationSize : TLargeInteger;
    EndOfFile : TLargeInteger;
    NumberOfLinks : DWORD;
    DeletePending : boolean;
    Directory : boolean;
  end;
  PFileStandardInformation = ^TFileStandardInformation;

  TFileNetworkOpenInformation = packed record
    CreationTime : TLargeInteger;
    LastAccessTime : TLargeInteger;
    LastWriteTime : TLargeInteger;
    ChangeTime : TLargeInteger;
    AllocationSize : TLargeInteger;
    EndOfFile : TLargeInteger;
    FileAttributes : DWORD
  end;
  PFileNetworkOpenInformation = ^TFileNetworkOpenInformation;

  PDriverObject = ^TDriverObject;
  PDeviceObject = ^TDeviceObject;
  PFileObject = ^TFileObject;

  TfnDriverAddDevice = function (DriverObject : PDriverObject; DeviceObject : PDeviceObject) : DWORD; cdecl;
  PfnDriverAddDevice = ^TfnDriverAddDevice;

  TfnFastIOCheckIfPossible = function (FileObject : PFileObject;
                                       FileOffset : PLargeInteger;
                                       Length : DWORD;
                                       Wait : boolean;
                                       LockKey : DWORD;
                                       CheckForReadOperation : Boolean;
                                       IOStatus : PIOStatusBlock;
                                       DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOCheckIfPossible =^TfnFastIOCheckIfPossible;

  TfnFastIORead = function (FileObject : PFileObject;
                            FileOffset : PLargeInteger;
                            Length : DWORD;
                            Wait : boolean;
                            LockKey : DWORD;
                            Buffer : pointer;
                            IOStatus : PIOStatusBlock;
                            DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIORead = ^TfnFastIORead;

  TfnFastIOWrite = function (FileObject : PFileObject;
                            FileOffset : PLargeInteger;
                            Length : DWORD;
                            Wait : boolean;
                            LockKey : DWORD;
                            Buffer : pointer;
                            IOStatus : PIOStatusBlock;
                            DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOWrite = ^TfnFastIOWrite;

  TfnFastIOQueryBasicInfo = function (FileObject : PFileObject;
                                      Wait : boolean;
                                      Buffer : PFileBasicInformation;
                                      IOStatus : PIOStatusBlock;
                                      DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOQueryBasicInfo = ^TfnFastIOQueryBasicInfo;

  TfnFastIOQueryStandardInfo = function (FileObject : PFileObject;
                                         Wait : boolean;
                                         Buffer : PFileStandardInformation;
                                         IOStatus : PIOStatusBlock;
                                         DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOQueryStandardInfo = ^TfnFastIOQueryStandardInfo ;

  TfnFastIOLock = function (FileObject : PFileObject;
                            FileOffset : PLargeInteger;
                            Length : DWORD;
                            ProcessId : PEPROCESS;
                            Key : DWORD;
                            FailImmediately : boolean;
                            ExclusiveLock : boolean;
                            Wait : boolean;
                            IOStatus : PIOStatusBlock;
                            DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOLock = ^TfnFastIOLock;

  TfnFastIOUnLockSingle = function (FileObject : PFileObject;
                                    FileOffset : PLargeInteger;
                                    Length : DWORD;
                                    ProcessId : PEPROCESS;
                                    Key : DWORD;
                                    IOStatus : PIOStatusBlock;
                                    DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOUnLockSingle = ^TfnFastIOUnLockSingle;

  TfnFastIOUnLockAll = function (FileObject : PFileObject;
                                 ProcessId : PEPROCESS;
                                 IOStatus : PIOStatusBlock;
                                 DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOUnLockAll = ^TfnFastIOUnLockAll;


  TfnFastIOUnLockAllByKey = function (FileObject : PFileObject;
                                      ProcessId : PEPROCESS;
                                      Key : DWORD;
                                      IOStatus : PIOStatusBlock;
                                      DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOUnLockAllByKey = ^TfnFastIOUnLockAllByKey;

  TfnFastIODeviceControl = function (FileObject : PFileObject;
                                     Wait : boolean;
                                     InputBuffer : Pointer;
                                     InputBufferLength : DWORD;
                                     OutputBuffer : Pointer;
                                     OutputBufferLength : DWORD;
                                     IoControlCode : DWORD;
                                     IOStatus : PIOStatusBlock;
                                     DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIODeviceControl = ^TfnFastIODeviceControl;

  TfnFastIOAcquireFile  = procedure (FileObject : PFileObject); cdecl;
  PfnFastIOAcquireFile  = ^TfnFastIOAcquireFile;
  TfnFastIOReleaseFile  = procedure (FileObject : PFileObject); cdecl;
  PfnFastIOReleaseFile  = ^TfnFastIOReleaseFile;
  TfnFastIODetachDevice = procedure (SourceDevice, TargetDevice : PDeviceObject); cdecl;
  PfnFastIODetachDevice = ^TfnFastIODetachDevice;

//
// This structure is used by the server to quickly get the information needed
// to service a server open call.  It is takes what would be two fast io calls
// one for basic information and the other for standard information and makes
// it into one call.
//


  TfnFastIOQueryNetworkOpenInfo = function (FileObject : PFileObject;
                                            Wait : boolean;
                                            Buffer : PFileNetworkOpenInformation;
                                            IOStatus : PIOStatusBlock;
                                            DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOQueryNetworkOpenInfo = ^TfnFastIOQueryNetworkOpenInfo;

  TfnFastIOMDLRead = function (FileObject : PFileObject;
                               FileOffset : PLargeInteger;
                               Length : DWORD;
                               LockKey : DWORD;
                               var MdlChain : PMDL;
                               IOStatus : PIOStatusBlock;
                               DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOMDLRead = ^TfnFastIOMDLRead;

  TfnFastIOMDLReadComplete = function (FileObject : PFileObject;
                                       MdlChain : PMDL;
                                       DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOMDLReadComplete = ^TfnFastIOMDLReadComplete;

  TfnFastIOPrepareMDLWrite = function (FileObject : PFileObject;
                                       FileOffset : PLargeInteger;
                                       Length : DWORD;
                                       LockKey : DWORD;
                                       var MdlChain : PMDL;
                                       IOStatus : PIOStatusBlock;
                                       DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOPrepareMDLWrite = ^TfnFastIOPrepareMDLWrite;

  TfnFastIOMDLWriteComplete = function (FileObject : PFileObject;
                                        FileOffset : PLargeInteger;
                                        MdlChain : PMDL;
                                        DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOMDLWriteComplete = ^TfnFastIOMDLWriteComplete;


  TfnFastIOAcquireForModWrite = function (FileObject : PFileObject;
                                          EndingOffset : PLargeInteger;
                                          var ResourceToRelease : PERESOURCE;
                                          DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOAcquireForModWrite = ^TfnFastIOAcquireForModWrite;

  TfnFastIOReleaseForModWrite = function (FileObject : PFileObject;
                                          var ResourceToRelease : PERESOURCE;
                                          DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOReleaseForModWrite = ^TfnFastIOReleaseForModWrite;

  TfnFastIOAcquireForCCFlush = function  (FileObject : PFileObject;
                                          DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOAcquireForCCFlush =^TfnFastIOAcquireForCCFlush;

  TfnFastIOReleaseForCCFlush = function  (FileObject : PFileObject;
                                          DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOReleaseForCCFlush = ^TfnFastIOReleaseForCCFlush;

  TfnFastIOReadCompressed = function (FileObject : PFileObject;
                                      FileOffset : PLargeInteger;
                                      Length : DWORD;
                                      LockKey : DWORD;
                                      Buffer : pointer;
                                      var MdlChain : PMDL;
                                      IOStatus : PIOStatusBlock;
                                      CompressedDataInfo : Pointer;
                                      CompresedDataInfoLength : DWORD;
                                      DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOReadCompressed = ^TfnFastIOReadCompressed;

  TfnFastIOWriteCompressed = function (FileObject : PFileObject;
                                       FileOffset : PLargeInteger;
                                       Length : DWORD;
                                       LockKey : DWORD;
                                       Buffer : pointer;
                                       var MdlChain : PMDL;
                                       IOStatus : PIOStatusBlock;
                                       CompressedDataInfo : Pointer;
                                       CompresedDataInfoLength : DWORD;
                                       DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOWriteCompressed = ^TfnFastIOWriteCompressed;

  TfnFastIOReadCompleteCompressed = function (FileObject : PFileObject;
                                              MdlChain : PMDL;
                                              DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOReadCompleteCompressed = ^TfnFastIOReadCompleteCompressed;

  TfnFastIOWriteCompleteCompressed = function (FileObject : PFileObject;
                                               FileOffset : PLargeInteger;
                                               MdlChain : PMDL;
                                               DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOWriteCompleteCompressed = ^TfnFastIOWriteCompleteCompressed;

  TfnFastIOQueryOpen = function (Irp : Pointer; // PIRP;
                                 NetworkInformation : PFileNetworkOpenInformation;
                                 DeviceObject : PDeviceObject) : boolean; cdecl;
  PfnFastIOQueryOpen = ^TfnFastIOQueryOpen;

  TfnDriverInitialize = function (DriverObject : PDriverObject; RegistryPath : TUnicodeString) : DWORD; cdecl;
  PfnDriverInitialize = ^TfnDriverInitialize;

  TfnDriverStartIO = procedure (DeviceObject : PDeviceObject; IRP : Pointer (*PIRP*)); cdecl;
  PfnDriverStartIO = ^TfnDriverStartIO;

  TfnDriverDispatch = function (DeviceObject : PDeviceObject; IRP : Pointer (*PIRP*)) : DWORD; cdecl;
  PfnDriverDispatch = ^TfnDriverDispatch;

  TfnDriverUnload = procedure (DeviceObject : PDeviceObject); cdecl;
  PfnDriverUnload = ^TfnDriverUnload;

  IO_ALLOCATION_ACTION = (
    KeepObject = 1,
    DeallocateObject,
    DeallocateObjectKeepRegisters
  );

//
// Define device driver adapter/controller execution routine.
//

  TfnDriverControl = function (DeviceObject : PDeviceObject;
                               IRP : Pointer (*PIRP*);
                               MapRegisterBase : Pointer;
                               Context : Pointer) : IO_ALLOCATION_ACTION; cdecl;
  PfnDriverControl = ^TfnDriverControl;

  TFastIODispatch = packed record
    SizeOfFastIoDispatch : DWORD;
    FastIoCheckIfPossible : PfnFastIOCheckIfPossible;
    FastIoRead : PfnFastIORead;
    FastIoWrite : PfnFastIOWrite;
    FastIoQueryBasicInfo : PfnFastIOQueryBasicInfo;
    FastIoQueryStandardInfo : PfnFastIOQueryStandardInfo;
    FastIoLock : PfnFastIOLock;
    FastIoUnlockSingle : PfnFastIoUnlockSingle;
    FastIoUnlockAll : PfnFastIoUnlockAll;
    FastIoUnlockAllByKey : PfnFastIoUnlockAllByKey;
    FastIoDeviceControl : PfnFastIoDeviceControl;
    AcquireFileForNtCreateSection : PfnFastIOAcquireFile;
    ReleaseFileForNtCreateSection : PfnFastIOReleaseFile;
    FastIoDetachDevice : PfnFastIoDetachDevice;
    FastIoQueryNetworkOpenInfo : PfnFastIoQueryNetworkOpenInfo;
    AcquireForModWrite : PfnFastIOAcquireForModWrite;
    MdlRead : PfnFastIOMdlRead;
    MdlReadComplete : PfnFastIOMdlReadComplete;
    PrepareMdlWrite : PfnFastIOPrepareMdlWrite;
    MdlWriteComplete : PfnFastIOMdlWriteComplete;
    FastIoReadCompressed : PfnFastIoReadCompressed;
    FastIoWriteCompressed : PfnFastIoWriteCompressed;
    MdlReadCompleteCompressed : PfnFastIOReadCompleteCompressed;
    MdlWriteCompleteCompressed : PfnFastIOWriteCompleteCompressed;
    FastIoQueryOpen : PfnFastIoQueryOpen;
    ReleaseForModWrite : PfnFastIOReleaseForModWrite;
    AcquireForCcFlush : PfnFastIOAcquireForCcFlush;
    ReleaseForCcFlush : PfnFastIOReleaseForCcFlush;
  end;
  PFastIODispatch = ^TFastIODispatch;

  TDriverExtension = packed record
    DriverObject : PDriverObject;
    AddDevice : PfnDriverAddDevice;
    Count : DWORD;
    ServiceKeyName : TUnicodeString
  end;
  PDriverExtension = ^TDriverExtension;


  TDriverObject = packed record
    _Type : word;
    Size : word;
    DeviceObject : PDeviceObject;
    Flags : DWORD;

    DriverStart  : Pointer;
    DriverSize : DWORD;
    DriverSection : Pointer;
    DriverExtension : PDriverExtension;

    DriverName : TUnicodeString;
    HardwareDatabase : TUnicodeString;
    FastIoDispatch : PFastIODispatch;

    DriverInit : PfnDriverInitialize;
    DriverStartIo : PfnDriverStartIO;
    DriverUnload : PfnDriverUnload;
    MajorFunction : array [0..IRP_MJ_MAXIMUM_FUNCTION] of PfnDriverDispatch
  end;

  TVPB = packed record // Volume Parameter Block (VPB)
    _Type : word;
    Size : word;
    Flahs : word;
    VolumeLabelLength : word; // in bytes
    DeviceObject : PDeviceObject;
    RealDevice : PDeviceObject;
    SerialNumber : DWORD;
    ReferenceCount : DWORD;
    VolumeLabel : array [0..(MAXIMUM_VOLUME_LABEL_LENGTH div sizeof(WideChar)) - 1] of WideChar
  end;
  PVPB = ^TVPB;

  KDEVICE_QUEUE = packed record
    _Type : word;
    Size : word;
    DeviceListHead : TListEntry;
    Lock : KSPIN_LOCK;
    Busy : Boolean
  end;
  PKDEVICE_QUEUE = ^KDEVICE_QUEUE;

  KDEVICE_QUEUE_ENTRY = packed record
    DeviceListEntry : TListEntry;
    SortKey : DWORD;
    Inserted : Boolean
  end;
  PKDEVICE_QUEUE_ENTRY = ^KDEVICE_QUEUE_ENTRY;


  PKDPC = ^KDPC;
  TfnKDeferredRoutine = procedure (Dpc : PKDPC; DeferredContext, SystemArgument1, SystemArgument2 : Pointer); cdecl;
  PfnKDeferredRoutine = ^TfnKDeferredRoutine;

  KDPC = packed record // Deferred procedure call
    _Type : word;
    Number : Byte;
    Importance : Byte;
    DpcListEntry : TListEntry;
    DeferredRoutine : PfnKDeferredRoutine;
    DeferredContext : Pointer;
    SystemArgument1 : Pointer;
    SystemArgument2 : Pointer;
    Lock : ^PDWORD
  end;

  TWaitContextBlock = packed record
    WaitQueueEntry : KDEVICE_QUEUE_ENTRY;
    DeviceRoutine : PfnDriverControl;
    DeviceContext : Pointer;
    NumberOfMapRegisters : DWORD;
    DeviceObject : Pointer;
    CurrentIrp : Pointer;
    BufferChainingDpc : PKDPC
  end;
  PWaitContextBlock = ^TWaitContextBlock;


  PDevObjExtension = ^TDevObjExtension;
  TDeviceObject = packed record
    _Type : word;
    Size : word;
    ReferenceCount : LongInt;

    DriverObject : PDriverObject;
    NextDevice : PDeviceObject;
    AttachedDevice : PDeviceObject;
    CurrentIRP : Pointer; // PIRP
    Timer : PIO_TIMER;
    Flags : ULong;
    Characteristics : ULONG;
    Vpb : PVPB;
    DeviceExtension : pointer;
    DeviceType : DEVICE_TYPE;
    StachSize : char;
    queue : record case Integer of
      0 : (ListEntry : TListEntry);
      1 : (Wcb : TWaitContextBlock)
    end;
    AlignmentRequirement : DWORD;
    DeviceQueue : KDEVICE_QUEUE;
    Dpc : KDPC;

    ActiveThreadCount : DWORD;
    SecurityDescriptor : PSecurityDescriptor;
    DeviceLock : KEVENT;

    SectorSize : word;
    Spare1 : word;

    DeviceObjectExtension : PDevObjExtension;
    Reserved : pointer
  end;

  TDevObjExtension = packed record
    _Type : word;
    Size : word;
    DeviceObject : PDeviceObject
  end;

  TSectionObjectPointers = packed record
    DataSectionObject : Pointer;
    SharedCacheMap : Pointer;
    ImageSectionObject : Pointer
  end;
  PSectionObjectPointers = ^TSectionObjectPointers;

  TIOCompletionContext = packed record
    Port : Pointer;
    Key : Pointer
  end;
  PIOCompletionContext = ^TIOCompletionContext;

  TFileObject = packed record
    _Type : word;
    Size : word;
    DeviceObject : PDeviceObject;
    Vpb : PVPB;
    fsContext : pointer;
    FsContext2 : pointer;
    SectionObjectPointer : PSectionObjectPointers;
    PrivateCacheMap : pointer;
    FinalStatus : DWORD;
    RelatedFileObject : PFileObject;
    LockOperation : boolean;
    DeletePending : boolean;
    ReadAccess : boolean;
    WriteAccess : boolean;
    DeleteAccess : Boolean;
    SharedRead : boolean;
    SharedWrite : boolean;
    SharedDelete : boolean;
    Flags : DWORD;
    FileName : TUnicodeString;
    CurrentByteOffset : LARGE_INTEGER;
    Waiters : DWORD;
    Busy : DWORD;
    LastLock : pointer;
    Lock : KEvent;
    Event : KEvent;
    CompletionContext : PIOCompletionContext
  end;

const
  TDI_QUERY_BROADCAST_ADDRESS      = $00000001;
  TDI_QUERY_PROVIDER_INFORMATION   = $00000002;   // temp, renamed ...
  TDI_QUERY_PROVIDER_INFO          = $00000002;   // ... to this
  TDI_QUERY_ADDRESS_INFO           = $00000003;
  TDI_QUERY_CONNECTION_INFO        = $00000004;
  TDI_QUERY_PROVIDER_STATISTICS    = $00000005;
  TDI_QUERY_DATAGRAM_INFO          = $00000006;
  TDI_QUERY_DATA_LINK_ADDRESS      = $00000007;
  TDI_QUERY_NETWORK_ADDRESS        = $00000008;
  TDI_QUERY_MAX_DATAGRAM_INFO      = $00000009;

type
  TTDIRequest = packed record
    Handle : record case Integer of
      0 : (AddressHandle : THandle);
      1 : (ConnectionContext : Pointer);
      2 : (ControlChannel : THandle)
    end;

    RequestNotifyObject : Pointer;
    RequestContext : Pointer;
    TdiStatus : LongInt
  end;
  PTDIRequest = ^TTDIRequest;

  TTDIConnectionInformation = packed record
    UserDataLength : LongInt;        // length of user data buffer
    UserData : Pointer;              // pointer to user data buffer
    OptionsLength : LongInt;         // length of follwoing buffer
    Options : Pointer;               // pointer to buffer containing options
    RemoteAddressLength : LongInt;   // length of following buffer
    RemoteAddress : Pointer;         // buffer containing the remote address
  end;
  PTDIConnectionInformation = ^TTDIConnectionInformation;

  TTDIRequestQueryInformation = packed record
    Request : TTDIRequest;
    QueryType : DWORD;
    RequestConnectionInformation : PTDIConnectionInformation
  end;
  PTDIRequestQueryInformation = ^TTDIRequestQueryInformation;

  TTDIConnectionInfo = record
    State : DWORD;                        // current state of the connection.
    Event : DWORD;                        // last event on the connection.
    TransmittedTsdus : DWORD;             // TSDUs sent on this connection.
    ReceivedTsdus : DWORD;                // TSDUs received on this connection.
    TransmissionErrors : DWORD;           // TSDUs transmitted in error/this connection.
    ReceiveErrors : DWORD;                // TSDUs received in error/this connection.
    Throughput : TLargeInteger;           // estimated throughput on this connection.
    Delay : TLargeInteger;                // estimated delay on this connection.
    SendBufferSize : DWORD;               // size of buffer for sends - only
                                          // meaningful for internal buffering
                                          // protocols like tcp
    ReceiveBufferSize : DWORD;            // size of buffer for receives - only
                                          // meaningful for internal buffering
                                          // protocols like tcp
    Unreliable : Boolean;                 // is this connection "unreliable".
  end;
  PTDIConnectionInfo = ^TTDIConnectionInfo;

  TTAAddress = packed record
    AddressLength : Word;       // length in bytes of Address[] in this
    AddressType : Word;         // type of this address
    Address :array [0..0] of byte;
  end;
  PTAAddress = ^TTAAddress;

  TTransportAddress = record
    TAAddressCount : DWORD;                // number of addresses following
    Address : array [0..0] of TTAAddress;    // actually TAAddressCount elements long
  end;
  PTransportAddress = ^TTransportAddress;

  TTDIAddressInfo = record
    ActivityCount : DWORD;                // outstanding open file objects/this address.
    Address : TTransportAddress;          // the actual address & its components.
  end;
  PTDIAddressInfo = ^TTDIAddressInfo;

  TTDIAddressIP = packed record
    sin_port : word;
    in_addr : LongInt;
    sin_zero : array [0..7] of byte
  end;

  TTAIPAddress = packed record
    TAAddressCount : LongInt;

    Address : array [0..0] of packed record
                AddressLength : word;
                AddressType : word;
                Address : array [0..0] of TTDIAddressIP
              end
  end;
  PTAIPAddress = ^TTAIPAddress;

  TfnZwQuerySystemInformation = function  (tag : SYSTEM_INFORMATION_CLASS; buffer : pointer; bufferSize : DWORD; var returnedSize : DWORD) : DWORD; stdcall;
  TfnZwQueryObject = function (Handle : THandle; ActionCode : DWORD; buf : Pointer; bufLen : DWORD; bytesWritten : PDWORD) : DWORD; stdcall;
  TfnZwDuplicateObject = function (SourceProcessHandle, SourceHandle, TargetProcessHandle : THandle; var TargetHandle : THandle; DesiredAccess : ACCESS_MASK; HandleAttributes, Options : DWORD) : DWORD; stdcall;
  TfnZwOpenProcess = function (var Handle : THandle; DesiredAccess : ACCESS_MASK; var ObjectAttributes : TObjectAttributes; var ClientID : TClientID) : DWORD; stdcall;
  TfnZwOpenSection = function (var Handle : THandle; DesiredAccess : ACCESS_MASK; var ObjectAttributes : TObjectAttributes) : DWORD; stdcall;
  TfnZwOpenFile = function (var Handle : THandle; DesiredAccess : ACCESS_MASK; var ObjectAttributes : TObjectAttributes; var StatusBlock : TIOStatusBlock; ShareAccess, OpenOptions : DWORD) : DWORD; stdcall;
  TfnZwCreateFile = function (var Handle : THandle; DesiredAccess : ACCESS_MASK; var ObjectAttributes : TObjectAttributes; var StatusBlock : TIOStatusBlock; AllocationSize : PLargeInteger; FileAttributes, ShareAccess, CreateDisposition, CreateOptions : ULONG; EaBuffer : pointer; EaLength : ULONG) : DWORD; stdcall;
  TfnZwDeviceIoControlFile = function (FileHandle, Event : THandle; ApcRoutine, ApcContext : pointer; var StatusBlock : TIOStatusBlock; IOControlCode : ULONG; InputBuffer : pointer; InputBufferLength : ULONG; OutputBuffer : pointer; OutputBufferLength : ULONG) : DWORD; stdcall;
  TfnZwWaitForSingleObject = function (Handle : THandle; Alertable : BOOL; Timeout : PLargeInteger) : DWORD; stdcall;
  TfnZwQueryInformationFile = function (Handle : THandle; var iosb : TIOStatusBlock; info : pointer; len : DWORD; cls :FILE_INFORMATION_CLASS) : DWORD; stdcall;

var
  fnZwQuerySystemInformation : TfnZwQuerySystemInformation;
  fnZwQueryObject : TfnZwQueryObject;
  fnZwDuplicateObject : TfnZwDuplicateObject;
  fnZwOpenProcess : TfnZwOpenProcess;
  fnZwOpenSection : TfnZwOpenSection;
  fnZwOpenFile : TfnZwOpenFile;
  fnZwCreateFile : TfnZwCreateFile;
  fnZwDeviceIoControlFile : TfnZwDeviceIoControlFile;
  fnZwWaitForSingleObject : TfnZwWaitForSingleObject;
  fnZwQueryInformationFile : TfnZwQueryInformationFile;

type
  TUnicodeStr = class
   public
    Value : TUnicodeString;
    constructor CreateFromStr (const st : string);
    constructor Create (size : Integer);
    destructor Destroy; override;
  end;

  HARDWARE_PTE = DWORD;
  PHARDWARE_PTE = ^HARDWARE_PTE;

const
  PAGE_SIZE = $1000;
  PTE_VALID = $1;
  PTE_WRITE = $2;
  PTE_OWNER = $4;
  PTE_WRITETHROUGH = $8;
  PTE_CACHEDISABLE = $10;
  PTE_ACCESSED = $20;
  PTE_DIRTY = $40;
  PTE_LARGEPAGE = $80;
  PTE_GLOBAL = $100;
  PTE_COPYONWRITE = $200;
  PTE_PROTOTYPE = $400;
  PTE_RESERVED = $800;

  FILE_DEVICE_TRANSPORT = $21;
  FILE_ANY_ACCESS       = 0;

  METHOD_BUFFERED                 = 0;
  METHOD_IN_DIRECT                = 1;
  METHOD_OUT_DIRECT               = 2;
  METHOD_NEITHER                  = 3;

function PTE_PageFrameNumber (PTE : DWORD) : DWORD;
function BYTE_OFFSET (va : pointer) : DWORD;
function TDICtlCode (Func, Method : DWORD) : DWORD;
function IOCTL_TDI_QUERY_INFORMATION : DWORD;
procedure InitializeObjectAttributes (var p : TObjectAttributes; n : TUnicodeStr; a : ULONG; r : THandle; s : PSECURITY_DESCRIPTOR);
function CtlCode (DeviceType, Func, Method, Access : DWORD) : DWORD;

implementation
var
  modNTDLL : THandle = 0;

function BYTE_OFFSET (va : pointer) : DWORD;
begin
  result := ULONG (va) and (PAGE_SIZE - 1)
end;

function PTE_PageFrameNumber (PTE : DWORD) : DWORD;
begin
  result := (PTE shr 12);
end;

function CtlCode (DeviceType, Func, Method, Access : DWORD) : DWORD;
begin
  result := (DeviceType shl 16) or (Access shl 14) or (func shl 2) or Method
end;


function TDICtlCode (Func, Method : DWORD) : DWORD;
begin
  result := CtlCode (FILE_DEVICE_TRANSPORT, Func, Method, FILE_ANY_ACCESS)
end;

function IOCTL_TDI_QUERY_INFORMATION : DWORD;
begin
  result := TDICtlCode (4, METHOD_OUT_DIRECT)
end;

procedure InitializeObjectAttributes (var p : TObjectAttributes; n : TUnicodeStr; a : ULONG; r : THandle; s : PSECURITY_DESCRIPTOR);
begin
  p.Length := SizeOf (TObjectAttributes);
  p.RootDirectory := r;
  p.Attributes := a;
  p.ObjectName := @n.Value;
  p.SecurityDescriptor := s;
  p.SecurityQualityOfService := Nil
end;

// InitializeObjectAttributes(
//     OUT POBJECT_ATTRIBUTES p,
//     IN PUNICODE_STRING n,
//     IN ULONG a,
//     IN HANDLE r,
//     IN PSECURITY_DESCRIPTOR s
//     )

procedure AllocUnicodeString (var str : TUnicodeString; maxLen : WORD);
begin
  str.MaximumLength := maxLen;
  str.Length := 0;
  GetMem (str.Buffer, 2 * (maxLen + 1));
end;

procedure FreeUnicodeString (var str : TUnicodeString);
begin
  FreeMem (str.Buffer);
  str.MaximumLength := 0;
  str.Length := 0
end;


const SE_DEBUG_NAME = 'SeDebugPrivilege';

function EnableNTPrivilege (const privilege : string; state : Integer) : Integer;
var
  hToken : THandle;
  aluid : TLargeInteger;
  cbPrevTP : DWORD;
  tp, fPrevTP : PTokenPrivileges;
begin
  result := 0;
  if OpenProcessToken (GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  try
    LookupPrivilegeValue (Nil, PChar (privilege), aluid);

    cbPrevTP := SizeOf (TTokenPrivileges) + sizeof (TLUIDAndAttributes);

    GetMem (tp, cbPrevTP);
    GetMem (fPrevTP, cbPrevTP);
    try

      tp^.PrivilegeCount := 1;
      tp^.Privileges [0].Luid := aLuid;
      tp^.Privileges [0].Attributes := state;

      if not AdjustTokenPrivileges (hToken, False, tp^, cbPrevTP, fPrevTP^, cbPrevTP) then
        RaiseLastOSError;

      result := fPrevTP^.Privileges [0].Attributes;

    finally
     FreeMem (fPrevTP);
     FreeMem (tp);
    end
  finally
    CloseHandle (hToken);
  end
end;

var
  oldState : DWORD;
{ TUnicodeStr }

constructor TUnicodeStr.Create(size: Integer);
begin
  Value.MaximumLength := size;
  Value.Length := 0;
  GetMem (Value.Buffer, sizeof (WideChar) * (size + 1))
end;

constructor TUnicodeStr.CreateFromStr(const st: string);
var
  len : Integer;
  wst : WideString;
begin
  len := Length (st);
  Value.Length := (len + 0) * sizeof (WideChar);
  Value.MaximumLength := (len + 1) * sizeof (WideChar);
  GetMem (Value.buffer, sizeof (WideChar) * (len + 1));
  wst := st;
  lstrcpyw (Value.buffer, PWideChar (wst))
end;

destructor TUnicodeStr.Destroy;
begin
  ReallocMem (Value.Buffer, 0);

  inherited;
end;

initialization
  modNTDLL := LoadLibrary ('ntdll.dll');

  fnZwQuerySystemInformation := GetProcAddress (modNTDLL, 'ZwQuerySystemInformation');
  fnZwQueryObject := GetProcAddress (modNTDLL, 'ZwQueryObject');
  fnZwDuplicateObject := GetProcAddress (modNTDLL, 'ZwDuplicateObject');
  fnZwOpenProcess := GetProcAddress (modNTDLL, 'ZwOpenProcess');
  fnZwOpenSection := GetProcAddress (modNTDLL, 'ZwOpenSection');
  fnZwOpenFile := GetProcAddress (modNTDLL, 'ZwOpenFile');
  fnZwCreateFile := GetProcAddress (modNTDLL, 'ZwCreateFile');
  fnZwDeviceIoControlFile := GetProcAddress (modNTDLL, 'ZwDeviceIoControlFile');
  fnZwWaitForSingleObject := GetProcAddress (modNTDLL, 'ZwWaitForSingleObject');
  fnZwQueryInformationFile := GetProcAddress (modNTDLL, 'ZwQueryInformationFile');

  oldState := EnableNTPrivilege (SE_DEBUG_NAME, SE_PRIVILEGE_ENABLED);

finalization
  EnableNTPrivilege (SE_DEBUG_NAME, oldState);
  FreeLibrary (modNTDLL);
end.

