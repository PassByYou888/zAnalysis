{******************************************************************************}
{* Core class library  written by QQ 600585@qq.com                            *}
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

(*
  update history
  2017-12-6
  timetick
*)

unit CoreClasses;

interface

uses SysUtils, Classes, Types,
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE FPC}
  Threading,
  {$ENDIF FPC}
  {$ENDIF parallel}
  PascalStrings,
  SyncObjs
  {$IFDEF FPC}
    , Contnrs, fgl
  {$ELSE FPC}
  , System.Generics.Collections
  {$ENDIF FPC}
  ,Math;

{$I zDefine.inc}

const
  fmCreate        = Classes.fmCreate;
  soFromBeginning = Classes.soFromBeginning;
  soFromCurrent   = Classes.soFromCurrent;
  soFromEnd       = Classes.soFromEnd;

  fmOpenRead      = SysUtils.fmOpenRead;
  fmOpenWrite     = SysUtils.fmOpenWrite;
  fmOpenReadWrite = SysUtils.fmOpenReadWrite;

  fmShareExclusive = SysUtils.fmShareExclusive;
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  fmShareDenyNone  = SysUtils.fmShareDenyNone;

type
  TTimeTickValue = Cardinal;
  TTimeTick      = TTimeTickValue;

  TSeekOrigin = Classes.TSeekOrigin;
  TNotify     = Classes.TNotifyEvent;

  TCoreClassObject     = TObject;
  TCoreClassPersistent = TPersistent;

  TCoreClassStream         = TStream;
  TCoreClassFileStream     = TFileStream;
  TCoreClassStringStream   = TStringStream;
  TCoreClassResourceStream = TResourceStream;

  TCoreClassThread = TThread;

  CoreClassException = Exception;

  TCoreClassMemoryStream = TMemoryStream;
  TCoreClassStrings    = TStrings;
  TCoreClassStringList = TStringList;
  TCoreClassReader     = TReader;
  TCoreClassWriter     = TWriter;
  TCoreClassComponent  = TComponent;

  {$IFDEF FPC}
  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  PCoreClassPointerList      = Classes.PPointerList;
  TCoreClassPointerList      = Classes.TPointerList;
  TCoreClassListSortCompare  = Classes.TListSortCompare;
  TCoreClassListNotification = Classes.TListNotification;

  TCoreClassList             = Class(TList)
    property ListData: PPointerList read GetList;
  end;

  TCoreClassListForObj = class(TObjectList)
  public
    constructor Create;
  end;
  {$ELSE FPC}
  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TGenericsList<T>=class(System.Generics.Collections.TList<T>)
    function ListData: Pointer;
  end;

  TGenericsObjectList<T:class>=class(System.Generics.Collections.TList<T>)
    function ListData: Pointer;
  end;

  TCoreClassPointerList = array of Pointer;
  PCoreClassPointerList = ^TCoreClassPointerList;

  TCoreClassList_ = class(System.Generics.Collections.TList<Pointer>)
  end;

  TCoreClassList = class(TCoreClassList_)
    function ListData: PCoreClassPointerList;
  end;

  TCoreClassForObjectList = array of TCoreClassObject;
  PCoreClassForObjectList = ^TCoreClassForObjectList;

  TCoreClassListForObj_ = class(System.Generics.Collections.TList<TCoreClassObject>)
  end;

  TCoreClassListForObj = class(TCoreClassListForObj_)
    function ListData: PCoreClassForObjectList;
  end;
  {$ENDIF FPC}

  TComputeThread = class(TCoreClassThread)
  private type
    TRunWithThreadCall               = procedure(Sender: TComputeThread);
    TRunWithThreadMethod             = procedure(Sender: TComputeThread) of object;
    {$IFNDEF FPC} TRunWithThreadProc = reference to procedure(Sender: TComputeThread); {$ENDIF FPC}
  protected
    OnRunCall: TRunWithThreadCall;
    OnRunMethod: TRunWithThreadMethod;
    {$IFNDEF FPC} OnRunProc: TRunWithThreadProc; {$ENDIF FPC}
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
    {$IFNDEF FPC} OnDoneProc: TRunWithThreadProc; {$ENDIF FPC}
    procedure Execute; override;
    procedure Done_Sync;
  public
    UserData: Pointer;
    UserObject: TCoreClassObject;

    constructor Create;
    class function RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall): TComputeThread;
    class function RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod): TComputeThread;
    {$IFNDEF FPC} class function RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc): TComputeThread; {$ENDIF FPC}
  end;


  TExecutePlatform = (epWin32, epWin64, epOSX, epIOS, epIOSSIM, epANDROID, epLinux64, epUnknow);

const
  {$IF Defined(WIN32)}
  CurrentPlatform = TExecutePlatform.epWin32;
  {$ELSEIF Defined(WIN64)}
  CurrentPlatform = TExecutePlatform.epWin64;
  {$ELSEIF Defined(OSX)}
  CurrentPlatform = TExecutePlatform.epOSX;
  {$ELSEIF Defined(IOS)}
    {$IFDEF CPUARM}
    CurrentPlatform = TExecutePlatform.epIOS;
    {$ELSE CPUARM}
    CurrentPlatform = TExecutePlatform.epIOSSIM;
    {$ENDIF CPUARM}
  {$ELSEIF Defined(ANDROID)}
  CurrentPlatform = TExecutePlatform.epANDROID;
  {$ELSEIF Defined(Linux)}
  CurrentPlatform = TExecutePlatform.epLinux64;
  {$ELSE}
  CurrentPlatform = TExecutePlatform.epUnknow;
  {$IFEND}

procedure Nop;

function CheckThreadSynchronize(Timeout: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure DisposeObject(const obj: TObject); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DisposeObject(const objs: array of TObject); overload;
procedure FreeObject(const obj: TObject); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FreeObject(const objs: array of TObject); overload;

procedure LockID(const ID:Byte);
procedure UnLockID(const ID:Byte);

procedure LockObject(obj:TObject);
procedure UnLockObject(obj:TObject);

procedure FillPtrByte(const Dest:Pointer; Count: NativeUInt; const Value: Byte); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareMemory(const P1, P2: Pointer; const MLen: NativeUInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CopyPtr(const sour, dest:Pointer; Count: NativeUInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RaiseInfo(const n: SystemString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RaiseInfo(const n: SystemString; const Args: array of const); overload;

function IsMobile: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function GetTimeTickCount: TTimeTickValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GetTimeTick: TTimeTickValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GetCrashTimeTick: TTimeTickValue;

function ROL8(const Value:Byte; Shift: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROL16(const Value: Word; Shift: Byte): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROL64(const Value: UInt64; Shift: Byte): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROR8(const Value:Byte; Shift: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROR16(const Value: Word; Shift: Byte): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ROR64(const Value: UInt64; Shift: Byte): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Swap(var v1,v2:Byte); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Word); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Int64); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:UInt64); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:SystemString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Single); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Double); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Swap(var v1,v2:Pointer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SAR16(const AValue: SmallInt; const Shift: Byte): SmallInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SAR32(const AValue: Integer; Shift: Byte): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SAR64(const AValue: Int64; Shift: Byte): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MemoryAlign(addr: Pointer; alignment: NativeUInt): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

threadvar MHGlobalHookEnabled: Boolean;

implementation

procedure Nop;
begin
end;

function CheckThreadSynchronize(Timeout: Integer): Boolean;
begin
  Result := CheckSynchronize(Timeout);
end;

procedure DisposeObject(const obj: TObject);
begin
  if obj <> nil then
    begin
      try
        {$IFDEF AUTOREFCOUNT}
        obj.DisposeOf;
        {$ELSE}
        obj.Free;
        {$ENDIF}
      except
      end;
    end;
end;

procedure DisposeObject(const objs: array of TObject);
var
  obj: TObject;
begin
  for obj in objs do
      DisposeObject(obj);
end;

procedure FreeObject(const obj: TObject);
begin
  if obj <> nil then
    begin
      try
        {$IFDEF AUTOREFCOUNT}
        obj.DisposeOf;
        {$ELSE}
        obj.Free;
        {$ENDIF}
      except
      end;
    end;
end;

procedure FreeObject(const objs: array of TObject);
var
  obj: TObject;
begin
  for obj in objs do
      FreeObject(obj);
end;

{$I CoreAtomic.inc}

var
  LockIDBuff: packed array [0..$FF] of TCoreClassPersistent;

procedure InitLockIDBuff;
var
  i: Byte;
begin
  for i := 0 to $FF do
      LockIDBuff[i] := TCoreClassPersistent.Create;
end;

procedure FreeLockIDBuff;
var
  i: Integer;
begin
  for i := 0 to $FF do
      DisposeObject(LockIDBuff[i]);
end;

procedure LockID(const ID: Byte);
begin
  LockObject(LockIDBuff[ID]);
end;

procedure UnLockID(const ID: Byte);
begin
  UnLockObject(LockIDBuff[ID]);
end;

procedure LockObject(obj:TObject);
begin
{$IFDEF FPC}
  _LockCriticalObj(obj);
{$ELSE}
  {$IFDEF CriticalSimulateAtomic}
  _LockCriticalObj(obj);
  {$ELSE}
  TMonitor.Enter(obj);
  {$ENDIF}
{$ENDIF}
end;

procedure UnLockObject(obj:TObject);
begin
{$IFDEF FPC}
  _UnLockCriticalObj(obj);
{$ELSE}
  {$IFDEF CriticalSimulateAtomic}
  _UnLockCriticalObj(obj);
  {$ELSE}
  TMonitor.Exit(obj);
  {$ENDIF}
{$ENDIF}
end;

procedure FillPtrByte(const Dest: Pointer; Count: NativeUInt; const Value: Byte);
var
  Index: NativeInt;
  V    : UInt64;
  PB   : PByte;
begin
  PB := Dest;

  if Count >= 8 then
    begin
      V := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
      V := V or (V shl 32);

      for index := (Count shr 3) - 1 downto 0 do
        begin
          PUInt64(PB)^ := V;
          Inc(PB, 8);
        end;
      { Get the remainder (mod 8) }
      Count := Count and $7;
    end;

  // Fill remain.
  if Count > 0 then
    for index := Count - 1 downto 0 do
      begin
        PB^ := Value;
        Inc(PB);
      end;
end;

function CompareMemory(const P1, P2: Pointer; const MLen: NativeUInt): Boolean;
begin;
  if MLen = 0 then
    Result := True
  else
    Result := CompareMem(P1, P2, MLen);
end;

procedure CopyPtr(const sour, Dest: Pointer; Count: NativeUInt);
begin
  move(sour^, Dest^, Count);
end;

procedure RaiseInfo(const n: SystemString);
begin
  raise Exception.Create(n);
end;

procedure RaiseInfo(const n: SystemString; const Args: array of const);
begin
  raise Exception.Create(Format(n, Args));
end;

function IsMobile: Boolean;
begin
  case CurrentPlatform of
    epIOS, epIOSSIM, epANDROID: Result := True;
    else Result := False;
  end;
end;

function GetTimeTickCount: TTimeTickValue;
begin
  Result := TCoreClassThread.GetTickCount;
end;

function GetTimeTick: TTimeTickValue;
begin
  Result := TCoreClassThread.GetTickCount;
end;

function GetCrashTimeTick: TTimeTickValue;
begin
  Result := $FFFFFFFF - GetTimeTick;
end;

function ROL8(const Value: Byte; Shift: Byte): Byte;
begin
  Shift := Shift and $07;
  Result := Byte((Value shl Shift) or (Value shr (8 - Shift)));
end;

function ROL16(const Value: Word; Shift: Byte): Word;
begin
  Shift := Shift and $0F;
  Result := Word((Value shl Shift) or (Value shr (16 - Shift)));
end;

function ROL32(const Value: Cardinal; Shift: Byte): Cardinal;
begin
  Shift := Shift and $1F;
  Result := Cardinal((Value shl Shift) or (Value shr (32 - Shift)));
end;

function ROL64(const Value: UInt64; Shift: Byte): UInt64;
begin
  Shift := Shift and $3F;
  Result := UInt64((Value shl Shift) or (Value shr (64 - Shift)));
end;

function ROR8(const Value: Byte; Shift: Byte): Byte;
begin
  Shift := Shift and $07;
  Result := UInt8((Value shr Shift) or (Value shl (8 - Shift)));
end;

function ROR16(const Value: Word; Shift: Byte): Word;
begin
  Shift := Shift and $0F;
  Result := Word((Value shr Shift) or (Value shl (16 - Shift)));
end;

function ROR32(const Value: Cardinal; Shift: Byte): Cardinal;
begin
  Shift := Shift and $1F;
  Result := Cardinal((Value shr Shift) or (Value shl (32 - Shift)));
end;

function ROR64(const Value: UInt64; Shift: Byte): UInt64;
begin
  Shift := Shift and $3F;
  Result := UInt64((Value shr Shift) or (Value shl (64 - Shift)));
end;

procedure Swap(var v1,v2: Byte);
var
  V: Byte;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1,v2: Word);
var
  V: Word;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: Integer);
var
  V: Integer;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: Int64);
var
  V: Int64;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: UInt64);
var
  V: UInt64;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: SystemString);
var
  V: SystemString;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: Single);
var
  V: Single;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: Double);
var
  V: Double;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure Swap(var v1, v2: Pointer);
var
  V: Pointer;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

function SAR16(const AValue: SmallInt; const Shift: Byte): SmallInt;
begin
  Result := SmallInt(Word(Word(Word(AValue) shr (Shift and 15)) or (Word(SmallInt(Word(0 - Word(Word(AValue) shr 15)) and Word(SmallInt(0 - (Ord((Shift and 15) <> 0) { and 1 } ))))) shl (16 - (Shift and 15)))));
end;

function SAR32(const AValue: Integer; Shift: Byte): Integer;
begin
  Result := longint(Cardinal(Cardinal(Cardinal(AValue) shr (Shift and 31)) or (Cardinal(longint(Cardinal(0 - Cardinal(Cardinal(AValue) shr 31)) and Cardinal(longint(0 - (Ord((Shift and 31) <> 0) { and 1 } ))))) shl (32 - (Shift and 31)))));
end;

function SAR64(const AValue: Int64; Shift: Byte): Int64;
begin
  Result := Int64(UInt64(UInt64(UInt64(AValue) shr (Shift and 63)) or (UInt64(Int64(UInt64(0 - UInt64(UInt64(AValue) shr 63)) and UInt64(Int64(0 - (Ord((Shift and 63) <> 0) { and 1 } ))))) shl (64 - (Shift and 63)))));
end;

function MemoryAlign(addr: Pointer; alignment: NativeUInt): Pointer;
var
  tmp: NativeUInt;
begin
  tmp := NativeUInt(addr) + (alignment - 1);
  Result := Pointer(tmp - (tmp mod alignment));
end;


{$IFDEF FPC}


function TCoreClassInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

constructor TCoreClassListForObj.Create;
begin
  inherited Create(False);
end;

{$ELSE}


function TCoreClassInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

function TGenericsList<T>.ListData: Pointer;
begin
  Result := @(inherited List);
end;

function TGenericsObjectList<T>.ListData: Pointer;
begin
  Result := @(inherited List);
end;

function TCoreClassList.ListData: PCoreClassPointerList;
begin
  Result := @(inherited List);
end;

function TCoreClassListForObj.ListData: PCoreClassForObjectList;
begin
  Result := @(inherited List);
end;

{$ENDIF}



procedure TComputeThread.Execute;
begin
  try
    if Assigned(OnRunCall) then
        OnRunCall(Self);
    if Assigned(OnRunMethod) then
        OnRunMethod(Self);
    {$IFNDEF FPC}
    if Assigned(OnRunProc) then
        OnRunProc(Self);
    {$ENDIF FPC}
  except
  end;

  {$IFDEF FPC}
  Synchronize(@Done_Sync);
  {$ELSE FPC}
  Synchronize(Done_Sync);
  {$ENDIF FPC}
end;

procedure TComputeThread.Done_Sync;
begin
  try
    if Assigned(OnDoneCall) then
        OnDoneCall(Self);
    if Assigned(OnDoneMethod) then
        OnDoneMethod(Self);
    {$IFNDEF FPC}
    if Assigned(OnDoneProc) then
        OnDoneProc(Self);
    {$ENDIF FPC}
  except
  end;
end;

constructor TComputeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  OnRunCall := nil;
  OnRunMethod := nil;
  {$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
  {$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

class function TComputeThread.RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall): TComputeThread;
begin
  Result := TComputeThread.Create;
  Result.FreeOnTerminate := True;

  Result.OnRunCall := OnRun;
  Result.OnDoneCall := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

class function TComputeThread.RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod): TComputeThread;
begin
  Result := TComputeThread.Create;
  Result.FreeOnTerminate := True;

  Result.OnRunMethod := OnRun;
  Result.OnDoneMethod := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

{$IFNDEF FPC}


class function TComputeThread.RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc): TComputeThread;
begin
  Result := TComputeThread.Create;
  Result.FreeOnTerminate := True;

  Result.OnRunProc := OnRun;
  Result.OnDoneProc := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;
{$ENDIF FPC}


initialization
  InitCriticalLock;
  InitLockIDBuff;
  MHGlobalHookEnabled := True;

  // float check
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
finalization
  FreeCriticalLock;
  FreeLockIDBuff;
  MHGlobalHookEnabled := False;
end.


