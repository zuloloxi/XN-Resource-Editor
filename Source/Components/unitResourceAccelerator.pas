(*======================================================================*
 | unitResourceAccelerator                                              |
 |                                                                      |
 | Encapsulates Accelerator resources in resources                      |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001,2008                                 |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)

unit unitResourceAccelerator;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, System.Contnrs,
  Vcl.Menus, unitResourceDetails;

type
  TAccelerator = packed record
    flags: Word;
    code: Word;
    id: Word;
    padding: Word;
  end;
  PAccelerator = ^TAccelerator;

  TAcceleratorResourceDetails = class (TResourceDetails)
  private
    FCount: Integer;
    function GetCount: Integer;
    function GetAccelerator(idx: Integer): TAccelerator;
    function GetAccelPointer (idx: Integer): PAccelerator;
  public
    constructor Create (AParent: TResourceModule; ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer; AData: pointer); override;
    class function GetBaseType: UnicodeString; override;

    procedure InitNew; override;
    function Add (flags, code, id: Integer): Integer;
    procedure Delete (idx: Integer);
    procedure SetAccelDetails (idx: Integer; flags, code, id: Integer);

    property Count: Integer read GetCount;
    property Accelerator [idx: Integer]: TAccelerator read GetAccelerator;
  end;

implementation

{ TAcceleratorResourceDetails }

function TAcceleratorResourceDetails.Add(flags, code, id: Integer): Integer;
var
  ct: Integer;
  p: PAccelerator;
begin
  ct := Count;
  Data.Size := Data.Size + sizeof (TAccelerator);
  Inc(FCount);
  p := GetAccelPointer (ct);
  p^.flags := flags or $80;
  p^.code := code;
  p^.id := id;
  p^.padding := 0;

  if Count > 1 then
  begin
    p := GetAccelPointer (Count - 2);
    p^.flags := p^.flags and not $80
  end;
  Result := ct;
end;

constructor TAcceleratorResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer;
  AData: pointer);
begin
  inherited Create (AParent, ALanguage, AName, AType, ASize, AData);

  FCount := -1;
end;

procedure TAcceleratorResourceDetails.Delete(idx: Integer);
var
  p, p1: PAccelerator;
begin
  if idx >= Count then Exit;

  if idx < Count - 1 then
  begin
    p := GetAccelPointer (idx);
    p1 := GetAccelPointer (idx + 1);
    Move (p1^, p^, sizeof (TAccelerator) * (Count - idx - 1));
  end;

  Dec(FCount);
  Data.Size := Data.Size - sizeof (TAccelerator);

  if Count > 0 then
  begin
    p := GetAccelPointer (Count - 1);
    p^.flags := p^.flags or $80
  end
end;

function TAcceleratorResourceDetails.GetAccelerator(
  idx: Integer): TAccelerator;
begin
  Result := GetAccelPointer (idx)^
end;

function TAcceleratorResourceDetails.GetAccelPointer(
  idx: Integer): PAccelerator;
begin
  if idx < Count then
  begin
    Result := PAccelerator (Data.Memory);
    Inc(result, idx)
  end
  else
    raise ERangeError.Create('Index out of bounds');
end;

class function TAcceleratorResourceDetails.GetBaseType: UnicodeString;
begin
  Result := IntToStr (Integer (RT_ACCELERATOR));
end;

function TAcceleratorResourceDetails.GetCount: Integer;
var
  p: PAccelerator;
  sz: Integer;
begin
  if FCount = -1 then
  begin
    p := PAccelerator (Data.Memory);
    FCount := 0;
    sz := 0;
    while sz + sizeof (TAccelerator) <= Data.Size do
    begin
      Inc(FCount);
      if (p^.flags and $80) <> 0 then
        Break;
      Inc(p);
      Inc(sz, sizeof (TAccelerator))
    end
  end;
  Result := FCount;
end;

procedure TAcceleratorResourceDetails.InitNew;
begin
  inherited;
end;


procedure TAcceleratorResourceDetails.SetAccelDetails(idx, flags, code,
  id: Integer);
var
  p: PAccelerator;
begin
  p := GetAccelPointer (idx);
  if p <> Nil then
  begin
    if idx = Count - 1 then
      flags := flags or $80;
    p^.flags := flags;
    p^.id := id;
    p^.code := code
  end
end;

initialization
  RegisterResourceDetails (TAcceleratorResourceDetails);
finalization
  UnregisterResourceDetails (TAcceleratorResourceDetails);
end.
