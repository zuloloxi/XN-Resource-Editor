(*======================================================================*
 | unitResourceVersionInfo                                              |
 |                                                                      |
 | Encapsulates Version Info resources in resources                     |
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
unit unitResourceVersionInfo;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type
  TFileFlags = (ffDebug, ffInfoInferred, ffPatched, ffPreRelease,
    ffPrivateBuild, ffSpecialBuild);
  TVersionFileFlags = set of TFileFlags;

  TVersionStringValue = class
  private
    FKeyName: UnicodeString;
    FValue: UnicodeString;
    FLangId: Integer;
    FCodePage: Integer;
  public
    constructor Create (const AKeyName, AValue: UnicodeString; ALangId, ACodePage: Integer);
    property KeyName: UnicodeString read FKeyName;
    property Value: UnicodeString read FValue;
  end;

  TVersionInfoResourceDetails = class (TResourceDetails)
  private
    FChildStrings: TObjectList;
    FFixedInfo: PVSFixedFileInfo;
    FTranslations: TList;
    procedure GetFixedFileInfo;
    procedure UpdateData;
    procedure ExportToStream (strm: TStream);

    function GetFileFlags: TVersionFileFlags;
    function GetFileVersion: TULargeInteger;
    function GetKey(idx: Integer): TVersionStringValue;
    function GetKeyCount: Integer;
    function GetProductVersion: TULargeInteger;
    procedure SetFileFlags(const Value: TVersionFileFlags);
    procedure SetFileVersion(const Value: TULargeInteger);
    procedure SetProductVersion(const Value: TULargeInteger);
  protected
    constructor Create (AParent: TResourceModule; ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer; AData: pointer); override;
    procedure InitNew; override;
  public
    constructor CreateNew(AParent: TResourceModule; ALanguage: Integer; const AName: UnicodeString); override;
    destructor Destroy; override;
    class function GetBaseType: UnicodeString; override;
    procedure ChangeData (newData: TMemoryStream); override;
    function SetKeyValue (const AKeyName, AValue: UnicodeString): Integer;
    procedure ChangeKey (const AOldKey, ANewKey: UnicodeString);
    procedure DeleteKey (idx: Integer);
    function IndexOf (const AKeyName: UnicodeString): Integer;
    property ProductVersion: TULargeInteger read GetProductVersion write SetProductVersion;
    property FileVersion: TULargeInteger read GetFileVersion write SetFileVersion;
    property FileFlags: TVersionFileFlags read GetFileFlags write SetFileFlags;
    property KeyCount: Integer read GetKeyCount;
    property Key [idx: Integer]: TVersionStringValue read GetKey;
  end;

implementation


resourcestring
  rstFlagsChanged = 'change flags';
  rstFileVersionChanged = 'change file version';
  rstProductVersionChanged = 'change product version';
  rstVersion      = 'Version';
  rstInvalidVersionInfoResource = 'Invalid version info resource';
  rstStringChanged = 'change string';
  rstStringAdded = 'add string';
  rstStringDeleted = 'delete string';
  rstCodePageChanged = 'change code page';
  rstKeyNameChanged = 'change string name';

{ TVersionInfoResourceDetails }

constructor TVersionInfoResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const AName: UnicodeString);
begin
  FChildStrings := TObjectList.Create;
  FTranslations := TList.Create;

  inherited;
end;

destructor TVersionInfoResourceDetails.Destroy;
begin
  FChildStrings.Free;
  FTranslations.Free;

  inherited;
end;

procedure TVersionInfoResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;

  FFixedInfo := nil;
end;

procedure TVersionInfoResourceDetails.ChangeKey(const AOldKey,
  ANewKey: UnicodeString);
var
  idx: Integer;
begin
  if AOldKey <> ANewKey then
  begin
    idx := IndexOf (AOldKey);
    if idx > -1 then
    begin
      Key [idx].FKeyName := ANewKey;
      UpdateData
    end
    else
      SetKeyValue (ANewKey, '');
  end;
end;

constructor TVersionInfoResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer;
  AData: pointer);
begin
  FChildStrings := TObjectList.Create;
  FTranslations := TList.Create;
  inherited Create (AParent, ALanguage, AName, AType, ASize, AData);
end;

procedure TVersionInfoResourceDetails.DeleteKey(idx: Integer);
begin
  FChildStrings.Delete (idx);
  UpdateData;
end;

procedure TVersionInfoResourceDetails.ExportToStream(strm: TStream);
var
  zeros, v: DWORD;
  wSize: WORD;
  stringInfoStream: TMemoryStream;
  strg: TVersionStringValue;
  i, p, p1: Integer;
  wValue: WideString;

  procedure PadStream (strm: TStream);
  begin
    if strm.Position mod 4 <> 0 then
      strm.Write (zeros, 4 - (strm.Position mod 4))
  end;

  procedure SaveVersionHeader (strm: TStream; wLength, wValueLength, wType: Word; const wKey: UnicodeString; const value);
  var
    valueLen: Word;
    keyLen: Word;
  begin
    strm.Write (wLength, sizeof (wLength));

    strm.Write (wValueLength, sizeof (wValueLength));
    strm.Write (wType, sizeof (wType));
    keyLen := (Length (wKey) + 1) * sizeof (WideChar);
    strm.Write (wKey [1], keyLen);

    PadStream (strm);

    if wValueLength > 0 then
    begin
      valueLen := wValueLength;
      if wType = 1 then
        valueLen := valueLen * sizeof (WideChar);
      strm.Write (value, valueLen)
    end;
  end;

begin { ExportToStream }
  GetFixedFileInfo;
  if FFixedInfo <> Nil then
  begin
    zeros := 0;

    SaveVersionHeader (strm, 0, sizeof (FFixedInfo^), 0, 'VS_VERSION_INFO', FFixedInfo^);

    if FChildStrings.Count > 0 then
    begin
      stringInfoStream := TMemoryStream.Create;
      try
        SaveVersionHeader (stringInfoStream, 0, 0, 0, IntToHex (ResourceLanguage, 4) + IntToHex (CodePage, 4), zeros);

        for i := 0 to FChildStrings.Count - 1 do
        begin
          PadStream (stringInfoStream);

          p := stringInfoStream.Position;
          strg := TVersionStringValue (FChildStrings [i]);
          wValue := strg.FValue;
          SaveVersionHeader (stringInfoStream, 0, Length (strg.FValue) + 1, 1, strg.KeyName, wValue [1]);
          wSize := stringInfoStream.Size - p;
          stringInfoStream.Seek (p, TSeekOrigin.soBeginning);
          stringInfoStream.Write (wSize, sizeof (wSize));
          stringInfoStream.Seek (0, soFromEnd);

        end;

        stringInfoStream.Seek (0, TSeekOrigin.soBeginning);
        wSize := stringInfoStream.Size;
        stringInfoStream.Write (wSize, sizeof (wSize));

        PadStream (strm);
        p := strm.Position;
        SaveVersionHeader (strm, 0, 0, 0, 'StringFileInfo', zeros);
        strm.Write (stringInfoStream.Memory^, stringInfoStream.size);
        wSize := strm.Size - p;
      finally
        stringInfoStream.Free
      end;
      strm.Seek (p, TSeekOrigin.soBeginning);
      strm.Write (wSize, sizeof (wSize));
      strm.Seek (0, soFromEnd)
    end;

    if FTranslations.Count > 0 then
    begin
      PadStream (strm);
      p := strm.Position;
      SaveVersionHeader (strm, 0, 0, 0, 'VarFileInfo', zeros);
      PadStream (strm);

      p1 := strm.Position;
      SaveVersionHeader (strm, 0, 0, 0, 'Translation', zeros);

      for i := 0 to FTranslations.Count - 1 do
      begin
        v := Integer (FTranslations [i]);
        strm.Write (v, sizeof (v))
      end;

      wSize := strm.Size - p1;
      strm.Seek (p1, TSeekOrigin.soBeginning);
      strm.Write (wSize, sizeof (wSize));
      wSize := sizeof (Integer) * FTranslations.Count;
      strm.Write (wSize, sizeof (wSize));

      wSize := strm.Size - p;
      strm.Seek (p, TSeekOrigin.soBeginning);
      strm.Write (wSize, sizeof (wSize));
    end;

    strm.Seek (0, TSeekOrigin.soBeginning);
    wSize := strm.Size;
    strm.Write (wSize, sizeof (wSize));
    strm.Seek (0, soFromEnd);
  end
  else
    raise Exception.Create ('Invalid version resource');
end;

class function TVersionInfoResourceDetails.GetBaseType: UnicodeString;
begin
  Result := IntToStr (Integer (RT_VERSION));
end;

function TVersionInfoResourceDetails.GetFileFlags: TVersionFileFlags;
var
  flags: Integer;
begin
  GetFixedFileInfo;
  Result := [];
  flags := FFixedInfo^.dwFileFlags and FFixedInfo^.dwFileFlagsMask;

  if (flags and VS_FF_DEBUG)        <> 0 then Result := result + [ffDebug];
  if (flags and VS_FF_INFOINFERRED) <> 0 then Result := result + [ffInfoInferred];
  if (flags and VS_FF_PATCHED)      <> 0 then Result := result + [ffPatched];
  if (flags and VS_FF_PRERELEASE)   <> 0 then Result := result + [ffPreRelease];
  if (flags and VS_FF_PRIVATEBUILD) <> 0 then Result := result + [ffPrivateBuild];
  if (flags and VS_FF_SPECIALBUILD) <> 0 then Result := result + [ffSpecialBuild];
end;

function TVersionInfoResourceDetails.GetFileVersion: TULargeInteger;
begin
  GetFixedFileInfo;
  _ULARGE_INTEGER(Result).LowPart := FFixedInfo^.dwFileVersionLS;
  _ULARGE_INTEGER(Result).HighPart := FFixedInfo^.dwFileVersionMS;
end;

procedure TVersionInfoResourceDetails.GetFixedFileInfo;
var
  p: PByte;
  t, wLength, wValueLength, wType: Word;
  key: UnicodeString;

  varwLength, varwValueLength, varwType: Word;
  varKey: UnicodeString;

  function GetVersionHeader (var p: PByte; var wLength, wValueLength, wType: Word; var wKey: UnicodeString): Integer;
  var
    baseP: PByte;
    szKey: PWideChar;
  begin
    baseP := p;
    wLength := PWord (p)^;
    Inc(p, sizeof (Word));
    wValueLength := PWord (p)^;
    Inc(p, sizeof (Word));
    wType := PWord (p)^;
    Inc(p, sizeof (Word));
    szKey := PWideChar (p);
    Inc(p, (lstrlenw (szKey) + 1) * sizeof (WideChar));
    while Integer (p) mod 4 <> 0 do
      Inc(p);
    Result := p - baseP;
    wKey := szKey;
  end;

  procedure GetStringChildren (var base: PByte; len: Word);
  var
    p, strBase: PByte;
    t, wLength, wValueLength, wType, wStrLength, wStrValueLength, wStrType: Word;
    key, value: UnicodeString;
    langID, codePage: Integer;

  begin
    p := base;
    while (p - base) < len do
    begin
      t := GetVersionHeader (p, wLength, wValueLength, wType, key);
      Dec(wLength, t);

      langID := StrToInt('$' + Copy (key, 1, 4));
      codePage := StrToInt('$' + Copy (key, 5, 4));

      strBase := p;
      FChildStrings.Clear;
      FTranslations.Clear;

      while (p - strBase) < wLength do
      begin
        t := GetVersionHeader (p, wStrLength, wStrValueLength, wStrType, key);
        Dec(wStrLength, t);

        if wStrValueLength = 0 then
          value := ''
        else
          value := PWideChar (p);
        Inc(p, wStrLength);
        while Integer (p) mod 4 <> 0 do
          Inc(p);

        if codePage = 0 then
          codePage := self.codePage;
        FChildStrings.Add (TVersionStringValue.Create (key, Value, langID, codePage));
      end
    end;
    base := p
  end;

  procedure GetVarChildren (var base: PByte; len: Word);
  var
    p, strBase: PByte;
    t, wLength, wValueLength, wType: Word;
    key: UnicodeString;
    v: DWORD;
  begin
    p := base;
    while (p - base) < len do
    begin
      t := GetVersionHeader (p, wLength, wValueLength, wType, key);
      Dec(wLength, t);

      strBase := p;
      FTranslations.Clear;

      while (p - strBase) < wLength do
      begin
        v := PDWORD (p)^;
        Inc(p, sizeof (DWORD));
        FTranslations.Add (pointer (v));
      end
    end;
    base := p
  end;

begin
  if FFixedInfo <> nil then Exit;

  FChildStrings.Clear;
  FTranslations.Clear;
  p := data.memory;
  GetVersionHeader (p, wLength, wValueLength, wType, key);

  if wValueLength <> 0 then
  begin
    FFixedInfo := PVSFixedFileInfo (p);
    if FFixedInfo^.dwSignature <> $feef04bd then
      raise Exception.Create (rstInvalidVersionInfoResource);

    Inc(p, wValueLength);
    while Integer (p) mod 4 <> 0 do
      Inc(p);
  end
  else
    FFixedInfo := Nil;

  while wLength > (Integer (p) - Integer (data.memory)) do
  begin
    t := GetVersionHeader (p, varwLength, varwValueLength, varwType, varKey);
    Dec(varwLength, t);

    if varKey = 'StringFileInfo' then
      GetStringChildren (p, varwLength)
    else
      if varKey = 'VarFileInfo' then
        GetVarChildren (p, varwLength)
      else
        break;
  end
end;

function TVersionInfoResourceDetails.GetKey(
  idx: Integer): TVersionStringValue;
begin
  GetFixedFileInfo;
  Result := TVersionStringValue (FChildStrings [idx])
end;

function TVersionInfoResourceDetails.GetKeyCount: Integer;
begin
  GetFixedFileInfo;
  Result := FChildStrings.Count
end;

function TVersionInfoResourceDetails.GetProductVersion: TULargeInteger;
begin
  GetFixedFileInfo;
  _ULARGE_INTEGER(Result).LowPart := FFixedInfo^.dwProductVersionLS;
  _ULARGE_INTEGER(Result).HighPart := FFixedInfo^.dwProductVersionMS
end;

function TVersionInfoResourceDetails.IndexOf(
  const AKeyName: UnicodeString): Integer;
var
  i: Integer;
  k: TVersionStringValue;
begin
  Result := -1;
  for i := 0 to KeyCount - 1 do
  begin
    k := Key [i];
    if CompareText(k.KeyName, AKeyName) = 0 then
    begin
      Result := i;
      break
    end
  end
end;

procedure TVersionInfoResourceDetails.InitNew;
var
  w, l: Word;
  fixedInfo: TVSFixedFileInfo;
  ws: UnicodeString;
begin
  l := 0;

  w := 0;
  Data.Write(w, sizeof (w));

  w := sizeof (fixedInfo);
  Data.Write (w, sizeof (w));

  w := 0;
  Data.Write (w, sizeof (w));

  ws := 'VS_VERSION_INFO';
  Data.Write(ws [1], (Length (ws) + 1) * sizeof (WideChar));

  w := 0;
  while Data.Size mod sizeof (DWORD) <> 0 do
    Data.Write (w, sizeof (w));

  ZeroMemory (@fixedInfo, sizeof (fixedInfo));
  fixedInfo.dwSignature        := $FEEF04BD;
  fixedInfo.dwStrucVersion     := $00010000;
  fixedInfo.dwFileVersionMS    := $00010000;
  fixedInfo.dwFileVersionLS    := $00000000;
  fixedInfo.dwProductVersionMS := $00010000;
  fixedInfo.dwProductVersionLS := $00000000;
  fixedInfo.dwFileFlagsMask    := $3f;
  fixedInfo.dwFileFlags        := 0;
  fixedInfo.dwFileOS           := 4;
  fixedInfo.dwFileType         := VFT_UNKNOWN;
  fixedInfo.dwFileSubtype      := VFT2_UNKNOWN;
  fixedInfo.dwFileDateMS       := 0;
  fixedInfo.dwFileDateLS       := 0;

  Data.Write(fixedInfo, sizeof (fixedInfo));

  w := 0;
  while Data.Size mod sizeof (DWORD) <> 0 do
    Data.Write (w, sizeof (w));

  l := Data.Size;
  Data.Seek(0, TSeekOrigin.soBeginning);

  Data.Write(l, sizeof (l))
end;

procedure TVersionInfoResourceDetails.SetFileFlags(
  const Value: TVersionFileFlags);
var
  flags: DWORD;
begin
  GetFixedFileInfo;

  flags := 0;
  if ffDebug in value then flags := flags or VS_FF_DEBUG;
  if ffInfoInferred in value then flags := flags or VS_FF_INFOINFERRED;
  if ffPatched in value then flags := flags or VS_FF_PATCHED;
  if ffPreRelease in value then flags := flags or VS_FF_PRERELEASE;
  if ffPrivateBuild in value then flags := flags or VS_FF_PRIVATEBUILD;
  if ffSpecialBuild in value then flags := flags or VS_FF_SPECIALBUILD;

  if (FFixedInfo^.dwFileFlags and FFixedInfo^.dwFileFlagsMask) <> flags then
    FFixedInfo^.dwFileFlags := (FFixedInfo^.dwFileFlags and not FFixedInfo^.dwFileFlagsMask) or flags;
end;

procedure TVersionInfoResourceDetails.SetFileVersion(
  const Value: TULargeInteger);
begin
  GetFixedFileInfo;
  if
    (_ULARGE_INTEGER(value).LowPart <> FFixedInfo^.dwFileVersionLS) or
    (_ULARGE_INTEGER(value).HighPart <> FFixedInfo^.dwFileVersionMS) then
  begin
    FFixedInfo^.dwFileVersionLS := _ULARGE_INTEGER(value).LowPart;
    FFixedInfo^.dwFileVersionMS := _ULARGE_INTEGER(value).HighPart;
  end
end;

function TVersionInfoResourceDetails.SetKeyValue(const AKeyName,
  AValue: UnicodeString): Integer;
var
  idx: Integer;
  k: TVersionStringValue;
begin
  idx := IndexOf (AKeyName);

  if idx = -1 then
  begin
    if AKeyName <> '' then
      idx := FChildStrings.Add (TVersionStringValue.Create (AKeyNAme, AValue, ResourceLanguage, CodePage))
  end
  else
  begin
    k := Key [idx];
    if (AValue <> k.FValue) or (AKeyName <> k.FKeyName) then
    begin
      k.FKeyName := AKeyName;
      k.FValue := AValue;
    end
  end;

  Result := idx;
  UpdateData
end;

procedure TVersionInfoResourceDetails.SetProductVersion(
  const Value: TULargeInteger);
begin
  GetFixedFileInfo;
  if
    (_ULARGE_INTEGER(value).LowPart <> FFixedInfo^.dwProductVersionLS) or
    (_ULARGE_INTEGER(value).HighPart <> FFixedInfo^.dwProductVersionMS) then
  begin
    FFixedInfo^.dwProductVersionLS := _ULARGE_INTEGER(value).LowPart;
    FFixedInfo^.dwProductVersionMS := _ULARGE_INTEGER(value).HighPart;
  end
end;

procedure TVersionInfoResourceDetails.UpdateData;
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  try
    ExportToStream (st);
    st.Seek (0, TSeekOrigin.soBeginning);
    data.Seek (0, TSeekOrigin.soBeginning);
    data.size := 0;
    data.CopyFrom (st, st.Size);
  finally
    st.Free
  end
end;

{ TVersionStringValue }

constructor TVersionStringValue.Create(const AKeyName, AValue: UnicodeString; ALangId, ACodePage: Integer);
begin
  FKeyName := AKeyName;
  FValue := AValue;
  FLangId := ALangId;
  FCodePage := ACodePage;
end;

initialization
  RegisterResourceDetails (TVersionInfoResourceDetails);
finalization
  UnregisterResourceDetails (TVersionInfoResourceDetails);
end.
