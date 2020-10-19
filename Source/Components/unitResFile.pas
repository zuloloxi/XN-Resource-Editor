unit unitResFile;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type
  TResourceList = class (TResourceModule)
  private
    FResourceList: TObjectList;
  protected
    function GetResourceCount: Integer; override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign (src: TResourceModule);
    procedure InsertResource (idx: Integer; details: TResourceDetails); override;
    procedure DeleteResource (idx: Integer); override;
    function AddResource (details: TResourceDetails): Integer; override;
    function IndexOfResource (details: TResourceDetails): Integer; override;
    procedure SortResources; override;
  end;

  TResModule = class (TResourceList)
  private
    F16Bit: Boolean;
    procedure ParseResource(header, data: PAnsiChar; dataSize: Integer);
  protected
  public
    procedure SaveToStream (Stream: TStream); override;
    procedure LoadFromStream (Stream: TStream); override;
  end;

implementation

{ TResModule }

procedure TResModule.ParseResource (header, data: PAnsiChar; dataSize: Integer);
var
  p: PAnsiChar;
  sName, sType: String;
  res: TResourceDetails;
  language, memoryFlags: Word;
  version, dataVersion, characteristics: DWORD;

  function GetName: String;
  begin
    if PWord (p)^ = $ffff then
    begin
      Inc(p, sizeof (Word));
      Result := IntToStr (PWord (p)^);
      Inc(p, sizeof (Word))
    end
    else
    begin
      Result := UnicodeString (PWideChar (p));
      Inc(p, (Length (Result) + 1) * sizeof (WideChar))
    end
  end;

begin
  try
    p := header;
    Inc(p, 2 * sizeof (Integer));
    sType := GetName;
    sName := GetName;

    if (Integer (p) mod 4) <> 0 then
      Inc(p, 4 - Integer (p) mod 4);

    dataVersion := PDWORD (p)^;
    Inc(p, sizeof (DWORD));
    memoryFlags := PWORD (p)^;
    Inc(p, sizeof (Word));
    language := PWORD (p)^;
    Inc(p, sizeof (Word));
    version := PDWORD (p)^;
    Inc(p, sizeof (DWORD));
    characteristics := PDWORD (p)^;
    Inc(p, sizeof (DWORD));

    if (dataSize <> 0) or (sName <> '0') then
    begin
      res := TResourceDetails.CreateResourceDetails (self, language, sName, sType, dataSize, data);
      res.Characteristics := characteristics;
      res.Version := version;
      res.MemoryFlags := memoryFlags;
      res.DataVersion := dataVersion;
      AddResource (res)
    end
    else       // NB!!!  32 bit .RES files start with a dummy '32-bit indicator'
               // resource !!!  Is this documented?  I don't think so!

      F16Bit := False;
  except
    raise Exception.Create('The resource file is corrupt');

  end;
end;

procedure TResModule.LoadFromStream(Stream: TStream);
var
  buffer, p, q: PAnsiChar;
  bufLen, n, DataSize, HeaderSize, ChunkSize: Integer;
begin
  bufLen := Stream.Size;
  GetMem (buffer, bufLen);
  try
    Stream.ReadBuffer (buffer^, bufLen);             // Read the entite file

    p := buffer;
    n := 0;
    F16Bit := True;
                                              // Parse each resource
    while n + 2 * sizeof (Integer) < bufLen do
    begin
      DataSize := PInteger (p)^;
      q := p;
      Inc(q, SizeOf  (Integer));
      HeaderSize := PInteger (q)^;
      q := p;
      Inc(q, HeaderSize);

      ParseResource (p, q, DataSize);
      ChunkSize := DataSize + HeaderSize;
      ChunkSize := ((ChunkSize + 3) div 4) * 4;
      Inc(p, ChunkSize);
      Inc(n, ChunkSize);
    end;

  finally
    FreeMem (buffer)
  end;
  SortResources
end;

procedure TResModule.SaveToStream(Stream: TStream);
var
  res: TResourceDetails;
  dataSize, headerSize, totalSize: Integer;
  header: array [0..1023] of char;
  i: Integer;

  function GetResHeader (header: PChar): DWORD;
  var
    pos: DWORD;
    len, dw: DWORD;
    w: Word;
    i: Integer;
    ws: WideString;
  begin
    pos := 0;
    ZeroMemory (header, 1024);

    i := ResourceNameToInt(res.ResourceType);
    if i = -1 then
    begin
      ws := res.ResourceType;
      len := (Length (ws) + 1) * sizeof (WideChar);
      Move (PWideChar (ws)^, header [pos], len);
      Inc(pos, len)
    end
    else
    begin
      w := $ffff;
      Move (w, header [pos], sizeof (w));
      Inc(pos, sizeof (w));

      w := Word (i);
      Move (w, header [pos], sizeof (w));
      Inc(pos, sizeof (w))
    end;

    i := ResourceNameToInt(res.ResourceName);
    if i = -1 then
    begin
      ws := res.ResourceName;
      len := (Length (ws) + 1) * sizeof (WideChar);
      Move (PWideChar (ws)^, header [pos], len);
      Inc(pos, len)
    end
    else
    begin
      w := $ffff;
      Move (w, header [pos], sizeof (w));
      Inc(pos, sizeof (w));

      w := Word (i);
      Move (w, header [pos], sizeof (w));
      Inc(pos, sizeof (w))
    end;

    if (pos mod 4) <> 0 then
      Inc(pos, 4 - (pos mod 4));

    dw := res.DataVersion;
    Move (dw, header [pos], sizeof (DWORD));
    Inc(pos, sizeof (DWORD));

    w := res.MemoryFlags;
    Move (w, header [pos], sizeof (WORD));
    Inc(pos, sizeof (WORD));

    w := res.ResourceLanguage;
    Move (w, header [pos], sizeof (WORD));
    Inc(pos, sizeof (WORD));

    dw := res.Version;
    Move (dw, header [pos], sizeof (DWORD));
    Inc(pos, sizeof (DWORD));

    dw := res.Characteristics;
    Move (dw, header [pos], sizeof (DWORD));
    Inc(pos, sizeof (DWORD));
    Result := pos;
  end;

begin
  if not F16Bit then               // Write 32-bit resource indicator (An empty type 0 resource)
  begin
    res := TResourceDetails.CreateNew (nil, 0, '0');
    try
      dataSize := res.Data.Size;

      Stream.WriteBuffer (dataSize, sizeof (dataSize));
      headerSize := GetResHeader (header);

      totalSize := headerSize + 2 * sizeof (DWORD);

      Stream.WriteBuffer (totalSize, sizeof (headerSize));
      Stream.WriteBuffer (header, headerSize);
    finally
      res.Free
    end
  end;

  dataSize := 0;
  if ResourceCount > 0 then
    for i := 0 to ResourceCount - 1 do
    begin
      res := ResourceDetails [i];
      dataSize := res.Data.Size;

      Stream.WriteBuffer (dataSize, sizeof (dataSize));
      headerSize := GetResHeader (header);

      totalSize := headerSize + 2 * sizeof (DWORD);

      Stream.WriteBuffer (totalSize, sizeof (headerSize));
      Stream.WriteBuffer (header, headerSize);
      Stream.WriteBuffer (res.Data.Memory^, dataSize);

      totalSize := dataSize + totalSize;
      ZeroMemory (@header, sizeof (header));

      if (totalSize mod 4) <> 0 then
        Stream.WriteBuffer (header, 4 - (totalSize mod 4));
    end
end;

{ TResourceList }

function TResourceList.AddResource(details: TResourceDetails): Integer;
begin
  Result := FResourceList.Add (details);
end;

procedure TResourceList.Assign(src: TResourceModule);
var
  i: Integer;
  res: TResourceDetails;
begin
  FResourceList.Clear;

  for i := 0 to src.ResourceCount - 1 do
  begin
    res := TResourceDetails.CreateResourceDetails (
      Self,
      src.ResourceDetails [i].ResourceLanguage,
      src.ResourceDetails [i].ResourceName,
      src.ResourceDetails [i].ResourceType,
      src.ResourceDetails [i].Data.Size,
      src.ResourceDetails [i].Data.Memory);

    FResourceList.Add (res)
  end
end;

constructor TResourceList.Create;
begin
  FResourceList := TObjectList.Create;
end;

procedure TResourceList.DeleteResource(idx: Integer);
var
  res: TResourceDetails;
begin
  res := ResourceDetails [idx];
  inherited;
  idx := IndexOfResource (Res);
  if idx <> -1 then
    FResourceList.Delete (idx)
end;

destructor TResourceList.Destroy;
begin
  FResourceList.Free;
  inherited;
end;

function TResourceList.GetResourceCount: Integer;
begin
  Result := FResourceList.Count
end;

function TResourceList.GetResourceDetails(idx: Integer): TResourceDetails;
begin
  Result := TResourceDetails (FResourceList [idx])
end;

function TResourceList.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := FResourceList.IndexOf (details)
end;

procedure TResourceList.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  FResourceList.Insert(idx, details)
end;

procedure TResourceList.SortResources;
begin
  FResourceList.Sort(compareDetails);
end;

end.
