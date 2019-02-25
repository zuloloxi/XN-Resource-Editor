(*======================================================================*
 | unitStreamTextReader                                                 |
 |                                                                      |
 | Various classes for reading (and writing) text from streams:         |
 |                                                                      |
 |   TStreamTextReader is a fairly fast, buffered CRLF or LF separated  |
 |   line reader                                                        |
 |                                                                      |
 |   TStreamWideTextReader is a fairly fast, buffered CRLF or LF        |
 |   separated line reader for wide-text files                          |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  CRS 2009 release version                  |
 *======================================================================*)
unit unitStreamTextReader;

interface

uses
  Windows, Classes, SysUtils;

type
  EStreamTextIO = class (Exception);

  TCircularBuffer = class
  private
    FBuffer: TBytes;

    FWritePos, FReadPos: Integer;
    FU32W: Word;

    function ReadUTF8 (b: byte; var ch: char): Boolean;
    function ReadUTF7 (b: byte; var ch: char): Boolean;

    function GetAvailableBytes: Integer;
  public
    procedure Write (data: TBytes; l: Integer = -1);
    function ReadByte (var b: byte): Boolean;
    function ReadChar (var ch: char; encoding: TEncoding): Boolean;

    property AvailableBytes: Integer read GetAvailableBytes;
  end;


  TStreamTextIO = class
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FReadData: TBytes;
    FReadBuffer: TCircularBuffer;
  public
    constructor Create (AStream: TStream; AEncoding: TEncoding = nil);
    destructor Destroy; override;
    procedure WriteLn (const st: string);
    function ReadLn (var st: string): Boolean;
  end;

implementation


{ TStreamTextIO }

constructor TStreamTextIO.Create(AStream: TStream; AEncoding: TEncoding = nil);
begin
  if AEncoding = Nil then
    FEncoding := TEncoding.ASCII
  else
    FEncoding := AEncoding;

  FStream := AStream
end;

destructor TStreamTextIO.Destroy;
begin
  FReadBuffer.Free;

  inherited;
end;

var
  c: Integer = 0;

function TStreamTextIO.ReadLn(var st: string): Boolean;
var
  bts: TBytes;
  enc: TEncoding;
  l, p: Integer;
  ch: char;
  s: string;
  w: Word;
begin
  if FReadBuffer = Nil then
    FReadBuffer := TCircularBuffer.Create;

  SetLength (FReadData, 4096);

  SetLength (s, 4096);
  p := 0;

  repeat
    if not FReadBuffer.ReadChar (ch, FEncoding) then
    begin
      Inc(c);

      if FStream.Position = 0 then
      begin
        if FStream.Size < 4 then
          SetLength (bts, FStream.Size)
        else
          SetLength (bts, 4);

        FStream.Read(bts [0], Length (bts));

        enc := Nil;
        FStream.Position := TEncoding.GetBufferEncoding (bts, enc);

        if FStream.Position <> 0 then
          FEncoding := enc;
      end;

      l := FStream.Read(FReadData [0], Length (FReadData));

      if l = 0 then
        Exit(False);

      FReadBuffer.Write(FReadData, l);
    end
    else
    begin
      w := Word (ch);

      case w of
        13: continue;
        10: break;
        12: break;
        $85: break;
        $2028: break;
        $2029: break;
      end;

      Inc(p);
      if p = Length (s) then
        SetLength (s, Length (s) + 4096);

      s [p] := ch;
    end;
  until false;

  st := Copy (s, 1, p);
  Result := true
end;

procedure TStreamTextIO.WriteLn(const st: string);
var
  bts: TBytes;
  l: Integer;
begin
  if FStream.Position = 0 then
  begin
    bts := FEncoding.GetPreamble;
    l := Length (bts);
    if l <> 0 then
      FStream.Write (bts [0], l);
  end;

  bts := FEncoding.GetBytes(st + #13#10);
  FStream.Write(bts [0], Length (bts));
end;

{ TCircularBuffer }

function TCircularBuffer.GetAvailableBytes: Integer;
begin
  if FReadPos <= FWritePos then
    Result := FWritePos - FReadPos
  else
    Result := Length (FBuffer) - FReadPos + FWritePos;
end;

function TCircularBuffer.ReadByte(var b: byte): Boolean;
begin
  Result := AvailableBytes > 0;
  if Result then
  begin
    if FReadPos = Length (FBuffer) then
      FReadPos := 0;
    b := FBuffer [FReadPos];
    Inc(FReadPos);
  end
end;

function TCircularBuffer.ReadChar(var ch: char; encoding: TEncoding): Boolean;
var
  b, b1: byte;
  sp: Integer;
begin
  if FU32W <>0 then
  begin
    ch := Char (FU32W);
    FU32W := 0;
    exit(true);
  end;

  sp := FReadPos;
  Result := ReadByte (b);

  try
    if (not Result) or encoding.IsSingleByte then
    begin
      if Result then
        ch := char (b);
      Exit;
    end;

    if encoding = TEncoding.UTF8 then
    begin
      if (b and $80) <> 0 then
        Result := ReadUTF8 (b, ch)
      else
        if b = 0 then
          ch := #$fffd
        else
          ch := Char (b);
    end
    else
    if encoding = TEncoding.UTF7 then
    begin
      if (b in [$21..$7e]) and not(AnsiChar (b) in ['~', '\', '+']) then
        Result := ReadUTF7 (b, ch)
    end
    else
    if encoding = TEncoding.Unicode then
    begin
      Result := ReadByte (b1);
      if Result then
        ch := char (MakeWord (b, b1))
    end
    else
    if encoding = TEncoding.BigEndianUnicode then
    begin
      Result := ReadByte (b1);
      if Result then
        ch := char (MakeWord (b1, b))
    end
    else
      raise EStreamTextIO.Create ('Non-standard encoding type not supported');
  finally
    if not Result then
      FReadPos := sp
  end
end;

function TCircularBuffer.ReadUTF7(b: byte; var ch: char): Boolean;
begin
  ch := Char (b);
  Result := True;
end;

function TCircularBuffer.ReadUTF8(b: byte; var ch: char): Boolean;
var
  b1, b2, b3: byte;
  w: Word;
  u: DWORD;
begin
  try
    if (b and $c0) <> $c0 then
      raise EStreamTextIO.Create ('Invalid UTF8');

    if (b and $e0) = $c0 then  // 2 char encoding - 5 bytes from b and 6 from b1
    begin
      Result := ReadByte (b1);
      if Result then
      begin
        if (b1 and $c0) <> $80 then
          raise EStreamTextIO.Create ('Invalid UTF8');

        w := ((b and $1f) shl 6) or (b1 and $3f);

        if (w < $80) then
          raise EStreamTextIO.Create ('Invalid UTF8');   // Safety check

        ch := char (w);
      end
    end
    else
    if (b and $f0) = $e0 then  // 3 char encoding - 4 bytes from b, 6 from b1 & 6 from b2
    begin
      Result := ReadByte (b1) and ReadByte (b2);

      if Result then
      begin
        if ((b1 and $c0) <> $80) or ((b2 and $c0) <> $80) then
          raise EStreamTextIO.Create ('Invalid UTF8');

        w := ((b and $0f) shl 12) or ((b1 and $3f) shl 6) or (b2 and $3f);

        if w < $800 then
          raise EStreamTextIO.Create ('Invalid UTF8');   // Safety check

        ch := char (w);
      end
    end
    else
    if (b and $0f) = 0 then
    begin                       // 4 char encoding - 3 bytes from b, 6 from b1, b2 & b3
      Result := ReadByte (b1) and ReadByte (b2) and ReadByte (b3);

      if Result then
      begin
        if ((b1 and $c0) <> $80) or ((b2 and $c0) <> $80)or ((b3 and $c0) <> $80) then
          raise EStreamTextIO.Create ('Invalid UTF8');

        u := ((b and $7) shl 18) or ((b1 and $3f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f);

        if (u < $10000) then
          raise EStreamTextIO.Create ('Invalid UTF8');  // Safety check

        // Character doesn't fit in a single 2 byte Word, so convert it to
        // 2 UTF16 Words.

        ch :=  Char (u div $400 + $D800);
        FU32W := u mod $400 + $DC00;
      end
    end
    else
      raise EStreamTextIO.Create ('Invalid UTF8');

  except
    // Invalid UTF8.  Skip bytes until we hit a byte that looks like the
    // start of the next character
    repeat
      Result := ReadByte (b)
    until (not Result) or ((b and $80) = 0) or ((b and $c0) = $c0);

    if Result then
    begin
      ch := #$fffd;         // Return the
      FU32W := b
    end
  end
end;

procedure TCircularBuffer.Write(data: TBytes; l: Integer);
var
  spaceAvail, newSpaceLen, dataLen, datap: Integer;
  wrapped: Boolean;
begin
  wrapped := False;
  if l <> -1 then
    dataLen := l
  else
    dataLen := Length (data);

  if FWritePos = Length (FBuffer) then
  begin
    FWritePos := 0;
    wrapped := True
  end;

  if FReadPos > FWritePos then
    spaceAvail := FReadPos - FWritePos
  else
    spaceAvail := Length (FBuffer) - FWritePos + FReadPos;

  if spaceAvail < dataLen then
  begin
    newSpaceLen := dataLen - spaceAvail;
    newSpaceLen := ((newSpaceLen + 4095) div 4096) * 4096;

    SetLength (FBuffer, Length (FBuffer) + newSpaceLen);

    if FReadPos > FWritePos then
    begin
      Move (FBuffer [FReadPos], FBuffer [FReadPos + newSpaceLen], newSpaceLen-FReadPos);
      Inc(FReadPos, newSpaceLen)
    end
  end;

  datap := 0;
  if wrapped and (FReadPos = Length (FBuffer)) then
    FReadPos := 0;

  if FReadPos > FWritePos then
    spaceAvail := FReadPos - FWritePos
  else
    spaceAvail := Length (FBuffer) - FWritePos;

  if spaceAvail < dataLen then
  begin
    if spaceAvail = 0 then
      FReadPos := 0
    else
      Move (data [datap], FBuffer [FWritePos], spaceAvail);
    FWritePos := 0;
    Inc(datap, spaceAvail);
    Dec(dataLen, spaceAvail)
  end;

  if dataLen > 0 then
  begin
    Move (data [datap], FBuffer [FWritePos], dataLen);
    Inc(FWritePos, dataLen)
  end
end;

end.


