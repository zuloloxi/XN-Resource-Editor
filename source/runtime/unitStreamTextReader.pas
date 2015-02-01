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

uses Windows, Classes, SysUtils;

type

EStreamTextIO = class (Exception);

TCircularBuffer = class
private
  fBuffer : TBytes;

  fWritePos, fReadPos : Integer;
  fu32w : word;

  function ReadUTF8 (b : byte; var ch : char) : boolean;
  function ReadUTF7 (b : byte; var ch : char) : boolean;

  function GetAvailableBytes: Integer;
public
  procedure Write (data : TBytes; l : Integer = -1);
  function ReadByte (var b : byte) : boolean;
  function ReadChar (var ch : char; encoding : TEncoding) : boolean;

  property AvailableBytes : Integer read GetAvailableBytes;
end;


TStreamTextIO = class
private
  fStream : TStream;
  fEncoding : TEncoding;
  fReadData : TBytes;
  fReadBuffer : TCircularBuffer;
public
  constructor Create (AStream : TStream; AEncoding : TEncoding = nil);
  destructor Destroy; override;
  procedure WriteLn (const st : string);
  function ReadLn (var st : string) : boolean;
end;

implementation


{ TStreamTextIO }

constructor TStreamTextIO.Create(AStream: TStream; AEncoding : TEncoding = nil);
begin
  if AEncoding = Nil then
    fEncoding := TEncoding.ASCII
  else
    fEncoding := AEncoding;

  fStream := AStream
end;

destructor TStreamTextIO.Destroy;
begin
  fReadBuffer.Free;

  inherited;
end;

var
  c : Integer = 0;

function TStreamTextIO.ReadLn(var st: string): boolean;
var
  bts : TBytes;
  enc : TEncoding;
  l, p : Integer;
  ch : char;
  s : string;
  w : word;
begin
  if fReadBuffer = Nil then
    fReadBuffer := TCircularBuffer.Create;

  SetLength (fReadData, 4096);

  SetLength (s, 4096);
  p := 0;

  repeat
    if not fReadBuffer.ReadChar (ch, fEncoding) then
    begin
      Inc (c);

      if fStream.Position = 0 then
      begin
        if fStream.Size < 4 then
          SetLength (bts, fStream.Size)
        else
          SetLength (bts, 4);

        fStream.Read(bts [0], Length (bts));

        enc := Nil;
        fStream.Position := TEncoding.GetBufferEncoding (bts, enc);

        if fStream.Position <> 0 then
          fEncoding := enc;
      end;

      l := fStream.Read(fReadData [0], Length (fReadData));

      if l = 0 then
        Exit (False);

      fReadBuffer.Write(fReadData, l);
    end
    else
    begin
      w := word (ch);

      case w of
        13 : continue;
        10 : break;
        12 : break;
        $85 : break;
        $2028 : break;
        $2029 : break;
      end;

      Inc (p);
      if p = Length (s) then
        SetLength (s, Length (s) + 4096);

      s [p] := ch;
    end;
  until false;

  st := Copy (s, 1, p);
  result := true
end;

procedure TStreamTextIO.WriteLn(const st: string);
var
  bts : TBytes;
  l : Integer;
begin
  if fStream.Position = 0 then
  begin
    bts := fEncoding.GetPreamble;
    l := Length (bts);
    if l <> 0 then
      fStream.Write (bts [0], l);
  end;

  bts := fEncoding.GetBytes(st + #13#10);
  fStream.Write(bts [0], Length (bts));
end;

{ TCircularBuffer }

function TCircularBuffer.GetAvailableBytes : Integer;
begin
  if fReadPos <= fWritePos then
    result := fWritePos - fReadPos
  else
    result := Length (fBuffer) - fReadPos + fWritePos;
end;

function TCircularBuffer.ReadByte(var b: byte): boolean;
begin
  result := AvailableBytes > 0;
  if result then
  begin
    if fReadPos = Length (fBuffer) then
      fReadPos := 0;
    b := fBuffer [fReadPos];
    Inc (fReadPos);
  end
end;

function TCircularBuffer.ReadChar(var ch: char; encoding: TEncoding): boolean;
var
  b, b1 : byte;
  sp : Integer;
begin
  if fu32w <>0 then
  begin
    ch := Char (fu32w);
    fu32w := 0;
    exit (true);
  end;

  sp := fReadPos;
  result := ReadByte (b);

  try
    if (not result) or encoding.IsSingleByte then
    begin
      if result then
        ch := char (b);
      Exit;
    end;

    if encoding = TEncoding.UTF8 then
    begin
      if (b and $80) <> 0 then
        result := ReadUTF8 (b, ch)
      else
        if b = 0 then
          ch := #$fffd
        else
          ch := Char (b);
    end
    else
    if encoding = TEncoding.UTF7 then
    begin
      if (b in [$21..$7e]) and not (AnsiChar (b) in ['~', '\', '+']) then
        result := ReadUTF7 (b, ch)
    end
    else
    if encoding = TEncoding.Unicode then
    begin
      result := ReadByte (b1);
      if result then
        ch := char (MakeWord (b, b1))
    end
    else
    if encoding = TEncoding.BigEndianUnicode then
    begin
      result := ReadByte (b1);
      if result then
        ch := char (MakeWord (b1, b))
    end
    else
      raise EStreamTextIO.Create ('Non-standard encoding type not supported');
  finally
    if not result then
      fReadPos := sp
  end
end;

function TCircularBuffer.ReadUTF7(b: byte; var ch: char): boolean;
begin
  ch := Char (b);
  result := True;

end;

function TCircularBuffer.ReadUTF8(b: byte; var ch: char): boolean;
var
  b1, b2, b3 : byte;
  w : word;
  u : DWORD;
begin
  try
    if (b and $c0) <> $c0 then
      raise EStreamTextIO.Create ('Invalid UTF8');

    if (b and $e0) = $c0 then  // 2 char encoding - 5 bytes from b and 6 from b1
    begin
      result := ReadByte (b1);
      if result then
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
      result := ReadByte (b1) and ReadByte (b2);

      if result then
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
      result := ReadByte (b1) and ReadByte (b2) and ReadByte (b3);

      if result then
      begin
        if ((b1 and $c0) <> $80) or ((b2 and $c0) <> $80)or ((b3 and $c0) <> $80) then
          raise EStreamTextIO.Create ('Invalid UTF8');

        u := ((b and $7) shl 18) or ((b1 and $3f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f);

        if (u < $10000) then
          raise EStreamTextIO.Create ('Invalid UTF8');  // Safety check

        // Character doesn't fit in a single 2 byte word, so convert it to
        // 2 UTF16 words.

        ch :=  Char (u div $400 + $D800);
        fu32w := u mod $400 + $DC00;
      end
    end
    else
      raise EStreamTextIO.Create ('Invalid UTF8');

  except
    // Invalid UTF8.  Skip bytes until we hit a byte that looks like the
    // start of the next character
    repeat
      result := ReadByte (b)
    until (not result) or ((b and $80) = 0) or ((b and $c0) = $c0);

    if result then
    begin
      ch := #$fffd;         // Return the
      fu32w := b
    end
  end
end;

procedure TCircularBuffer.Write(data: TBytes; l : Integer);
var
  spaceAvail, newSpaceLen, dataLen, datap : Integer;
  wrapped : boolean;
begin
  wrapped := False;
  if l <> -1 then
    dataLen := l
  else
    dataLen := Length (data);

  if fWritePos = Length (fBuffer) then
  begin
    fWritePos := 0;
    wrapped := True
  end;

  if fReadPos > fWritePos then
    spaceAvail := fReadPos - fWritePos
  else
    spaceAvail := Length (fBuffer) - fWritePos + fReadPos;

  if spaceAvail < dataLen then
  begin
    newSpaceLen := dataLen - spaceAvail;
    newSpaceLen := ((newSpaceLen + 4095) div 4096) * 4096;

    SetLength (fBuffer, Length (fBuffer) + newSpaceLen);

    if fReadPos > fWritePos then
    begin
      Move (fBuffer [fReadPos], fBuffer [fReadPos + newSpaceLen], newSpaceLen-fReadPos);
      Inc (fReadPos, newSpaceLen)
    end
  end;

  datap := 0;
  if wrapped and (fReadPos = Length (fBuffer)) then
    fReadPos := 0;

  if fReadPos > fWritePos then
    spaceAvail := fReadPos - fWritePos
  else
    spaceAvail := Length (fBuffer) - fWritePos;

  if spaceAvail < dataLen then
  begin
    if spaceAvail = 0 then
      fReadPos := 0
    else
      Move (data [datap], fBuffer [fWritePos], spaceAvail);
    fWritePos := 0;
    Inc (datap, spaceAvail);
    Dec (dataLen, spaceAvail)
  end;

  if dataLen > 0 then
  begin
    Move (data [datap], fBuffer [fWritePos], dataLen);
    Inc (fWritePos, dataLen)
  end
end;

end.


