unit unitMIMECodec;

interface

uses Windows;

type
TEncoder = class
public
  constructor Create; virtual;
	procedure Encode (const inb : AnsiString; var outb : AnsiString); virtual; abstract;
end;

TDecoder = class
public
  constructor Create; virtual;
	procedure Decode (const inb : AnsiString; var outb : AnsiString); virtual; abstract;
end;

TEncoder3To4 = class (TEncoder)
private
  procedure EncodeUnit (In1, In2, In3 : AnsiChar; var DOut: DWORD);
protected
  fFillChar : AnsiChar;
  fEncodingTable : AnsiString;
public
	procedure Encode (const inb : AnsiString; var outb : AnsiString); override;
end;

TDecoder3To4 = class (TDecoder)
private
  procedure DecodeUnit (AIn : DWORD; var b1, b2, b3 : byte);
protected
  fFillChar : AnsiChar;
  fDecodeTable : AnsiString;
public
	procedure Decode (const inb : AnsiString; var outb : AnsiString); override;
end;

TMIMEEncoder = class (TEncoder3To4)
public
  constructor Create; override;
  function EncodeString (const st : AnsiString) : AnsiString;
end;

TMIMEDecoder = class (TDecoder3To4)
public
  constructor Create; override;
  function DecodeString (const st : AnsiString) : AnsiString;
end;

implementation

type
CardinalBytes = packed record case Integer of
  0 : (b1, b2, b3, b4 : byte);
  1 : (whole : DWORD);
  2 : (charArray : array [0..3] of AnsiChar);
end;

ThreeByteRec = packed record case Integer of
  0 : (bytes : array [0..2] of byte);
  1 : (ThreeChars : array [0..2] of AnsiChar);
  2 : (TwoChars : array [0..1] of AnsiChar; waste1 : AnsIChar);
  3 : (OneChar : AnsiChar; waste2 : array [0..1] of AnsiChar)
end;

{ TEncoder3To4 }

procedure TEncoder3To4.Encode(const inb : AnsiString; var outb: AnsiString);
var
  LIn1 : AnsiChar;
  LIn2 : AnsiChar;
  LIn3 : AnsiChar;
  LUnit : CardinalBytes;
  LBufSize, LLen, LPos, LSize : Integer;
  pOut : PAnsiChar;

begin
  outb := '';

  LBufSize := Length (inb);
  if LBufSize = 0 then Exit;

  SetLength (outb, ((LBufSize + 2) div 3) * 4);
  pOut := @outb [1];

	LLen := 0;
	LPos := 0;

	while LPos < LBufSize do
  begin
    Inc (LPos);
    LIn1 := inb [LPos];

    if LPos < LBufSize then
    begin
      Inc (LPos);
      LIn2 := inb [LPos];

      if LPos < LBufSize then
      begin
        Inc (LPos);
        LIn3 := inb [LPos];
        LSize := 3;
      end
      else
      begin
        LIn3 := #0;
        LSize := 2
      end;
    end
    else
    begin
      LIn3 := #0;
      LIn2 := #0;
      LSize := 1
    end;

		EncodeUnit (LIn1, LIn2, LIn3, LUnit.whole);

    Move (LUnit.whole, (pOut + LLen)^, 4);
    Inc (LLen, 4);


		if LSize < 3 then
    begin
			pOut [LLen - 1] := fFillChar;
			if LSize = 1 then
				pOut [LLen - 2] := fFillChar
    end
  end
end;

procedure TEncoder3To4.EncodeUnit (In1, In2, In3: AnsIChar; var DOut: DWORD);
var
  LUnit : CardinalBytes;
begin
  LUnit.charArray [0] := fEncodingTable [((byte (In1) shr 2) and 63)+1];
  LUnit.charArray [1] := fEncodingTable [(((byte (In1) shl 4) or (byte (In2) shr 4)) and 63) + 1];
  LUnit.charArray [2] := fEncodingTable [(((byte (In2) shl 2) or (byte (In3) shr 6)) and 63) + 1];
  LUnit.charArray [3] := fEncodingTable [(byte (In3) and 63)+1];

  DOut := LUnit.whole
end;

{ TMIMEEncoder }

constructor TMIMEEncoder.Create;
begin
  fFillChar := '=';
  fEncodingTable := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
end;

function TMIMEEncoder.EncodeString(const st: AnsiString): AnsiString;
begin
  Encode (st, result)
end;

{ TEncoder }

constructor TEncoder.Create;
begin
// stub
end;

{ TMIMEDecoder }

constructor TMIMEDecoder.Create;
begin
  inherited;

end;

function TMIMEDecoder.DecodeString(const st: AnsiString): AnsiString;
begin
  Decode (st, result);
end;

{ TDecoder3To4 }
procedure TDecoder3To4.DecodeUnit (AIn : DWORD; var b1, b2, b3 : byte);
var
  LUnit : CardinalBytes;
begin
	LUnit.whole := AIn;
	LUnit.whole := (DWORD (fDecodeTable [LUnit.b1]) shl 18) or
                 (DWORD (fDecodeTable [LUnit.b2]) shl 12) or
                 (DWORD (fDecodeTable [LUnit.b3]) shl 6) or
                 (DWORD (fDecodeTable [LUnit.b4]));

	b1 := LUnit.b3;
	b2 := LUnit.b2;
	b3 := LUnit.b1;
end;

procedure TDecoder3To4.Decode(const inb: AnsiString; var outb: AnsiString);
var
  LBout : ThreeByteRec;
  LUnit : CardinalBytes;
  LInPos, LOutPos, LInSize : Integer;
  AIn, AOut : PAnsiChar;
begin
  LInPos := 0;
  LOutPos := 0;
  LInSize := Length (inb);

  if (LInSize = 0) or ((LInSize mod 4) <> 0) then
  begin
    outb := '';
    exit
  end;

  AIn := @inb [1];
  SetLength (outb, (LInSize * 3) div 4);
  AOut := @outb [1];

  while LInPos < LInSize do
  begin
    Move (AIn [LinPos], LUnit, sizeof (LUnit));
    Inc (LInPos, SizeOf (LUnit));

		DecodeUnit (LUnit.whole, &LBOut.Bytes [0], &LBOut.Bytes [1], &LBOut.Bytes [2]);

		// If the 3rd (&4th) char of input is filler, then the output is
		// just one character
		if LUnit.charArray [2] = fFillChar then
    begin
			AOut [LOutPos] := LBOut.OneChar; Inc (LOutPos);
    end
		else
      if LUnit.charArray [3] = fFillChar then
      begin
        AOut [LOutPos] := LBOut.TwoChars [0]; Inc (LOutPos);
        AOut [LOutPos] := LBOut.TwoChars [1]; Inc (LOutPos);
      end
			else
      begin
				AOut [LOutPos] := LBOut.ThreeChars [0]; Inc (LOutPos);
				AOut [LOutPos] := LBOut.ThreeChars [1]; Inc (LOutPos);
				AOut [LOutPos] := LBOut.ThreeChars [2]; Inc (LoutPos)
      end
  end;

  SetLength (outb, LOutPos);
end;

{ TDecoder }

constructor TDecoder.Create;
begin
// stub
end;

end.
