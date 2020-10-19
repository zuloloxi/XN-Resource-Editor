unit DialogConsts;

interface

uses
  Windows, Classes, SysUtils;

const
  BUTTON_ID    = $80;
  EDIT_ID      = $81;
  STATIC_ID    = $82;
  LISTBOX_ID   = $83;
  SCROLLBAR_ID = $84;
  COMBOBOX_ID  = $85;

type
  TSZOrID = record
    isID: Boolean;
    sz: UnicodeString;
    id: Integer;
  end;

  TDlgTemplateEx = packed record
    dlgVer: Word;
    signature: Word;
    helpID: DWORD;
    exStyle: DWORD;
    style: DWORD;
    cDlgItems: WORD;
    x, y, cx, cy: smallint;

    // Then follows menu, class, title sz or id

    // if DS_SETFONT in style then follows

(*
    pointsize: Word;
    weight: Word;
    italic: Byte;
    charset: Byte;
    typeface: TszOrID;  //(sz only!)
*)

  end;

  PDlgTemplateEx = ^TDlgTemplateEx;

  TDlgItemTemplateEx = packed record
    helpID: DWORD;
    exStyle: DWORD;
    Style: DWORD;
    x: Smallint;
    y: Smallint;
    cx: Smallint;
    cy: Smallint;
    id: Word;

    // Then follows class and title sz or ID

    // Then follows extraCount: WORD, followed by creation data
  end;
  PDlgItemTemplateEx = ^TDlgItemTemplateEx;


procedure WriteSzOrId(Stream: TStream; const id: TSzOrId);
function StringToSzOrID(const st: UnicodeString): TszOrID;
procedure Pad(Stream: TStream);

implementation

procedure WriteSzOrId (Stream: TStream; const id: TSzOrId);
var
  w: Word;
  ws: UnicodeString;
begin
  if id.isID then
  begin
    w := $ffff;
    Stream.Write (w, SizeOf (w));

    w := id.id;
    Stream.Write (w, SizeOf (w))
  end
  else
  begin
    ws := id.sz;
    if ws = '' then
    begin
      w := 0;
      Stream.Write(w, sizeof (w))
    end
    else
      Stream.Write (ws [1], (Length (ws) + 1) * SizeOf (WideChar))
  end
end;

function StringToSzOrID (const st: UnicodeString): TszOrID;
var
  i: Integer;
begin
  Result.isID := True;
  Result.sz := st;

  for i := 1 to Length (st) do
    if (st [i] < '0') or (st [i] > '9') then
    begin
      Result.isID := False;
      break
    end;

  if Result.isID then
  begin
    Result.id := StrToInt(st);
    if Result.id > $ffff then
      Result.isID := False
  end;

  if Result.isID then
    Result.sz := ''
  else
    Result.id := 0
end;

procedure Pad (Stream: TStream);
var
  Padding: Integer;
begin
  if Stream.Position mod 4 <> 0 then
  begin
    Padding := 0;
    Stream.Write (Padding, 4 - (Stream.Position mod 4))
  end
end;


end.
