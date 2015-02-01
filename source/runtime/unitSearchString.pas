(*======================================================================*
 | unitStringSearcher                                                   |
 |                                                                      |
 | Useful classes and functions for searching strings                   |
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
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)
unit unitSearchString;

interface

uses Windows, Classes, SysUtils, StrUtils, unitCharsetMap;

type
  TStrArray = array of String;
  TStringSearcher = class
  protected
    fCaseSensitive : boolean;
    fSearchString : String;
    fOrWords : TStrArray;
    fNotWords : TStrArray;
    fAndWords : TStrArray;

    nOrWords : Integer;
    nAndWords : Integer;
    nNotWords : Integer;

  public
    constructor Create (const ASearchString : String; ACaseSensitive : boolean);

    function Matches (AString : String) : boolean;
    procedure Parse (searchString : String); virtual; abstract;
  end;

  TGoogleLikeStringSearcher = class (TStringSearcher)
  public
    procedure Parse (searchString : String); override;
  end;

function ExtractString (const search : string; var s : string) : string;
function SplitString (const search : string; var s : string; trimRemainder : boolean = true) : string; overload;
function SplitString (const search : AnsiString; var s : AnsiString; trimRemainder : boolean = true) : AnsiString; overload;
function SplitToken (var st : string) : string;
function DelimPos (const delims : string; const st : string; out delim : char) : Integer;
function DelimSplitString (const search : string; var s : string; out delim : char) : string;
function SearchStringArray (arr : array of string; const st : string) : Integer;
function StringArrayContains (arr : array of string; const st : string) : boolean;
function WildContains (const a, b : String) : boolean;
function SearchQuotedString (const st : string; const delims : string; quote : char = '"'; brk : char = #0):  Integer;
function SearchRQuotedString (const st : string; const delims : string; quote : char = '"'; brk : char = #0):  Integer;
function IsCharAlNum (ch : char) : boolean;
function IsCharHex (ch : char) : boolean;

function CompareAnsiText (const s1, s2 : AnsiString) : Integer;
function SameAnsiText (const s1, s2 : AnsiString) : boolean;


implementation

function IsCharAlNum (ch : char) : boolean;
begin
  result := ((ch >= '0') and (ch <= '9')) or
            ((ch >= 'A') and (ch <= 'Z')) or
            ((ch >= 'a') and (ch <= 'z'))

end;

function IsCharHex (ch : char) : boolean;
begin
  result := ((ch >= '0') and (ch <= '9')) or
            ((ch >= 'A') and (ch <= 'F')) or
            ((ch >= 'a') and (ch <= 'f'))
end;

(*----------------------------------------------------------------------*
 | function ExtractString : string                                      |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return an empty string    |
 | and leave the source string unchanged.                               |
 *----------------------------------------------------------------------*)
function ExtractString (const search : string; var s : string) : string;
var
  p, l : Integer;
  pc : PChar;

begin
  l := Length (search);
  if l = 1 then
  begin
    pc := StrScan (PChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := (Integer (pc) - Integer (PChar (s))) div sizeof (char) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + l, maxInt))
  end
  else
    result := ''
end;

(*----------------------------------------------------------------------*
 | function SplitString : string                                        |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return the entire source  |
 | string, and set the source string to an empty string                 |
 *----------------------------------------------------------------------*)
function SplitString (const search : string; var s : string; trimRemainder : boolean = true) : string;
var
  p, l : Integer;
  pc : PChar;
begin
  l := Length (search);
  if l = 1 then
  begin
    pc := StrScan (PChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := (Integer (pc) - Integer (PChar (s))) div sizeof (char) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    result := Trim (Copy (s, 1, p - 1));

    s := Copy (s, p + l, maxInt);
    if trimRemainder then
      s := Trim (s)
  end
  else
  begin
    result := Trim (s);
    s := ''
  end
end;

function AnsiTrim(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function SplitString (const search : AnsiString; var s : AnsiString; trimRemainder : boolean = true) : AnsiString;
var
  p, l : Integer;
  pc : PAnsiChar;
begin
  l := Length (search);
  if l = 1 then
  begin
    pc := StrScan (PAnsiChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := (Integer (pc) - Integer (PAnsiChar (s))) div sizeof (AnsiChar) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    result := AnsiTrim (Copy (s, 1, p - 1));

    s := Copy (s, p + l, maxInt);
    if trimRemainder then
      s := AnsiTrim (s)
  end
  else
  begin
    result := AnsiTrim (s);
    s := ''
  end
end;

function SplitToken (var st : string) : string;
var
  p, p1 : Integer;
begin
  p := Pos (' ', st);
  p1 := Pos (#9, st);
  if p = 0 then p := MaxInt;
  if p1 = 0 then p1 := MaxInt;
  if p < p1 then
    result := SplitString (' ', st)
  else
    result := SplitString (#9, st)
end;


function WildContains (const a, b : String) : boolean;
var
  p, offs, l, l1 : Integer;
begin
  l := Length (a);
  l1 := Length (b);

  if (l1 = 0) or (l = 0) then
  begin
    result := False;
    Exit
  end;

  if b [l1] = '*' then
  begin
    result := ContainsStr (a, Copy (b, 1, l1 - 1));
    exit
  end;

  offs := 1;
  repeat
    p := PosEx (b, a, offs);
    offs := 0;
    if p > 0 then
    begin
      if (p > 1) and IsCharAlNum (a [p - 1]) then
      begin
        offs := p + 1;
        p := 0;
        continue
      end;

      if ((p + l1) < l) and IsCharAlNum (a [p + l1]) then
      begin
        offs := p + l1 + 1;
        p := 0;
        continue
      end
    end
  until (p <> 0) or (offs = 0);

  result := p <> 0
end;

{ TStringSearcher }

constructor TStringSearcher.Create(const ASearchString: String; ACaseSensitive : boolean);
begin
  fCaseSensitive := ACaseSensitive;
  fSearchString := ASearchString;
  Parse (ASearchString)
end;

function TStringSearcher.Matches(AString: String): boolean;
type
  TMatch = (mYes, mNo, mMaybe);
var
  i : Integer;
  ok : TMatch;

begin
  if not fCaseSensitive then
    AString := UpperCase (AString);
  ok := mMaybe;

  for i := 0 to nOrWords - 1 do
    if WildContains (AString, fOrWords [i]) then
    begin
      ok := mYes;
      break
    end;

  if ok = mMaybe then
    for i := 0 to nAndWords - 1 do
      if not WildContains (AString, fAndWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    for i := 0 to nNotWords - 1 do
      if WildContains (AString, fNotWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    result := (nAndWords > 0) or (nNotWords > 0)
  else
    result := ok = mYes
end;

{ TGoogleLikeStringSearcher }

procedure TGoogleLikeStringSearcher.Parse(searchString: String);
type
  tOP = (opAnd, opOr, opNot);
var
  l : Integer;
  s1 : String;
  op : tOp;

  procedure AddToVarArray (var arr : TStrArray; const st : String; var n : Integer);
  begin
    if n = Length (arr) then
      SetLength (arr, n + 5);
    arr [n] := st;
    Inc (n)
  end;

begin
  if CompareText (fSearchString, searchString) = 0 then
    Exit;
  fSearchString := searchString;
  nAndWords := 0;
  nOrWords := 0;
  nNotWords := 0;
  if not fCaseSensitive then
    searchString := UpperCase (searchString);

  l := Length (searchString);
  op := opAnd;
  while l > 0 do
  begin
    case searchString [1] of
      '+' :
        begin
          op := opAnd;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end;
      '-' :
        begin
          op := opNot;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end
    end;

    if l = 0 then break;

    if searchString [1] = '"' then
    begin
      Delete (searchString, 1, 1);
      s1 := SplitString ('"', searchString)
    end
    else
    begin
      s1 := SplitString (' ', searchString);
      if UpperCase (s1) = 'OR' then
      begin
        op := opOR;
        l := Length (searchString);
        continue
      end
    end;

    if s1 <> '' then
      case op of
        opAnd : AddToVarArray (fAndWords, s1, nAndWords);
        opOr  : AddToVarArray (fOrWords, s1, nOrWords);
        opNot : AddToVarArray (fNotWords, s1, nNotWords)
      end;

    op := opAnd;
    l := Length (searchString)
  end
end;

function SearchStringArray (arr : array of string; const st : string) : Integer;

  function bsearch (s, e : Integer) : Integer;
  var
    m, c : Integer;
  begin
    if s <= e then
    begin
      m := s + (e - s) div 2;

      c := CompareText (st, arr [m]);

      if c = 0 then
        result := m
      else
        if c > 0 then
          result := bsearch (m + 1, e)
        else
          result := bsearch (s, m - 1)
    end
    else
      result := -1
  end;

begin
  result := bsearch (Low (arr), High (arr))
end;

function StringArrayContains (arr : array of string; const st : string) : boolean;
begin
  result := SearchStringArray (arr, st) >= 0
end;


function DelimPos (const delims : string; const st : string; out delim : char) : Integer;
var
  i, p : Integer;
begin
  if delims = '' then
  begin
    result := 0;
    exit
  end;

  result := MaxInt;
  for i := 1 to Length (delims) do
  begin
    p := Pos (delims [i], st);
    if (p > 0) and (p < result) then
    begin
      delim := delims [i];
      result := p
    end
  end;

  if result = MaxInt then
    result := 0
end;

function DelimSplitString (const search : string; var s : string; out delim : char) : string;
var
  p : Integer;
begin
  p := DelimPos (search, s, delim);
  if p > 0 then
  begin
    result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + 1, maxInt))
  end
  else
  begin
    result := Trim (s);
    s := ''
  end
end;

function SearchQuotedString (const st : string; const delims : string; quote : char; brk : char):  Integer;
var
  p : Integer;
  c : char;
  inQuote : boolean;
  l : Integer;
begin
  result := 0;
  l := Length (st);
  inQuote := False;

  for p := 1 to l do
  begin
    c := st [p];

    if c = brk then
      break
    else
      if c = quote then
        InQuote := not InQuote
      else
        if not InQuote and (Pos (c, delims) > 0) then
        begin
          result := p;
          break
        end
  end
end;

function SearchRQuotedString (const st : string; const delims : string; quote : char; brk : char):  Integer;
var
  p : Integer;
  c : char;
  inQuote : boolean;
  l : Integer;
begin
  result := 0;
  l := Length (st);
  inQuote := False;

  for p := 1 to l do
  begin
    c := st [p];

    if c = brk then
      break
    else
      if c = quote then
        InQuote := not InQuote
      else
        if not InQuote and (Pos (c, delims) > 0) then
          result := p;
  end
end;

function CompareAnsiText (const s1, s2 : AnsiString) : Integer;
begin
  result := CompareStringA (LOCALE_USER_DEFAULT, NORM_IGNORECASE, PAnsiChar(S1),
      Length(S1), PAnsiChar(S2), Length(S2)) - CSTR_EQUAL;
end;

function SameAnsiText (const s1, s2 : AnsiString) : boolean;
begin
  result := CompareAnsiText (s1, s2) = 0
end;


end.

