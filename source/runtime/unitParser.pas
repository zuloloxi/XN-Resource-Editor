(*======================================================================*
 | unitParser                                                           |
 |                                                                      |
 | TParser and TCPreProcessor classes, used eg. to parse 'C' header     |
 | files in my resource editor                                          |
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
unit unitParser;

interface

uses Windows, Classes, SysUtils, unitStreamTextReader, unitCExpression, Character;

const
  ttUnknown = 0;
  ttIdentifier = 1;
  ttNumber = 2;
  ttChar = 3;
  ttString = 4;

  ttOpAdd = 10;
  ttOpSub = 11;
  ttOpMul = 12;
  ttOpDiv = 13;
type

TfnCheckChar = function (ch : char) : boolean of object;

TParser = class
private
  fReader : TStreamTextIO;

  fLineBuf : String;
  fLineNo : Integer;
  fLinePos : Integer;

  fCh : Char;

  fTokenType : Integer;
  fToken : string;
  fTokenChar : Char;
  fDecimalSeparator: Char;
  fSOL: boolean;
  fFirstChar : boolean;
  fTokenSol : boolean;
    function IsDecimalSeparator(ch: char): boolean;

protected
  procedure RawGetToken (fnCheckChar : TfnCheckChar; tp : Integer);

public
  constructor Create (AStream : TStream);
  destructor Destroy; override;
  procedure Parse; virtual; abstract;

  function GetChar : Char;
  function GetNonWhitespace : Char;
  procedure SkipLine;
  function GetToken : boolean; virtual;
  procedure SkipWhitespace;
  procedure NextToken;

  function IsWhitespace (ch : char) : boolean; virtual;
  function IsFirstIdChar (ch : char) : boolean; virtual;
  function IsNextIdChar (ch : char) : boolean; virtual;
  function IsFirstNumChar (ch : char) : boolean; virtual;
  function IsNextNumChar (ch : char) : boolean; virtual;
  function IsHexChar (ch : char) : boolean; virtual;

  function NextIdentifier (const errMsg : string = 'Identifier expected') : string;
  function NextString (const errMsg : string = 'String constant expected') : string;
  function NextInteger (const errMsg : string = 'Integer constant expected') : Integer;
  function NextChar (ch : Char) : Char;

  procedure ExpectChar (ch : Char); virtual;
  function ExpectIdentifier (const errMsg : string = 'Identifier expected') : string;
  function ExpectString (const errMsg : string = 'String constant expected') : string; virtual;
  function ExpectInteger (const errMsg : string = 'Integer constant expected') : Integer; virtual;

  property LinePos : Integer read fLinePos;
  property LineNo : Integer read fLineNo;
  property Ch : Char read fCh;
  property SOL : boolean read fSOL;
  property TokenType : Integer read fTokenType;
  property Token : string read fToken;
  property TokenChar : Char read fTokenChar;
  property TokenSOL : boolean read fTokenSOL;

  property DecimalSeparator : Char read fDecimalSeparator write fDecimalSeparator;
end;

const
  dtInclude = 1;
  dtDefine  = 2;
  dtIfdef   = 3;
  dtIfNDef  = 4;
  dtEndif   = 5;
  dtUndef   = 6;
  dtElse    = 7;
  dtIf      = 8;
  dtPragma  = 9;
  dtError   = 10;

type
TCPreProcessor = class (TParser)
private
  fPathName: string;
  fIdentifiers : TStringList;
  fIfLevel : Integer;
  fIncludePath: string;
  fDirectives : TStringList;

  procedure CheckIdentifiers;
  procedure DoInclude;
  procedure DoDefine;
  procedure DoIfDef;
  procedure DoIfNDef;
  procedure DoEndif;
  procedure DoUndef;
  procedure DoElse;
  procedure DoIf;
  procedure DoPragma;
  function GetIncludePathName (const FileName : string) : string;
  procedure SkipIfElseBlock;
protected
  procedure HandleDirective;
  procedure HandlePragma (const st : string); virtual;
  function GetRestOfLine : string;
  procedure NextFileString (const errMsg : string);
public
  constructor Create (AStream : TStream);
  destructor Destroy; override;
  property PathName : string read fPathName write fPathName;
  property IncludePath : string read fIncludePath write fIncludePath;
  function GetToken : boolean; override;

  function IsFirstNumChar (ch : char) : boolean; override;

  procedure ExpectChar (ch : char); override;
  function ExpectString (const errMsg : string = 'String constant expected') : string; override;
  function ExpectInteger (const errMsg : string = 'Integer constant expected') : Integer; override;

  function Defined (id : string) : boolean;
  procedure AddIdentifier (const id, line : string);
  procedure DeleteIdentifier (const id : string);
  function IsIdentifier (const id : string) :boolean;
  function Resolve (var TokenType : Integer; var st : string) : TValue;
  function Calc (const st : string) : TValue;
  function ResolveToken : TValue;
end;

EParser = class (Exception);

implementation

uses unitSearchString;

{ TParser }

constructor TParser.Create(AStream: TStream);
begin
  fReader := TStreamTextIO.Create (AStream);
(*
  fWhitespace := ' '#9#10;
  fFirstIdChars := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  fNextNumChars := '0123456789';
  fNextIdChars := fFirstIdChars + fNextNumChars;
  fFirstNumChars := '$' + fNextNumChars;
*)
  fDecimalSeparator := '.';
  fSOL := True;
  fFirstChar := True;
end;

destructor TParser.Destroy;
begin
  fReader.Free;

  inherited;
end;

procedure TParser.ExpectChar(ch: Char);
begin
  if (TokenType <> ttChar) or (fTokenChar <> ch) then
    raise EParser.Create(ch + ' expected');
end;

function TParser.ExpectIdentifier(const errMsg: string): string;
begin
  if TokenType <> ttIdentifier then
    raise EParser.Create(errMsg);
  result := fToken;
end;

function TParser.ExpectInteger(const errMsg: string): Integer;
begin
  if tokenType = ttNumber then
    result := StrToInt (token)
  else
    raise EParser.Create(errMsg);
end;

function TParser.ExpectString(const errMsg: string): string;
begin
  if tokenType = ttString then
    result := Token
  else
    raise EParser.Create(errMsg);
end;

function TParser.GetChar: Char;
var
  lineCont : boolean;
  st : string;
begin
  lineCont := False;
  repeat
    if lineCont then
      MessageBeep ($ffff);
    while fLinePos >= Length (fLineBuf) do
    begin

      if not fReader.ReadLn(st) then
      begin
        fCh := #0;
        result := fCh;
        exit
      end;

      fLineBuf := st;

      fLinePos := 0;
      fSOL := not LineCont;
      fFirstChar := fSOL;
      Inc (fLineNo);
    end;

    fCh := fLineBuf [fLinePos + 1];
    Inc (fLinePos);
    LineCont := True;
  until (fCh <> '\') or (fLinePos < Length (fLineBuf));

  if fSOL and not IsWhitespace (ch) then
  begin
    fSOL := fFirstChar;
    fFirstChar := False
  end;

  result := fCh;
end;

function TParser.GetNonWhitespace: Char;
begin
  repeat
    result := GetChar
  until not IsWhitespace (ch)
end;

function TParser.GetToken : boolean;
var
  st : string;

begin
  result := True;
  fTokenType := ttUnknown;
  SkipWhitespace;
  fTokenSOL := fSol;
  if IsFirstIdChar (ch) then
    RawGetToken (IsNextIdChar, ttIdentifier)
  else
    if IsFirstNumChar (fCh) then
    begin
      RawGetToken (IsNextNumChar, ttNumber);
      if IsDecimalSeparator (fCh) and not fSOL then
      begin
        st := fToken;
        GetChar;
        RawGetToken (IsNextNumChar, ttNumber);
        fToken := st + DecimalSeparator + fToken
      end
      else
        if (fCh = 'x') and (Token = '0') then
        begin
          st := fToken;
          GetChar;
          RawGetToken (IsHexChar, ttNumber);
          fToken := '$' + ftoken;
        end

    end
    else
      if fCh = #0 then
        result := False
      else
      begin
        fTokenChar := fCh;
        fTokenType := ttChar;
        GetChar;
      end
end;

function TParser.IsDecimalSeparator(ch: char): boolean;
begin
  result := ch = '.'
end;

function TParser.IsFirstIdChar(ch: char): boolean;
begin
  result := TCharacter.IsLetter (ch) or (ch = '_')
end;

function TParser.IsFirstNumChar(ch: char): boolean;
begin
  result := IsNextNumChar (ch) or (ch = '$');
end;

function TParser.IsHexChar(ch: char): boolean;
begin
  result := CharInSet (ch, ['A'..'F', 'a'..'f', '0'..'9']);
end;

function TParser.IsNextIdChar(ch: char): boolean;
begin
  result := IsFirstIdChar (ch) or CharInSet (ch, ['0'..'9'])
end;

function TParser.IsNextNumChar(ch: char): boolean;
begin
  result := CharInSet (ch, ['0'..'9'])
end;

function TParser.IsWhitespace(ch: char): boolean;
begin
  result := TCharacter.IsWhitespace (ch)
end;

function TParser.NextChar (ch :Char) : Char;
begin
  NextToken;
  ExpectChar (ch);
  result := fTokenChar;
end;

function TParser.NextIdentifier (const errMsg : string) : string;
begin
  NextToken;
  ExpectIdentifier (errMsg);
end;

function TParser.NextInteger(const errMsg: string): Integer;
begin
  NextToken;
  result := ExpectInteger (errMsg);
end;

function TParser.NextString(const errMsg: string) : string;
begin
  NextToken;
  result := ExpectString (errMsg);
end;

procedure TParser.NextToken;
begin
  GetToken;
end;

procedure TParser.RawGetToken(fnCheckChar : TfnCheckChar; tp: Integer);
begin
  fToken := fCh;
  if fLinePos < Length (fLineBuf) then
    while fnCheckChar (GetChar) do
    begin
      fToken := ftoken + fCh;
      if fLinePos = Length (fLineBuf) then
      begin
        GetChar;
        break
      end
    end
  else
    GetChar;
  fTokenType := tp
end;

procedure TParser.SkipLine;
begin
  if not fSOL then
  begin
    fLinePos := Length (fLineBuf);
    GetChar
  end
end;

procedure TParser.SkipWhitespace;
begin
  while IsWhitespace (fCh) do
    GetChar;
end;

{ TCPreProcessor }

procedure TCPreProcessor.AddIdentifier(const id, line: string);
begin
  if not Defined (id) then
  begin
    CheckIdentifiers;
    fIdentifiers.AddObject(id, TStrValue.Create(line))
  end
end;

function TCPreProcessor.Calc(const st: string): TValue;
begin
  CalcExpression (st, fIdentifiers, result);
end;

procedure TCPreProcessor.CheckIdentifiers;
begin
  if fIdentifiers = Nil then
  begin
    fIdentifiers := TStringList.Create;
    fIdentifiers.Duplicates := dupError;
    fIdentifiers.CaseSensitive := True;
    fIdentifiers.Sorted := True
  end
end;

constructor TCPreProcessor.Create(AStream: TStream);
begin
  inherited Create (AStream);

  fDirectives := TStringList.Create;
  fDirectives.CaseSensitive := True;
  fDirectives.AddObject ('include', TObject (dtInclude));
  fDirectives.AddObject ('define',  TObject (dtDefine));
  fDirectives.AddObject ('ifdef',   TObject (dtIfdef));
  fDirectives.AddObject ('ifndef',  TObject (dtIfndef));
  fDirectives.AddObject ('endif',   TObject (dtEndif));
  fDirectives.AddObject ('undef',   TObject (dtUndef));
  fDirectives.AddObject ('else',    TObject (dtElse));
  fDirectives.AddObject ('if',      TObject (dtIf));
  fDirectives.AddObject ('pragma',  TObject (dtPragma));
  fDirectives.AddObject ('error',   TObject (dtError));
  fDirectives.Sorted := True;
end;

function TCPreProcessor.Defined(id: string): boolean;
begin
  if fIdentifiers = Nil then
    result := False
  else
    result := fIdentifiers.IndexOf (id) >= 0
end;

procedure TCPreProcessor.DeleteIdentifier(const id: string);
begin
  if Defined (id) then
  begin
    CheckIdentifiers;
    fIdentifiers.Delete (fIdentifiers.IndexOf (id))
  end
  else
    raise EParser.CreateFmt ('Identifier %s not found', [id]);
end;

destructor TCPreProcessor.Destroy;
var
  i : Integer;
begin
  if Assigned (fIdentifiers) then
  begin
    for i := fIdentifiers.Count - 1 downto 0 do
      fIdentifiers.Objects [i].Free;
    fIdentifiers.Free
  end;

  fDirectives.Free;

  inherited;
end;

procedure TCPreProcessor.DoDefine;
var
  id : string;
begin
  NextIdentifier ('Identifier expected in #define');
  id := fToken;
  GetRestOfLine;
  AddIdentifier (id, fToken);
end;

procedure TCPreProcessor.DoElse;
begin
  if fIfLevel > 0 then
    SkipIfElseBlock
  else
    raise EParser.Create('Unexpected #else');
  SkipLine;
end;

procedure TCPreProcessor.DoEndif;
begin
  if fIfLevel > 0 then
  begin
    Dec (fIfLevel);
    SkipLine;
  end
  else
    raise EParser.Create('Unexpected endif');

end;

procedure TCPreProcessor.DoIf;
var
  expr : string;
  val : TValue;
begin
  GetRestOfLine;
  expr := fToken;

  CalcExpression (expr, fIdentifiers, val);

  if (val.tp <> vInteger) then
   raise EParser.Create('Must be an integer expression');

  Inc (fIfLevel);

  if val.iVal = 0 then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc (fIfLevel);
  SkipLine;

  if not Defined (fToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfNDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc (fIfLevel);
  SkipLine;

  if Defined (fToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoInclude;
var
  oldReader : TStreamTextIO;
  oldLinePos, oldLineNo, oldIfLevel : Integer;
  oldLineBuf : string;
  oldSOL : boolean;
  oldCh : Char;
  oldPathName : string;
  r : TStreamTextIO;
  f : TFileStream;
  fName : string;
begin
  NextFileString ('File name expected');

  oldReader := fReader;
  oldLinePos := fLinePos;
  oldLineNo := fLineNo;
  oldLineBuf := fLineBuf;
  oldIfLevel := fIfLevel;
  oldSol := fSOL;
  oldCh := fCh;
  oldPathName := PathName;

  r := Nil;
  fName := GetIncludePathName (fToken);
  f := TFileStream.Create(fName, fmOpenRead or fmShareDenyWrite);
  try
    r := TStreamTextIO.Create(f);
    fLinePos := 0;
    fLineNo := 0;
    fReader := r;
    fSOL := True;
    fFirstChar := True;
    fLineBuf := '';
    fIfLevel := 0;
    PathName := ExtractFilePath (fName);

    Parse;
  finally
    fReader := oldReader;
    fLinePos := oldLinePos;
    fLineNo := oldLineNo;
    fFirstChar := False;
    fLineBuf := oldLineBuf;
    fIfLevel := oldIfLevel;
    fSOL := oldSOL;
    fCh := oldCh;
    fPathName := oldPathName;
    r.Free;
    f.Free
  end;
  SkipLine;
end;

procedure TCPreProcessor.DoPragma;
begin
  HandlePragma (GetRestOfLine);
end;

procedure TCPreProcessor.DoUndef;
var
  id : string;
begin
  NextIdentifier ('Identifier expected in #undef');
  id := fToken;
  GetRestOfLine;

  DeleteIdentifier (id);
end;

procedure TCPreProcessor.ExpectChar(ch: Char);
begin
  inherited;

end;

function TCPreProcessor.ExpectInteger(const errMsg: string): Integer;
begin
  if TokenType = ttIdentifier then
    Resolve (fTokenType, fToken);

  result := inherited ExpectInteger (errMsg);
end;

function TCPreProcessor.ExpectString(const errMsg: string): string;
begin
  if TokenType = ttIdentifier then
    Resolve (fTokenType, fToken);

  result := inherited ExpectString (errMsg);
end;

function TCPreProcessor.GetIncludePathName(const FileName: string): string;
var
  st, p : string;
  ch : Char;
begin
  result := PathName + fileName;

  if not FileExists (result) then
  begin
    st := IncludePath;
    if st = '' then
      st := GetEnvironmentVariable ('include');

    while st <> '' do
    begin
      p := SplitString (';', st);

      if p <> '' then
      begin
        ch := p [Length (p)];
        if (ch <> '\') and (ch <> ':') then
          p := p + '\';

        result := p + fileName;
        if FileExists (result) then
          Exit
      end
    end;

    result := FileName
  end
end;

function TCPreProcessor.GetRestOfLine: string;
begin
  result := '';
  if fSol then
  begin
    fToken := '';
    exit;
  end;
  SkipWhitespace;
  fToken := '';
  repeat
    if fCh = '/' then
      if GetChar = '/' then
      begin
        SkipLine;
        break
      end
      else
        if fCh = '*' then
        begin
          GetChar;
          repeat
            if fLinePos >= Length (fLineBuf) then
              break;

            if fCh = '*' then
            begin
              if GetChar = '/' then
                break
            end
            else
              GetChar
          until fLinePos >= Length (fLineBuf)
        end
        else
          fToken := fToken + '/' + fCh
    else
      fToken := fToken + fCh;
    if fLinePos = Length (fLineBuf) then
    begin
      GetChar;
      break
    end;
    GetChar
  until False;
  fToken := Trim (fToken);
  result := fToken;
end;

function TCPreProcessor.GetToken: boolean;
var
  retry : boolean;
begin
  result := True;
  retry := True;
  while retry do
  begin
    retry := False;
    result := inherited GetToken;
    if not result then
    begin
      if fIfLevel <> 0 then
        raise EParser.Create('Unexpected end of file');
      Exit;
    end;

    if TokenType = ttChar then
    case fTokenChar of
      '/' : if fCh = '/' then
            begin
              SkipLine;
              retry := True;
            end
            else
            if fCh = '*' then
            begin
              repeat
                if GetChar = #0 then break;
                if (fCh = '*') and (GetChar = '/') then
                  break;
              until fCh = #0;
              GetChar;
              retry := True;
            end
            else
              fTokenType := ttOpDiv;
      '#' :
            if fTokenSOL then
            begin
              inherited GetToken;
              if fTokenType <> ttIdentifier then
                raise EParserError.Create('Syntax error in directive');
              HandleDirective;
              retry := True;
            end;
      '"' : begin
              fToken := '';
              while fCh <> #0 do
              begin
                case fCh of
                  '"' : break;
                  '\' :
                    case GetChar of
                      '"' : fToken := fToken + '"';
                      'n' : fToken := fToken + #10;
                      'r' : fToken := fToken + #13;
                      't' : fToken := fToken + #9;
                      '\' : fToken := fToken + '\';
                      '0' : fToken := fToken + #0;
                      else
                        raise EParserError.Create('Invalid escape sequence');
                    end;
                  else
                    fToken := fToken + fCh
                end;
                GetChar
              end;
              fTokenType := ttString;
              GetChar;
            end;
    end
  end
end;

procedure TCPreProcessor.HandleDirective;
var
  idx : Integer;
begin
  idx := fDirectives.IndexOf(LowerCase (fToken));
  if idx >= 0 then
  case Integer (fDirectives.Objects [idx]) of
    dtInclude : DoInclude;
    dtDefine  : DoDefine;
    dtIfDef   : DoIfDef;
    dtIfnDef  : DoIfNDef;
    dtEndif   : DoEndif;
    dtUndef   : DoUndef;
    dtElse    : DoElse;
    dtIf      : DoIf;
    dtPragma  : DoPragma;
    else
      raise EParser.Create('Unknown directive #' + fToken)
  end
  else
    raise EParser.Create('Unknown directive #' + fToken)
end;

procedure TCPreProcessor.HandlePragma(const st: string);
begin

end;

function TCPreProcessor.IsFirstNumChar(ch: char): boolean;
begin
  result := TCharacter.IsNumber (ch)
end;

function TCPreProcessor.IsIdentifier(const id: string): boolean;
begin
  result := fIdentifiers.IndexOf(id) >= 0
end;

procedure TCPreProcessor.NextFileString(const errMsg: string);
begin
  GetChar;
  if fCh = '<' then
  begin
    GetChar;
    fToken := '';
    while fCh <> #0 do
    begin
      case fCh of
        '>' : break;
        else
          fToken := fToken + fCh
      end;
      GetChar
    end;
    fTokenType := ttString
  end
  else
    if ch = '"' then
    begin
      GetChar;
      fToken := '';
      while fCh <> #0 do
      begin
        case fCh of
          '"' : break;
          else
            fToken := fToken + fCh
        end;
        GetChar
      end;
      fTokenType := ttString
    end
    else
      GetToken;

  if TokenType <> ttString then
    raise EParser.Create(errMsg);
end;

function TCPreProcessor.Resolve(var TokenType : Integer; var st: string): TValue;
begin
  if (TokenType = ttIdentifier) and IsIdentifier (st) then
  begin
    CalcExpression (st, fIdentifiers, result);

    case result.tp of
      vString  : begin st := result.sVal; tokenType := ttString; end;
      vInteger : begin st := IntToStr (result.iVal); tokenType := ttNumber; end;
      vReal    : begin st := FloatToStr (result.rVal); tokenType := ttNumber; end;
    end
  end
end;

function TCPreProcessor.ResolveToken: TValue;
begin
  if (TokenType = ttNumber) or (TokenType = ttIdentifier) then
    result := Calc (Token)
  else
    if TokenType = ttString then
    begin
      result.tp := vString;
      result.sVal := Token
    end
    else
      raise EParser.Create('Value expected');

end;

procedure TCPreProcessor.SkipIfElseBlock;
var
  level : Integer;
begin
  level := 0;
  repeat
    SkipWhitespace;
    if fCh = #0 then Break;
    if fCh = '#' then
    begin
      GetChar;
      inherited GetToken;
      if fToken = 'endif' then
        if level = 0 then
          break
        else
          Dec (level)
      else
        if (fToken = 'ifdef') or (ftoken = 'ifndef') or (ftoken = 'if') then
          Inc (level)
    end
    else
      fSOL := False;
    SkipLine
  until False;
  Dec (fIfLevel)
end;

end.
