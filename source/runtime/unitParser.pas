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
 | Copyright � Colin Wilson 2005  All Rights Reserved                   |
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
  TfnCheckChar = function (ch: char): boolean of object;

  TParser = class
  private
    FReader: TStreamTextIO;

    FLineBuf: String;
    FLineNo: Integer;
    FLinePos: Integer;

    FCh: Char;

    FTokenType: Integer;
    FToken: string;
    FTokenChar: Char;
    FDecimalSeparator: Char;
    FSOL: boolean;
    FFirstChar: boolean;
    FTokenSol: boolean;
    function IsDecimalSeparator(ch: char): boolean;

  protected
    procedure RawGetToken (fnCheckChar: TfnCheckChar; tp: Integer);

  public
    constructor Create (AStream: TStream);
    destructor Destroy; override;
    procedure Parse; virtual; abstract;

    function GetChar: Char;
    function GetNonWhitespace: Char;
    procedure SkipLine;
    function GetToken: boolean; virtual;
    procedure SkipWhitespace;
    procedure NextToken;

    function IsWhitespace (ch: char): boolean; virtual;
    function IsFirstIdChar (ch: char): boolean; virtual;
    function IsNextIdChar (ch: char): boolean; virtual;
    function IsFirstNumChar (ch: char): boolean; virtual;
    function IsNextNumChar (ch: char): boolean; virtual;
    function IsHexChar (ch: char): boolean; virtual;

    function NextIdentifier (const errMsg: string = 'Identifier expected'): string;
    function NextString (const errMsg: string = 'String constant expected'): string;
    function NextInteger (const errMsg: string = 'Integer constant expected'): Integer;
    function NextChar (ch: Char): Char;

    procedure ExpectChar (ch: Char); virtual;
    function ExpectIdentifier (const errMsg: string = 'Identifier expected'): string;
    function ExpectString (const errMsg: string = 'String constant expected'): string; virtual;
    function ExpectInteger (const errMsg: string = 'Integer constant expected'): Integer; virtual;

    property LinePos: Integer read FLinePos;
    property LineNo: Integer read FLineNo;
    property Ch: Char read FCh;
    property SOL: boolean read FSOL;
    property TokenType: Integer read FTokenType;
    property Token: string read FToken;
    property TokenChar: Char read FTokenChar;
    property TokenSOL: boolean read FTokenSol;

    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
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
    FPathName: string;
    FIdentifiers: TStringList;
    FIfLevel: Integer;
    FIncludePath: string;
    FDirectives: TStringList;

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
    function GetIncludePathName (const FileName: string): string;
    procedure SkipIfElseBlock;
  protected
    procedure HandleDirective;
    procedure HandlePragma (const st: string); virtual;
    function GetRestOfLine: string;
    procedure NextFileString (const errMsg: string);
  public
    constructor Create (AStream: TStream);
    destructor Destroy; override;
    property PathName: string read FPathName write FPathName;
    property IncludePath: string read FIncludePath write FIncludePath;
    function GetToken: boolean; override;

    function IsFirstNumChar (ch: char): boolean; override;

    procedure ExpectChar (ch: char); override;
    function ExpectString (const errMsg: string = 'String constant expected'): string; override;
    function ExpectInteger (const errMsg: string = 'Integer constant expected'): Integer; override;

    function Defined (id: string): boolean;
    procedure AddIdentifier (const id, line: string);
    procedure DeleteIdentifier (const id: string);
    function IsIdentifier (const id: string) :boolean;
    function Resolve (var TokenType: Integer; var st: string): TValue;
    function Calc (const st: string): TValue;
    function ResolveToken: TValue;
  end;

  EParser = class (Exception);

implementation

uses unitSearchString;

{ TParser }

constructor TParser.Create(AStream: TStream);
begin
  FReader := TStreamTextIO.Create (AStream);
(*
  fWhitespace := ' '#9#10;
  fFirstIdChars := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  fNextNumChars := '0123456789';
  fNextIdChars := fFirstIdChars + fNextNumChars;
  fFirstNumChars := '$' + fNextNumChars;
*)
  FDecimalSeparator := '.';
  FSOL := True;
  FFirstChar := True;
end;

destructor TParser.Destroy;
begin
  FReader.Free;

  inherited;
end;

procedure TParser.ExpectChar(ch: Char);
begin
  if (TokenType <> ttChar) or (FTokenChar <> ch) then
    raise EParser.Create(ch + ' expected');
end;

function TParser.ExpectIdentifier(const errMsg: string): string;
begin
  if TokenType <> ttIdentifier then
    raise EParser.Create(errMsg);
  result := FToken;
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
  lineCont: boolean;
  st: string;
begin
  lineCont := False;
  repeat
    if lineCont then
      MessageBeep ($ffff);
    while FLinePos >= Length (FLineBuf) do
    begin

      if not FReader.ReadLn(st) then
      begin
        FCh := #0;
        result := FCh;
        exit
      end;

      FLineBuf := st;

      FLinePos := 0;
      FSOL := not LineCont;
      FFirstChar := FSOL;
      Inc (FLineNo);
    end;

    FCh := FLineBuf [FLinePos + 1];
    Inc (FLinePos);
    LineCont := True;
  until (FCh <> '\') or (FLinePos < Length (FLineBuf));

  if FSOL and not IsWhitespace (ch) then
  begin
    FSOL := FFirstChar;
    FFirstChar := False
  end;

  result := FCh;
end;

function TParser.GetNonWhitespace: Char;
begin
  repeat
    result := GetChar
  until not IsWhitespace (ch)
end;

function TParser.GetToken: boolean;
var
  st: string;

begin
  result := True;
  FTokenType := ttUnknown;
  SkipWhitespace;
  FTokenSol := FSOL;
  if IsFirstIdChar (ch) then
    RawGetToken (IsNextIdChar, ttIdentifier)
  else
    if IsFirstNumChar (FCh) then
    begin
      RawGetToken (IsNextNumChar, ttNumber);
      if IsDecimalSeparator (FCh) and not FSOL then
      begin
        st := FToken;
        GetChar;
        RawGetToken (IsNextNumChar, ttNumber);
        FToken := st + DecimalSeparator + FToken
      end
      else
        if (FCh = 'x') and (Token = '0') then
        begin
          st := FToken;
          GetChar;
          RawGetToken (IsHexChar, ttNumber);
          FToken := '$' + FToken;
        end

    end
    else
      if FCh = #0 then
        result := False
      else
      begin
        FTokenChar := FCh;
        FTokenType := ttChar;
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

function TParser.NextChar (ch :Char): Char;
begin
  NextToken;
  ExpectChar (ch);
  result := FTokenChar;
end;

function TParser.NextIdentifier (const errMsg: string): string;
begin
  NextToken;
  ExpectIdentifier (errMsg);
end;

function TParser.NextInteger(const errMsg: string): Integer;
begin
  NextToken;
  result := ExpectInteger (errMsg);
end;

function TParser.NextString(const errMsg: string): string;
begin
  NextToken;
  result := ExpectString (errMsg);
end;

procedure TParser.NextToken;
begin
  GetToken;
end;

procedure TParser.RawGetToken(fnCheckChar: TfnCheckChar; tp: Integer);
begin
  FToken := FCh;
  if FLinePos < Length (FLineBuf) then
    while fnCheckChar (GetChar) do
    begin
      FToken := FToken + FCh;
      if FLinePos = Length (FLineBuf) then
      begin
        GetChar;
        break
      end
    end
  else
    GetChar;
  FTokenType := tp
end;

procedure TParser.SkipLine;
begin
  if not FSOL then
  begin
    FLinePos := Length (FLineBuf);
    GetChar
  end
end;

procedure TParser.SkipWhitespace;
begin
  while IsWhitespace (FCh) do
    GetChar;
end;

{ TCPreProcessor }

procedure TCPreProcessor.AddIdentifier(const id, line: string);
begin
  if not Defined (id) then
  begin
    CheckIdentifiers;
    FIdentifiers.AddObject(id, TStrValue.Create(line))
  end
end;

function TCPreProcessor.Calc(const st: string): TValue;
begin
  CalcExpression (st, FIdentifiers, result);
end;

procedure TCPreProcessor.CheckIdentifiers;
begin
  if FIdentifiers = Nil then
  begin
    FIdentifiers := TStringList.Create;
    FIdentifiers.Duplicates := dupError;
    FIdentifiers.CaseSensitive := True;
    FIdentifiers.Sorted := True
  end
end;

constructor TCPreProcessor.Create(AStream: TStream);
begin
  inherited Create (AStream);

  FDirectives := TStringList.Create;
  FDirectives.CaseSensitive := True;
  FDirectives.AddObject ('include', TObject (dtInclude));
  FDirectives.AddObject ('define',  TObject (dtDefine));
  FDirectives.AddObject ('ifdef',   TObject (dtIfdef));
  FDirectives.AddObject ('ifndef',  TObject (dtIfndef));
  FDirectives.AddObject ('endif',   TObject (dtEndif));
  FDirectives.AddObject ('undef',   TObject (dtUndef));
  FDirectives.AddObject ('else',    TObject (dtElse));
  FDirectives.AddObject ('if',      TObject (dtIf));
  FDirectives.AddObject ('pragma',  TObject (dtPragma));
  FDirectives.AddObject ('error',   TObject (dtError));
  FDirectives.Sorted := True;
end;

function TCPreProcessor.Defined(id: string): boolean;
begin
  if FIdentifiers = Nil then
    result := False
  else
    result := FIdentifiers.IndexOf (id) >= 0
end;

procedure TCPreProcessor.DeleteIdentifier(const id: string);
begin
  if Defined (id) then
  begin
    CheckIdentifiers;
    FIdentifiers.Delete (FIdentifiers.IndexOf (id))
  end
  else
    raise EParser.CreateFmt ('Identifier %s not found', [id]);
end;

destructor TCPreProcessor.Destroy;
var
  i: Integer;
begin
  if Assigned (FIdentifiers) then
  begin
    for i := FIdentifiers.Count - 1 downto 0 do
      FIdentifiers.Objects [i].Free;
    FIdentifiers.Free
  end;

  FDirectives.Free;

  inherited;
end;

procedure TCPreProcessor.DoDefine;
var
  id: string;
begin
  NextIdentifier ('Identifier expected in #define');
  id := FToken;
  GetRestOfLine;
  AddIdentifier (id, FToken);
end;

procedure TCPreProcessor.DoElse;
begin
  if FIfLevel > 0 then
    SkipIfElseBlock
  else
    raise EParser.Create('Unexpected #else');
  SkipLine;
end;

procedure TCPreProcessor.DoEndif;
begin
  if FIfLevel > 0 then
  begin
    Dec (FIfLevel);
    SkipLine;
  end
  else
    raise EParser.Create('Unexpected endif');

end;

procedure TCPreProcessor.DoIf;
var
  expr: string;
  val: TValue;
begin
  GetRestOfLine;
  expr := FToken;

  CalcExpression (expr, FIdentifiers, val);

  if (val.tp <> vInteger) then
   raise EParser.Create('Must be an integer expression');

  Inc (FIfLevel);

  if val.iVal = 0 then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc (FIfLevel);
  SkipLine;

  if not Defined (FToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfNDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc (FIfLevel);
  SkipLine;

  if Defined (FToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoInclude;
var
  oldReader: TStreamTextIO;
  oldLinePos, oldLineNo, oldIfLevel: Integer;
  oldLineBuf: string;
  oldSOL: boolean;
  oldCh: Char;
  oldPathName: string;
  r: TStreamTextIO;
  f: TFileStream;
  fName: string;
begin
  NextFileString ('File name expected');

  oldReader := FReader;
  oldLinePos := FLinePos;
  oldLineNo := FLineNo;
  oldLineBuf := FLineBuf;
  oldIfLevel := FIfLevel;
  oldSol := FSOL;
  oldCh := FCh;
  oldPathName := PathName;

  r := Nil;
  fName := GetIncludePathName (FToken);
  f := TFileStream.Create(fName, fmOpenRead or fmShareDenyWrite);
  try
    r := TStreamTextIO.Create(f);
    FLinePos := 0;
    FLineNo := 0;
    FReader := r;
    FSOL := True;
    FFirstChar := True;
    FLineBuf := '';
    FIfLevel := 0;
    PathName := ExtractFilePath (fName);

    Parse;
  finally
    FReader := oldReader;
    FLinePos := oldLinePos;
    FLineNo := oldLineNo;
    FFirstChar := False;
    FLineBuf := oldLineBuf;
    FIfLevel := oldIfLevel;
    FSOL := oldSOL;
    FCh := oldCh;
    FPathName := oldPathName;
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
  id: string;
begin
  NextIdentifier ('Identifier expected in #undef');
  id := FToken;
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
    Resolve (FTokenType, FToken);

  result := inherited ExpectInteger (errMsg);
end;

function TCPreProcessor.ExpectString(const errMsg: string): string;
begin
  if TokenType = ttIdentifier then
    Resolve (FTokenType, FToken);

  result := inherited ExpectString (errMsg);
end;

function TCPreProcessor.GetIncludePathName(const FileName: string): string;
var
  st, p: string;
  ch: Char;
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
  if FSOL then
  begin
    FToken := '';
    exit;
  end;
  SkipWhitespace;
  FToken := '';
  repeat
    if FCh = '/' then
      if GetChar = '/' then
      begin
        SkipLine;
        break
      end
      else
        if FCh = '*' then
        begin
          GetChar;
          repeat
            if FLinePos >= Length (FLineBuf) then
              break;

            if FCh = '*' then
            begin
              if GetChar = '/' then
                break
            end
            else
              GetChar
          until FLinePos >= Length (FLineBuf)
        end
        else
          FToken := FToken + '/' + FCh
    else
      FToken := FToken + FCh;
    if FLinePos = Length (FLineBuf) then
    begin
      GetChar;
      break
    end;
    GetChar
  until False;
  FToken := Trim (FToken);
  result := FToken;
end;

function TCPreProcessor.GetToken: boolean;
var
  retry: boolean;
begin
  result := True;
  retry := True;
  while retry do
  begin
    retry := False;
    result := inherited GetToken;
    if not result then
    begin
      if FIfLevel <> 0 then
        raise EParser.Create('Unexpected end of file');
      Exit;
    end;

    if TokenType = ttChar then
    case FTokenChar of
      '/': if FCh = '/' then
            begin
              SkipLine;
              retry := True;
            end
            else
            if FCh = '*' then
            begin
              repeat
                if GetChar = #0 then break;
                if (FCh = '*') and (GetChar = '/') then
                  break;
              until FCh = #0;
              GetChar;
              retry := True;
            end
            else
              FTokenType := ttOpDiv;
      '#' :
            if FTokenSol then
            begin
              inherited GetToken;
              if FTokenType <> ttIdentifier then
                raise EParserError.Create('Syntax error in directive');
              HandleDirective;
              retry := True;
            end;
      '"': begin
              FToken := '';
              while FCh <> #0 do
              begin
                case FCh of
                  '"': break;
                  '\' :
                    case GetChar of
                      '"': FToken := FToken + '"';
                      'n': FToken := FToken + #10;
                      'r': FToken := FToken + #13;
                      't': FToken := FToken + #9;
                      '\': FToken := FToken + '\';
                      '0': FToken := FToken + #0;
                      else
                        raise EParserError.Create('Invalid escape sequence');
                    end;
                  else
                    FToken := FToken + FCh
                end;
                GetChar
              end;
              FTokenType := ttString;
              GetChar;
            end;
    end
  end
end;

procedure TCPreProcessor.HandleDirective;
var
  idx: Integer;
begin
  idx := FDirectives.IndexOf(LowerCase (FToken));
  if idx >= 0 then
  case Integer (FDirectives.Objects [idx]) of
    dtInclude:
      DoInclude;
    dtDefine:
      DoDefine;
    dtIfDef:
      DoIfDef;
    dtIfnDef:
      DoIfNDef;
    dtEndif:
      DoEndif;
    dtUndef:
      DoUndef;
    dtElse:
      DoElse;
    dtIf:
      DoIf;
    dtPragma:
      DoPragma;
    else
      raise EParser.Create('Unknown directive #' + FToken)
  end
  else
    raise EParser.Create('Unknown directive #' + FToken)
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
  result := FIdentifiers.IndexOf(id) >= 0
end;

procedure TCPreProcessor.NextFileString(const errMsg: string);
begin
  GetChar;
  if FCh = '<' then
  begin
    GetChar;
    FToken := '';
    while FCh <> #0 do
    begin
      case FCh of
        '>': break;
        else
          FToken := FToken + FCh
      end;
      GetChar
    end;
    FTokenType := ttString
  end
  else
    if ch = '"' then
    begin
      GetChar;
      FToken := '';
      while FCh <> #0 do
      begin
        case FCh of
          '"': break;
          else
            FToken := FToken + FCh
        end;
        GetChar
      end;
      FTokenType := ttString
    end
    else
      GetToken;

  if TokenType <> ttString then
    raise EParser.Create(errMsg);
end;

function TCPreProcessor.Resolve(var TokenType: Integer; var st: string): TValue;
begin
  if (TokenType = ttIdentifier) and IsIdentifier (st) then
  begin
    CalcExpression (st, FIdentifiers, result);

    case result.tp of
      vString:
        begin
          st := result.sVal;
          tokenType := ttString;
        end;
      vInteger:
        begin
          st := IntToStr (result.iVal);
          tokenType := ttNumber;
        end;
      vReal:
        begin
          st := FloatToStr (result.rVal);
          tokenType := ttNumber;
        end;
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
  level: Integer;
begin
  level := 0;
  repeat
    SkipWhitespace;
    if FCh = #0 then Break;
    if FCh = '#' then
    begin
      GetChar;
      inherited GetToken;
      if FToken = 'endif' then
        if level = 0 then
          break
        else
          Dec (level)
      else
        if (FToken = 'ifdef') or (FToken = 'ifndef') or (FToken = 'if') then
          Inc (level)
    end
    else
      FSOL := False;
    SkipLine
  until False;
  Dec(FIfLevel);
end;

end.
