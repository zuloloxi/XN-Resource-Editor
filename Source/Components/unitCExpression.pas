(*======================================================================*
 | unitCExpression                                                      |
 |                                                                      |
 | 'C' Expression evaluator                                             |
 |                                                                      |
 | Recursive-descent parser/evaluator.  Handles a small subset of a 'C' |
 | expression:                                                          |
 |                                                                      |
 | Supported operators:                                                 |
 |                                                                      |
 |   '+'      Addition and unary 'positive'- I, R                       |
 |   '-'      Subtraction and unary negate - I, R                       |
 |   '*'      Multiplication               - I*I=I, R*R=R, I*R=R, R*I=R |
 |   '/'      Division                     - I/I=R, R/R=R, I/R=R, R/I=R |
 |   '\'      Integer division         - I\I=I, R\R=E, I\R=E, R\I=E     |
 |   '%'      Integer modulus          - I%I=I, R%R=E, I%R=E, R%I=E     |
 |   '&'      Integer  'and'           - I&I=I, R&R=E, I&R=E, R&I=E     |
 |   '&&'     Logical 'and'            - I&&I=I, R&&R=E, I&&R=E, R&&I=E |                                    |
 |   '|'      Integer  'or'            - I|I=I, R|R=E, I|R=E, R|I=E     |                                 |
 |   '||'     Logical 'or'             - I||I=I, R||R=E, I||R=E, R||I=E |                                 |
 |   '!'      Unary 'not'              - I, R=E                         |
 |   '!='     Inequality                                                |
 |   '=='     Equality                                                  |
 |   '<='     Less than or equal                                        |
 |   '>='     Greater than or equal                                     |
 |   '<'      Less                                                      |
 |   '>'      Greater                                                   |
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

unit unitCExpression;

interface

uses
  Classes, SysUtils, StrUtils, AnsiStrings;

type
  TValueType = (vInteger, vString, vReal);
  TValue = record
    tp: TValueType;
    iVal: Integer;
    sVal: string;
    rVal: extended;
  end;

  TStrValue = class
  private
    FValue: string;
  public
    constructor Create (const AValue: string);
    property Value: string read FValue;
  end;

procedure CalcExpression (const st: string; defines: TStringList; var value: TValue);

implementation

type
  TOperator = (opNop, opEq, opNotEq, opLessEq, opGtrEq, opLess, opGtr, opAdd,
    opSub, opLor, opOr, opMult, opDiv, opIDiv, opMod, oplAnd, opAnd, opNot);

procedure DoOp(op: TOperator; var x: TValue; y: TValue);

  procedure AssignBool (bool, rev: Boolean);
  begin
    x.tp := vInteger;
    if rev then
      bool := not bool;
    if bool then
      x.iVal := -1
    else
      x.iVal := 0
  end;

  procedure TypeMismatch;
  begin
    raise Exception.Create('Type mismatch in expression');
  end;

begin
  // Perform automatic type conversion
  if (x.tp = vReal) and (y.tp = vInteger) then
  begin
    y.rVal := y.iVal;
    y.tp := vReal
  end;

  if (x.tp = vInteger) and (y.tp = vReal) then
  begin
    x.rVal := x.iVal;
    x.tp := vReal
  end;

  if x.tp <> y.tp then
    TypeMismatch;

  if op in [opAnd, opOr, opLand, opLor, opIDiv, opMod] then
    if x.tp <> vInteger then
      TypeMismatch;

  if op in [opSub, opMult, opDiv] then
    if (x.tp <> vInteger) and (x.tp <> vReal) then
      TypeMismatch;

  case op of
    opEq,
    opNotEq: case x.tp of
                vInteger: AssignBool (x.iVal = y.iVal, op = opNotEq);
                vString: AssignBool (x.sVal = y.sVal, op = opNotEq);
                vReal: AssignBool (x.rVal = y.rVal, op = opNotEq);
              end;

    opLess,
    opGtrEq: case x.tp of
                vInteger: AssignBool (x.iVal < y.iVal, op = opGtrEq);
                vString: AssignBool (x.sVal < y.sVal, op = opGtrEq);
                vReal: AssignBool (x.rVal < y.rVal, op = opGtrEq);
              end;

    opGtr,
    opLessEq: case x.tp of
                vInteger: AssignBool (x.iVal > y.iVal, op = opLessEq);
                vString: AssignBool (x.sVal > y.sVal, op = opLessEq);
                vReal: AssignBool (x.rVal > y.rVal, op = opLessEq);
              end;

    oplAnd: AssignBool ((x.iVal and y.iVal) <> 0, false);
    oplOr: AssignBool ((x.iVal or y.iVal) <> 0, false);
    opAnd: x.iVal := x.iVal and y.iVal;
    opOr: x.iVal := x.iVal or y.iVal;

    opAdd: case x.tp of
               vInteger: x.iVal := x.iVal + y.iVal;
               vReal: x.rVal := x.rVal + y.rVal;
               vString: x.sVal := x.sVal + y.sVal
             end;

    opSub: if x.tp = vInteger then
               x.iVal := x.iVal - y.iVal
             else
               x.rVal := x.rVal - y.rVal;

    opMult: if x.tp = vInteger then
               x.iVal := x.iVal * y.iVal
             else
               x.rVal := x.rVal * y.rVal;

    opDiv: if x.tp = vInteger then
             begin
               x.rVal := x.iVal div y.iVal;
               x.tp := vReal
             end
             else
               x.rVal := x.rVal / y.rVal;

    opIDiv: x.iVal := x.iVal div y.iVal;
    opMod: x.iVal := x.iVal mod y.iVal;
  end;
end;

procedure DoUnaryOp (op: TOperator; var x: TValue);
begin
  if x.tp = vString then
    raise Exception.Create ('Type mismatch in expression');

  case op of
    opSub: if x.tp = vInteger then
              x.iVal := -x.iVal
            else
              x.rVal := -x.rVal;
    opNot: if x.tp <> vInteger then
              raise Exception.Create ('Type mismatch in expression')
            else
              if x.iVal <> 0 then
                x.iVal := 0
              else
                x.iVal := -1

  end
end;

procedure CalcExpression (const st: string; defines: TStringList; var value: TValue);
var
  pos: Integer;
  ch: char;

  function CalcBoolean: TValue; forward;

  function GetChar: char;
  begin
    if pos < Length (st) then
    begin
      ch := st [pos + 1];
      Inc(pos)
    end
    else
      ch := #0;
    Result := ch;
  end;

  function GetNonWhitespace: char;
  begin
    repeat GetChar until (ch <> ' ') and (ch <> #9);
    Result := ch
  end;

  procedure SkipWhitespace;
  begin
    if (ch = ' ') or (ch = #9) then
      GetNonWhitespace
  end;

  function IsIdChar (ch: char): Boolean;
  begin
    Result := System.Pos (ch, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_') > 0
  end;

  function IsCharHex (ch: char): Boolean;
  begin
    Result := System.Pos(ch, 'ABCDEFabcdef0123456789') > 0
  end;

  function IsCharNumeric (ch: char): Boolean;
  begin
    Result := System.Pos(ch, '0123456789') > 0
  end;

  function CalcId: TValue;
  var
    id: string;
    idx: Integer;
  begin
    id := ch;
    while IsIdChar (GetChar) do
      id := id + ch;

    SkipWhitespace;

    if id = 'defined' then
    begin
      if (ch = '(') then
      begin
        GetNonWhitespace;
        id := ch;
        while IsIdChar (GetChar) do
          id := id + ch;
        SkipWhitespace;
        if ch <> ')' then
          raise Exception.Create ('Missing '')''');
        idx := defines.IndexOf(id);
        result.tp := vInteger;
        if idx = -1 then
          result.iVal := 0
        else
          result.iVal := -1
      end
      else
        raise Exception.Create('''('' expected');
      GetNonWhitespace;
      exit;
    end;

    idx := defines.IndexOf(id);
    if idx >= 0 then
      CalcExpression (TStrValue (defines.Objects [idx]).FValue, defines, result)
    else
    begin
      result.tp := vInteger;
      result.iVal := 0
    end
  end;

  function CalcNumber: TValue;
  var
    n: string;
    hexFlag: Boolean;
    dotPos: Integer;
  begin
    n := ch;
    hexFlag := False;
    dotPos := 0;

    if UpperCase (GetChar) = 'X' then
    begin
      n := '';
      hexFlag := True;
      GetChar;
    end;

    while (hexFlag and (IsCharHex (ch))) or IsCharNumeric (ch) do
    begin
      n := n + ch;

      if GetChar = '.' then
        if (dotPos = 0) and not hexFlag then
        begin
          dotPos := Length (n);
          n := n + ch;
          GetChar
        end
        else
          break
    end;
    SkipWhitespace;

    if dotPos = Length (n) then
    begin
      Delete (n, Length (n), 1);
      ch := '.';
      Dec(pos);
      dotPos := 0;
    end;

    if hexFlag then
    begin
      result.tp := vInteger;
      result.iVal := StrToInt('$' + n)
    end
    else
      if dotPos = 0 then
      begin
        result.tp := vInteger;
        result.iVal := StrToInt(n)
      end
      else
      begin
        result.tp := vReal;
        result.rVal := StrToFloat(n)
      end
  end;

  function CalcCString: TValue;
  var
    st: string;
  begin
    st := '';
    while GetChar <> #0 do
    case ch of
      '"': break;
      '\' :
        case GetChar of
          '"': st := st + '"';
          'n': st := st + #10;
          'r': st := st + #13;
          't': st := st + #9;
          '\': st := st + '\';
          '0': st := st + #0;
          else
            raise EParserError.Create('Invalid escape sequence');
        end;
      else
        st := st + ch
    end;
    GetChar;
    result.tp := vString;
    result.sVal := st
  end;

  function CalcTerm: TValue;
  begin
    case ch of
      '(': begin
              GetNonWhitespace;
              Result := CalcBoolean;
              if ch <> ')' then
              raise Exception.Create ('Mismatched parentheses')
            end;
       'A'..'Z', 'a'..'z', '_' :
              Result := CalcId;
       '0'..'9' :
              Result := CalcNumber;
       '"' :
              Result := CalcCString;
    end
  end;

  function CalcSignedTerm: TValue;
  begin
    if ch = '+' then
    begin
      GetNonWhitespace;
      Result := CalcSignedTerm
    end
    else
      if ch = '-' then
      begin
        GetNonWhitespace;
        Result := CalcSignedTerm;
        DoUnaryOp (opSub, result);
      end
      else
        if ch = '!' then
        begin
          GetNonWhitespace;
          Result := CalcSignedTerm;
          DoUnaryOp (opNot, result)
        end
        else
          Result := CalcTerm
  end;

  function CalcMultiplication: TValue;
  var
    op: TOperator;
  begin
    Result := CalcSignedTerm;
    while (ch = '*') or (ch = '/') or (ch = '\') or (ch = '%') or (ch = '&') do
    begin

      op := opNop;
      case ch of
       '*': begin op := opMult; GetChar; end;
       '/': begin op := opDiv;  GetChar; end;
       '\': begin op := opIDiv; GetChar; end;
       '%': begin op := opMod;  GetChar; end;
       '&': if GetChar = '&' then
             begin
               op := oplAnd;
               GetChar
             end
             else
               op := opAnd;
      end;
      SkipWhitespace;
      if op <> opNop then
        DoOp (op, result, CalcSignedTerm)
      else
        break
    end
  end;

  function CalcAddition: TValue;
  var
    op: TOperator;
  begin
    Result := CalcMultiplication;
    while (ch = '+') or (ch = '-') or (ch = '|') do
    begin
      op := opNop;
      case ch of
        '+': begin op := opAdd; GetChar; end;
        '-': begin op := opSub; GetChar; end;
        '|': if GetChar = '|' then
              begin
                GetChar;
                op := oplor
              end
              else
                op := opOr;
      end;
      SkipWhitespace;
      if op <> opNop then
        DoOp (op, result, CalcMultiplication)
      else
        break
    end
  end;

  function CalcBoolean: TValue;
  var
    op: TOperator;
  begin
    Result := CalcAddition;

    while (ch = '=') or (ch = '!') or (ch = '<') or (ch = '>') do
    begin

      op := opNop;
      case ch of
        '=': if GetChar = '=' then
              begin
                GetChar;
                op := opEq
              end;
        '!': if GetChar = '=' then
              begin
                GetChar;
                op := opNotEq
              end;
        '<': if GetChar = '=' then
              begin
                op := opLessEq;
                GetChar
              end
              else
                op := opLess;

        '>': if GetChar = '=' then
              begin
                op := opGtrEq;
                GetChar;
              end
              else
                op := opGtr
      end;

      SkipWhitespace;

      if op <> opNop then
        DoOp (op, result, CalcAddition)
      else
        break
    end
  end;

begin
  pos := 0;

  GetNonWhitespace;
  value := CalcAddition;
end;

{ TStrValue }

constructor TStrValue.Create(const AValue: string);
begin
  FValue := AValue;
end;

end.
