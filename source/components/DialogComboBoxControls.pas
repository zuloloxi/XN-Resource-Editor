(*======================================================================*
 | DialogComboBoxControls unit for PEResourceExplorer                   |
 |                                                                      |
 | Handle ComboBox controls in dialog editor                            |
 |                                                                      |
 | If the dialog template specifies WS_VSCROLL, it isn't given to the   |
 | combobox control.  Instead it's given to the listbox bit - and only  |
 | if 'Disable No Scroll' is true (!)                                   |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY Kind, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      15/02/2002  CPWW  Original                                  |
 *======================================================================*)

unit DialogComboBoxControls;

interface

uses 
  Windows, Messages, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type
  //----------------------------------------------------------------
  // TComboBoxControlInfo
  TComboBoxControlInfo = class (TStandardControlInfo)
  private
    FEditWnd: HWND;
    FListBoxWnd: HWND;
  
    procedure GetCBHandles;
  protected
    function GetClassSzOrID: TszOrID; override;
    function GetStyle: DWORD; override;
    procedure SetStyle(st: DWORD; value: Boolean); override;
    procedure Init; override;
  
  public
    constructor Create (AOwner: TDialogEditor; AItemID: Integer; AControlHWND: HWND; ADropControl: TDropControl); override;
    class procedure CreateControlParams (var params: TCreateControlParams); override;
    class function GetDescription: string; override;
  
    function GetPropertyCount(Kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(Kind: TPropertyKind; Indx: Integer): Integer; override;
    function GetPropertyEnumName(Kind: TPropertyKind; Indx, enum: Integer): string; override;
    function GetPropertyName(Kind: TPropertyKind; Indx: Integer): string; override;
    function GetPropertyValue(Kind: TPropertyKind; Indx: Integer): Variant; override;
    function GetPropertyType(Kind: TPropertyKind; Indx: Integer): TPropertyType; override;
    procedure SetPropertyValue(Kind: TPropertyKind; Indx: Integer;const Value: Variant); override;
  end;

implementation

uses 
  DialogStrings;

const
  ComboBoxControlPropertyGeneralCount = 0;
  ComboBoxControlPropertyStyleCount = 10;
  ComboBoxControlPropertyExtendedCount = 0;
  ComboBoxControlPropertyCount: array [TPropertyKind] of Integer = (ComboBoxControlPropertyGeneralCount, ComboBoxControlPropertyStyleCount, ComboBoxControlPropertyExtendedCount);
//  ComboBoxControlPropertyGeneralName: array [0..ComboBoxControlPropertyGeneralCount - 1] of string = (rstCaption);
  ComboBoxControlPropertyStyleName: array [0..ComboBoxControlPropertyStyleCount - 1] of string = (rstType, rstOwnerDraw, rstSort, rstVScroll, rstNoIntegralHeight, rstOEMConvert, rstAutoHScroll, rstDisableNoScroll, rstUppercase, rstLowercase);
//  ComboBoxControlPropertyExtendedName: array [0..ComboBoxControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  ComboBoxControlPropertyGeneralType: array [0..ComboBoxControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ComboBoxControlPropertyStyleType: array [0..ComboBoxControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  ComboBoxControlPropertyExtendedType: array [0..ComboBoxControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

type
  TComboBoxInfo = record
    cbSize: DWORD;
    rcItem: TRect;
    rcButton: TRect;
    stateButton: DWORD;
    hwndCombo: DWORD;
    hwndEdit: DWORD;
    hwndList: DWORD
  end;
  PComboBoxInfo = ^TComboBoxInfo;

TfnGetComboBoxInfo = function (hwndCombo: HWND; pcbi: PComboBoxInfo): Boolean; stdcall;

var
  fnGetComboBoxInfo: TfnGetComboBoxInfo = nil;

{ TComboBoxControlInfo }

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.Create                                          |
 |                                                                      |
 | Constructor                                                          |
 *----------------------------------------------------------------------*)
constructor TComboBoxControlInfo.Create(AOwner: TDialogEditor;
  AItemID: Integer; AControlHWND: HWND; ADropControl: TDropControl);
begin
  inherited;

  FEditWnd := $ffffffff;
  FListBoxWnd := $ffffffff
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.CreateControlParams                             |
 |                                                                      |
 | Set default styles                                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   var params: TCreateControlParams                                   |
 *----------------------------------------------------------------------*)
class procedure TComboBoxControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or WS_TABSTOP or WS_VSCROLL or CBS_DROPDOWN;
  params.exStyle := params.exStyle or WS_EX_CLIENTEDGE;
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetCBHandles                                    |
 |                                                                      |
 | Get list box & edit control handles.  nb.  Needs NT4 SP6             |
 *----------------------------------------------------------------------*)
procedure TComboBoxControlInfo.GetCBHandles;
var
  info: TComboBoxInfo;
begin
  if FEditWnd = $ffffffff then
  begin
    FillChar (info, SizeOf (info), 0);
    info.cbSize := SizeOf (info);

    if Assigned(fnGetComboboxInfo) then
      fnGetComboBoxInfo (ControlHandle, @info);
    FEditWnd := info.hwndEdit;
    FListBoxWnd := info.hwndList
  end
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetClassSzOrID                                  |
 |                                                                      |
 | Get Class ID                                                         |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := ComboBox_ID
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetDescription                                  |
 |                                                                      |
 | Get the description                                                  |
 *----------------------------------------------------------------------*)
class function TComboBoxControlInfo.GetDescription: string;
begin
  Result := rstComboBox
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyCount                                |
 |                                                                      |
 | Get the property count for each Kind of property                     |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyCount(
  Kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount(Kind) + ComboBoxControlPropertyCount [Kind]
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyEnumCount                            |
 |                                                                      |
 | Get the count of enumerations for enumerated properties              |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyEnumCount(Kind: TPropertyKind;
  Indx: Integer): Integer;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyEnumCount(Kind, Indx)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));

    Result := 0;
    case Kind of
      pkStyle :
        case Indx of
          0: Result := 3;  // 'Type' property - Simple; DropDown; DroppdownList
          1: Result := 3;  // 'OwnerDraw' propery - No; Fixed; Variable
        end
    end;
  end
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyEnumName                             |
 |                                                                      |
 | Get the enum strings for enumerated properties                       |
 |                                                                      |
 | Parameters:                                                          |
 |   Kind: TPropertyKind; Indx, enum: Integer                            |
 |                                                                      |
 | The function returns string                                          |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyEnumName(Kind: TPropertyKind; Indx,
  enum: Integer): string;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyEnumName (Kind, Indx, enum)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));
    Result := '';
    case Kind of
      pkStyle :
        case Indx of
          0:                   // 'Type' property
            case enum of
              0: Result := rstSimple;
              1: Result := rstDropdown;
              2: Result := rstDropdownList
            end;
          1:                   // 'OwnerDraw' property
            case enum of
              0: Result := rstNo;
              1: Result := rstFixed;
              2: Result := rstVariable
            end
        end
    end
  end
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyName                                 |
 |                                                                      |
 | Get the property name                                                |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyName(Kind: TPropertyKind;
  Indx: Integer): string;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyName (Kind, Indx)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));
    Result := '';
    case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralName [Indx];
      pkStyle: Result := ComboBoxControlPropertyStyleName [Indx];
//      pkExtended: Result := StaticControlPropertyExtendedName [Indx];
    end
  end
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyType                                 |
 |                                                                      |
 | Get a property type                                                  |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyType(Kind: TPropertyKind;
  Indx: Integer): TPropertyType;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyType (Kind, Indx)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));
    Result := ptInteger;
    case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralType [Indx];
      pkStyle: Result := ComboBoxControlPropertyStyleType [Indx];
//      pkExtended: Result := StaticControlPropertyExtendedType [Indx];
    end
  end
end;

(*----------------------------------------------------------------------*
 | TComboBoxControlInfo.GetPropertyValue                                |
 |                                                                      |
 | Get a property value.                                                |
 |                                                                      |
 | Parameters:                                                          |
 |   Kind: TPropertyKind; Indx: Integer                                  |
 *----------------------------------------------------------------------*)
function TComboBoxControlInfo.GetPropertyValue(Kind: TPropertyKind;
  Indx: Integer): Variant;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyValue (Kind, Indx)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));

    case Kind of
      pkStyle :
        case Indx of
          0: case Style and 3 of       // 'Type' property
                CBS_SIMPLE: Result := 0;
                CBS_DROPDOWN: Result := 1;
                CBS_DROPDOWNLIST: Result := 2;
                else
                  Result := 0
              end;
                                        // OwnerDraw property
          1: if HasStyle [CBS_OWNERDRAWFIXED] then
                Result := 1
              else
                if HasStyle [CBS_OWNERDRAWVARIABLE] then
                  Result := 2
                else
                  Result := 0;
          2: Result := HasStyle [CBS_SORT];
          3: Result := HasStyle [WS_VSCROLL];
          4: Result := HasStyle [CBS_NOINTEGRALHEIGHT];
          5: Result := HasStyle [CBS_OEMCONVERT];
          6: Result := HasStyle [CBS_AUTOHSCROLL];
          7: Result := HasStyle [CBS_DISABLENOSCROLL];
          8: Result := HasStyle [CBS_UPPERCASE];
          9: Result := HasStyle [CBS_LOWERCASE];
        end
    end
  end
end;

function TComboBoxControlInfo.GetStyle: DWORD;
begin
  if not fGotStyle then
  begin
    Result := inherited GetStyle;

    GetCBHandles;
    if FListBoxWnd <> 0 then
      if (GetWindowLong (ControlHandle, GWL_STYLE) and WS_VSCROLL) <> 0 then
        Result := Result or WS_VSCROLL
  end
  else
    Result := inherited GetStyle;
end;

procedure TComboBoxControlInfo.Init;
begin
  GetCBHandles;
  if (FListBoxWnd <> 0) and ((FOrigStyle and WS_VSCROLL) <> 0) then
    SetStyle (WS_VSCROLL, True)
end;

procedure TComboBoxControlInfo.SetPropertyValue(Kind: TPropertyKind;
  Indx: Integer; const Value: Variant);
var
  RecreateRequired: Boolean;
begin
  if Indx < inherited GetPropertyCount(Kind) then
    inherited SetPropertyValue (Kind, Indx, Value)
  else
  begin
    Dec(Indx, inherited GetPropertyCount(Kind));
    RecreateRequired := False;

    case Kind of
      pkStyle :
        case Indx of
          0 :
            begin
              case Value of
                0: SetMaskedStyle (CBS_SIMPLE, 3);
                1: SetMaskedStyle (CBS_DROPDOWN, 3);
                2: SetMaskedStyle (CBS_DROPDOWNLIST, 3)
              end;

              RecreateRequired := True
            end;

          1 :
            begin
              case Value of
                0: SetMaskedStyle (0, CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE);
                1: SetMaskedStyle (CBS_OWNERDRAWFIXED, CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE);
                2: SetMaskedStyle (CBS_OWNERDRAWVARIABLE, CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE)
              end;

              RecreateRequired := True
            end;

          2: HasStyle [CBS_SORT] := Value;
          3: HasStyle [WS_VSCROLL] := Value;
          4: HasStyle [CBS_NOINTEGRALHEIGHT] := Value;
          5: HasStyle [CBS_OEMCONVERT] := Value;
          6: HasStyle [CBS_AUTOHSCROLL] := Value;
          7: begin HasStyle [CBS_DISABLENOSCROLL] := Value; RecreateRequired := True; end;
          8: HasStyle [CBS_UPPERCASE] := Value;
          9: HasStyle [CBS_LOWERCASE] := Value;
        end
    end;

    if RecreateRequired then
    begin
      FOrigStyle := Style;
      RecreateWnd;
    end;
  end;
end;

var
  hUser32: THandle;

procedure TComboBoxControlInfo.SetStyle(st: DWORD; value: Boolean);
begin
  inherited;

  if st = WS_VSCROLL then
  begin
    GetCBHandles;
    if FListBoxWnd <> 0 then
      if Value then
        SetWindowLong (FListBoxWnd, GWL_STYLE, GetWindowLong (FListBoxWnd, GWL_STYLE) or WS_VSCROLL)
      else
        SetWindowLong (FListBoxWnd, GWL_STYLE, GetWindowLong (FListBoxWnd, GWL_STYLE) and not WS_VSCROLL);
  end;
end;

begin
  hUser32 := LoadLibrary ('user32.dll');
  if hUser32 <> 0 then
    fnGetComboBoxInfo := GetProcAddress (hUser32, 'GetComboBoxInfo');
end.
