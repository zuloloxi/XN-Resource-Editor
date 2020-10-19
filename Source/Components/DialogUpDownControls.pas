unit DialogUpDownControls;

interface

uses
  Windows, Classes, SysUtils, CommCtrl, ComponentDialogEditor, DialogConsts;

type
  TUpDownControlInfo = class (TStandardControlInfo)
  public
    class procedure CreateControlParams (var Params: TCreateControlParams); override;
    class function GetDescription: string; override;
    function GetPropertyCount(Kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(Kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(Kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(Kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(Kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(Kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(Kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

implementation

uses
  DialogStrings;

const
  UpDownControlPropertyGeneralCount = 0;
  UpDownControlPropertyStyleCount = 8;
  UpDownControlPropertyExtendedCount = 0;
  UpDownControlPropertyCount: array [TPropertyKind] of Integer = (UpDownControlPropertyGeneralCount, UpDownControlPropertyStyleCount, UpDownControlPropertyExtendedCount);
//  UpDownControlPropertyGeneralName: array [0..UpDownControlPropertyGeneralCount - 1] of string = (rstCaption);
  UpDownControlPropertyStyleName: array [0..UpDownControlPropertyStyleCount - 1] of string = (rstOrientation, rstAlignment, rstAutoBuddy, rstSetBuddyInteger, rstNoThousands, rstWrap, rstArrowKeys, rstHotTrack);
//  UpDownControlPropertyExtendedName: array [0..UpDownControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  UpDownControlPropertyGeneralType: array [0..UpDownControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  UpDownControlPropertyStyleType: array [0..UpDownControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  UpDownControlPropertyExtendedType: array [0..UpDownControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TUpDownControlInfo }

class procedure TUpDownControlInfo.CreateControlParams(
  var Params: TCreateControlParams);
begin
  inherited;
  Params.Style := Params.Style or UDS_ARROWKEYS;
end;

class function TUpDownControlInfo.GetDescription: string;
begin
  Result := rstUpDown
end;

function TUpDownControlInfo.GetPropertyCount(
  Kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount(Kind) + UpDownControlPropertyCount [Kind]
end;

function TUpDownControlInfo.GetPropertyEnumCount(Kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyEnumCount(Kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));
    Result := 0;
    case Kind of
      pkStyle :
        case idx of
          0:
            Result := 2;
          1:
            Result := 3
        end;
    end;
  end;
end;

function TUpDownControlInfo.GetPropertyEnumName(Kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyEnumName (Kind, idx, enum)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));
    Result := '';
    case Kind of
      pkStyle :
        case idx of
          0:
            case enum of
              0:
                Result := rstVertical;
              1:
                Result := rstHorizontal;
            end;
          1:
            case enum of
              0:
                Result := rstUnattached;
              1:
                Result := rstLeft;
              2:
                Result := rstRight
            end;
        end
    end
  end
end;

function TUpDownControlInfo.GetPropertyName(Kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyName (Kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));
    Result := '';
     case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralName [idx];
      pkStyle: Result := UpDownControlPropertyStyleName [idx];
//      pkExtended: Result := StaticControlPropertyExtendedName [idx];
    end;
  end;
end;

function TUpDownControlInfo.GetPropertyType(Kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyType (Kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));
    Result := ptInteger;
    case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralType [idx];
      pkStyle: Result := UpDownControlPropertyStyleType [idx];
//      pkExtended: Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TUpDownControlInfo.GetPropertyValue(Kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount(Kind) then
    Result := inherited GetPropertyValue (Kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));

    case Kind of
      pkStyle :
        case idx of
          0:
            if HasStyle[UDS_HORZ] then
              Result := 1
            else
              Result := 0;
          1:
            case Style and (UDS_ALIGNLEFT or UDS_ALIGNRIGHT) of
              UDS_ALIGNLEFT:
                Result := 1;
              UDS_ALIGNRIGHT:
                Result := 2;
              else
                Result := 0
              end;
          2:
            Result := HasStyle[UDS_AUTOBUDDY];
          3:
            Result := HasStyle[UDS_SETBUDDYINT];
          4:
            Result := HasStyle[UDS_NOTHOUSANDS];
          5:
            Result := HasStyle[UDS_WRAP];
          6:
            Result := HasStyle[UDS_ARROWKEYS];
          7:
            Result := HasStyle[UDS_HOTTRACK];
        end
    end
  end
end;

procedure TUpDownControlInfo.SetPropertyValue(Kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  RecreateRequired: Boolean;
begin
  if idx < inherited GetPropertyCount(Kind) then
    inherited SetPropertyValue (Kind, idx, Value)
  else
  begin
    Dec(idx, inherited GetPropertyCount(Kind));
    RecreateRequired := True;

    case Kind of
      pkStyle :
        case idx of
          0:
            HasStyle[UDS_HORZ] := Value = 1;
          1:
            case Value of
              0:
                SetMaskedStyle (0, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
              1:
                SetMaskedStyle (UDS_ALIGNLEFT, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
              2:
                SetMaskedStyle (UDS_ALIGNRIGHT, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
            end;

          2:
            HasStyle[UDS_AUTOBUDDY] := Value;
          3:
            HasStyle[UDS_SETBUDDYINT] := Value;
          4:
            HasStyle[UDS_NOTHOUSANDS] := Value;
          5:
            HasStyle[UDS_WRAP] := Value;
          6:
            HasStyle[UDS_ARROWKEYS] := Value;
          7:
            HasStyle[UDS_HOTTRACK] := Value;
        end
    end;

    if RecreateRequired then
      RecreateWnd;
  end;
end;

end.
