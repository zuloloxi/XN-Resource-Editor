unit DialogListViewControls;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, Vcl.Graphics,
  WinAPI.CommCtrl, Vcl.Controls, ComponentDialogEditor, DialogConsts;

type
  TListviewControlInfo = class (TStandardControlInfo)
  private
    FItemText: array of string;
    FOldImageList, FOldSmallImageList: HIMAGELIST;
    FImages, FSmallImages: TImageList;
  public
    class procedure CreateControlParams (var Params: TCreateControlParams); override;
    class function GetDescription: string; override;

    destructor Destroy; override;

    function GetPropertyCount(kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
    procedure Init; override;
  end;

implementation

uses 
  DialogStrings;

const
  LVS_SORTMASK = $0030;  // Should be in CommCtrl.pas

  ListviewControlPropertyGeneralCount = 0;
  ListviewControlPropertyStyleCount = 4;
  ListviewControlPropertyExtendedCount = 0;
  ListviewControlPropertyCount: array [TPropertyKind] of Integer = (ListviewControlPropertyGeneralCount, ListviewControlPropertyStyleCount, ListviewControlPropertyExtendedCount);
//  ListviewControlPropertyGeneralName: array [0..ListviewControlPropertyGeneralCount - 1] of string = (rstCaption);
  ListviewControlPropertyStyleName: array [0..ListviewControlPropertyStyleCount - 1] of string = (rstView, rstAlign, rstSort, rstShareImageLists);
//  ListviewControlPropertyExtendedName: array [0..ListviewControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  ListviewControlPropertyGeneralType: array [0..ListviewControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ListviewControlPropertyStyleType: array [0..ListviewControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptEnum, ptBoolean);
//  ListviewControlPropertyExtendedType: array [0..ListviewControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);


{ TListviewControlInfo }

class procedure TListviewControlInfo.CreateControlParams(
  var Params: TCreateControlParams);
begin
  inherited;
  Params.cx := 70;
  Params.cy := 50;
end;

destructor TListviewControlInfo.Destroy;
begin
// Restore the old image list.  If the ShareImageList style was not set
// this ensures that the original image list gets destroyed.

  ListView_SetImageList(ControlHandle, FOldImageList, LVSIL_NORMAL);
  ListView_SetImageList(ControlHandle, FOldSmallImageList, LVSIL_SMALL);

  FImages.Free;
  FSmallImages.Free;

  inherited;
end;

class function TListviewControlInfo.GetDescription: string;
begin
  Result := rstListView
end;

function TListviewControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount(kind) + ListviewControlPropertyCount [kind]
end;

function TListviewControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount(kind) then
    Result := inherited GetPropertyEnumCount(kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));
    Result := 0;
    case kind of
      pkStyle :
        case idx of
          0: Result := 4;
          1: Result := 2;
          2: Result := 3;
        end
    end
  end
end;

function TListviewControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount(kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));
    Result := '';
    case kind of
      pkStyle :
        case idx of
          0: case enum of
                0: Result := rstIcon;
                1: Result := rstSmallIcon;
                2: Result := rstReport;
                3: Result := rstList;
              end;
          1: case enum of
                0: Result := rstTop;
                1: Result := rstleft;
              end;

          2: case enum of
                0: Result := rstNone;
                1: Result := rstAscending;
                2: Result := rstDescending;
              end;
        end
    end
  end
end;

function TListviewControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount(kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));
    Result := '';
     case kind of
//      pkGeneral: Result := StaticControlPropertyGeneralName [idx];
      pkStyle: Result := ListviewControlPropertyStyleName [idx];
//      pkExtended: Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TListviewControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount(kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));
    Result := ptInteger;
    case kind of
//      pkGeneral: Result := StaticControlPropertyGeneralType [idx];
      pkStyle: Result := ListviewControlPropertyStyleType [idx];
//      pkExtended: Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TListviewControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount(kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));

    case kind of
      pkStyle :
        case idx of
          0: case Style and LVS_TYPEMASK of
                LVS_REPORT: 
                  Result := 2;
                LVS_SMALLICON: 
                  Result := 1;
                LVS_LIST: 
                  Result := 3;
                else
                  Result := 0
              end;
          1: case style and LVS_ALIGNMASK of
                LVS_ALIGNLEFT: Result := 1;
                else
                  Result := 0
              end;
          2: case style and LVS_SORTMASK of
                LVS_SORTASCENDING: Result := 1;
                LVS_SORTDESCENDING: Result := 2;
                else
                  Result := 0
              end;
          3: Result := HasStyle [LVS_SHAREIMAGELISTS];
        end
    end
  end
end;

procedure TListviewControlInfo.Init;
var
  itm: TLVItem;
  col: TLVColumn;
  i, count: Integer;
  ico: TIcon;
begin
  FImages := TImageList.Create(nil);
  FSmallImages := TImageList.Create (nil);

  ico := TIcon.Create;
  try
    FImages.Width := 32;
    FImages.Height := 32;
    ico.Handle := LoadImage (0, PChar (OIC_WINLOGO), IMAGE_ICON, FImages.Width, FImages.Height, LR_SHARED);
    FImages.AddIcon(ico);

    FSmallImages.Width := 16;
    FSmallImages.Height := 16;
    ico.Handle := LoadImage (0, PChar (OIC_WINLOGO), IMAGE_ICON, FSmallImages.Width, FSmallImages.Height, LR_SHARED);
    FSmallImages.AddIcon(ico);

  finally
    ico.Free
  end;
  FOldImageList := ListView_SetImageList(ControlHandle, FImages.Handle, LVSIL_NORMAL);
  FOldSmallImageList := ListView_SetImageList(ControlHandle, FSmallImages.Handle, LVSIL_SMALL);
  count := 7;
  SetLength (FItemText, count);
  ListView_SetItemCount(ControlHandle, count);

  for i := 0 to count - 1 do
  begin
    FillChar (itm, sizeof (itm), 0);
    itm.mask := LVIF_TEXT;
    itm.iItem := i;
    itm.iImage := 0;

    FItemText [i] := Format('Item %d', [i]);
    itm.pszText := PChar (FItemText [i]);
    ListView_InsertItem (ControlHandle, itm)
  end;

  col.mask := LVCF_TEXT or LVCF_WIDTH;
  col.cx := 70;
  col.pszText := 'Hello World';

  ListView_InsertColumn (ControlHandle, 0, col);
end;

procedure TListviewControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired: Boolean;
begin
  if idx < inherited GetPropertyCount(kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec(idx, inherited GetPropertyCount(kind));
    recreateRequired := False;

    case kind of
      pkStyle :
        case idx of
          0: begin
                case Value of
                  0: SetMaskedStyle (0, LVS_TYPEMASK);
                  1: SetMaskedStyle (LVS_SMALLICON, LVS_TYPEMASK);
                  2: SetMaskedStyle (LVS_REPORT, LVS_TYPEMASK);
                  3: SetMaskedStyle (LVS_LIST, LVS_TYPEMASK)
                end;
                recreateRequired := True;
              end;
          1: begin
                case Value of
                  0: SetMaskedStyle (LVS_ALIGNTOP, LVS_ALIGNMASK);
                  1: SetMaskedStyle (LVS_ALIGNLEFT, LVS_ALIGNMASK);
                end;
                recreateRequired := True;
              end;

          2: begin
                case Value of
                  0: SetMaskedStyle (0, LVS_SORTMASK);
                  1: SetMaskedStyle (LVS_SORTASCENDING, LVS_SORTMASK);
                  2: SetMaskedStyle (LVS_SORTDESCENDING, LVS_SORTMASK);
                end;
                recreateRequired := True;
              end;

          3: HasStyle [LVS_SHAREIMAGELISTS] := Value;
        end;
    end;

    if recreateRequired then
      RecreateWnd;
  end;
end;

end.
