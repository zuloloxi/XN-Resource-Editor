unit cmpCountryComboBox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, unitCharsetMap;

type
  TCountryComboBox = class(TCustomComboBox)
  private
    FCodes: TCountryCodes;
    FPreferredCountryCodes: string;
    procedure SetPreferredCountryCodes(const Value: string);
    function GetCountryCode(idx: Integer): TCountryCode;
  protected
    procedure CreateWnd; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    property CountryCode [idx: Integer]: TCountryCode read GetCountryCode;
    property Items;
    property Text;
  published
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;

    // Use telephone dialling code for counties - 1 = USA, 44 = UK etc.
    // Eg. Set this property to '44,1' to sort with United Kindom first, United States second, and the
    //     other countries next

    property PreferredCountryCodes: string read FPreferredCountryCodes write SetPreferredCountryCodes;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TCountryComboBox }

constructor TCountryComboBox.Create(AOwner: TComponent);
begin
  inherited;

  Style := csDropDownList;
end;

procedure TCountryComboBox.CreateWnd;
var
  i: Integer;
  cc: TCountryCode;
begin
  inherited;

  if csDesigning in ComponentState then
    Items.Add ('Countries')
  else
  begin
    if FCodes = Nil then
      FCodes := TCountryCodes.Create;

    FCodes.SortByName(FPreferredCountryCodes);

    Items.BeginUpdate;
    try
      Items.Clear;
      for i := 0 to FCodes.Count - 1 do
      begin
        cc := FCodes.CountryCode [i];

        Items.Add(cc.Name)
      end;


    finally
      Items.EndUpdate
    end;

    if Items.Count > 0 then
      ItemIndex := 0
  end
end;

destructor TCountryComboBox.Destroy;
begin
  FreeAndNil (FCodes);

  inherited;
end;

function TCountryComboBox.GetCountryCode(idx: Integer): TCountryCode;
begin
  result := TCountryCode (FCodes [idx]);
end;

procedure TCountryComboBox.SetPreferredCountryCodes(const Value: string);
begin
  if Value <> FPreferredCountryCodes then
  begin
    FPreferredCountryCodes := Value;
    if not (csDesigning in ComponentState) then
      RecreateWnd;
  end
end;

end.
