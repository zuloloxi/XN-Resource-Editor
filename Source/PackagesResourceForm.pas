unit PackagesResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls,
  VCL.ExtCtrls, ResourceForm, ComponentPropertyListBox, unitResourceRCData;

type
  TFormPackagesResource = class(TFormResource)
    PageControlRequiresContains: TPageControl;
    TabSheetRequires: TTabSheet;
    TabSheetContains: TTabSheet;
    PanelMain: TPanel;
    PropertyListBoxFlags: TPropertyListBox;
    Splitter: TSplitter;
    ListViewRequires: TListView;
    ListViewContains: TListView;
    procedure FormShow(Sender: TObject);
  private
    FDetails: TRCDataPackagesResourceDetails;
  public
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

{ TFormPackagesResource }

procedure TFormPackagesResource.SetObject(const Value: TObject);
var
  Prop: TPropertyListProperty;
  Index: Integer;
  st: string;
  Flgs: Integer;
begin
  inherited;

  FDetails := Obj as TRCDataPackagesResourceDetails;

  Prop := PropertyListBoxFlags.FindProperty ('Environment');
  Prop.PropertyValue := FDetails.Environment;

  Prop := PropertyListBoxFlags.FindProperty ('Module Type');
  Prop.PropertyValue := FDetails.ModuleType;

  Prop := PropertyListBoxFlags.FindProperty ('Never Build');
  Prop.PropertyValue := FDetails.NeverBuild;

  Prop := PropertyListBoxFlags.FindProperty ('Design Only');
  Prop.PropertyValue := FDetails.DesignTimeOnly;

  Prop := PropertyListBoxFlags.FindProperty ('Runtime Only');
  Prop.PropertyValue := FDetails.RuntimeOnly;

  Prop := PropertyListBoxFlags.FindProperty ('Check Duplicates');
  Prop.PropertyValue := FDetails.CheckForDuplicates;

  ListViewRequires.Items.BeginUpdate;
  try
    for Index := 0 to FDetails.RequiresCount - 1 do
      with ListViewRequires.Items.Add do
        Caption := FDetails.Requires [Index];
    if ListViewRequires.Items.Count > 0 then
      ListViewRequires.ItemIndex := 0;
  finally
    ListViewRequires.Items.EndUpdate
  end;

  ListViewContains.Items.BeginUpdate;
  try
    for Index := 0 to FDetails.ContainsCount - 1 do
      with ListViewContains.Items.Add do
      begin
        Caption := FDetails.Contains [Index];

        { PackageUnitFlags:
          bit      meaning
          -----------------------------------------------------------------------------------------
          0      | main unit
          1      | package unit(dpk source)
          2      | $WEAKPACKAGEUNIT unit
          3      | original containment of $WEAKPACKAGEUNIt(package into which it was compiled)
          4      | implicitly imported
          5..7   | reserved
        }

        Flgs := FDetails.ContainsFlag [Index];

        st := '';
        if (Flgs and  1) <> 0 then st := st + ', main unit';
        if (Flgs and  2) <> 0 then st := st + ', package unit';
        if (Flgs and  4) <> 0 then st := st + ', $WEAKPACKAGEUNIT unit';
        if (Flgs and  8) <> 0 then st := st + ', original containment of $WEAKPACKAGEUNIT';
        if (Flgs and 16) <> 0 then st := st + ', implictly imported';

        system.Delete (st, 1, 2);
        SubItems.Add (st)
      end;

      if ListViewContains.Items.Count > 0 then
        ListViewContains.ItemIndex := 0

  finally
    ListViewContains.Items.EndUpdate
  end
end;

procedure TFormPackagesResource.FormShow(Sender: TObject);
begin
  inherited;

  PageControlRequiresContains.ActivePageIndex := 0;
end;

end.
