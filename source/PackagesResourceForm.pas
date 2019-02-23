unit PackagesResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ComCtrls, ExtCtrls, cmpPropertyListBox, unitResourceRCData;

type
  TfmPackagesResource = class(TfmResource)
    pcRequiresContains: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    plbFlags: TPropertyListBox;
    Splitter1: TSplitter;
    lvRequires: TListView;
    lvContains: TListView;
    procedure FormShow(Sender: TObject);
  private
    FDetails: TRCDataPackagesResourceDetails;
  public
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

{ TfmPackagesResource }

procedure TfmPackagesResource.SetObject(const Value: TObject);
var
  prop: TPropertyListProperty;
  i: Integer;
  st: string;
  flgs: Integer;
begin
  inherited;

  FDetails := Obj as TRCDataPackagesResourceDetails;

  prop := plbFlags.FindProperty ('Environment');
  prop.PropertyValue := FDetails.Environment;

  prop := plbFlags.FindProperty ('Module Type');
  prop.PropertyValue := FDetails.ModuleType;

  prop := plbFlags.FindProperty ('Never Build');
  prop.PropertyValue := FDetails.NeverBuild;

  prop := plbFlags.FindProperty ('Design Only');
  prop.PropertyValue := FDetails.DesignTimeOnly;

  prop := plbFlags.FindProperty ('Runtime Only');
  prop.PropertyValue := FDetails.RuntimeOnly;

  prop := plbFlags.FindProperty ('Check Duplicates');
  prop.PropertyValue := FDetails.CheckForDuplicates;

  lvRequires.Items.BeginUpdate;
  try
    for i := 0 to FDetails.RequiresCount - 1 do
      with lvRequires.Items.Add do
        Caption := FDetails.Requires [i];
    if lvRequires.Items.Count > 0 then
      lvRequires.ItemIndex := 0;
  finally
    lvRequires.Items.EndUpdate
  end;

  lvContains.Items.BeginUpdate;
  try
    for i := 0 to FDetails.ContainsCount - 1 do
      with lvContains.Items.Add do
      begin
        Caption := FDetails.Contains [i];
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
        flgs := FDetails.ContainsFlag [i];

        st := '';
        if (flgs and  1) <> 0 then st := st + ', main unit';
        if (flgs and  2) <> 0 then st := st + ', package unit';
        if (flgs and  4) <> 0 then st := st + ', $WEAKPACKAGEUNIT unit';
        if (flgs and  8) <> 0 then st := st + ', original containment of $WEAKPACKAGEUNIT';
        if (flgs and 16) <> 0 then st := st + ', implictly imported';

        system.Delete (st, 1, 2);
        SubItems.Add (st)
      end;

      if lvContains.Items.Count > 0 then
        lvContains.ItemIndex := 0


  finally
    lvContains.Items.EndUpdate
  end
end;

procedure TfmPackagesResource.FormShow(Sender: TObject);
begin
  inherited;

  pcRequiresContains.ActivePageIndex := 0;
end;

end.
