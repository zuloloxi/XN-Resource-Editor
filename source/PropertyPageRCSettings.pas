unit PropertyPageRCSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, VirtualTrees, PropertyPageForm,
  ComponentPersistentPosition, unitIncludePaths;

type
  TPropertyPageRCSettingsData = class (TPropertyPageData)
  private
    FIncludePathType: Integer;
    FCustomIncludePath: string;
    FIncludePathPackageName: string;
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TfmPropertyPageRCSettings = class(TfmPropertyPage)
    ButtonCustomIncludePath: TButton;
    EditCustomIncludePath: TEdit;
    RadioButtonCompilerIncludePath: TRadioButton;
    RadioButtonCustomIncludePath: TRadioButton;
    RadioButtonEnvironmentVariableIncludePath: TRadioButton;
    vstIncludePackages: TVirtualStringTree;
    procedure RadioButtonCustomIncludePathClick(Sender: TObject);
    procedure vstIncludePackagesChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstIncludePackagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstIncludePackagesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
  private
    FIncludePathPackages: TIncludePathPackages;
    FData: TPropertyPageRCSettingsData;
    function GetNodePackage (Node: PVirtualNode): TIncludePathPackage;
    function GetNodeForPackage (const packageName: string): PVirtualNode;
    procedure UpdateCustomText;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls (AData: TPropertyPageData); override;
  end;

implementation

uses
  unitCREdProperties;

{$R *.DFM}

type
  PObject = ^TObject;

{ TfmPropertyPageRCSettings }

class function TfmPropertyPageRCSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageRCSettingsData;
end;

procedure TfmPropertyPageRCSettings.PopulateControls(AData: TPropertyPageData);
var
  Node: PVirtualNode;
begin
  Inherited;
  FIncludePathPackages := TIncludePathPackages.Create;
  vstIncludePackages.RootNodeCount := FIncludePathPackages.Count;

  FData := TPropertyPageRCSettingsData (AData);

  case FData.FIncludePathType of
    includePathPackage:
      begin
        RadioButtonCompilerIncludePath.Checked := True;
        Node := GetNodeForPackage (FData.FIncludePathPackageName);
        if Assigned(Node) then
          Node.CheckState := csCheckedNormal
      end;

    includePathEnv:
      RadioButtonEnvironmentVariableIncludePath.Checked:= True;

    includePathCustom:
      begin
        RadioButtonCustomIncludePath.Checked := True;
        EditCustomIncludePath.Text := FData.FCustomIncludePath;
      end;
  end;
  UpdateCustomText;
end;

procedure TfmPropertyPageRCSettings.UpdateActions;
begin
  case FData.FIncludePathType of
    includePathPackage:
      begin
        vstIncludePackages.Enabled := True;
        ButtonCustomIncludePath.Enabled := False;
        EditCustomIncludePath.Enabled := False;
      end;
    includePathCustom:
      begin
        vstIncludePackages.Enabled := False;
        ButtonCustomIncludePath.Enabled := True;
        EditCustomIncludePath.Enabled := True;
      end;
    includePathEnv:
      begin
        vstIncludePackages.Enabled := False;
        ButtonCustomIncludePath.Enabled := False;
        EditCustomIncludePath.Enabled := False;
      end;
  end
end;

function TfmPropertyPageRCSettings.GetNodePackage(
  Node: PVirtualNode): TIncludePathPackage;
var
  data: PObject;
begin
  data := PObject(vstIncludePackages.GetNodeData(Node));
  Result := TIncludePathPackage(data^);
end;

function TfmPropertyPageRCSettings.GetNodeForPackage(
  const packageName: string): PVirtualNode;
begin
  Result := vstIncludePackages.GetFirst;
  while Assigned(Result) do
  begin
    if SameText(GetNodePackage (Result).Name, packageName) then
      break
    else
      Result := vstIncludePackages.GetNext(Result)
  end
end;

procedure TfmPropertyPageRCSettings.UpdateCustomText;
var
  st: string;
begin
  case FData.FIncludePathType of
    includePathPackage:
      st := GetIncludePathForPackage (FData.FIncludePathPackageName);
    includePathCustom:
      st := FData.FCustomIncludePath;
    includePathEnv:
      st := GetEnvironmentVariable ('Include');
  end;

  EditCustomIncludePath.Text := st;
  FData.FCustomIncludePath := st;
end;

{ TPropertyPageRCSettingsData }

procedure TPropertyPageRCSettingsData.Apply;
begin
  gProperties.IncludePathType := FIncludePathType;
  case FIncludePathType of
    includePathPackage:
      gProperties.IncludePathPackageName := FIncludePathPackageName;
    includePathCustom:
      gProperties.IncludePath := FCustomIncludePath;
  end
end;

procedure TPropertyPageRCSettingsData.Initialize;
begin
  FIncludePathType := gProperties.IncludePathType;
  FCustomIncludePath := gProperties.IncludePath;
  FIncludePathPackageName := gProperties.IncludePathPackageName
end;

procedure TfmPropertyPageRCSettings.FormDestroy(Sender: TObject);
begin
  inherited;
  FIncludePathPackages.Free;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data: PObject;
begin
  data := PObject(vstIncludePackages.GetNodeData(Node));
  data^ := FIncludePathPackages.Package [Node^.Index];
  Node^.CheckType := ctRadioButton ;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Package: TIncludePathPackage;
begin
  Package := GetNodePackage(Node);
  if Assigned(Package) then
    CellText := Package.Name
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  if Node.CheckState = csCheckedNormal then
  begin
    FData.FIncludePathPackageName := GetNodePackage (Node).Name;
    UpdateCustomText
  end;
end;

procedure TfmPropertyPageRCSettings.RadioButtonCustomIncludePathClick(Sender: TObject);
var
  CompilerPath: Boolean;
  p: PVirtualNode;
  Package: TIncludePathPackage;
begin
  CompilerPath := RadioButtonCompilerIncludePath.Checked;

  p := vstIncludePackages.GetFirst;
  while Assigned(p) do
  begin
    Package := GetNodePackage(p);
    if CompilerPath and SameText(Package.Name, FData.FIncludePathPackageName) then
      p.CheckState := csCheckedNormal
    else
      p.CheckState := csUncheckedNormal;

    p := vstIncludePackages.GetNext(p);
  end;

  if RadioButtonCompilerIncludePath.Checked then
    FData.FIncludePathType := includePathPackage
  else
    if RadioButtonEnvironmentVariableIncludePath.Checked then
      FData.FIncludePathType := includePathEnv
    else
      FData.FIncludePathType := includePathCustom;

  UpdateCustomText;
end;

end.
