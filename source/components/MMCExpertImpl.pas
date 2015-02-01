unit MMCExpertImpl;

interface

uses Windows, SysUtils, Classes, ToolsApi, Controls;

{$R MMCEXPERTCODESNIPPETS.RES}

type
TMMCExpert = class (TNotifierObject, IOTAWizard, IOTAProjectWizard, IOTARepositoryWizard, IUnknown)

{ IOTAWizard }
  function GetIDString : string;
  function GetState : TWizardState;
  function GetName : string;

{ IOTAProjectWizard }
  function GetAuthor : string;
  function GetComment : string;
  function GetPage : string;
  function GetGlyph : Cardinal;
  procedure Execute;
end;

TWizardProjectCreator = class (TNotifierObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50, IOTAProjectCreator80)
private
  fCompanyName: string;
  fFileDescription: string;
  fProductName: string;
  procedure SetProjectOptions (Project : IOTAProject);

public

{ IOTACreator }

  function GetCreatorType: string;
  function GetExisting: Boolean;
  function GetFileSystem: string;
  function GetOwner: IOTAModule;
  function GetUnnamed: Boolean;

{ IOTAProjectCreator }

  function GetFileName: string;
  function GetOptionFileName: string;
  function GetShowSource: Boolean;
  procedure NewDefaultModule;
  function NewOptionSource(const ProjectName: string): IOTAFile;
  procedure NewProjectResource(const Project: IOTAProject);
  function NewProjectSource(const ProjectName: string): IOTAFile;

{ IOTAProjectCreator50 }

  procedure NewDefaultProjectModule(const Project: IOTAProject);

{ IOTAProjectCreator80 }
  function GetProjectPersonality: string;

  constructor Create (const AProductName, AFileDescription, ACompanyName : string);
  property ProductName : string read fProductName;
  property FileDescription : string read fFileDescription;
  property CompanyName : string read fCompanyName;
end;

TSnapinDataModuleCreator = class (TNotifierObject, IOTACreator, IOTAModuleCreator)
private
  fOwner : IOTAModule;
  fStaticItemText : string;

public
{ IOTACreator }

  function GetCreatorType: string;
  function GetExisting: Boolean;
  function GetFileSystem: string;
  function GetOwner: IOTAModule;
  function GetUnnamed: Boolean;

{ IOTAModuleCreator }

  function GetAncestorName: string;
  function GetImplFileName: string;
  function GetIntfFileName: string;
  function GetFormName: string;
  function GetMainForm: Boolean;
  function GetShowForm: Boolean;
  function GetShowSource: Boolean;
  function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
  function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  procedure FormCreated(const FormEditor: IOTAFormEditor);

  constructor Create (AOwner : IOTAModule; const AStaticItemText : string);
end;

TMMCProjectFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName : string);
end;

TMMCSnapinDataFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName : string);
end;

TMMCSnapinDataFormFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
  fScopeItemText : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName, AScopeItemText : string);
end;

var
  PackageServices : IOTAPackageServices;
  WizardServices : IOTAWizardServices;
  ModuleServices : IOTAModuleServices;

  module : IOTAModule;

procedure Register;

implementation

uses ComObj, ActiveX, MMCExpertForm, unitCodeSnippets, TypInfo, SnapinData;

var
  snipets : TCodeSnipets;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [TSnapinData]);
  RegisterPackageWizard (TMMCExpert.Create as IOTAWizard)
end;

procedure InitWizard;
begin
  snipets := TCodeSnipets.Create (HInstance);
  PackageServices := (BorlandIDEServices as IOTAPackageServices);
  WizardServices := (BorlandIDEServices as IOTAWizardServices);
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
end;

procedure ExitWizard;
begin
  FreeAndNil (snipets);
end;

function FormatSnippet (const s : string; const ss : array of string; const rs : array of string) : string;
var
  tempStr : string;
  idx : Integer;
  max : Integer;

  function CreateGUID : string;
  var
    r : TGUID;
  begin
    CoCreateGUID (r);
    result := GUIDToString (r);
  end;

begin
  max := High (ss);
  if High (rs) < max then
    max := High (rs);

  result := s;
  for idx := 0 to max do
    result := StringReplace (result, ss [idx], rs [idx], [rfReplaceAll, rfIgnoreCase]);

  repeat
    tempStr := StringReplace (result, '%CreateGUID%', CreateGuid, [rfIgnoreCase]);
    if tempStr = result then
      break;
    result := tempStr
  until False;
end;

procedure DisplayMessage (const msg : string);
begin
  MessageBox (0, PChar (msg), '', MB_ICONINFORMATION)
end;

{ TMMCExpert }

procedure TMMCExpert.Execute;
var
  frm : TfmSnapinWizard;
  prj : IOTAProject;
  creator : IOTACreator;
  md : IOTAModule;
//  i : Integer;
//  names : TOTAOptionNameArray;
//  opt : TOTAOptionName;
begin
  frm := TfmSnapinWizard.Create (Nil);
  try
    if frm.ShowModal = mrOK then
    begin
      moduleServices.CloseAll;

      creator := TWizardProjectCreator.Create (
          frm.edSnapinName.Text,
          frm.edSnapinDescription.Text,
          frm.edSnapinProvider.Text);

      md := moduleServices.CreateModule(creator);

      prj := md as IOTAProject;

      moduleServices.CreateModule (TSnapinDataModuleCreator.Create (prj, frm.edStaticScopeItem.Text));
    end;
  finally
    frm.Free
  end
end;

function TMMCExpert.GetAuthor: string;
begin
  result := 'Colin Wilson'
end;

function TMMCExpert.GetComment: string;
begin
  result := 'MMC Snapin Expert'
end;

function TMMCExpert.GetGlyph: Cardinal;
begin
  result := LoadIcon (HInstance, 'MMCWIZARD');
end;

function TMMCExpert.GetIDString: string;
begin
  result := 'ColinWilson.Experts.MMCSnapin.1';
end;

function TMMCExpert.GetName: string;
begin
  result := 'MMC Snapin Project';
end;

function TMMCExpert.GetPage: string;
begin
  result := 'Projects';
end;

function TMMCExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{ TWizardProjectCreator }

constructor TWizardProjectCreator.Create(const AProductName,
  AFileDescription, ACompanyName : string);
begin
  fProductName := AProductName;
  fFileDescription := AFileDescription;
  fCompanyName := ACompanyName;
end;

function TWizardProjectCreator.GetCreatorType: string;
begin
  result := 'Library';
//  result := '';         // Do project source ourselves
end;

function TWizardProjectCreator.GetExisting: Boolean;
begin
  result := False;
end;

function TWizardProjectCreator.GetFileName: string;
begin
  result := '';
end;

function TWizardProjectCreator.GetFileSystem: string;
begin
  result := '';  // Return the ID string of a IOTAFileSystem
end;

function TWizardProjectCreator.GetOptionFileName: string;
begin
  result := '';
end;

function TWizardProjectCreator.GetOwner: IOTAModule;
begin
  result := Nil;
end;

function TWizardProjectCreator.GetProjectPersonality: string;
begin
  result := sDelphiPersonality
end;

function TWizardProjectCreator.GetShowSource: Boolean;
begin
  result := True;
end;

function TWizardProjectCreator.GetUnnamed: Boolean;
begin
  result := True
end;

procedure TWizardProjectCreator.NewDefaultModule;
begin
end;

type
  TAddTypeLibModule = function (const Project: IOTAProject): IOTATypeLibModule;

procedure NewTypeLibrary (const Project : IOTAProject);
const
//  fnOrdinal = 570;
  fnName = '@Tlbide@AddTypeLibModule$qqrx48System@%DelphiInterface$t20Toolsapi@IOTAProject%';
var
  comcorelib : THandle;
  fnAddTypeLibModule : TAddTypeLibModule;
begin
  comcorelib := LoadLibrary ('comcore120.bpl');
  if comcorelib <> 0 then
  try
    fnAddTypeLibModule := GetProcAddress (comcorelib, PChar (fnName));
    if Assigned (fnAddTypeLibModule) then
      fnAddTypeLibModule (Project);
  finally
    FreeLibrary (comcorelib)
  end;
end;

procedure TWizardProjectCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
var
  i : Integer;
  editor : IOTAEditor;

begin
  SetProjectOptions (Project);
  OutputDebugString ('NewDefaultProjectModule');

  for i := 0 to Project.ModuleFileCount - 1 do
  begin
    editor := Project.ModuleFileEditors [i];
    OutputDebugString (PChar (editor.FileName));
  end;

  NewTypeLibrary (Project)
end;

function TWizardProjectCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  result := Nil
end;

procedure TWizardProjectCreator.NewProjectResource(
  const Project: IOTAProject);
begin
  SetProjectOptions (Project);
end;

function TWizardProjectCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  result := TMMCProjectFile.Create (ProjectName);
end;

procedure TWizardProjectCreator.SetProjectOptions(Project: IOTAProject);
var
  keys  : Integer;
  stlKeys : TStringList;
begin
  Project.ProjectOptions.Values ['IncludeVersionInfo'] := True;
  keys := Project.ProjectOptions.Values ['Keys'];
  if keys <> 0 then
  begin
    stlKeys := TStringList (keys);
    stlKeys.Values ['ProductName'] := ProductName;
    stlKeys.Values ['FileDescription'] := FileDescription;
    stlKeys.Values ['CompanyName'] := CompanyName
  end
end;

{ TMMCProjectFile }

constructor TMMCProjectFile.Create(const AFileName: string);
begin
  fFileName := AFileName
end;

function TMMCProjectFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCProjectFile.GetSource: string;
begin
  with snipets do
    result := FormatSnippet (GetSnippet ('MMCProjectSource'), ['%ProjectName%'], [fFileName]);
end;

{ TSnapinDataModuleCreator }

constructor TSnapinDataModuleCreator.Create(AOwner: IOTAModule; const AStaticItemText : string);
begin
  fOwner := AOwner;
  fStaticItemtext := AStaticItemText
end;

procedure TSnapinDataModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
end;

function TSnapinDataModuleCreator.GetAncestorName: string;
begin
  result := 'DataModule'
end;

function TSnapinDataModuleCreator.GetCreatorType: string;
begin
  result := sForm;
end;

function TSnapinDataModuleCreator.GetExisting: Boolean;
begin
 result := False
end;

function TSnapinDataModuleCreator.GetFileSystem: string;
begin
  result := '';
end;

function TSnapinDataModuleCreator.GetFormName: string;
begin
  result := '';
end;

function TSnapinDataModuleCreator.GetImplFileName: string;
begin
  result := '';
end;

function TSnapinDataModuleCreator.GetIntfFileName: string;
begin
  result := '';
end;

function TSnapinDataModuleCreator.GetMainForm: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetOwner: IOTAModule;
begin
  result := fOwner
end;

function TSnapinDataModuleCreator.GetShowForm: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetShowSource: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetUnnamed: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  result := TMMCSnapinDataFormFile.Create (FormIdent, FStaticItemText);
end;

function TSnapinDataModuleCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  result := TMMCSnapinDataFile.Create (ModuleIdent);
end;

function TSnapinDataModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
// C++ Interface (.h) file.
  result := Nil;
end;

{ TMMSnapinDataFile }

constructor TMMCSnapinDataFile.Create(const AFileName: string);
begin
  fFileName := AFileName;
end;

function TMMCSnapinDataFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCSnapinDataFile.GetSource: string;
begin
  with snipets do
    result := FormatSnippet (GetSnippet ('MMCSnapinDataImplSource'), ['%FileName%'], [fFileName]);
end;

{ TMMCSnapinDataFormFile }

constructor TMMCSnapinDataFormFile.Create(const AFileName, AScopeItemText: string);
begin
  fScopeItemText := AScopeItemText;
  fFileName := AFileName;
end;

function TMMCSnapinDataFormFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCSnapinDataFormFile.GetSource: string;
begin
  with snipets do
    result := FormatSnippet (GetSnippet ('MMCSnapinDataFormSource'), ['%ScopeItemText%'], ['''' + fScopeItemText + '''']);
end;

initialization
  InitWizard;
finalization
  ExitWizard;
end.
