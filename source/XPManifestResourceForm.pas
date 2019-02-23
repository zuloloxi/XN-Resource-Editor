unit XPManifestResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ResourceForm, unitResourceXPManifests;

type
  TfmXPManifestResource = class(TfmResource)
    MemoManifest: TMemo;
    procedure MemoManifestExit(Sender: TObject);
  private
    FDetails: TXPManifestResourceDetails;
  protected
    function GetCanCopy: Boolean; override;
    function GetCanCut: Boolean; override;
    function GetCanPaste: Boolean; override;
    function GetCanSelectAll: Boolean; override;
    function GetCanDelete: Boolean; override;
  public
    procedure SetObject(const Value: TObject); override;
    procedure Cut; override;
    procedure Copy; override;
    procedure Paste; override;
    procedure SelectAll; override;
    procedure EditDelete; override;
  end;

implementation

{$R *.dfm}

resourcestring
  rstChangeManifest = 'change manifest';

{ TfmXPManifestResource }

procedure TfmXPManifestResource.Copy;
begin
  MemoManifest.CopyToClipboard;
end;

procedure TfmXPManifestResource.Cut;
begin
  MemoManifest.CutToClipboard;
end;

procedure TfmXPManifestResource.EditDelete;
begin
  MemoManifest.SetSelTextBuf('');
end;

function TfmXPManifestResource.GetCanCopy: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TfmXPManifestResource.GetCanCut: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TfmXPManifestResource.GetCanDelete: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TfmXPManifestResource.GetCanPaste: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TfmXPManifestResource.GetCanSelectAll: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

procedure TfmXPManifestResource.Paste;
begin
  MemoManifest.PasteFromClipboard;
end;

procedure TfmXPManifestResource.SelectAll;
begin
  MemoManifest.SelectAll;
end;

procedure TfmXPManifestResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TXPManifestResourceDetails;
  MemoManifest.Lines.Text := FDetails.Text;
end;

procedure TfmXPManifestResource.MemoManifestExit(Sender: TObject);
begin
  if MemoManifest.CanUndo then
  begin
    AddUndoEntry (rstChangeManifest);
    FDetails.Text := MemoManifest.Lines.Text;
  end
end;

end.
