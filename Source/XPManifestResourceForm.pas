unit XPManifestResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ResourceForm, unitResourceXPManifests;

type
  TFormXPManifestResource = class(TFormResource)
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

{ TFormXPManifestResource }

procedure TFormXPManifestResource.Copy;
begin
  MemoManifest.CopyToClipboard;
end;

procedure TFormXPManifestResource.Cut;
begin
  MemoManifest.CutToClipboard;
end;

procedure TFormXPManifestResource.EditDelete;
begin
  MemoManifest.SetSelTextBuf('');
end;

function TFormXPManifestResource.GetCanCopy: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TFormXPManifestResource.GetCanCut: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TFormXPManifestResource.GetCanDelete: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TFormXPManifestResource.GetCanPaste: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

function TFormXPManifestResource.GetCanSelectAll: Boolean;
begin
  Result := MemoManifest.SelLength > 0;
end;

procedure TFormXPManifestResource.Paste;
begin
  MemoManifest.PasteFromClipboard;
end;

procedure TFormXPManifestResource.SelectAll;
begin
  MemoManifest.SelectAll;
end;

procedure TFormXPManifestResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TXPManifestResourceDetails;
  MemoManifest.Lines.Text := FDetails.Text;
end;

procedure TFormXPManifestResource.MemoManifestExit(Sender: TObject);
begin
  if MemoManifest.CanUndo then
  begin
    AddUndoEntry (rstChangeManifest);
    FDetails.Text := MemoManifest.Lines.Text;
  end
end;

end.
