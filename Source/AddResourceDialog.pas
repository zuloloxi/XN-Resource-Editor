unit AddResourceDialog;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, unitResourceDetails, unitResourceJPEG;

type
  TFormAddResource = class(TForm)
    ListView: TListView;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FResourceDetailsClass: TResourceDetailsClass;
  public
    property ResourceDetailsClass: TResourceDetailsClass read FResourceDetailsClass;
  end;

implementation

uses
  MainForm, unitResourceGraphics, unitResourceMessages, unitResourceDialogs,
  unitResourceMenus, unitResourceXPManifests, unitResourceGIF,
  unitResourceVersionInfo, unitResourceToolbar, unitResourceAccelerator,
  unitResourceExaminer;

{$R *.DFM}

const
  addableItems: array [0..13] of TResourceDetailsClass = (
    TCursorGroupResourceDetails,
    TBitmapResourceDetails,
    TIconGroupResourceDetails,
    TDIBResourceDetails,
    TStringResourceDetails,
    TMessageResourceDetails,
    TMenuResourceDetails,
    TDialogResourceDetails,
    TJpegResourceDetails,
    TGIFResourceDetails,
    TXPManifestResourceDetails,
    TVersionInfoResourceDetails,
    TToolbarResourceDetails,
    TAcceleratorResourceDetails
  );

procedure TFormAddResource.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := Low (AddableItems) to High (AddableItems) do
    with ListView.Items.Add do
    begin
      Caption := GetTypeName(AddableItems[i].GetBaseType);
      ImageIndex := GetTypeImage(AddableItems[i].GetBaseType);
    end
end;

procedure TFormAddResource.ButtonOKClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
    FResourceDetailsClass := AddableItems[ListView.Selected.Index]
  else
    FResourceDetailsClass := nil;
end;

procedure TFormAddResource.ListViewDblClick(Sender: TObject);
begin
  ButtonOKClick(Self);
  ModalResult := mrOK
end;

procedure TFormAddResource.FormResize(Sender: TObject);
begin
  ListView.Columns[0].Width := ListView.Width - 16;
end;

end.
