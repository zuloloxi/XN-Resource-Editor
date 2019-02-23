(*======================================================================*
 | unitResourceDialogs                                                  |
 |                                                                      |
 | Encapsulates Dialog resources in resources                           |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001,2008                                 |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)
unit unitResourceDialogs;

interface

uses
  Windows, Classes, SysUtils, unitResourceDetails, DialogConsts;

type
  TDialogResourceDetails = class (TResourceDetails)
  public
    class function GetBaseType: UnicodeString; override;
    procedure InitNew; override;

    procedure BeginInit(X, Y, CX, CY: Integer; Style, ExStyle: DWORD; Menu, Cls, Title: TSzOrID; FontSize: DWORD; const FaceName: UnicodeString);
    procedure BeginInitEx (X, Y, CX, CY: Integer; Style, ExStyle, HelpId: DWORD; Menu, Cls, Title: TSzOrID; FontSize, FontWeight, FontItalic, FontCharset: DWORD; const FaceName: UnicodeString);
    procedure InitAddControl (Cls: TSzOrID; id: DWORD; const text: TSzOrID; X, Y, CX, CY: Integer; Style, ExStyle: DWORD; dataLen: DWORD; const dat);
    procedure InitAddControlEx (Cls: TSzOrID; id: DWORD; const text: TSzOrID; X, Y, CX, CY: Integer; Style, ExStyle, HelpId: DWORD; dataLen: DWORD; const dat);
    procedure EndInit(ctrlCount: DWORD);
    procedure EndInitEx (ctrlCount: DWORD);
  end;

implementation

{ TDialogResourceDetails }

procedure TDialogResourceDetails.BeginInit(X, Y, CX, CY: Integer; Style,
  ExStyle: DWORD; Menu, Cls, Title: TSzOrID; FontSize: DWORD; const FaceName: UnicodeString);
var
  Template: TDlgTemplate;
  w: Word;
begin
  Data.Clear;
  if FaceName <> '' then
    Style := Style or DS_SETFONT;
  Template.style := Style;
  Template.dwExtendedStyle := exstyle;
  Template.X := X;
  Template.Y := Y;
  Template.CX := CX;
  Template.CY := CY;
  Template.cdit := 0;

  Data.Write(Template, SizeOf(Template));

  WriteSzOrID(Data, Menu);
  WriteSzOrID(Data, Cls);
  WriteSzOrID(Data, Title);

  w := FontSize;
  Data.Write(w, SizeOf(w));

  Data.Write (PWideChar (FaceName)^, (Length (FaceName) + 1) * SizeOf(WideChar))
end;

procedure TDialogResourceDetails.BeginInitEx(X, Y, CX, CY: Integer; Style,
  ExStyle, HelpId: DWORD; Menu, Cls, Title: TSzOrID; FontSize, FontWeight, FontItalic, FontCharset: DWORD;
  const FaceName: UnicodeString);
var
  Template: TDlgTemplateEx;
  w: Word;
  b: byte;
begin
  Data.Clear;
  if FaceName <> '' then
    Style := Style or DS_SETFONT;
  Template.dlgVer := 1;
  Template.style := Style;
  Template.exStyle := exstyle;
  Template.X := X;
  Template.Y := Y;
  Template.CX := CX;
  Template.CY := CY;
  Template.cDlgItems :=0;
  Template.signature := $ffff;
  Template.HelpId := HelpId;

  Data.Write (Template, SizeOf(Template));

  WriteSzOrID (Data, Menu);
  WriteSzOrID (Data, Cls);
  WriteSzOrID (Data, Title);

  w := FontSize;
  Data.Write(w, SizeOf(w));

  w := FontWeight;
  Data.Write(w, SizeOf(w));

  b := FontItalic;
  Data.Write(b, SizeOf(b));

  b := FontCharset;
  Data.Write(b, SizeOf(b));

  Data.Write (PWideChar (FaceName)^, (Length (FaceName) + 1) * SizeOf(WideChar))
end;

procedure TDialogResourceDetails.EndInit(ctrlCount: DWORD);
var
  p: PDlgTemplate;
begin
  p := PDlgTemplate (Data.Memory);
  p^.cdit := CtrlCount
end;

procedure TDialogResourceDetails.EndInitEx(ctrlCount: DWORD);
var
  p: PDlgTemplateEx;
begin
  p := PDlgTemplateEx (Data.Memory);
  p^.cDlgItems := CtrlCount
end;

class function TDialogResourceDetails.GetBaseType: UnicodeString;
begin
  Result := IntToStr (Integer (RT_DIALOG));
end;

procedure TDialogResourceDetails.InitAddControl(Cls: TSzOrID; id: DWORD;
  const text: TSzOrID; X, Y, CX, CY: Integer; Style, ExStyle: DWORD; dataLen: DWORD; const dat);
var
  templ: TDlgItemTemplate;
  w: Word;
begin
  Pad (Data);

  templ.style := Style;
  templ.dwExtendedStyle := ExStyle;
  templ.X := X;
  templ.Y := Y;
  templ.CX := CX;
  templ.CY := CY;
  templ.id := id;

  Data.Write(templ, SizeOf(templ));

//  Pad (Data);

  WriteSZOrID (Data, Cls);

  WriteSZOrID (Data, text);

  w := dataLen;
  Data.Write (w, SizeOf(w));

  if w > 0 then
    Data.Write(dat, w)
end;

procedure TDialogResourceDetails.InitAddControlEx(Cls: TSzOrID; id: DWORD;
  const text: TSzOrID; X, Y, CX, CY: Integer; Style, ExStyle, HelpId,
  dataLen: DWORD; const dat);
var
  TemplateEx: TDlgItemTemplateEx;
  w: Word;
begin
  Pad (Data);

  TemplateEx.style := Style;
  TemplateEx.exStyle := ExStyle;
  TemplateEx.X := X;
  TemplateEx.Y := Y;
  TemplateEx.CX := CX;
  TemplateEx.CY := CY;
  TemplateEx.id := id;
  TemplateEx.HelpId := 0;

  Data.Write(TemplateEx, SizeOf(TemplateEx));

  Pad(Data);

  WriteSZOrID(Data, Cls);

  WriteSZOrID(Data, text);

  w := dataLen;
  Data.Write(w, SizeOf(w));

  if w > 0 then
    Data.Write(dat, w)
end;

procedure TDialogResourceDetails.InitNew;
var
  id: TszOrID;
begin
  // Defaults from VC6

  id.isID := False;
  id.sz := '';

  BeginInit(0, 0, 186, 95, DS_MODALFRAME or WS_POPUP or WS_CAPTION or
    WS_SYSMENU or DS_SETFONT or WS_VISIBLE, 0,  id, id, id, 8, 'MS Shell Dlg');
  EndInit(0);
end;

initialization
  RegisterResourceDetails (TDialogResourceDetails);
finalization
  UnregisterResourceDetails (TDialogResourceDetails);
end.
