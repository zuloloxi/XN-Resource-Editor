(*======================================================================*
 | unitResourceDialogs                                                  |
 |                                                                      |
 | Encapsulates Dialog resources in resources                           |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001,2008                                 |
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

uses Windows, Classes, SysUtils, unitResourceDetails, DialogConsts;

type
TDialogResourceDetails = class (TResourceDetails)
public
  class function GetBaseType : UnicodeString; override;
  procedure InitNew; override;

  procedure BeginInit (x, y, cx, cy : Integer; Style, ExStyle : DWORD; menu, cls, title : TSzOrID; fontSize : DWORD; const faceName : UnicodeString);
  procedure BeginInitEx (x, y, cx, cy : Integer; Style, ExStyle, helpID : DWORD; menu, cls, title : TSzOrID; fontSize, fontWeight, fontItalic, fontCharset : DWORD; const faceName : UnicodeString);
  procedure InitAddControl (cls : TSzOrID; id : DWORD; const text : TSzOrID; x, y, cx, cy : Integer; Style, ExStyle : DWORD; dataLen : DWORD; const dat);
  procedure InitAddControlEx (cls : TSzOrID; id : DWORD; const text : TSzOrID; x, y, cx, cy : Integer; Style, ExStyle, helpId : DWORD; dataLen : DWORD; const dat);
  procedure EndInit (ctrlCount : DWORD);
  procedure EndInitEx (ctrlCount : DWORD);
end;

implementation

{ TDialogResourceDetails }

procedure TDialogResourceDetails.BeginInit(x, y, cx, cy : Integer; Style,
  ExStyle: DWORD; menu, cls, title : TSzOrID; fontSize : DWORD; const faceName : UnicodeString);
var
  template : TDlgTemplate;
  w : word;
begin
  Data.Clear;
  if faceName <> '' then
    Style := Style or DS_SETFONT;
  template.style := Style;
  template.dwExtendedStyle := exstyle;
  template.x := x;
  template.y := y;
  template.cx := cx;
  template.cy := cy;
  template.cdit :=0;

  data.Write (template, SizeOf (template));

  WriteSzOrID (data, menu);
  WriteSzOrID (data, cls);
  WriteSzOrID (data, title);

  w := fontSize;
  data.Write(w, sizeof (w));

  data.Write (PWideChar (faceName)^, (Length (faceName) + 1) * SizeOf (WideChar))
end;

procedure TDialogResourceDetails.BeginInitEx(x, y, cx, cy: Integer; Style,
  ExStyle, helpId: DWORD; menu, cls, title: TSzOrID; fontSize, fontWeight, fontItalic, fontCharset: DWORD;
  const faceName: UnicodeString);
var
  template : TDlgTemplateEx;
  w : word;
  b : byte;
begin
  Data.Clear;
  if faceName <> '' then
    Style := Style or DS_SETFONT;
  template.dlgVer := 1;
  template.style := Style;
  template.exStyle := exstyle;
  template.x := x;
  template.y := y;
  template.cx := cx;
  template.cy := cy;
  template.cDlgItems :=0;
  template.signature := $ffff;
  template.helpID := helpId;

  data.Write (template, SizeOf (template));

  WriteSzOrID (data, menu);
  WriteSzOrID (data, cls);
  WriteSzOrID (data, title);

  w := fontSize;
  data.Write(w, sizeof (w));

  w := fontWeight;
  data.Write(w, sizeof (w));

  b := fontItalic;
  data.Write(b, sizeof (b));

  b := fontCharset;
  data.Write(b, sizeof (b));

  data.Write (PWideChar (faceName)^, (Length (faceName) + 1) * SizeOf (WideChar))
end;

procedure TDialogResourceDetails.EndInit (ctrlCount : DWORD);
var
  p : PDlgTemplate;
begin
  p := PDlgTemplate (Data.Memory);
  p^.cdit := CtrlCount
end;

procedure TDialogResourceDetails.EndInitEx(ctrlCount: DWORD);
var
  p : PDlgTemplateEx;
begin
  p := PDlgTemplateEx (Data.Memory);
  p^.cDlgItems := CtrlCount
end;

class function TDialogResourceDetails.GetBaseType: UnicodeString;
begin
  result := IntToStr (Integer (RT_DIALOG));
end;

procedure TDialogResourceDetails.InitAddControl(cls: TSzOrID; id: DWORD;
  const text: TSzOrID; x, y, cx, cy: Integer; Style, ExStyle: DWORD; dataLen : DWORD; const dat);
var
  templ : TDlgItemTemplate;
  w : word;
begin
  Pad (data);

  templ.style := Style;
  templ.dwExtendedStyle := ExStyle;
  templ.x := x;
  templ.y := y;
  templ.cx := cx;
  templ.cy := cy;
  templ.id := id;

  data.Write(templ, sizeof (templ));

//  Pad (Data);

  WriteSZOrID (Data, cls);

  WriteSZOrID (Data, text);

  w := dataLen;
  Data.Write (w, sizeof (w));

  if w > 0 then
    Data.Write(dat, w)
end;

procedure TDialogResourceDetails.InitAddControlEx(cls: TSzOrID; id: DWORD;
  const text: TSzOrID; x, y, cx, cy: Integer; Style, ExStyle, helpId,
  dataLen: DWORD; const dat);
var
  templ : TDlgItemTemplateEx;
  w : word;
begin
  Pad (data);

  templ.style := Style;
  templ.exStyle := ExStyle;
  templ.x := x;
  templ.y := y;
  templ.cx := cx;
  templ.cy := cy;
  templ.id := id;
  templ.helpID := 0;

  data.Write(templ, sizeof (templ));

  Pad (Data);

  WriteSZOrID (Data, cls);

  WriteSZOrID (Data, text);

  w := dataLen;
  Data.Write (w, sizeof (w));

  if w > 0 then
    Data.Write(dat, w)
end;

procedure TDialogResourceDetails.InitNew;
var
  id : TszOrID;
begin
  // Defaults from VC6

  id.isID := False;
  id.sz := '';

  BeginInit (0, 0, 186, 95, DS_MODALFRAME or WS_POPUP or WS_CAPTION or WS_SYSMENU or DS_SETFONT or WS_VISIBLE, 0,  id, id, id, 8, 'MS Shell Dlg');
  EndInit (0);
end;

initialization
  RegisterResourceDetails (TDialogResourceDetails);
finalization
  UnregisterResourceDetails (TDialogResourceDetails);
end.
