(*======================================================================*
 | unitResourceToolbar                                                  |
 |                                                                      |
 | Encapsulates Toolbar resources in resources                          |
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
unit unitResourceToolbar;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails, Menus;

const
  RT_TOOLBAR = MakeIntResource (241);

type
TToolbarResourceDetails = class (TResourceDetails)
  private
//    fHelpID : Integer;                    // Extended menu's help ID
  protected
    constructor Create (AParent : TResourceModule; ALanguage : Integer; const AName, AType : UnicodeString; ASize : Integer; AData : pointer); override;

  public
    destructor Destroy; override;

    class function GetBaseType : UnicodeString; override;
    procedure ChangeData (newData : TMemoryStream); override;

    procedure InitNew; override;
end;

implementation

type

TToolbarData = packed record  // From a CodeGuru message quoting MFC source...
  wVersion : word;
  wBtnWidth : word;
  wBtnHeight : word;
  wBtnCount : word;
  wButtonIDs : array [0..0] of word;
end;

{ TToolbarResourceDetails }

procedure TToolbarResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;
end;

constructor TToolbarResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer;
  AData: pointer);
begin
  inherited Create (AParent, ALanguage, AName, AType, ASize, AData);
end;

destructor TToolbarResourceDetails.Destroy;
begin
  inherited;
end;

class function TToolbarResourceDetails.GetBaseType: UnicodeString;
begin
  result := IntToStr (Integer (RT_TOOLBAR));
end;

procedure TToolbarResourceDetails.InitNew;
var
  dat : TToolbarData;
begin
  dat.wVersion := 1;
  dat.wBtnWidth := 16;
  dat.wBtnHeight := 15;
  dat.wBtnCount := 0;

  data.Write(dat, sizeof (dat) - sizeof (dat.wButtonIDs))
end;

initialization
  RegisterResourceDetails (TToolbarResourceDetails);
finalization
  UnregisterResourceDetails (TToolbarResourceDetails);
end.


