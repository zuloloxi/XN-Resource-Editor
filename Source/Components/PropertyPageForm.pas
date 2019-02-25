{*======================================================================*
 | PropertyPageForm                                                     |
 |                                                                      |
 | Base class for property forms that live on a PropertyBaseForm        |
 |                                                                      |
 | When deriving a property page:                                       |
 |                                                                      |
 |   1.  Create a data class, derived from TPropertyPageData.  This     |
 |       holds the data for the form, until it is applied.              |
 |                                                                      |
 |   2.  Override the data classes Initialize method to initialize the  |
 |       data.  Initialize it from your options directly, or from the   |
 |       data in 'Param' - a general purpose pointer optionally passed  |
 |       in TPropertyBaseForm.AddPropertyPageDetails.                   |
 |                                                                      |
 |   3.  Override the data classes Apply method to apply the data to    |
 |       your options.  Return false if the data can't be applied.      |
 |                                                                      |
 |   4.  Override the pages GetDataClass method to return your data     |
 |       class type.                                                    |
 |                                                                      |
 |   5.  Override the pages PopulateControls method.  You *must* first  |
 |       call inherited PopulateControls.  Then save the data ('as' the |
 |       required type) in a local variable, then populate the page's   |
 |       controls.                                                      |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2005,2006  All Rights Reserved              |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      19/01/2005  CPWW  Original                                  |
 | 1.1      01/02/2006  CPWW  Generic version                           |
 *======================================================================*}
unit PropertyPageForm;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TPropertyPageData = class
  private
    FCaption: string;
    FHelpText: string;
    FHelpKeyWord: string;
    FParam: Integer;
    FMinX: Integer;
    FMinY: Integer;
    FInitialized: Boolean;
  protected
    property Param: Integer read FParam;
    procedure Initialize; virtual;
    function GetCaption: string; virtual;
    function GetHelpText: string; virtual;
  public
    constructor Create (const ACaption, AHelpText, AHelpKeyWord: string;
      AMinCX, AMinCY: Integer; AParam: Integer = 0);
    function Apply: Boolean; virtual;
    procedure Cancel; virtual;
    procedure Error; virtual;

    property Caption: string read GetCaption;
    property Initialized: Boolean read FInitialized write FInitialized;
    property HelpText: string read GetHelpText;
    property HelpKeyWord: string read FHelpKeyWord;

    property MinX: Integer read FMinX;
    property MinY: Integer read FMinY;
  end;

  TPropertyPageDataClass = class of TPropertyPageData;

  TFormPropertyPage = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    stSectionDetails: TLabel;
  private
    FAltKeyWord: string;
  protected
    FPopulating: Boolean;
  public
    class function GetDataClass: TPropertyPageDataClass; virtual; abstract;
    procedure PopulateControls (AData: TPropertyPageData); virtual;
    property Populating: Boolean read FPopulating write FPopulating;
    property AltKeyWord: string read FAltKeyWord;
  end;

  TPropertyPageClass = class of TFormPropertyPage;

var
  fmPropertyPage: TFormPropertyPage;

implementation

{$R *.dfm}

{ TFormPropertyPage }

procedure TFormPropertyPage.PopulateControls (AData: TPropertyPageData);
begin
  if not AData.FInitialized then
  begin
    AData.Initialize;
    AData.FInitialized := True
  end;
  stSectionDetails.Caption := AData.HelpText;
  FAltKeyWord := AData.HelpKeyWord;
end;

{ TPropertyPageData }

function TPropertyPageData.Apply: Boolean;
begin
  Result := True;  // Stub - return true to indicate success
end;

procedure TPropertyPageData.Cancel;
begin
// Stub
end;

constructor TPropertyPageData.Create(const ACaption, AHelpText,
  AHelpKeyWord: string; AMinCX, AMinCY: Integer; AParam: Integer);
begin
  FCaption := ACaption;
  FHelpText := AHelpText;
  FHelpKeyWord := AHelpKeyWord;
  FParam := AParam;
  FMinX := AMinCX;
  FMinY := AMinCY;
end;

procedure TPropertyPageData.Error;
begin
  // Stub
end;

function TPropertyPageData.GetCaption: string;
begin
  Result := FCaption
end;

function TPropertyPageData.GetHelpText: string;
begin
  Result := FHelpText
end;

procedure TPropertyPageData.Initialize;
begin
// Stub
end;

end.
