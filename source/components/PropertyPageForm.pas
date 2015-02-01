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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TPropertyPageData = class
  private
    fCaption: string;
    fHelpText: string;
    fHelpKeyword : string;
    fParam: Integer;
    fMinX : Integer;
    fMinY : Integer;
    fInitialized : boolean;
  protected
    property Param : Integer read fParam;
    procedure Initialize; virtual;
    function GetCaption: string; virtual;
    function GetHelpText: string; virtual;
  public
    constructor Create (const ACaption, AHelpText, AHelpKeyword : string; AMinCX, AMinCY : Integer; AParam : Integer = 0);
    function Apply : boolean; virtual;
    procedure Cancel; virtual;
    procedure Error; virtual;

    property Caption : string read GetCaption;
    property Initialized : boolean read fInitialized write fInitialized;
    property HelpText : string read GetHelpText;
    property HelpKeyword : string read fHelpKeyword;

    property MinX : Integer read fMinX;
    property MinY : Integer read fMinY;
  end;

  TPropertyPageDataClass = class of TPropertyPageData;

  TfmPropertyPage = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    stSectionDetails: TLabel;
  private
    fAltKeyword : string;
  protected
    fPopulating : boolean;
  public
    class function GetDataClass : TPropertyPageDataClass; virtual; abstract;
    procedure PopulateControls (AData : TPropertyPageData); virtual;
    property Populating : boolean read fPopulating write fPopulating;
    property AltKeyword : string read fAltKeyword;
  end;

  TPropertyPageClass = class of TfmPropertyPage;

var
  fmPropertyPage: TfmPropertyPage;

implementation

{$R *.dfm}

{ TfmPropertyPage }

procedure TfmPropertyPage.PopulateControls (AData : TPropertyPageData);
begin
  if not AData.fInitialized then
  begin
    AData.Initialize;
    AData.fInitialized := True
  end;
  stSectionDetails.Caption := AData.HelpText;
  fAltKeyword := AData.HelpKeyword;
end;

{ TPropertyPageData }

function TPropertyPageData.Apply : boolean;
begin
  result := True;  // Stub - return true to indicate success
end;

procedure TPropertyPageData.Cancel;
begin
// Stub
end;

constructor TPropertyPageData.Create(const ACaption, AHelpText, AHelpKeyword: string; AMinCX, AMinCY : Integer;
  AParam: Integer);
begin
  fCaption := ACaption;
  fHelpText := AHelpText;
  fHelpKeyword := AHelpKeyword;
  fParam := AParam;
  fMiNX := AMinCX;
  fMinY := AMinCY;
end;

procedure TPropertyPageData.Error;
begin
  // Stub
end;

function TPropertyPageData.GetCaption: string;
begin
  result := fCaption
end;

function TPropertyPageData.GetHelpText: string;
begin
  result := fHelpText
end;

procedure TPropertyPageData.Initialize;
begin
// Stub
end;

end.
