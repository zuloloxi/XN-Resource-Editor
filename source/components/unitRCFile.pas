{*======================================================================*
 | unitRCFile                                                           |
 |                                                                      |
 | .RC file module handler                                              |
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
 | Copyright © Colin Wilson 2004  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      24/11/2004  CPWW  Original                                  |
 *======================================================================*}

unit unitRCFile;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, Vcl.Menus, Vcl.Graphics,
  System.Contnrs, unitResFile, unitResourceDetails, DialogConsts;

type
  //---------------------------------------------------------------------------
  // RC Module
  TRCModule = class (TResourceList)
  private
    FFileName: string;
    FIncludePath: string;
  public
    constructor Create;

    procedure SaveToStream (stream: TStream); override;
    procedure LoadFromFile (const FileName: string); override;
    procedure LoadFromStream (stream: TStream); override;

    property IncludePath: string read FIncludePath write FIncludePath;
  end;

implementation

uses
  unitStreamTextReader, unitSearchString, unitParser, unitResourceGraphics,
  unitCExpression, unitResourceToolbar, unitResourceDialogs,
  unitResourceMessages, unitResourceAccelerator, unitResourceVersionInfo,
  unitResourceMenus;

const
  BS_PUSHBOX = 10;      // Missing from windows.pas.  Obsolete?

//---------------------------------------------------------------
// KeyWord IDs

  kwLanguage        = 1;
  kwIcon            = 2;
  kwDialog          = 3;
  kwMenu            = 4;
  kwBitmap          = 5;
  kwCursor          = 6;
  kwToolbar         = 7;
  kwCaption         = 9;
  kwCharacteristics = 10;
  kwClass           = 11;
  kwExStyle         = 12;
  kwFont            = 13;
  kwStyle           = 14;
  kwVersion         = 15;
  kwDialogEx        = 16;
  kwMessageTable    = 17;
  kwAccelerators    = 18;
  kwStringTable     = 19;
  kwVersionInfo     = 20;

  kwBegin           = 50;
  kwEnd             = 51;

  kwDiscardable     = 100;
  kwPure            = 101;
  kwMoveable        = 102;
  kwPreload         = 103;
  kwLoadOnCall      = 104;

  kwVirtkey         = 110;
  kwAscii           = 111;
  kwNoInvert        = 112;
  kwAlt             = 113;
  kwShift           = 114;

  kwPopup           = 120;
  kwMenuItem        = 121;

  kwButton          = 150;
  kwSeparator       = 151;
  kwLText           = 152;
  kwDefPushButton   = 153;
  kwControl         = 154;
  kwEditText        = 155;
  kwPushButton      = 156;
  kwAuto3State      = 157;
  kwAutoCheckBox    = 158;
  kwAutoRadioButton = 159;
  kwCheckBox        = 160;
  kwState3          = 161;
  kwRadioButton     = 162;
  kwCText           = 163;
  kwRText           = 164;
  kwGroupBox        = 165;
  kwPushBox         = 166;
  kwListBox         = 167;
  kwComboBox        = 168;

type
  kwKeyWordRange    = 0..255;
  TSupportedKeyWords = set of kwKeyWordRange;

const
//---------------------------------------------------------------------------
// KeyWords supported for each resource type
  DialogOptionKeyWords   : TSupportedKeyWords = [kwCaption, kwCharacteristics, kwClass, kwExStyle, kwFont, kwLanguage, kwMenu, kwStyle, kwVersion];
  AcceleratorsOptionKeyWords: TSupportedKeyWords = [kwCharacteristics, kwLanguage, kwVersion];
  MenuOptionKeyWords     : TSupportedKeyWords = [kwCharacteristics, kwLanguage, kwVersion];
  RCDataOptionKeyWords   : TSupportedKeyWords = [kwCharacteristics, kwLanguage, kwVersion];
  StringtableOptionKeyWords: TSupportedKeyWords = [kwCharacteristics, kwLanguage, kwVersion];

type
  TKeyWordProc = procedure of object;

//----------------------------------------------------------------------------
  TKeyWordDetails = class       // Per keyWord details - used to populate FKeyWords object list
    kw: Integer;               // The KeyWord ID
    proc: TKeyWordProc;        // Handler method
    constructor Create (AKeyWord: Integer; AProc: TKeyWordProc);
  end;

  TRCParser = class;

  TResourceOptions = class      // Options - populated by CreateResourceOptions
  private
    FParser: TRCParser;
    FSupportedKeyWords: TSupportedKeyWords;
    FCharacteristics: DWORD;
    FCaption: string;
    FClass: TSzOrID;
    FFontSize: DWORD;
    FExStyle: DWORD;
    FStyle: DWORD;
    FVersion: DWORD;
    FLanguage: Integer;
    FFontFace: string;
    FMenuId: TSzOrID;
    FFontWeight: Integer;
    FFontItalic: Integer;
    FFontCharset: Integer;
  public
    constructor Create (Parser: TRCParser; SupportedKeyWords: TSupportedKeyWords);
    property Caption: string read FCaption;
    property Characteristics: DWORD read FCharacteristics;
    property _Class: TSzOrID read FClass;
    property ExStyle: DWORD read FExStyle;
    property FontSize: DWORD read FFontSize;
    property FontFace: string read FFontFace;
    property Language: Integer read FLanguage;
    property MenuId: TSzOrID read FMenuId;
    property Style: DWORD read FStyle;
    property Version: DWORD read FVersion;
  end;

  TControlParamsOption = (cpHasText, cpNeedsCXCY, cpNeedsStyle);
  TControlParamsOptions = set of TControlParamsOption;

//------------------------------------------------------------------------
// Main parser class.  Inherits from TCPreProcessor - which handles all
// '#' directives
  TRCParser = class (TCPreProcessor)
  private
    FName: TValue;
    FEOF: Boolean;

    FLangId: Integer;

    FParent: TResourceModule;
    FKeyWords: TStringList;

    procedure DoLanguage;
    procedure DoIcon;
    procedure DoCursor;
    procedure DoDialog;
    procedure DoDialogEx;
    procedure DoMenu;
    procedure DoBitmap;
    procedure DoToolbar;
    procedure DoMessageTable;
    procedure DoAccelerators;
    procedure DoVersionInfo;
    procedure DoStringTable;
    procedure SkipYeOldeMemoryAttributes;
    function KeyWord (const st: string): TKeyWordDetails;
    function KeyID: Integer;    // Returns keyWord ID of current token - or -1
    function CreateResourceOptions (SupportedKeyWords: TSupportedKeyWords; var validKeyWords: TSupportedKeyWords): TResourceOptions;
    function NextExpression (var v: TValue): Boolean;
    function NextIntegerExpression: Integer;

    function GetControlParams (ex: Boolean; var text: TSzOrID; var id: DWORD; var x, y, cx, cy: Integer; var style, exStyle, helpId: DWORD; options: TControlParamsOptions): Boolean;
  public
    constructor Create (stream: TStream; AParent: TResourceModule);
    destructor Destroy; override;
    procedure Parse; override;
  end;

type
  TkwRec = record
    kw: string;
    id: Integer;
    pr: pointer;
    resID: PChar;
  end;

const
  NoKeyWords = 52;

var
  KeyWordTable: array [0..NoKeyWords - 1] of TkwRec = (

  // Resource types

    (kw: 'LANGUAGE';        id: kwLanguage;        pr: @TRCParser.DoLanguage),
    (kw: 'ICON';            id: kwIcon;            pr: @TRCParser.DoIcon;    resId: RT_GROUP_ICON) ,
    (kw: 'DIALOG';          id: kwDialog;          pr: @TRCParser.DoDialog;  resId: RT_DIALOG),
    (kw: 'MENU';            id: kwMenu;            pr: @TRCParser.DoMenu;    resId: RT_MENU),
    (kw: 'BITMAP';          id: kwBitmap;          pr: @TRCParser.DoBitmap;  resId: RT_BITMAP),
    (kw: 'CURSOR';          id: kwCursor;          pr: @TRCParser.DoCursor;  resId: RT_GROUP_CURSOR),
    (kw: 'TOOLBAR';         id: kwToolbar;         pr: @TRCParser.DoToolbar; resId: RT_TOOLBAR),
    (kw: 'DIALOGEX';        id: kwDialogEx;        pr: @TRCParser.DoDialogEx),
    (kw: 'MESSAGETABLE';    id: kwMessageTable;    pr: @TRCParser.DoMessageTable; resId: RT_MESSAGETABLE),
    (kw: 'ACCELERATORS';    id: kwAccelerators;    pr: @TRCParser.DoAccelerators; resId: RT_ACCELERATOR),
    (kw: 'STRINGTABLE';     id: kwStringTable;     pr: @TRCParser.DoStringTable;  resId: RT_STRING),
    (kw: 'VERSIONINFO';     id: kwVersionInfo;     pr: @TRCParser.DoVersionInfo;  resId: RT_VERSION),


  // Resource options
    (kw: 'CAPTION';         id: kwCaption;         pr: nil),
    (kw: 'CHARACTERISTICS'; id: kwCharacteristics; pr: nil),
    (kw: 'CLASS';           id: kwClass;           pr: nil),
    (kw: 'EXSTYLE';         id: kwExStyle;         pr: nil),
    (kw: 'FONT';            id: kwFont;            pr: nil),
    (kw: 'STYLE';           id: kwStyle;           pr: nil),
    (kw: 'VERSION';         id: kwVersion;         pr: nil),

    // Obsolete resource memory modifiers
    (kw: 'DISCARDABLE';     id: kwDiscardable;     pr: nil),
    (kw: 'LOADONCALL';      id: kwLoadOnCall;      pr: nil),
    (kw: 'MOVEABLE';        id: kwMoveable;        pr: nil),
    (kw: 'PRELOAD';         id: kwPreload;         pr: nil),
    (kw: 'PURE';            id: kwPure;            pr: nil),

    // Accelerator keyWords
    (kw: 'VIRTKEY';         id: kwVirtkey;         pr: nil),
    (kw: 'ASCII';           id: kwAscii;           pr: nil),
    (kw: 'NOINVERT';        id: kwNoInvert;        pr: nil),
    (kw: 'ALT';             id: kwAlt;             pr: nil),
    (kw: 'SHIFT';           id: kwShift;           pr: nil),
//  nb.  The CONTROL keyWord is used for accelerators and also
//       custom controls.  It is defined below

    (kw: 'POPUP';           id: kwPopup;           pr: nil),
    (kw: 'MENUITEM';        id: kwMenuItem;        pr: nil),

    (kw: 'BEGIN';           id: kwBegin;           pr: nil),
    (kw: 'END';             id: kwEnd;             pr: nil),

// Control identifiers
    (kw: 'BUTTON';          id: kwButton;          pr: nil),
    (kw: 'SEPARATOR';       id: kwSeparator;       pr: nil),
    (kw: 'LTEXT';           id: kwLText;           pr: nil),
    (kw: 'DEFPUSHBUTTON';   id: kwDefPushButton;   pr: nil),
    (kw: 'CONTROL';         id: kwControl;         pr: nil),
    (kw: 'EDITTEXT';        id: kwEditText;        pr: nil),
    (kw: 'PUSHBUTTON';      id: kwPushButton;      pr: nil),

    (kw: 'AUTO3STATE';      id: kwAuto3State;      pr: nil),
    (kw: 'AUTOCHECKBOX';    id: kwAutoCheckBox;    pr: nil),
    (kw: 'AUTORADIOBUTTON'; id: kwAutoRadioButton; pr: nil),
    (kw: 'CHECKBOX';        id: kwCheckBox;        pr: nil),
    (kw: 'STATE3';          id: kwState3;          pr: nil),
    (kw: 'RADIOBUTTON';     id: kwRadioButton;     pr: nil),
    (kw: 'CTEXT';           id: kwCText;           pr: nil),
    (kw: 'RTEXT';           id: kwRText;           pr: nil),
    (kw: 'GROUPBOX';        id: kwGroupBox;        pr: nil),
    (kw: 'PUSHBOX';         id: kwPushBox;         pr: nil),
    (kw: 'LISTBOX';         id: kwListBox;         pr: nil),
    (kw: 'COMBOBOX';        id: kwComboBox;        pr: nil)
  );

{*----------------------------------------------------------------------*
 | function MakeLangId                                                  |
 |                                                                      |
 | Returns a LANGID from primary and sub language constituents          |
 *----------------------------------------------------------------------*}
function MakeLangId (pri, sec: Word): Integer;
begin
  if (pri > 0) and (sec = 0) then
    sec := 1;
  Result := (pri and $3ff) or (sec shl 10)
end;

{*----------------------------------------------------------------------*
 | function ValToStr                                                    |
 |                                                                      |
 | Returns a string representation of a value returned by the C         |
 | pre-processor's expressions evaluator.                               |
 *----------------------------------------------------------------------*}
function ValToStr (val: TValue): string;
begin
  if val.tp = vInteger then
    Result := IntToStr (val.iVal)
  else
    if val.tp = vString then
      Result := val.sVal
    else
      raise EParser.Create('Integer or string expected');
end;

{*----------------------------------------------------------------------*
 | function ValToSzOrID                                                 |
 |                                                                      |
 | Returns a resource SzOrID identifier from a value returned by the    |
 | C evaluator                                                          |
 *----------------------------------------------------------------------*}
function ValToSzOrID (val: TValue): TSzOrId;
begin
  if val.tp = vInteger then
  begin
    Result.isID := True;
    Result.id := val.iVal
  end
  else
    if val.tp = vString then
    begin
      Result.isID := False;
      Result.sz := val.sVal
    end
    else
      raise EParser.Create('Type mismatch');
end;

{ TRCModule }

{*----------------------------------------------------------------------*
 | procedure TRCModule.LoadFromFile                                     |
 |                                                                      |
 | Load and parse an RC file.  Overridden from the base class to save   |
 | the passed in path for use in relative #includes etc. (see           |
 | LoadFromStream)                                                      |
 *----------------------------------------------------------------------*}
constructor TRCModule.Create;
begin
  inherited;
  FIncludePath := GetEnvironmentVariable ('include');
end;

procedure TRCModule.LoadFromFile(const FileName: string);
begin
  FFileName := FileName;
  inherited;
end;

{*----------------------------------------------------------------------*
 | procedure TRCModule.LoadFromStream                                   |
 |                                                                      |
 | Load and parse the data from a stream.                               |
 |                                                                      |
 | Parameters:                                                          |
 |   stream: TStream                    The stream to parse             |
 *----------------------------------------------------------------------*}
procedure TRCModule.LoadFromStream(stream: TStream);
var
  parser: TRCParser;
begin
  parser := TRCParser.Create(stream, self);
  parser.PathName := ExtractFilePath (FFileName);

  // Add identifiers so that MSVC resource script load correctly
  parser.AddIdentifier('RC_INVOKED', '');
  parser.AddIdentifier('_WIN32', '');

  parser.IncludePath := IncludePath;
  parser.Parse;
  SortResources;
  ClearDirty
end;

{*----------------------------------------------------------------------*
 | TRCModule.SaveToStream                                               |
 |                                                                      |
 | Save RC data.  Not yet implemented                                   |
 *----------------------------------------------------------------------*}
procedure TRCModule.SaveToStream(stream: TStream);
begin
  inherited;            // Inherited function throws an exception
end;

{ TRCParser }
{*----------------------------------------------------------------------*
 | constructor TRCParser.Create                                         |
 |                                                                      |
 | Create a TRCParser                                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   stream: TStream                    The stream to parse             |
 |   AParent: TResourceModule           Parent module.                  |
 *----------------------------------------------------------------------*}
constructor TRCParser.Create(stream: TStream; AParent: TResourceModule);
type
  t = record case Boolean of
    true: (p: TKeyWordProc);
    false: (pt, slf: Pointer);
  end;
var
  i: Integer;
  tt: t;

begin
  FParent := AParent;
  FLangId := SysLocale.DefaultLCID;  // Check - maybe should be US LCID
  inherited Create (stream);

  FKeyWords := TStringList.Create;
  FKeyWords.CaseSensitive := false;
                                        // Populate the FKeyWords object list
                                        // from the KeyWordTable
  for i := 0 to NoKeyWords - 1 do
    with KeyWordTable [i] do
    begin
      tt.pt := pr;
      tt.slf := self;                   // Fixup the 'self' pointer in the
                                        // methood call
      FKeyWords.AddObject(kw, TKeyWordDetails.Create(id, tt.p));
    end;

  FKeyWords.Sorted := True;
end;

{*----------------------------------------------------------------------*
 | function TRCParser.CreateResourceOptions                             |
 |                                                                      |
 | Create a TResourceOptions class, and populate it from the resource   |
 | options for the particular resource type.  Return the options        |
 | keyWords actually found, so that the rest of the supported keyWords  |
 | can be filled with default values.                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   SupportedKeyWords: TSupportedKeyWords      KeyWords supported by   |
 |                                              the resource type       |
 |                                                                      |
 |   var validKeyWords: TSupportedKeyWords     KeyWords actually found |
 |                                              in the RC stream        |
 |                                                                      |
 | The function creates and returns the TResourceOptions class.         |
 *----------------------------------------------------------------------*}
function TRCParser.CreateResourceOptions(
  SupportedKeyWords: TSupportedKeyWords; var validKeyWords: TSupportedKeyWords): TResourceOptions;
var
  kw: Integer;
  v: TValue;
  pri, sec: Word;
  hasToken: Boolean;
begin
  validKeyWords := [];
  if KeyID in SupportedKeyWords then
  begin
    Result := TResourceOptions.Create(self, SupportedKeyWords);
    Result.FLanguage := FLangId;
    validKeyWords := [kwLanguage];

    repeat
      kw := KeyID;
                        // Finish when we find a keyWord that's not
                        // in the supported list.  If everything is going OK
                        // this will be the first token that makes up the
                        // actual resource data.

      if not(kw in SupportedKeyWords) then break;

      hasToken := False;
      case kw of
        kwCaption: begin       // Parse a CAPTION statement followed by
                                // "captiontext" in double quotes.
                      Result.FCaption := NextString;
                      Include (validKeyWords, kwCaption)
                    end;

        kwCharacteristics:     // Parse a CHARACTERISTICS statement followed
                                // by a DWORD value.  Which is irrelevant
                    begin
                      Result.FCharacteristics := NextInteger;
                      Include (validKeyWords, kwCharacteristics)
                    end;

        kwStyle: begin       // Parse a STYLE statement followed by a DWORD
                                // expression containing a window style

                      NextExpression (v);
                      hasToken := True;
                      if v.tp = vInteger then
                        Result.FStyle := v.iVal
                      else
                        raise Exception.Create ('Integer expected in STYLE');
                      Include (validKeyWords, kwStyle);
                    end;

        kwExStyle: begin       // Parse an EXSTYLE statement followed by a DWORD
                                // expressions containing a window Ex Style

                      NextExpression (v);
                      hasToken := True;
                      if v.tp = vInteger then
                        Result.FExStyle := v.iVal
                      else
                        raise Exception.Create ('Integer expected in EXSTYLE');
                      Include (validKeyWords, kwExStyle);
                    end;

        kwFont: begin       // Parse a FONT statement followed by the
                                // fontsize DWORD and string font name.  If this
                                // occurs in an EXDIALOG, this is followed by the
                                // Weight, Italic and CharSet values
                      Result.FFontSize := NextInteger;
                      NextChar (',');
                      Result.FFontFace := NextString;
                      GetToken;
                      hasToken := True;
                      if (TokenType = ttChar) and (TokenChar = ',') then
                      begin
                        Result.FFontWeight := NextIntegerExpression;
                        ExpectChar (',');
                        Result.FFontItalic := NextIntegerExpression;
                        ExpectChar (',');
                        Result.FFontCharset := NextIntegerExpression;
                      end;
                      Include (validKeyWords, kwFont);
                    end;

        kwLanguage :begin       // Parse a LANGUAGE statement followed by the
                                // primary and sub language components
                      pri := NextInteger;
                      NextChar (',');
                      sec := NextInteger;
                      Result.FLanguage := MakeLangID (pri, sec);
                    end;

        kwMenu : begin      // Parse a MENU stetement.  Not to be confused
                                // with a menu resource type identifier - this
                                // option may appear in DIALOG and DIALOGEX resources.
                                // The keyWord 'MENU' is followed by the menu SZ or ID

                       GetToken;
                       Result.FMenuId := ValToSzOrID (ResolveToken);
                       Include (validKeyWords, kwMenu);
                     end;

        kwClass: begin      // Parse a CLASS statement followed by an Sz or ID
                                // window (dialog) class identifier.
                       GetToken;
                       Result.FClass := ValToSzOrID (ResolveToken);
                       Include (validKeyWords, kwClass);
                     end;

        kwVersion: begin      // VERSION statement followed by an ignored
                                // version DWORD.  Not to be confused with a
                                // VERSIONINFO resource type
                       Result.FVersion := NextInteger;
                       Include (validKeyWords, kwVersion)
                     end
      end;
      if not hasToken then
        FEOF := not GetToken;
    until FEOF
  end
  else
    Result := nil;
end;

{*----------------------------------------------------------------------*
 | Destructor for TRCParser                                             |
 *----------------------------------------------------------------------*}
destructor TRCParser.Destroy;
var
  i: Integer;
begin
  for i := FKeyWords.Count - 1 downto 0 do
    FKeyWords.Objects [i].Free;
  FKeyWords.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoAccelerators                                   |
 |                                                                      |
 | Handle an ACCELERATORS resource.                                     |
 |                                                                      |
 | ACCELERATORS:: acctablename ACCELERATORS [optional-statements]       |
 |                             { event, idvalue, | [type] [options]...} |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoAccelerators;
var
  ac: TAcceleratorResourceDetails;
  v: TValue;
  vk: Integer;
  flags: Integer;
  l, id: Integer;
  options: TResourceOptions;
  validKeyWords: TSupportedKeyWords;

begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  options := CreateResourceOptions (AcceleratorsOptionKeyWords, validKeyWords);
  try
   if Assigned(options) then
      ac := TAcceleratorResourceDetails.CreateNew(FParent, options.Language, ValToStr (FName))
    else
      ac := TAcceleratorResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

    if KeyID = kwBegin then
    while NextExpression (v) do // First expression contains the virtual key
    begin
      if KeyID = kwEnd then
        break;
      
      vk := 0;
      if vk <> 0 then;          // Get's rid of erroneous compiler warning
      if v.tp = vString then
      begin
        l := Length (v.sVal);
        if (l = 2) and (v.sVal [1] = '^') then
          vk := Ord (v.sVal [2]) and $1f
        else
          if l = 1 then
            vk := Ord (v.sVal [1])
          else
            if vk = 0 then
              raise EParser.Create('Single character expected')
      end
      else
        if v.tp = vInteger then
          vk := v.iVal
        else
          raise EParser.Create('Character or virtual key code expected');

                                // Next identifier contains the 'ID'
      id := NextIntegerExpression;

      flags := 0;

                                // Now get the (optional) flags.  Technically
                                // VIRTKEY or ASCII should be the first flag if
                                // it's specified.  But no real need to enforce
                                // this.
      while not SOL do
      begin
        GetToken;

        case KeyID of
          kwVirtKey: flags := flags or FVIRTKEY;
          kwNoInvert: flags := flags or FNOINVERT;
          kwShift: flags := flags or FSHIFT;
          kwAlt  : flags := flags or FALT;
          kwControl: flags := flags or FCONTROL;
        end;
      end;

      ac.Add(flags, vk, id);
    end;
    if KeyID <> kwEnd then
      raise EParser.Create('End expected');
    GetToken;
  finally
    options.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoBitmap                                         |
 |                                                                      |
 | Handle a BITMAP resource                                             |
 |                                                                      |
 | BITMAP:: nameID BITMAP filename                                      |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoBitmap;
var
  FFileName: string;
  bmp: TBitmapResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  FFileName := ExpectString ('File name expected');

  bmp := TBitmapResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));
  bmp.LoadImage(PathName + FFileName);
  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoCursor                                         |
 |                                                                      |
 | Handle a CURSOR resource                                             |
 |                                                                      |
 | CURSOR:: nameID CURSOR filename                                      |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoCursor;
var
  FFileName: string;
  cur: TIconCursorGroupResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  FFileName := ExpectString ('File name expected');
  cur := TCursorGroupResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));
  cur.LoadImage(PathName + FFileName);
  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoDialog                                         |
 |                                                                      |
 | Handle a DIALOG resource                                             |
 |                                                                      |
 | DIALOG:: namemeID DIALOG x, y, width, height  [optional-statements]  |
 |                             {      control-statement      . . .  }   |
 |                                                                      |
 | control-statement::CONTROLID [,options ...]                          |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoDialog;
var
  x, y, cx, cy, ctlCount: Integer;
  id, style, exstyle, fontSize, dummy: DWORD;
  options: TResourceOptions;
  dlg: TDialogResourceDetails;
  validKeyWords: TSupportedKeyWords;
  p: PDlgTemplate;
  menu, cls, title: TSzOrID;
  faceName: string;
  v: TValue;
  controlOptions: TControlParamsOptions;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  // Get dialog coordinates
  x := ExpectInteger; NextChar (',');
  y := NextInteger;   NextChar (',');
  cx := NextInteger;  NextChar (',');
  cy := NextInteger;  GetToken;

  // Get dialog options - eg. caption.
  options := CreateResourceOptions (DialogOptionKeyWords, validKeyWords);
  try

  // Create the dialog resource
    if Assigned(options) then
      dlg := TDialogResourceDetails.CreateNew(FParent, options.FLanguage, ValToStr (FName))
    else
      dlg := TDialogResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

    // Initialize a blank resource template
    p := PDlgTemplate (dlg.Data.Memory);
    style := p^.style;
    exstyle := p^.dwExtendedStyle;
    ctlCount := 0;
    menu.isID  := False; menu.sz := '';
    cls.isID   := False; cls.sz := '';
    fontSize := 8;
    title.isID := False; title.sz := '';
    faceName := 'MS Shell Dlg';

    // Fill in the resource template options.
    if Assigned(options) then
    begin
      if kwStyle in validKeyWords then style := options.Style;
      if kwExStyle in validKeyWords then exstyle := options.ExStyle;
      if kwCaption in validKeyWords then title := StringToSzOrID (options.FCaption);
      if kwClass   in validKeyWords then cls := options.FClass;
      if kwMenu    in validKeyWords then menu := options.FMenuId;
      if kwFont    in validKeyWords then
      begin
        fontSize := options.FFontSize;
        faceName := options.FFontFace;
      end
    end;

    // Get the dialog controls
    dlg.BeginInit(x, y, cx, cy, style, exstyle, menu, cls, title, fontSize, faceName);
    try
      if KeyID = kwBegin then
      begin
        NextIdentifier;
        repeat
          exStyle := 0;
          if TokenType = ttIdentifier then
          case KeyID of
            kwLText,
            kwCText,
            kwRText,
            kwIcon:
              begin             // Handle Static Control
                cls.isID := True;
                cls.id := STATIC_ID;
                style := WS_CHILD or WS_VISIBLE or WS_GROUP;

                case KeyID of
                  kwLText: style := style or SS_LEFT;
                  kwCText: style := style or SS_CENTER;
                  kwRText: style := style or SS_RIGHT;
                  kwIcon: begin
                              style := style or SS_ICON;
                              controlOptions := [cpHasText];
                              cx := GetSystemMetrics (SM_CXICON);
                              cy := GetSystemMetrics (SM_CYICON)
                            end
                end;

                if KeyID <> kwICon then
                  controlOptions := [cpHasText, cpNeedsCXCY];

                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, controlOptions);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc(CtlCount)
              end;

            kwListBox:         // Handle ListBox control
              begin
                cls.isID := True;
                cls.id := LISTBOX_ID;
                style := WS_CHILD or WS_VISIBLE or WS_BORDER or LBS_NOTIFY;
                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc(CtlCount)
              end;

            kwComboBox:        // Handle ComboBox control
              begin
                cls.isID := True;
                cls.id := COMBOBOX_ID;
                style := WS_CHILD or WS_VISIBLE or CBS_SIMPLE or WS_TABSTOP;
                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc(CtlCount)
              end;

              kwPushButton,     // Handle Button-type control
              kwDefPushButton,
              kwAuto3State,
              kwAutoCheckBox,
              kwAutoRadioButton,
              kwCheckbox,
              kwRadioButton,
              kwState3,
              kwGroupBox,
              kwPushBox:
                begin
                  cls.isID := True;
                  cls.id := BUTTON_ID;
                  style := WS_CHILD or WS_VISIBLE or WS_TABSTOP;

                  case KeyID of
                    kwPushButton  : style := style or BS_PUSHBUTTON;
                    kwDefPushButton: style := style or BS_DEFPUSHBUTTON;
                    kwAuto3State  : style := style or BS_AUTO3STATE;
                    kwAutoCheckBox: style := style or BS_AUTOCHECKBOX;
                    kwAutoRadioButton: style := style or BS_AUTORADIOBUTTON;
                    kwCheckBox    : style := style or BS_CHECKBOX;
                    kwRadioButton : style := style or BS_RADIOBUTTON;
                    kwState3      : style := style or BS_3STATE;
                    kwGroupBox    : style := (style and (not WS_TABSTOP)) or BS_GROUPBOX;
                    kwPushBox     : style := style or BS_PUSHBOX;
                  end;

                  GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpHasText, cpNeedsCXCY]);
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc(CtlCount)
                end;

              kwEditText:              // Handle Edit control
                begin
                  cls.isID := True;
                  cls.id := EDIT_ID;
                  style := WS_CHILD or WS_VISIBLE or ES_LEFT or WS_BORDER or WS_TABSTOP;
                  GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc(CtlCount)
                end;

              kwControl:               // Handle custom control
                begin
                  NextExpression (v);
                  title := ValToSZOrId (v);
                  ExpectChar (',');
                  id := NextIntegerExpression;
                  ExpectChar (',');
                  NextExpression (v);
                  cls := ValToSZOrId (v);
                  ExpectChar (',');
                  style := WS_CHILD or WS_VISIBLE;
                  style := style or DWORD (NextIntegerExpression);
                  ExpectChar (',');
                  x := NextIntegerExpression;
                  ExpectChar (',');
                  y := NextIntegerExpression;
                  ExpectChar (',');
                  cx := NextIntegerExpression;
                  ExpectChar (',');
                  cy := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    exStyle := NextIntegerExpression;
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc(CtlCount)
                end
              else
                GetToken;
          end
          else
            GetToken;
        until KeyID = kwEnd
      end
    finally
      dlg.EndInit(CtlCount)
    end
  finally
    options.Free
  end;

  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoDialog                                         |
 |                                                                      |
 | Handle a DIALOG resource                                             |
 |                                                                      |
 | DIALOG:: namemeID DIALOG x, y, width, height  [optional-statements]  |
 |                             {      control-statement      . . .  }   |
 |                                                                      |
 | control-statement::CONTROLID [,options ...]                          |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoDialogEx;
var
  x, y, cx, cy, ctlCount: Integer;
  id, style, exstyle, fontSize, helpId, fontWeight, fontItalic, fontCharset: DWORD;
  options: TResourceOptions;
  dlg: TDialogResourceDetails;
  validKeyWords: TSupportedKeyWords;
  p: PDlgTemplate;
  menu, cls, title: TSzOrID;
  faceName: string;
  v: TValue;
  controlOptions: TControlParamsOptions;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  x := ExpectInteger; NextChar (',');
  y := NextInteger;   NextChar (',');
  cx := NextInteger;  NextChar (',');
  cy := NextInteger;  GetToken;
  helpId := 0;

  if (TokenType = ttChar) and (TokenChar = ',') then
  begin
    helpId := NextInteger;
    GetToken
  end;

  options := CreateResourceOptions (DialogOptionKeyWords, validKeyWords);
  try
    if Assigned(options) then
    begin
      dlg := TDialogResourceDetails.CreateNew(FParent, options.FLanguage, ValToStr (FName));
      if kwCaption in validKeyWords then
    end
    else
      dlg := TDialogResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

    p := PDlgTemplate (dlg.Data.Memory);
    style := p^.style;
    exstyle := p^.dwExtendedStyle;
    ctlCount := 0;
    menu.isID  := False; menu.sz := '';
    cls.isID   := False; cls.sz := '';
    title.isID := False; title.sz := '';
    fontSize := 8;
    faceName := 'MS Shell Dlg';
    fontWeight := 0;
    fontItalic := 0;
    fontCharset := DEFAULT_CHARSET;

    if Assigned(options) then
    begin
      if kwStyle in validKeyWords then style := options.Style;
      if kwExStyle in validKeyWords then exstyle := options.ExStyle;
      if kwCaption in validKeyWords then title := StringToSzOrID (options.FCaption);
      if kwClass   in validKeyWords then cls := options.FClass;
      if kwMenu    in validKeyWords then menu := options.FMenuId;
      if kwFont    in validKeyWords then
      begin
        fontSize := options.FFontSize;
        faceName := options.FFontFace;
        fontWeight := options.FFontWeight;
        fontItalic := options.FFontItalic;
        fontCharset := options.FFontCharset
      end
    end;

    dlg.BeginInitEx(x, y, cx, cy, style, exstyle, helpId, menu, cls, title, fontSize, fontWeight, fontITalic, fontCharset, faceName);
    try
      if KeyID = kwBegin then
      begin
        NextIdentifier;
        repeat
          exStyle := 0;
          if TokenType = ttIdentifier then
          case KeyID of
            kwLText,
            kwCText,
            kwRText:
              begin
                cls.isID := True;
                cls.id := STATIC_ID;
                style := WS_CHILD or WS_VISIBLE or WS_GROUP;

                case KeyID of
                  kwLText: style := style or SS_LEFT;
                  kwCText: style := style or SS_CENTER;
                  kwRText: style := style or SS_RIGHT;
                  kwIcon: begin
                              style := style or SS_ICON;
                              controlOptions := [cpHasText];
                              cx := GetSystemMetrics (SM_CXICON);
                              cy := GetSystemMetrics (SM_CYICON)
                            end
                end;

                if KeyID <> SS_ICON then
                  controlOptions := [cpHasText, cpNeedsCXCY];

                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, controlOptions);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc(CtlCount)
              end;

            kwListBox :
              begin
                cls.isID := True;
                cls.id := LISTBOX_ID;
                style := WS_CHILD or WS_VISIBLE or WS_BORDER or LBS_NOTIFY;
                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc(CtlCount)
              end;

            kwComboBox :
              begin
                cls.isID := True;
                cls.id := COMBOBOX_ID;
                style := WS_CHILD or WS_VISIBLE or CBS_SIMPLE or WS_TABSTOP;
                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc(CtlCount)
              end;

              kwPushButton,
              kwDefPushButton,
              kwAuto3State,
              kwAutoCheckBox,
              kwAutoRadioButton,
              kwCheckbox,
              kwRadioButton,
              kwState3,
              kwGroupBox,
              kwPushBox:
                begin
                  cls.isID := True;
                  cls.id := BUTTON_ID;
                  style := WS_CHILD or WS_VISIBLE or WS_TABSTOP;

                  case KeyID of
                    kwPushButton  : style := style or BS_PUSHBUTTON;
                    kwDefPushButton: style := style or BS_DEFPUSHBUTTON;
                    kwAuto3State  : style := style or BS_AUTO3STATE;
                    kwAutoCheckBox: style := style or BS_AUTOCHECKBOX;
                    kwAutoRadioButton: style := style or BS_AUTORADIOBUTTON;
                    kwCheckBox    : style := style or BS_CHECKBOX;
                    kwRadioButton : style := style or BS_RADIOBUTTON;
                    kwState3      : style := style or BS_3STATE;
                    kwGroupBox    : style := (style and (not WS_TABSTOP)) or BS_GROUPBOX;
                    kwPushBox     : style := style or BS_PUSHBOX;
                  end;

                  GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpHasText, cpNeedsCXCY]);
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc(CtlCount)
                end;

              kwEditText :
                begin
                  cls.isID := True;
                  cls.id := EDIT_ID;
                  style := WS_CHILD or WS_VISIBLE or ES_LEFT or WS_BORDER or WS_TABSTOP;
                  GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc(CtlCount)
                end;

              kwControl :
                begin
                  NextExpression (v);
                  title := ValToSZOrId (v);
                  ExpectChar (',');
                  id := NextIntegerExpression;
                  ExpectChar (',');
                  NextExpression (v);
                  cls := ValToSZOrId (v);
                  ExpectChar (',');
                  style := WS_CHILD or WS_VISIBLE;
                  style := style or DWORD (NextIntegerExpression);
                  ExpectChar (',');
                  x := NextIntegerExpression;
                  ExpectChar (',');
                  y := NextIntegerExpression;
                  ExpectChar (',');
                  cx := NextIntegerExpression;
                  ExpectChar (',');
                  cy := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    exStyle := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    helpId := NextIntegerExpression;
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc(CtlCount)
                end
              else
                GetToken;
          end
          else
            GetToken;
        until KeyID = kwEnd
      end
    finally
      dlg.EndInitEx (CtlCount)
    end
  finally
    options.Free
  end;

  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoIcon                                           |
 |                                                                      |
 | Handle an ICON resource                                              |
 |                                                                      |
 | ICON:: nameID ICON filename                                          |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoIcon;
var
  FFileName: string;
  ico: TIconCursorGroupResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  FFileName := ExpectString ('File name expected');
  ico := TIconGroupResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));
  ico.LoadImage(PathName + FFileName);
  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoLanguage                                       |
 |                                                                      |
 | Handle a LANGUAGE statement                                          |
 |                                                                      |
 | LANGUAGE:: Primary Language, SubLanguage                             |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoLanguage;
var
  pri, sec: Word;
begin
  pri := NextInteger;
  NextChar (',');
  sec := NextInteger;
  FLangId := MakeLangID (pri, sec);
  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoMenu                                           |
 |                                                                      |
 | Handle a MENU resource                                               |
 |                                                                      |
 | MENU:: menuID MENU  [[optional-statements]]                          |
 |                     {      item-definitions      . . .  }            |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoMenu;
var
  validKeyWords: TSupportedKeyWords;
  options: TResourceOptions;
  mn: TMenuResourceDetails;
  itm: TMenuItem;

// Get menuitem or popup options - like CHECKED, GRAYED, etc.
  procedure GetMenuOptions;
  begin
    GetRestOfLine;
    GetToken;
  end;

// Create a menu item, and populate it from the RC data.
// Called recursively for popups.
  function CreateMenuItem: TMenuItem;
  var
    st: string;
    id: Integer;
    it: TMenuItem;
  begin
    GetToken;
    Result := TMenuItem.Create(nil);
    repeat
      case KeyID of
        kwMenuItem: begin
                       GetToken;  // Menu caption, or SEPARATOR
                       id := 0;
                       if TokenType = ttString then
                       begin
                         st := Token;
                         NextChar (',');
                         id := NextIntegerExpression;
                       end
                       else
                         if (TokenType = ttIdentifier) and (Token = 'SEPARATOR') then
                         begin
                           st := '-';
                           GetToken
                         end
                         else
                           raise EParser.Create('Menu item name or SEPARATOR expected');
                       if TokenType = ttChar then
                         GetMenuOptions;

                       it := TMenuItem.Create(nil);
                       it.Caption := st;
                       it.Tag := id;    // Can't set menuitem.command because it's
                                        // read-only.  TMenuResourceDetails uses
                                        // Tag instead.
                       Result.Add(it);
                     end;
        kwPopup: begin
                        st := NextString;       // Popup menu caption
                        GetToken;
                        if TokenType = ttChar then
                          GetMenuOptions;

                        if KeyID = kwBegin then
                          it := CreateMenuItem  // Recurse to create the popup menu's items
                        else
                          it := TMenuItem.Create (nil);
                        it.Caption := st;
                        Result.Add(it)
                     end;
        else
          break
      end
    until KeyID = kwEnd;
    GetToken
  end;

begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  options := CreateResourceOptions (MenuOptionKeyWords, validKeyWords);
  try
   if Assigned(options) then
      mn := TMenuResourceDetails.CreateNew(FParent, options.Language, ValToStr (FName))
    else
      mn := TMenuResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

    if KeyID = kwBegin then
    begin
      itm := CreateMenuItem;
      try
        mn.SetItems(itm);
      finally
        itm.Free
      end
    end
  finally
    options.Free
  end
end;

procedure TRCParser.DoMessageTable;
var
  FFileName: string;
  mt: TMessageResourceDetails;
begin
  SkipWhitespace;
  if Ch = '"' then
    NextString
  else
    GetRestOfLine;
  FFileName := Token;
  mt := TMessageResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));
  mt.Data.LoadFromFile(PathName + FFileName);
  GetToken;
end;

procedure TRCParser.DoStringTable;
var
  kw: DWORD;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  if KeyID = kwBegin then
  repeat
    GetToken;
    kw := KeyID;
  until kw = kwEnd;
  GetToken;
end;

procedure TRCParser.DoToolbar;
var
//p  btnWidth, btnHeight: Integer;
//  tb: TToolbarResourceDetails;
  kw: Integer;
  v: TValue;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  (* btnWidth := *) ExpectInteger;
  NextChar (',');
  (* btnHeight := *) NextInteger;
  NextToken;

  (* tb := *) TToolbarResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

  if KeyID = kwBegin then
  repeat
    GetToken;
    kw := KeyID;

    case kw of
      kwButton :
        begin
          NextIdentifier;
          v := Calc (Token);
          GetToken
        end;

      kwSeparator :
      begin
        NextToken
      end
    end;

    kw := KeyID

  until kw = kwEnd;
  GetToken;
end;

procedure TRCParser.DoVersionInfo;
var
//  v: TVersionInfoResourceDetails;
  st: string;
  beginendlevel: Integer;

  procedure GetFIXEDINFOId (const id: string; var st: string);
  var
    msg: string;
  begin
    msg := id + ' expected';
    NextIdentifier (msg);
    if not SameText(id, Token) then
      raise EParser.Create(msg);

    st := GetRestOfLine;
  end;
begin
  repeat
    NextIdentifier ('FIXEDINFO Identifier expected');

    if (Token = 'BLOCK') or (Token = 'BEGIN') then
      break;
    st := GetRestOfLine
  until FEOF;

  if FEOF then Exit;

  (* v := *) TVersionInfoResourceDetails.CreateNew(FParent, FLangId, ValToStr (FName));

  if Token = 'BEGIN' then
    beginendlevel := 1
  else
    beginEndLevel := 0;

  repeat
    FEOF := not GetToken;
    if FEOF then
      break;
    if TokenType = ttIdentifier then
      if Token = 'BEGIN' then
        Inc(beginendlevel)
      else
        if Token = 'END' then
          Dec(beginendlevel)
  until beginendlevel = 0;
  GetToken;
end;

function TRCParser.GetControlParams(ex: Boolean; var text: TSzOrID; var id: DWORD;
  var x, y, cx, cy: Integer; var style, exStyle, helpId: DWORD; options: TControlParamsOptions): Boolean;
var
  v: TValue;
begin
  if cpHasText in options then
  begin
    NextExpression (v);
    text := ValToSzOrID (v);
    ExpectChar (',')
  end
  else
  begin
    text.isID := False;
    text.sz := ''
  end;
  id := NextIntegerExpression;
  ExpectChar (',');
  x := NextIntegerExpression;
  ExpectChar (',');
  y := NextIntegerExpression;

  if cpNeedsCXCY in options then
    ExpectChar (',');

  if (TokenType = ttChar) and (TokenChar = ',') then
  begin
    cx := NextIntegerExpression;
    ExpectChar (',');
    cy := NextIntegerExpression;

    if cpNeedsStyle in options then
      ExpectChar (',');

    if (TokenType = ttChar) and (TokenChar = ',') then
    begin
      style := style or DWORD (NextIntegerExpression);

      if (TokenType = ttChar) and (TokenChar = ',') then
        exStyle := NextIntegerExpression
    end
  end;

  if ex then
    if (TokenType = ttChar) and (TokenChar = ',') then
      helpId := NextIntegerExpression;
  Result := True
end;

function TRCParser.KeyID: Integer;
var
  kw: TKeyWordDetails;
begin
  if TokenType <> ttIdentifier then
    Result := -1
  else
  begin
    kw := KeyWord (Token);
    if Assigned(kw) then
      Result := kw.kw
    else
      Result := -1
  end
end;

function TRCParser.KeyWord(const st: string): TKeyWordDetails;
var
  idx: Integer;
begin
  idx := FKeyWords.IndexOf(st);
  if idx >= 0 then
    Result := TKeyWordDetails (FKeyWords.Objects [idx])
  else
    Result := nil;
end;

function TRCParser.NextExpression (var v: TValue): Boolean;
var
  st: string;
begin
  st := '';
  repeat
    FEOF := not GetToken;
    if FEOF then break;

    case TokenType of
     ttIdentifier:  if Assigned(KeyWord (Token)) then
                       break
                     else
                       st := st + Token;
     ttChar   :  if CharInSet(TokenChar, ['+', '-', '/', '\', '|', '~', '=', '&', '(', ')', '*']) then
                       st := st + TokenChar
                     else
                       break;

     ttNumber : st := st + Token;
     ttString : st := st + '"' + Token + '"'
    end
  until False;

  if st <> '' then
  begin
    v := Calc (st);
    Result := True
  end
  else
    Result := False;
end;

function TRCParser.NextIntegerExpression: Integer;
var
  v: TValue;
begin
  NextExpression (v);
  if v.tp <> vInteger then
    raise EParser.Create('Integer expression expected');
  Result := v.iVal
end;

procedure TRCParser.Parse;
var
  dets: TKeyWordDetails;
  tokenHandled: Boolean;
  i: Integer;
begin
  GetChar;
  FEOF := not GetToken;
  while not FEOF do
  begin
    tokenHandled := False;
    if TokenType = ttIdentifier then
    begin
      dets := KeyWord (Token);
      if not Assigned(dets) then
        FName := ResolveToken
      else
        if Assigned(dets.proc) then
        begin
          dets.proc;
          tokenHandled := True
        end
    end
    else
      if TokenType = ttNumber then
        if TokenSOL then
          FName := ResolveToken
        else
        begin
          for i := 0 to NoKeyWords -1 do
            if Integer (KeyWordTable [i].resID) = StrToInt(Token) then
            begin
              dets := KeyWord (KeyWordTable [i].kw);

              if Assigned(dets) and Assigned(dets.proc) then
              begin
                dets.proc;
                tokenHandled := True
              end;

              break;
            end
        end;
    if not tokenHandled then
      FEOF := not GetToken
  end
end;

procedure TRCParser.SkipYeOldeMemoryAttributes;
begin
  while not FEOF and (KeyID in [kwDiscardable, kwPure, kwMoveable, kwPreload, kwLoadOnCall]) do
    FEOF := not GetToken
end;

{ TKeyWordDetails }

constructor TKeyWordDetails.Create(AKeyWord: Integer; AProc: TKeyWordProc);
begin
  kw :=AKeyWord;
  Proc := AProc;
end;

{ TResourceOptions }

constructor TResourceOptions.Create(Parser: TRCParser; SupportedKeyWords: TSupportedKeyWords);
begin
  FParser := Parser;
  FSupportedKeyWords := SupportedKeyWords;
end;

end.
