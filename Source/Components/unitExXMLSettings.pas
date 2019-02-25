(*======================================================================*
 | unitExXMLSettings                                                    |
 |                                                                      |
 | XML application settings classes.                                    |
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
 | Copyright © Colin Wilson 2006  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/03/2006  CPWW  Original                                  |
 *======================================================================*)
unit unitExXMLSettings;

interface

uses
  Classes, SysUtils, MSXML, unitExSettings, unitExFileSettings;

type
  //-----------------------------------------------------------------------
  // TExXMLSettings.
  //
  // Class to store application and other settings to XML files
  TExXMLSettings = class (TExFileSettings)
  private
    FDoc: IXMLDOMDocument;
    FAutoReadOnly: Boolean;
    FSectionElement: IXMLDOMElement;
    FDirty: Boolean;

    function EncodeNodeName (nodeName: string): string;
    function DecodeNodeName (const nodeName: string): string;
    function SetupSectionElement: Boolean;

    function FindChild (elem: IXMLDOMElement; const name: string; section: Boolean): IXMLDOMElement;
    function AppendChild(elem: IXMLDOMElement; const name: string): IXMLDOMElement;
    function AppendChildSection (elem: IXMLDOMElement; const name: string): IXMLDOMElement;
    function AppendChildValue(elem: IXMLDOMElement; const name, value: string): IXMLDOMElement;
    function IsSection (node: IXMLDOMElement): Boolean;
    procedure EnumChildNames(elem: IXMLDOMElement; childNames: TStrings; section: Boolean);
      function GetXMLFileName: string;

  protected
    function IsOpen: Boolean; override;
    function  CheckIsOpen (readOnly, autoReadOnly: Boolean): TIsOpen; override;
    procedure InternalSetStringValue (const valueName, value: string); override;
    procedure SetSection(const SectionPath: string); override;
  public
    procedure Close; override;
    constructor CreateChild (AParent: TExSettings; const ASection: string); override;
    function Open (readOnly: Boolean = false): Boolean; override;
    procedure Flush; override;

    procedure DeleteValue (const valueName: string); override;
    procedure DeleteSection (const sectionName: string); override;
    function HasSection (const ASection: string): Boolean; override;
    function HasValue (const AValue: string): Boolean; override;
    procedure GetValueNames (names: TStrings); override;
    procedure GetSectionNames (names: TStrings); override;
    procedure RenameSection (const oldValue, newValue: string); override;
    procedure RenameValue (const oldValue, newValue: string); override;

    function GetStringValue  (const valueName: string; const deflt: string = ''): string; override;
    property FileName: string read GetXMLFileName;
  end;

implementation

uses
  ComObj, unitSearchString;

{ TExXMLSettings }

(*----------------------------------------------------------------------*
 | function TExXMLSettings.CheckIsOpen                                  |
 |                                                                      |
 | Ensure that the file is open.                                        |
 *----------------------------------------------------------------------*)
function TExXMLSettings.CheckIsOpen (readOnly, autoReadOnly: Boolean): TIsOpen;
var
  fn: string;
begin
  Result := inherited CheckIsOpen (readOnly, FAutoReadOnly);

  case result of
    woClosed :
      begin
        if Open (readOnly) then
          Result := woOpen;
        fReadOnly := False;
      end;
    woReopen :
      begin
        FAutoReadOnly := readOnly;  // Actually, readOnly will always be false
                                    // because we wouldn't be reopening it
                                    // otherwise!

        fn := FileName;
        if not readOnly then
          ForceDirectories (ExtractFilePath (fn));
        Result := woOpen
      end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Close                                       |
 |                                                                      |
 | Close the XML file                                                   |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.Close;
begin
  if Parent = Nil then
    Flush
  else
    TExXMLSettings (Parent).FDirty := TExXMLSettings (Parent).FDirty or FDirty;

  FSectionElement := Nil;
  FDoc := Nil;  // Release the DOC object
  FDirty := False;
end;

constructor TExXMLSettings.CreateChild(AParent: TExSettings;
  const ASection: string);
var
  parentDoc: IXMLDOMDocument;
begin
  inherited;

  parentDoc := TExXMLSettings (AParent).FDoc;

  if parentDoc = Nil then
  begin
    AParent.Open (ReadOnly);
    parentDoc := TExXMLSettings (AParent).FDoc
  end;

  FDoc := parentDoc;
  SetupSectionElement
end;


const
  escape = '-';
  
function TExXMLSettings.DecodeNodeName(const nodeName: string): string;
var
  l, ip, op: Integer;
  ch: char;
  st: string;
begin
  l := Length (nodeName);
  SetLength (result, l);

  ip := 1;
  op := 1;

  while ip <= l do
  begin
    ch := nodeName [ip];
    Inc(ip);

    if (ip = 2) and (ch = '_') then
    begin
      ch := nodeName [ip];
      Inc(ip)
    end;

    if ch = escape then
    begin
      if ip <= l - 2 then
      begin
        st := Copy (nodeName, ip, 2);
        Inc(ip, 2);
        ch := char (StrToInt('$' + st));
      end;
    end
    else
      if ch = '_' then
        ch := ' ';

    result [op] := ch;
    Inc(op)
  end;
  SetLength (result, op-1);
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.DeleteSection                               |
 |                                                                      |
 | Delete a section                                                     |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.DeleteSection(const sectionName: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen (false, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, sectionName, true);
    if n <> Nil then
    begin
      FSectionElement.removeChild(n);
      FDirty := True
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.DeleteValue                                 |
 |                                                                      |
 | Delete a value                                                       |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.DeleteValue(const valueName: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen (false, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, valueName, false);
    if n <> Nil then
    begin
      FSectionElement.removeChild(n);
      FDirty := True
    end
  end
end;

const
  specialChars: set of AnsiChar = ['$', '_', '%', '+', '!', '"', '£', '^', '&'];
  specialFirstChars = ['0'..'9', '.'];

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.EncodeNodeNames                             |
 |                                                                      |
 | Node names can't contain certian characters, and in addition they    |
 | can't start with numbers and dots.                                   |         |
 |                                                                      |
 | If a special character occurs, we escape it by replacing it with     |
 | '-', followed by the character's ordinal value in 2 hex digits.      |
 |                                                                      |
 | Spaces aren't valid either, so they get replaced with '_'.  If an    |
 | actual '_' occurs it is escaped.                                     |
 |                                                                      |
 | Finally, node names can't start with numbers or dots.  If they occur |
 | they are preceeded with a '_'.                                       |
 *----------------------------------------------------------------------*)
function TExXMLSettings.EncodeNodeName(nodeName: string): string;
var
  l, ip, op: Integer;
  ch: char;
  st: string;
  dontEscape: Boolean;
begin
  nodeName := Trim (nodeName);

  l := Length (nodeName);
  SetLength (result, l * 3);

  ip := 1;
  op := 1;
  while ip <= l do
  begin
    ch := nodeName [ip];
    Inc(ip);

    if ch = ' ' then
      result [op] := '_'
    else
    begin
      if (ch = escape) or (AnsiChar (ch) in specialChars) or ((ip = 2) and (AnsiChar (ch) in specialFirstChars)) then
      begin
        dontEscape := False;
        if ip = 2 then
        begin
          result [op] := '_';
          Inc(op);
          if (ch <> escape) and not(AnsiChar (ch) in specialChars)  then
            dontEscape := True
        end;

        if not dontEscape then
        begin
          result [op] := escape;
          Inc(op);

          st := IntToHex (Ord (ch), 2);
          result [op] := st [1];
          Inc(op);
          ch := st [2];
        end
      end;
      result [op] := ch
    end;

    Inc(op)
  end;
  SetLength (result, op - 1);
end;

procedure TExXMLSettings.EnumChildNames(elem: IXMLDOMElement;
  childNames: TStrings; section: Boolean);
var
  node: IXMLDOMNode;
begin
  childNames.Clear;
  if elem = Nil then Exit;

  node := elem.firstChild;
  while (node <> Nil) do
  begin
    if (node.nodeType = NODE_ELEMENT) and (section = IsSection (node as IXMLDOMElement)) then
      childNames.Add(DecodeNodeName (node.nodeName));
    node := node.nextSibling;
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.FindChild                                    |
 |                                                                      |
 | Find the XMLDOMNode for a child value or section                     |
 *----------------------------------------------------------------------*)
function TExXMLSettings.FindChild(elem: IXMLDOMElement;
  const name: string; section: Boolean): IXMLDOMElement;
var
  node: IXMLDOMNode;
  nm: string;

begin
  node := elem.firstChild;
  nm := EncodeNodeName (name);

  while node <> Nil do
  begin
    if node.nodeType = NODE_ELEMENT then
    begin
      Result := node as IXMLDOMElement;
      if (section = IsSection (Result)) and SameText(result.nodeName, nm) then
        break;
      node := node.nextSibling
    end
  end;

  if node = nil then
    Result := nil
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Flush                                       |
 |                                                                      |
 | Save the settings to the XML file                                    |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.Flush;
begin
  if (FDoc <> Nil) and FDirty then
  begin
    FDoc.Save (FileName);
    FDirty := False
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.GetStringValue                               |
 |                                                                      |
 | Return a sting value, or the default if the value doesn't exist      |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.GetSectionNames(names: TStrings);
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    EnumChildNames (FSectionElement, names, true);
end;

function TExXMLSettings.GetStringValue(const valueName, deflt: string): string;
var
  n: IXMLDOMElement;
begin
  CheckIsOpen (true, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, valueName, false);

    if n <> Nil then
      Result := n.text
    else
      Result := deflt
  end
end;

procedure TExXMLSettings.GetValueNames(names: TStrings);
begin
  CheckIsOpen (true, FAutoReadOnly);
  EnumChildNames (FSectionElement, names, false)
end;

function TExXMLSettings.GetXMLFileName: string;
begin
  Result := GetFileName ('.xml');
end;

function TExXMLSettings.HasSection(const ASection: string): Boolean;
var
  e: IXMLDOMElement;
  s, st: string;
begin
  CheckIsOpen (true, FAutoReadOnly);
  Result := False;

  if FSectionElement <> Nil then
  begin
    if ASection = '' then
      Result := True
    else
    begin
      s := ASection;
      e := FSectionElement;
      while s <> '' do
      begin
        st := SplitString ('\', s);
        e := FindChild (e, st, true);
        if e = Nil then break
        
      end;
      Result := e <> Nil
    end
  end
end;

function TExXMLSettings.HasValue(const AValue: string): Boolean;
var
  n: IXMLDOMElement;
begin
  CheckIsOpen (true, FAutoReadOnly);
  Result := False;

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, AValue, false);

    if n <> Nil then
      Result := True
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.AppendChild                                  |
 |                                                                      |
 | Append a child value or section to the specified section node        |
 *----------------------------------------------------------------------*)
function TExXMLSettings.AppendChild(elem: IXMLDOMElement; const name: string): IXMLDOMElement;
begin
  Result := FDoc.createElement(EncodeNodeName (name));
  elem.appendChild(Result);
  FDirty := True;
end;

function TExXMLSettings.AppendChildSection(elem: IXMLDOMElement;
  const name: string): IXMLDOMElement;
begin
  Result := AppendChild (elem, name);
  result.setAttribute('Section', true);
end;

function TExXMLSettings.AppendChildValue(elem: IXMLDOMElement; const name, value: string): IXMLDOMElement;
begin
  Result := AppendChild (elem, name);
  result.text := value;
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.InternalSetStringValue                      |
 |                                                                      |
 | Set a string value.                                                  |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.InternalSetStringValue(const valueName, value: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen (false, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, valueName, false);

    if n <> Nil then
      n.text := value
    else
      AppendChildValue(FSectionElement, valueName, value);

    FDirty := True
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.IsOpen                                       |
 |                                                                      |
 | Return true if the object is Open                                    |
 *----------------------------------------------------------------------*)
function TExXMLSettings.IsOpen: Boolean;
begin
  Result := FDoc <> Nil
end;

function TExXMLSettings.IsSection(node: IXMLDOMElement): Boolean;
var
  n: IXMLDOMNode;
begin
  n := node.getAttributeNode('Section');
  if n <> Nil then
    Result := n.nodeValue
  else
    Result := False
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Open                                        |
 |                                                                      |
 | Open the XML file.  Create it if it doesn't exist                    |
 *----------------------------------------------------------------------*)
function TExXMLSettings.Open(readOnly: Boolean): Boolean;
var
  fn, xml: string;
begin
  inherited Open (ReadOnly);
  if FDoc <> Nil then
    Close;
  FAutoReadOnly := readOnly;

  fn := FileName;
  if not readOnly then
    ForceDirectories (ExtractFilePath (fn))
  else
    if not FileExists (fn) then
    begin
      Result := False;
      Exit;
    end;

  FDoc := CoDOMDocument.Create;

  if not FDoc.load(fn) then
  begin
    xml := '<?xml version="1.0" encoding="UTF-8"?><' + EncodeNodeName (Application) + '></' + EncodeNodeName (Application) + '>';
    if not FDoc.loadXML(xml) then
      raise EExSettings.Create ('Unable to create the XML document');
  end;

  Result := SetupSectionElement
end;

procedure TExXMLSettings.RenameSection(const oldValue, newValue: string);
var
  newN, n: IXMLDOMElement;
  c: IXMLDOMNode;
begin
  CheckIsOpen (false, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, newValue, true);
    if n <> Nil then
      raise EExSettings.Create('Section ' + newValue + ' already exists');

    n := FindChild (FSectionElement, oldValue, true);
    if n = Nil then
      raise EExSettings.Create('Section ' + oldValue + ' does not exist');

    newN := FDoc.createElement((EncodeNodeName (newValue)));

    // We know that the only attribute is 'Section' - so rather than
    // cloning the attributes, simply create a new Section attribute
    newN.setAttribute('Section', true);

    c := n.firstChild;
    while c <> Nil do
    begin
      newN.appendChild(c.cloneNode(true));
      c := c.nextSibling;
    end;

    FSectionElement.replaceChild(newN, n);
    FDirty := True
  end
end;

procedure TExXMLSettings.RenameValue(const oldValue, newValue: string);
var
  newN, n: IXMLDOMElement;
begin
  CheckIsOpen (false, FAutoReadOnly);

  if FSectionElement <> Nil then
  begin
    n := FindChild (FSectionElement, oldValue, false);
    if n = Nil then
      raise EExSettings.Create('Value ' + oldValue + ' does not exist');

    newN := FDoc.createElement(EncodeNodeName (newValue));
    newN.text := n.text;
    FSectionElement.replaceChild(newN, n);

    FDirty := True
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.SetSection                                  |
 |                                                                      |
 | Override the 'Set' method for the Section property                   |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.SetSection(const SectionPath: string);
begin
  inherited;

  SetupSectionElement
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.SetupSectionElement                         |
 |                                                                      |
 | Find or create the node for the current section                      |
 *----------------------------------------------------------------------*)
function TExXMLSettings.SetupSectionElement: Boolean;
var
  s, n: string;
  node: IXMLDOMElement;
begin
  Result := True;
  if FDoc <> Nil then
  begin
    FSectionElement := FDoc.documentElement;

    s := Section;

    repeat
      n := SplitString ('\', s);
      if n = '' then break;

      node := FindChild (FSectionElement, n, true);

      if node = Nil then
        if ReadOnly then
        begin
          Result := False;
          break
        end
        else
          FSectionElement := AppendChildSection (FSectionElement, n)
      else
        FSectionElement := node;
    until false;
  end
  else
    FSectionElement := Nil;
end;

end.
