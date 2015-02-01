(*======================================================================*
 | unitCodeSnippets                                                     |
 |                                                                      |
 | TCodeSnipets loads code snippets from embedded resources - for OTA   |
 | wizards etc.                                                         |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      03/08/2005  CPWW  Original                                  |
 *======================================================================*)


unit unitCodeSnippets;

interface

uses Windows, Classes, SysUtils, ConTnrs;

type
TCodeSnippet = class
private
  fSnippetName: string;
  fCode: string;
public
  constructor Create (const ASnippetName, ACode : string);
  property SnippetName : string read fSnippetName;
  property Code : string read fCode;
end;

TCodeSnipets = class (TObjectList)
private
  procedure LoadSnipets (instance : THandle);
public
  constructor Create (AModuleInstance : THandle);
  function GetSnippet (const snippetName : string) : string;
  function FormatSnippet (const s : string; const ss : array of string; const rs : array of string) : string;
end;

implementation

uses ActiveX;

{ TCodeSnippet }

{*----------------------------------------------------------------------*
 | TCodeSnippet.Create                                                  |
 |                                                                      |
 | Constructor                                                          |
 *----------------------------------------------------------------------*}
constructor TCodeSnippet.Create(const ASnippetName, ACode: string);
begin
  fSnippetName := ASnippetName;
  fCode := ACode;
end;

{ TCodeSnipets }

{*----------------------------------------------------------------------*
 | TCodeSnipets.Create                                                  |
 |                                                                      |
 | Constructor                                                          |
 *----------------------------------------------------------------------*}
constructor TCodeSnipets.Create(AModuleInstance: THandle);
begin
  inherited Create;
  LoadSnipets (AModuleInstance);
end;

{*----------------------------------------------------------------------*
 | function TCodeSnipets.FormatSnippet                                  |
 |                                                                      |
 | Replaces all occurences of the strings in 'ss' with the              |
 | corresponding string in 'rs'.  Finally it replaces all occurences of |
 | %CreateGUID% with a freshly created GUID.                            |
 |                                                                      |
 | All this is case insensitive.                                        |
 *----------------------------------------------------------------------*}
function TCodeSnipets.FormatSnippet(const s: string; const ss,
  rs: array of string): string;
var
  tempStr : string;
  idx : Integer;
  max : Integer;

  function CreateGUID : string;
  var
    r : TGUID;
  begin
    CoCreateGUID (r);
    result := GUIDToString (r);
  end;

begin
  max := High (ss);
  if High (rs) < max then
    max := High (rs);

  result := s;                  // Replace 'ss' strings with 'rs' strings
  for idx := 0 to max do
    result := StringReplace (result, ss [idx], rs [idx], [rfReplaceAll, rfIgnoreCase]);

  repeat                        // Create GUIDs
    tempStr := StringReplace (result, '%CreateGUID%', CreateGuid, [rfIgnoreCase]);
    if tempStr = result then
      break;
    result := tempStr
  until False;
end;

{*----------------------------------------------------------------------*
 | function TCodeSnipets.GetSnippet                                     |
 |                                                                      |
 | Gets the code snippet that for the snippet name.                     |
 *----------------------------------------------------------------------*}
function TCodeSnipets.GetSnippet(const snippetName: string): string;
var
  i : Integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    if CompareText (snippetName, TCodeSnippet (Items [i]).SnippetName) = 0 then
    begin
      result := TCodeSnippet (Items [i]).Code;
      break
    end;

  if result = '' then
    raise Exception.Create ('Code Snippet ' + snippetName + ' not found')
end;

{*----------------------------------------------------------------------*
 | procedure TCodeSnipets.LoadSnipets                                   |
 |                                                                      |
 | Load snippets from the resource.                                     |
 |                                                                      |
 | Each snippet is surrounded by a 'start of snippet' and 'end of       |
 | snippet' marker.  'Start of Snippet' is a line starting with '-',    |
 | and containing a colon followed by the snippet name.                 |
 | 'End of Snippet' is simply a line starting with '-'.                 |
 |                                                                      |
 | Any text between two snippets is ignored.                            |
 *----------------------------------------------------------------------*}
procedure TCodeSnipets.LoadSnipets(instance: THandle);
var
  resInstance : THandle;
  res : THandle;
  codeResource : THandle;
  i, ps : Integer;
  l : TStringList;
  p: PAnsiChar;
  snippet : TStringList;
  snippetName : string;
  s : string;

begin
  ResInstance := FindResourceHInstance(instance);
  res := FindResource(ResInstance, 'SNIPETS', RT_RCDATA);

  if res > 0 then
  begin         // We've got a 'SNIPETS' resource!

    codeResource := LoadResource (ResInstance, res);
    p := LockResource (codeResource);

    l := TStringList.Create;
    try
      l.Text := UTF8ToString (p); // buffer;
                // Move snippets into stringlist 'l'
      snippet := Nil;
      for i := 0 to l.Count - 1 do
      begin
        s := l.Strings [i];
        if Copy (s, 1, 1) = '-' then    // It's either a snippet end marker or a snippet start marker

          if Assigned (snippet) then    // If there's a snippet going, it must be a snippet end
          begin
            Add (TCodeSnippet.Create (snippetName, snippet.Text));
            FreeAndNil (snippet)
          end
          else
          begin
            ps := Pos (':', s);
            if ps > 0 then
            begin
              snippet := TStringList.Create;
              snippetName := Trim (Copy (s, ps + 1, MaxInt));
            end
          end
        else
          if Assigned (snippet) then
            snippet.Add (s)
      end;

      if Assigned (snippet) then
      begin
        Add (TCodeSnippet.Create (snippetName, snippet.Text));
        FreeAndNil (snippet)
      end
    finally
      l.Free
    end
  end
end;

end.
