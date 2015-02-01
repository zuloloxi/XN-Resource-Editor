unit VirtualTreesExtensionsReg;

interface

uses
  Classes,
  ExVirtualStringTree;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [TExVirtualStringTree])
end;

end.
