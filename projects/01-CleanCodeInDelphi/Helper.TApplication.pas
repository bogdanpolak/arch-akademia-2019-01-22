unit Helper.TApplication;

interface

uses
  VCL.Forms;

type
  TApplicationHelper = class helper for TApplication
  public
    function IsDeveloperMode: Boolean;
  end;

implementation

uses
  System.SysUtils;

{ TApplicationHelper }

function TApplicationHelper.IsDeveloperMode: Boolean;
var
  Extention: string;
  ExeName: string;
  ProjectFileName: string;
begin
  {$IFDEF DEBUG}
    Extention := '.dpr';
    ExeName := ExtractFileName(Application.ExeName);
    ProjectFileName := ChangeFileExt(ExeName, Extention);
    Result := FileExists(ProjectFileName) or
      FileExists('..\..\' + ProjectFileName);
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

end.
