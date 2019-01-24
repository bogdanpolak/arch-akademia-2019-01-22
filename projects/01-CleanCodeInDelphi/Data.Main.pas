unit Data.Main;

interface

uses
  System.SysUtils, System.Classes, Data.DB,
  // ------------------------------------------------------------------------
  // FireDAC: FDConnection:
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.Phys.Intf,
  FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.VCLUI.Wait,
  // ------------------------------------------------------------------------
  // FireDAC: SQLite:
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  // ------------------------------------------------------------------------
  // FireDAC: FDQuery:
  FireDAC.Comp.DataSet, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Stan.Async,
  // ------------------------------------------------------------------------
  Utils.Messages,
  // TODO: [!!!] Usun¹æ zale¿noœæ od Frame.Welcome
  Frame.Welcome;

type
  TDataModMain = class(TDataModule)
    FDConnection1: TFDConnection;
    dsBooks: TFDQuery;
    dsReaders: TFDQuery;
    dsReports: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
    function DBVersionToString(VerDB: Integer): string;
    procedure LogInfo(frm: TFrameWelcome; level: Integer; const Msg: string;
      show: boolean);
  public
    procedure OpenDataSets;
    procedure VerifyAndConnectToDatabase(frm: TFrameWelcome);
    function FindReaderByEmil(const email: string): Variant;
  end;

const
  Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';

var
  DataModMain: TDataModMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.Variants,
  System.StrUtils,
  ClientAPI.Books,
  Utils.CipherAES128,
  Consts.Application;

const
  SecureKey = 'delphi-is-the-best';
  // SecurePassword = AES 128 ('masterkey',SecureKey)
  // SecurePassword = 'hC52IiCv4zYQY2PKLlSvBaOXc14X41Mc1rcVS6kyr3M=';
  // SecurePassword = AES 128 ('<null>',SecureKey)
  SecurePassword = 'EvUlRZOo3hzFEr/IRpHVMA==';
  SQL_GetDatabaseVersion = 'SELECT versionnr FROM DBInfo';

resourcestring
  SWelcomeScreen = 'Welcome screen';
  SDBServerGone = 'Database server is gone';
  SDBConnectionUserPwdInvalid = 'Invalid database configuration.' +
    ' Application database user or password is incorrect.';
  SDBConnectionError = 'Can''t connect to database server. Unknown error.';
  SDBRequireCreate = 'Database is empty. You need to execute script' +
    ' creating required data.';
  SDBErrorSelect = 'Can''t execute SELECT command on the database';
  StrNotSupportedDBVersion = 'Not supported database version. Please' +
    ' update database structures.';

function TDataModMain.DBVersionToString(VerDB: Integer): string;
begin
  Result := (VerDB div 1000).ToString + '.' + (VerDB mod 1000).ToString;
end;

procedure TDataModMain.VerifyAndConnectToDatabase(frm: TFrameWelcome);
var
  UserName: String;
  password: String;
  msg1: String;
  VersionNr: Variant;
  res: Variant;
begin
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Connect to database server
  // Check application user and database structure (DB version)
  //
  // TODO 2: [!] Dependency on DataModMain.FDConnection1 (discuss LoD)
  // Move this code into the DataModule
  try
    UserName := FireDAC.Comp.Client.FDManager.ConnectionDefs.ConnectionDefByName
      (DataModMain.FDConnection1.ConnectionDefName).Params.UserName;
    password := AES128_Decrypt(SecurePassword, SecureKey);
    if password = '<null>' then
      password := '';
    DataModMain.FDConnection1.Open(UserName, password);
  except
    on E: FireDAC.Stan.Error.EFDDBEngineException do
    begin
      case E.kind of
        ekUserPwdInvalid:
          msg1 := SDBConnectionUserPwdInvalid;
        ekServerGone:
          msg1 := SDBServerGone;
      else
        msg1 := SDBConnectionError
      end;
      LogInfo(frm, 0, msg1, True);
      LogInfo(frm, 1, E.Message, False);
      exit;
    end;
  end;
  try
    res := DataModMain.FDConnection1.ExecSQLScalar(SQL_GetDatabaseVersion);
  except
    on E: EFDDBEngineException do
    begin
      msg1 := IfThen(E.kind = ekObjNotExists, SDBRequireCreate, SDBErrorSelect);
      LogInfo(frm, 0, msg1, True);
      LogInfo(frm, 1, E.Message, False);
      exit;
    end;
  end;
  VersionNr := res;
  if VersionNr <> ExpectedDatabaseVersionNr then
  begin
    LogInfo(frm, 0, StrNotSupportedDBVersion, True);
    LogInfo(frm, 1, 'Oczekiwana wersja bazy: ' +
      DBVersionToString(ExpectedDatabaseVersionNr), True);
    LogInfo(frm, 1, 'Aktualna wersja bazy: ' +
      DBVersionToString(VersionNr), True);
  end;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  DataModMain.OpenDataSets;
end;

function TDataModMain.FindReaderByEmil(const email: string): Variant;
var
  ok: boolean;
begin
  ok := dsReaders.Locate('email', email, []);
  if ok then
    Result := dsReaders.FieldByName('ReaderId').Value
  else
    Result := System.Variants.Null()
end;

procedure TDataModMain.LogInfo(frm: TFrameWelcome; level: Integer;
  const Msg: string; show: boolean);
begin
  // TODO: Rozwi¹zaæ zale¿noœæ od TWelcomeFrame
  frm.AddInfo(level, Msg, show);
end;

procedure TDataModMain.OpenDataSets;
begin
  dsBooks.Open();
  dsReaders.Open();
  dsReports.Open();
end;

end.
