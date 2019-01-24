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
  Utils.General,
  Utils.Messages;

type
  TDataModMain = class(TDataModule)
    FDConnection1: TFDConnection;
    dsBooks: TFDQuery;
    dsReaders: TFDQuery;
    dsReports: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
    FOnLogInfo: TNotifyLogInfo;
    function DBVersionToString(VerDB: Integer): string;
    procedure LogInfo(level: Integer; const Msg: string;
      show: boolean);
    procedure OpenDataSets;
    procedure SetOnLogInfo(const Value: TNotifyLogInfo);
    procedure ConnectToDataBase;
    function GetDatabaseVersion: Variant;
    procedure LogDatabaseVersion;
  public
    property OnLogInfo: TNotifyLogInfo read FOnLogInfo write SetOnLogInfo;
    procedure VerifyAndConnectToDatabase;
    function FindReaderByEmil(const email: string): Variant;
  end;

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

procedure TDataModMain.ConnectToDataBase;
var
  UserName: String;
  password: String;
  msg1: String;
begin
  try
    UserName := FireDAC.Comp.Client.FDManager.ConnectionDefs.ConnectionDefByName
      (FDConnection1.ConnectionDefName).Params.UserName;
    password := AES128_Decrypt(SecurePassword, SecureKey);
    if password = '<null>' then
      password := '';
    FDConnection1.Open(UserName, password);
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
      LogInfo(0, msg1, True);
      LogInfo(1, E.Message, False);
    end;
  end;
end;

function TDataModMain.DBVersionToString(VerDB: Integer): string;
begin
  Result := (VerDB div 1000).ToString + '.' + (VerDB mod 1000).ToString;
end;

procedure TDataModMain.VerifyAndConnectToDatabase;
begin
  ConnectToDatabase;
  LogDatabaseVersion;
  // ----------------------------------------------------------
  OpenDataSets;
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

function TDataModMain.GetDatabaseVersion: Variant;
var
  Msg: string;
begin
  try
    Result := FDConnection1.ExecSQLScalar(SQL_GetDatabaseVersion);
  except
    on E: EFDDBEngineException do
    begin
      Msg := IfThen(E.kind = ekObjNotExists, SDBRequireCreate, SDBErrorSelect);
      LogInfo(0, Msg, True);
      LogInfo(1, E.Message, False);
      exit;
    end;
  end;
end;

procedure TDataModMain.LogDatabaseVersion;
var
  VersionNr: Variant;
begin
  VersionNr := GetDatabaseVersion;
  if VersionNr <> ExpectedDatabaseVersionNr then
  begin
    LogInfo(0, StrNotSupportedDBVersion, True);
    LogInfo(1, 'Oczekiwana wersja bazy: ' + DBVersionToString
      (ExpectedDatabaseVersionNr), True);
    LogInfo(1, 'Aktualna wersja bazy: ' + DBVersionToString(VersionNr), True);
  end;
end;

procedure TDataModMain.LogInfo(level: Integer;
  const Msg: string; show: boolean);
begin
  if Assigned(OnLogInfo) then
    OnLogInfo(Level, Msg, Show);
end;

procedure TDataModMain.OpenDataSets;
begin
  dsBooks.Open();
  dsReaders.Open();
  dsReports.Open();
end;

procedure TDataModMain.SetOnLogInfo(const Value: TNotifyLogInfo);
begin
  FOnLogInfo := Value;
end;

end.
