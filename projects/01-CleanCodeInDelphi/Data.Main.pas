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
  Utils.General;

type
  TDataModMain = class(TDataModule)
    FDConnection1: TFDConnection;
    dsBooks: TFDQuery;
    dsReaders: TFDQuery;
    dsReports: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
    function DBVersionToString(VerDB: Integer): string;
    procedure LogInfoStd (level: Integer; const Msg: string);
    procedure LogInfoDev (level: Integer; const Msg: string);
    procedure OpenDataSets;
    procedure ConnectToDataBase;
    function GetDatabaseVersion: Variant;
    procedure LogDatabaseVersion;
  public
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
  Consts.Application,
  Messaging.EventBus;

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
  StrExpectedDatabseVersion = 'Oczekiwana wersja bazy: ';
  StrAktualDatabseVersion = 'Aktualna wersja bazy: ';

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
      LogInfoStd(0, msg1);
      LogInfoDev(1, E.Message);
      Raise
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
      LogInfoStd(0, Msg);
      LogInfoDev(1, E.Message);
      Raise;
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
    LogInfoStd(0, StrNotSupportedDBVersion);
    LogInfoStd(1, StrExpectedDatabseVersion + DBVersionToString
      (ExpectedDatabaseVersionNr));
    LogInfoStd(1, StrAktualDatabseVersion + DBVersionToString(VersionNr));
  end;
end;

procedure TDataModMain.LogInfoStd(level: Integer; const Msg: string);
var
  AMessage: TEventMessage;
begin
  AMessage.TagString := Msg;
  AMessage.TagInt := Level;
  AMessage.TagBoolean := False;
  TEventBus._Post(EB_DBCONNECTION_AddInfo, AMessage);
end;

procedure TDataModMain.LogInfoDev(level: Integer; const Msg: string);
var
  AMessage: TEventMessage;
begin
  AMessage.TagString := Msg;
  AMessage.TagInt := Level;
  AMessage.TagBoolean := True;
  TEventBus._Post(EB_DBCONNECTION_AddInfo, AMessage);
end;

procedure TDataModMain.OpenDataSets;
begin
  dsBooks.Open();
  dsReaders.Open();
  dsReports.Open();
end;

end.
