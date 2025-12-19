{ Copyright ©2025 Hans van Buggenum }
unit DatabaseModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  ZConnection, ZDbcIntfs, ZExceptions, ZDataset, DB,
  model.intf;

type
  TConnectionResult = record
    Success: Boolean;
    Message: String;
    ErrorCode: Integer;
  end;

  TDataSetResult = record
    DataSet: TDataSource;
  end;


  { TDatabaseModule }
  TDatabaseModule = class(TObject)
    private
      fIsConnected : Boolean;

      procedure OptimizeOracleSession(Connection: TZConnection);
    protected
      fZConnection: TZConnection;
      fTransaction: TZTransaction;
      fZQuery: TZQuery;
      fDataSource: TDataSource;

    public
      constructor Create(); overload;
      destructor Destroy; override;

      function MakeDbConnection(const DbName, UserName, aPassword : String) : TConnectionResult;
      function RetrieveData(const SqlText: String): TDataSetResult;

      function IsConnected: Boolean;
      function GetConnection: TObject;

      property CurrentQuery: TZQuery read fZQuery;
      //property IsConnected: Boolean read FIsConnected write fIsConnected;
  end;

implementation


{ TDatabaseModule }
function TDatabaseModule.MakeDbConnection(const DbName, UserName, aPassword : String) : TConnectionResult;
var
  ErrorMsg: String;
begin
  // Initialize
  Result.Success:= False;
  Result.Message:= '';
  Result.ErrorCode:= 0;
  Result.Message:= '';

  if not Assigned(fZConnection) then begin
    Result.Message:= 'Database connection not initialized';
    fIsConnected := False;
    Exit;
  end;

  with fZConnection do begin
    //General
    Protocol:= 'Oracle';
    AutoCommit:= False;

    //connection date
    Database:= DbName;
    User:= UserName;
    Password:= aPassword;

    //Properties that make retrieving the data faster
    Properties.Clear;
    // Oracle-specifieke optimalisaties
    Properties.Add('arraysize=1000'); // Number of rows per fetch
    Properties.Add('prefetch_rows=1000'); // Prefetch count
    Properties.Add('statement_cache_size=20');

    // Character set settings
    Properties.Add('codepage=UTF8');
    Properties.Add('controls_cp=CP_UTF8');

    // Specific Oracle driver optimizations
    Properties.Add('BindDoubleAsString=0');
    Properties.Add('BindDecimalAsString=0');
    Properties.Add('compression=on');

    Catalog := ''; // Oracle doesn't use a catalog
  end;

  try
    fZConnection.Connected:= True;
    fZConnection.TransactIsolationLevel:= tiReadCommitted; // For read-only queries, this is the best option.
    OptimizeOracleSession(fZConnection);

    fIsConnected:= True;
    Result.Success:= True;
    Result.Message:= 'Success';
  Except
    on E: EZSQLException do begin
      { #note : Only in English. Makes it easier to find the error on the web }
      ErrorMsg := 'Database error (' + IntToStr(E.ErrorCode) + '): ';

      // Check for specific Oracle error codes
      case E.ErrorCode of
        12154: ErrorMsg := ErrorMsg + 'Cannot find the connection identifier. Check TNSNAMES.ORA';
        1017:  ErrorMsg := ErrorMsg + 'Invalid username/password';
        12541: ErrorMsg := ErrorMsg + 'No listener on the server';
        12514: ErrorMsg := ErrorMsg + 'Listener doesn''t know the service';
        12505: ErrorMsg := ErrorMsg + 'SID not found';
        1005: ErrorMsg := ErrorMsg + 'Null password given; logon denied';
        //... Maybe even more so
      else
        ErrorMsg := ErrorMsg + E.Message;
      end;
      Result.Success:= False;
      fIsConnected:= False;
      Result.Message:= ErrorMsg;
    end;

    on E: Exception do begin
      // Common Errors
      if Pos('could not resolve', LowerCase(E.Message)) > 0 then
        ErrorMsg:= 'Netwerkfout: Kan server niet vinden.'
      else if Pos('timeout', LowerCase(E.Message)) > 0 then
        ErrorMsg:= 'Timeout: Server reageert niet.'
      else if Pos('network', LowerCase(E.Message)) > 0 then
        ErrorMsg:= 'Netwerkfout: Controleer uw verbinding.'
      else
        ErrorMsg:= 'Fout: ' + E.Message;

      Result.Success:= False;
      fIsConnected:= False;
      Result.Message:= ErrorMsg;
    end;
  end;
end;

function TDatabaseModule.RetrieveData(const SqlText : String) : TDataSetResult;
begin
  with fZQuery do begin
      Close;
      SQL.Clear;
      SQL.Text:= SqlText;
      FetchRow:= 1000;
      //Prepared:= True; { Makes no sense. The query should only be extracted once.}
      try
        DisableControls;
        Open;
        EnableControls;
        Result.DataSet:= fDataSource;
      finally
        { #note : finally of except nog ivoeren }
      end;
  end;
end;

function TDatabaseModule.IsConnected : Boolean;
begin
  Result:= fIsConnected;
end;

function TDatabaseModule.GetConnection : TObject;
begin
  Result:= fZConnection;
end;

procedure TDatabaseModule.OptimizeOracleSession(Connection : TZConnection);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    // Set session parameters for better performance
    Query.SQL.Text :=
      'ALTER SESSION SET ' +
      'NLS_DATE_FORMAT = ''YYYY-MM-DD HH24:MI:SS'' ' +
      'NLS_NUMERIC_CHARACTERS = ''.,'' ' +
      'OPTIMIZER_MODE = ALL_ROWS ' +
//      'STATISTICS_LEVEL = ALL ' +  // geeft extra ovrhead. Niet doen
//      'CURSOR_SHARING = FORCE ' +     // Usually better for performance
      'OPTIMIZER_DYNAMIC_SAMPLING = 4';
//      'QUERY_REWRITE_ENABLED = TRUE'; // niet van toepassing iis voor materialized views
    Query.ExecSQL;


  finally
    Query.Free;
  end;
end;

constructor TDatabaseModule.Create();
begin
  fZConnection:= TZConnection.Create(Nil);
  fIsConnected:= False;

  fTransaction:= TZTransaction.create(Nil);
  fZQuery:= TZQuery.create(Nil);
  fDataSource:= TDataSource.Create(Nil);

  fTransaction.Connection:= fZConnection;
  fZQuery.Connection:= fZConnection;
  fDataSource.DataSet:= fZQuery;
end;

destructor TDatabaseModule.Destroy;
begin
  if Assigned(fDataSource) Then FreeAndNil(fDataSource);
  if Assigned(fZQuery) then FreeAndNil(fZQuery);
  if Assigned(fTransaction) then FreeAndNil(fTransaction);

  if Assigned(fZConnection) then begin
    if fZConnection.Connected then
      fZConnection.Disconnect;

    FreeAndNil(fZConnection);
  end;

  inherited Destroy;
end;



end.


