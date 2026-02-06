{ Copyright ©2025-2026 Hans van Buggenum }
unit DatabaseModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  oracleconnection, SQLDB, DB,
  model.intf;

type
  TConnectionResult = record
    Success: Boolean;
    Message: String;
    ErrorCode: Integer;
  end;

  TDataSetResult = record
    DataSource: TDataSource;
    DataSet: TDataSet;
    Success: Boolean;
  end;


  { TDatabaseModule }
  TDatabaseModule = class(TObject)
    private
      fIsConnected : Boolean;

      procedure OptimizeOracleSession(Connection: TOracleConnection);
    protected
      fConnection: TOracleConnection;
      fTransaction: TSQLTransaction;
      fSqlQuery: TSQLQuery;
      fDataSource: TDataSource;
    public
      constructor Create(); overload;
      destructor Destroy; override;

      function MakeDbConnection(const DbName, UserName, aPassword : String) : TConnectionResult;
      function DbDisconnect: TConnectionResult;
      function RetrieveData(const SqlText: String): TDataSetResult;

      function IsConnected: Boolean;
      function GetConnection: TObject;

      property CurrentQuery: TSQLQuery read fSqlQuery;
      //property IsConnected: Boolean read FIsConnected write fIsConnected;
  end;

implementation


{ TDatabaseModule }
function TDatabaseModule.MakeDbConnection(const DbName, UserName, aPassword: String): TConnectionResult;
var
  ErrorMsg: String;
  ErrorCode: Integer;
  OraError: String;
begin
  // Initialize
  Result.Success:= False;
  Result.Message:= '';
  Result.ErrorCode:= 0;

  if not Assigned(fConnection) then begin
    Result.Message:= 'DbConnNotInitialized';
    fIsConnected:= False;
    Exit;
  end;

  try
    // Eerst transaction parameters instellen VOOR connectie
    fTransaction.Params.Clear;
    fTransaction.Params.Add('read_committed');     // Transaction isolation level
    fTransaction.Params.Add('nowait');             // Geen wachten op locks

    // Dan connection parameters instellen
    with fConnection do begin
      // Performance parameters instellen (alleen als nog niet connected)
      if not Connected then
      begin
        Params.Clear;

        // Performance parameters
        Params.Add('arraysize=1000');                  // Rijen per fetch
        Params.Add('prefetch_rows=1000');              // Prefetch memory
        Params.Add('statement_cache_size=20');         // Prepared statements cache

        // Network parameters
        Params.Add('packet_size=8192');                // Network packet size

        // Character set - belangrijk voor Unicode
        Params.Add('codepage=UTF8');
        Params.Add('ClientCharset=UTF8');

        // Connection parameters
        Params.Add('LoginTimeout=10');                 // Timeout in seconden
        //Params.Add('ConnectTimeout=10');            // Alternatieve naam
        Params.Add('MultipleTransactions=False');      // Voor betere performance
      end;
    end;

    // Nu pas connecteren
    fConnection.DatabaseName:= DbName;
    fConnection.UserName:= UserName;
    fConnection.Password:= aPassword;
    fConnection.HostName:= ''; // Moet leeg zijn als van TNSNAMES.ora gebruik wordt gemaakt.

    fConnection.Connected:= True;

    // Session optimaliseren
    OptimizeOracleSession(fConnection);

    fIsConnected:= True;
    Result.Success:= True;
    Result.Message:= 'Success';

  except
    on E: EDatabaseError do begin
      ErrorMsg:= 'DbError|';
      ErrorCode:= 0;

      // Try to extract error information
      OraError:= UpperCase(E.Message);

      // Check voor specifieke Oracle fouten
      if Pos('ORA-12154', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'CannotFindConnIdentifier'
      else if Pos('ORA-01017', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'InvalidUserOrPwd'
      else if Pos('ORA-12541', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'NoListener'
      else if Pos('ORA-12514', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'ListenerDoesNotKnowServer'
      else if Pos('ORA-12505', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'SIDNotFound'
      else if Pos('ORA-01005', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'NullPwd'
      else if Pos('ORA-12170', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'ConnectTimeout'
      else if Pos('ORA-12535', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'OperationTimeout'
      else if Pos('ORA-12560', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'ProtocolAdapterError'
      else if Pos('TNS-', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'TNS Error: ' + E.Message
      else if Pos('NO DATA FOUND', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'NoDataFound'
      else if Pos('TOO MANY ROWS', OraError) > 0 then
        ErrorMsg:= ErrorMsg + 'TooManyRows'
      else
        ErrorMsg:= ErrorMsg + E.Message;

      Result.Success:= False;
      Result.ErrorCode:= ErrorCode;
      Result.Message:= ErrorMsg;
      fIsConnected:= False;

      // Zorg dat connection gesloten wordt bij error
      if fConnection.Connected then
      begin
        try
          fConnection.Connected:= False;
        except
          // Negeer errors bij sluiten
        end;
      end;
    end;

    on E: Exception do begin
      // Algemene exceptions
      OraError:= LowerCase(E.Message);

      if Pos('could not resolve', OraError) > 0 then
        ErrorMsg:= 'ServerNotFound'
      else if Pos('unknown host', OraError) > 0 then
        ErrorMsg:= 'UnknownHost'
      else if Pos('timeout', OraError) > 0 then
        ErrorMsg:= 'ServerNotResponding'
      else if Pos('network', OraError) > 0 then
        ErrorMsg:= 'CheckConnection'
      else if Pos('access violation', OraError) > 0 then
        ErrorMsg:= 'InternalError'
      else if Pos('ora-', OraError) > 0 then
        ErrorMsg:= 'OracleError: ' + E.Message  // Catch any ORA- error we missed
      else
        ErrorMsg:= 'Error: ' + E.Message;

      Result.Success:= False;
      fIsConnected:= False;
      Result.Message:= ErrorMsg;

      // Zorg dat connection gesloten wordt bij error
      if fConnection.Connected then
      begin
        try
          fConnection.Connected:= False;
        except
          // Negeer errors bij sluiten
        end;
      end;
    end;
  end;
end;

function TDatabaseModule.DbDisconnect: TConnectionResult;
begin
  if not Assigned(fConnection) then begin
    Result.Success:= False;
  end
  else begin
    fConnection.Connected:= False;
    Result.Success:= False;
  end;
end;

function TDatabaseModule.RetrieveData(const SqlText : String) : TDataSetResult;
begin
  with fSqlQuery do begin
    Close;
    SQL.Clear;
    SQL.Text:= SqlText;
    PacketRecords:= -1;
    Prepared;

    try
      DisableControls;
      Open;
      EnableControls;

      Result.DataSource:= fDataSource;

      if RecordCount> 0 then Result.Success:= True else Result.Success:= False;
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
  Result:= fConnection;
end;

procedure TDatabaseModule.OptimizeOracleSession(Connection: TOracleConnection);
var
  Query: TSQLQuery;
begin
  Query:= TSQLQuery.Create(nil);
  try
    Query.Database:= Connection;
    Query.Transaction:= fTransaction;

    // Set session parameters for better performance
    Query.SQL.Text:=
      'ALTER SESSION SET ' +
      'NLS_DATE_FORMAT = ''YYYY-MM-DD HH24:MI:SS'' ' +
      'NLS_NUMERIC_CHARACTERS = ''.,'' ' +
      'OPTIMIZER_MODE = ALL_ROWS ' +
      'CURSOR_SHARING = EXACT ' +      // FORCE kan problemen geven, EXACT is veiliger
      'OPTIMIZER_DYNAMIC_SAMPLING = 2 ' +
      'QUERY_REWRITE_ENABLED = TRUE ' +
      'RESULT_CACHE_MODE = MANUAL ' +  // Cache beheerd door applicatie
      'STATISTICS_LEVEL = BASIC';      // Minimal statistics overhead

    Query.ExecSQL;

    // Additional optimization for large result sets
    // Tijdelijk appart. { #todo : Nog een keer testen of dit verschil maakt }
    Query.SQL.Text:=
      'ALTER SESSION SET ' +
      'SORT_AREA_SIZE = 1048576 ' +
      'HASH_AREA_SIZE = 1048576 ' +
      'DB_FILE_MULTIBLOCK_READ_COUNT = 128';

    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

constructor TDatabaseModule.Create();
begin
  fConnection:= TOracleConnection.Create(Nil);
  fIsConnected:= False;

  fTransaction:= TSQLTransaction.create(Nil);
  fTransaction.DataBase:= fConnection;

  fSqlQuery:= TSQLQuery.create(Nil);
  fSqlQuery.MaxIndexesCount:= 100;  // Nodig voor het sorteren. Dit zijn 50 kolommen in 2 richtingen.
  fSqlQuery.Database:= fConnection;
  fSqlQuery.Transaction:= fTransaction;

  fDataSource:= TDataSource.Create(Nil);
  fDataSource.DataSet:= fSqlQuery;
end;

destructor TDatabaseModule.Destroy;
begin
  if Assigned(fDataSource) then FreeAndNil(fDataSource);
  if Assigned(fSqlQuery) then FreeAndNil(fSqlQuery);
  if Assigned(fTransaction) then FreeAndNil(fTransaction);

  if Assigned(fConnection) then begin
    if fConnection.Connected then
      fConnection.Connected:= False;

    FreeAndNil(fConnection);
  end;

  inherited Destroy;
end;



end.


