{ Copyright ©2025-2026 Hans van Buggenum }
unit DatabaseModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  oracleconnection, SQLDB, DB,
  model.intf;

type
  { TConnectionResult
    Structure for returning connection operation results
    Used to provide detailed feedback about connection attempts }
  TConnectionResult = record
    Success: Boolean;     // Indicates if the operation was successful
    Message: String;      // Descriptive message about the result
    ErrorCode: Integer;   // Error code (0 for success, non-zero for errors)
  end;

  { TDataSetResult
    Structure for returning dataset retrieval results
    Contains both data components and success status }
  TDataSetResult = record
    DataSource: TDataSource;  // Data source component for UI binding
    DataSet: TDataSet;        // Actual dataset containing query results
    Success: Boolean;         // Indicates if data retrieval was successful
  end;

  { TDatabaseModule
    Manages Oracle database connections and query execution
    Provides centralized database access with connection management,
    error handling, and performance optimization for Oracle databases.
    Implements connection pooling, session optimization, and safe cleanup. }
  TDatabaseModule = class(TObject)
  private
    fIsConnected: Boolean;  // Tracks connection status internally

    { Optimizes Oracle session parameters for better performance
      Configures NLS settings, optimizer modes, and memory allocations
      Parameters:
        Connection: Active Oracle connection to optimize }
    procedure OptimizeOracleSession(Connection: TOracleConnection);

  protected
    fConnection: TOracleConnection;  // Oracle database connection component
    fTransaction: TSQLTransaction;   // Database transaction manager
    fSqlQuery: TSQLQuery;            // SQL query execution component
    fDataSource: TDataSource;        // Data source for UI components

  public
    { Creates a new database module instance
      Initializes all database components with default settings }
    constructor Create(); overload;

    { Destroys the database module and cleans up all resources
      Ensures proper disconnection and release of database handles }
    destructor Destroy; override;

    { Establishes a connection to an Oracle database
      Configures connection parameters, sets performance options,
      and optimizes the session for optimal query execution
      Parameters:
        DbName: Database name (TNS alias or connection string)
        UserName: Database username
        aPassword: Database password
      Returns:
        TConnectionResult with detailed connection status }
    function MakeDbConnection(const DbName, UserName, aPassword: String): TConnectionResult;

    { Disconnects from the current database
      Closes the connection and releases database resources
      Returns:
        TConnectionResult with disconnection status }
    function DbDisconnect: TConnectionResult;

    { Executes a SQL query and returns the results
      Parameters:
        SqlText: SQL query to execute
      Returns:
        TDataSetResult containing data source, dataset, and success flag }
    function RetrieveData(const SqlText: String): TDataSetResult;

    { Checks if currently connected to a database
      Returns:
        True if connected, False otherwise }
    function IsConnected: Boolean;

    { Gets the underlying connection object
      Returns:
        TOracleConnection instance (as TObject) }
    function GetConnection: TObject;

    // Properties
    property CurrentQuery: TSQLQuery read fSqlQuery;  // Current active query
  end;

implementation

{ TDatabaseModule }

function TDatabaseModule.MakeDbConnection(const DbName, UserName, aPassword: String): TConnectionResult;
var
  ErrorMsg: String;
  ErrorCode: Integer;
  OraError: String;
begin
  // Initialize result
  Result.Success:= False;
  Result.Message:= '';
  Result.ErrorCode:= 0;

  if not Assigned(fConnection) then begin
    Result.Message:= 'DbConnNotInitialized';
    fIsConnected:= False;
    Exit;
  end;

  try
    // Set transaction parameters BEFORE establishing connection
    fTransaction.Params.Clear;
    fTransaction.Params.Add('read_committed');     // Transaction isolation level
    fTransaction.Params.Add('nowait');             // Don't wait for locks

    // Configure connection parameters
    with fConnection do begin
      // Set performance parameters (only if not already connected)
      if not Connected then
      begin
        Params.Clear;

        // Performance parameters
        Params.Add('arraysize=1000');                  // Rows per fetch
        Params.Add('prefetch_rows=1000');              // Prefetch memory
        Params.Add('statement_cache_size=20');         // Prepared statements cache

        // Network parameters
        Params.Add('packet_size=8192');                // Network packet size

        // Character set - important for Unicode support
        Params.Add('codepage=UTF8');
        Params.Add('ClientCharset=UTF8');

        // Connection parameters
        Params.Add('LoginTimeout=10');                 // Timeout in seconds
        //Params.Add('ConnectTimeout=10');             // Alternative parameter name
        Params.Add('MultipleTransactions=False');      // For better performance
      end;
    end;

    // Now establish connection
    fConnection.DatabaseName:= DbName;
    fConnection.UserName:= UserName;
    fConnection.Password:= aPassword;
    fConnection.HostName:= ''; // Must be empty when using TNSNAMES.ora

    fConnection.Connected:= True;

    // Optimize session parameters
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

      // Check for specific Oracle errors
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

      // Ensure connection is closed on error
      if fConnection.Connected then
      begin
        try
          fConnection.Connected:= False;
        except
          // Ignore errors during close operation
        end;
      end;
    end;

    on E: Exception do begin
      // General exceptions
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

      // Ensure connection is closed on error
      if fConnection.Connected then
      begin
        try
          fConnection.Connected:= False;
        except
          // Ignore errors during close operation
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

function TDatabaseModule.RetrieveData(const SqlText: String): TDataSetResult;
begin
  with fSqlQuery do begin
    Close;
    SQL.Clear;
    SQL.Text:= SqlText;
    PacketRecords:= -1;  // Fetch all records
    Prepared;  // Prepare the query for execution

    try
      DisableControls;  // Prevent UI updates during data loading
      Open;
      EnableControls;   // Re-enable UI updates

      Result.DataSource:= fDataSource;

      if RecordCount > 0 then
        Result.Success:= True
      else
        Result.Success:= False;
    finally
      { #note : finally or except block needs to be implemented }
    end;
  end;
end;

function TDatabaseModule.IsConnected: Boolean;
begin
  Result:= fIsConnected;
end;

function TDatabaseModule.GetConnection: TObject;
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
      'CURSOR_SHARING = EXACT ' +      // FORCE can cause issues, EXACT is safer
      'OPTIMIZER_DYNAMIC_SAMPLING = 2 ' +
      'QUERY_REWRITE_ENABLED = TRUE ' +
      'RESULT_CACHE_MODE = MANUAL ' +  // Cache managed by application
      'STATISTICS_LEVEL = BASIC';      // Minimal statistics overhead

    Query.ExecSQL;

    // Additional optimization for large result sets
    // Temporarily separate. { #todo : Test if this makes a difference }
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
  fSqlQuery.MaxIndexesCount:= 100;  // Required for sorting functionality. This allows 50 columns in 2 directions.
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
