{ Copyright ©2025-2026 Hans van Buggenum }
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
    Success: Boolean;
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
    Result.Message:= 'DbConnNotInitialized' ; //'Database connection not initialized';
    fIsConnected := False;
    Exit;
  end;

  with fZConnection do begin
    // General
    Protocol:= 'Oracle';
    AutoCommit:= False;
    ReadOnly:= True;
    TransactIsolationLevel:= tiNone;
    UseMetadata:= False;  // Data retrieval should now be a little faster...

    // Connection data
    Database:= DbName;
    User:= UserName;
    Password:= aPassword;

    // Properties that should make retrieving the data faster
    Properties.Clear;
    // Oracle-specific optimizations (don't help anything, network stays slow)
    Properties.Add('ora_metadata=0');
    Properties.Add('arraysize=5000'); // Number of rows per fetch
    Properties.Add('prefetch_rows=5000'); // Prefetch count
    Properties.Add('statement_cache_size=50');
    Properties.Add('compress=1'); // Network compression
    Properties.Add('packet_size=32768'); // Bigger packets
    // Properties.Add('sendblob=1'); // Blobs in one go
    Properties.Add('pooled=true');
    Properties.Add('max_connections=15');
    Properties.Add('min_connections=3');
    // Character set settings
    Properties.Add('codepage=UTF8');
    Properties.Add('controls_cp=CP_UTF8');
    // Specific Oracle driver optimizations
    Properties.Add('BindDoubleAsString=0');
    Properties.Add('BindDecimalAsString=0');

    Catalog := ''; // Oracle doesn't use a catalog
  end;

  try
    fZConnection.AutoCommit:= True; // So: Necessary because: TransactIsolationLevel:= tiReadCommitted.
    fZConnection.Connected:= True;
    fZConnection.TransactIsolationLevel:= tiReadCommitted; // For read-only queries, this is the best option. mar LET OP AutoCommit moert dan op true staan anders blijft de query naar dezelfde data kijken. Altijd eerst de transactie committen

    OptimizeOracleSession(fZConnection);

    fIsConnected:= True;
    Result.Success:= True;
    Result.Message:= 'Success';
  Except
    on E: EZSQLException do begin
      ErrorMsg := 'DbError' + '|' + ' (' + E.ErrorCode.ToString + '): ';

      // Check for specific Oracle error codes
      case E.ErrorCode of
        12154: ErrorMsg := ErrorMsg + '|' + 'CannotFindConnIdentifier';  // 'Cannot find the connection identifier. Check TNSNAMES.ORA';
        1017:  ErrorMsg := ErrorMsg + '|' + 'InvalidUserOrPwd';          // 'Invalid username/password';
        12541: ErrorMsg := ErrorMsg + '|' + 'NoListener';                // 'No listener on the server';
        12514: ErrorMsg := ErrorMsg + '|' + 'ListenerDoesNotKnowServer'; // 'Listener doesn''t know the service';
        12505: ErrorMsg := ErrorMsg + '|' + 'SIDNotFound';               // 'SID not found';
        1005:  ErrorMsg := ErrorMsg + '|' + 'NUllPwd';                   // 'Null password given; logon denied';
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
        ErrorMsg:= 'ServerNotFound'                         // 'Netwerkfout: Kan server niet vinden.'
      else if Pos('timeout', LowerCase(E.Message)) > 0 then
        ErrorMsg:= 'ServerNotResponding'                    // 'Timeout: Server reageert niet.'
      else if Pos('network', LowerCase(E.Message)) > 0 then
        ErrorMsg:= 'CheckConnection'                        //'Netwerkfout: Controleer uw verbinding.'
      else
        ErrorMsg:= 'Error: ' + E.Message;

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
    EmptyDataSet;

    SQL.Clear;
    SQL.Text:= SqlText;

    FetchRow:= 1000;       // Disbled makes retrieving data verry slow but it it needed for the progress counter
    CachedUpdates:= True;  // Should make it faster on the network. --> Difference is not noticeable
    Prepared:= True;       { Makes no sense. The query should only be extracted once.}
    ReadOnly:= True;

    try
      DisableControls;
      Open;
      EnableControls;

      Result.DataSet:= fDataSource;

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
  Result:= fZConnection;
end;

procedure TDatabaseModule.OptimizeOracleSession(Connection : TZConnection);
var
  Query: TZQuery;
begin
  Query:= TZQuery.Create(nil);
  try
    Query.Connection:= Connection;

    // Set session parameters for better performance
    Query.SQL.Text :=
      'ALTER SESSION SET ' +
      'NLS_DATE_FORMAT = ''YYYY-MM-DD HH24:MI:SS'' ' +
      'NLS_NUMERIC_CHARACTERS = ''.,'' ' +
      'OPTIMIZER_MODE = ALL_ROWS ' +
      'CURSOR_SHARING = FORCE ' +     // Usually better for performance
      'OPTIMIZER_DYNAMIC_SAMPLING = 4 ' +
      'DB_FILE_MULTIBLOCK_READ_COUNT = 128 ' +  // Higher read count
      'SORT_AREA_SIZE = 1048576 ' +             // Larger sort area
      'HASH_AREA_SIZE = 1048576';

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


