{ Copyright ©2025-2026 Hans van Buggenum }
unit QueryDataProvider;

interface

uses
  SysUtils, Classes, SQLDB, variants, uIGWSWDataProvider;

type
  { TQueryDataProvider
    Implements the IGWSWDataProvider interface for TSQLQuery-based data access.
    Provides database query operations with accurate record counting capabilities,
    SQL query manipulation for COUNT operations, and safe error handling.
    Used as an adapter between TSQLQuery components and the GWSW data provider interface. }
  TQueryDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    fQuery: TSQLQuery;           // Reference to the underlying SQL query component
    fCurrentRecord: Integer;     // Current record position (for tracking navigation)

    { Removes SQL comments from query text for COUNT query generation
      Handles both inline comments (--) and block comments (/* ... */)
      Parameters:
        SQL: Original SQL query text
      Returns:
        SQL text without comments, trimmed and consolidated }
    function RemoveSQLComments(const SQL: string): string;

    { Removes ORDER BY, LIMIT, OFFSET and other clauses for COUNT query preparation
      Strips clauses that would affect row counting but not the actual result set
      Parameters:
        SQL: SQL query text (preferably without comments)
      Returns:
        SQL text without sorting, limiting, or pagination clauses }
    function RemoveQueryClauses(const SQL: string): string;

  public
    { Creates a new query data provider instance
      Parameters:
        AQuery: TSQLQuery object to wrap (must not be nil)
        DisableControles: If True, disables UI control updates during operations }
    constructor Create(AQuery: TObject; DisableControles: Boolean = True);

    { Destroys the instance and re-enables UI controls if they were disabled }
    destructor Destroy; override;

    // IGWSWDataProvider interface implementation

    { Opens the underlying query if not already active and resets record position }
    procedure Open;

    { Resets the current record position but does not close the query }
    procedure Close;

    { Moves to the first record in the query result set
      Returns:
        True if successful (query contains data), False if query is empty }
    function First: Boolean;

    { Moves to the last record in the query result set
      Returns:
        True if successful (query contains data), False if query is empty }
    function Last : Boolean;

    { Advances to the next record in the query result set
      Returns:
        True if successful, False if already at end of result set }
    function Next: Boolean;

    { Checks if cursor is at end of the query result set
      Returns:
        True if no more records available, False otherwise }
    function EOF: Boolean;

    { Retrieves the value of a specified field from the current record
      Parameters:
        FieldName: Name of the field to retrieve
      Returns:
        Variant containing field value or Null if field doesn't exist or is null }
    function GetFieldValue(const FieldName: string): Variant;

    { Checks if a field exists in the query result set
      Parameters:
        FieldName: Name of the field to check
      Returns:
        True if field exists, False otherwise }
    function FieldExists(const FieldName: string): Boolean;

    { Retrieves the OBJECT_TYPE value from the current record
      Returns:
        OBJECT_TYPE field value or 'ONBEKEND' (unknown) if field doesn't exist or is empty }
    function GetObjectType: string;

    { Gets the approximate record count from TSQLQuery.RecordCount
      Note: May not be accurate for large datasets or before full traversal
      Returns:
        Approximate number of records in the result set }
    function GetRecordCount: Integer;

    { Gets an accurate record count by executing a modified COUNT query
      Creates a separate query to count rows without sorting/limiting clauses
      Returns:
        Exact number of records that match the query criteria }
    function GetAccurateRecordCount: Integer;

    { Identifies the data provider type
      Returns:
        Constant string 'TQuery' }
    function GetProviderType: string;
  end;

implementation

function TQueryDataProvider.RemoveSQLComments(const SQL: string): string;
var
  Lines: TStringList;
  i, CommentPos: Integer;
  Line: string;
begin
  Lines:= TStringList.Create;
  try
    Lines.Text:= SQL;

    for i:= 0 to Lines.Count - 1 do begin
      Line:= Lines[i];

      // Delete inline comment (-- to end of line)
      CommentPos:= Pos('--', Line);
      if CommentPos > 0 then
        Line:= Copy(Line, 1, CommentPos - 1);

      // Remove block comment /*... */
      while Pos('/*', Line) > 0 do begin
        CommentPos:= Pos('/*', Line);
        if Pos('*/', Line) > CommentPos then begin
          // Comment closes on the same line
          Line:= Copy(Line, 1, CommentPos - 1) +
                 Copy(Line, Pos('*/', Line) + 2, MaxInt);
        end
        else begin
          // Multi-line comment
          Line:= Copy(Line, 1, CommentPos - 1);
          Break;
        end;
      end;

      // Add clean line
      if Trim(Line) <> '' then
        Lines[i]:= Trim(Line)
      else
        Lines[i]:= '';
    end;

    // Remove blank lines and combine
    Result:= '';
    for i:= 0 to Lines.Count - 1 do begin
      if Lines[i] <> '' then begin
        if Result <> '' then
          Result:= Result + ' ';
        Result:= Result + Lines[i];
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TQueryDataProvider.RemoveQueryClauses(const SQL: string): string;
var
  UpperSQL, ResultSQL: string;
  ClausePos: Integer;
  i: Integer;
// List of clauses to remove (affect row selection/sorting but not COUNT result)
const Clauses: array[0..8] of string = (
  'ORDER BY', 'LIMIT', 'OFFSET', 'FETCH FIRST', 'FETCH NEXT',
  'TOP ', 'ROWNUM', 'ROW_NUMBER()', 'FOR UPDATE'
  );
begin
  ResultSQL:= SQL;
  UpperSQL:= UpperCase(ResultSQL);

  // Find and remove any clause
  for i:= 0 to High(Clauses) do begin
    ClausePos:= Pos(Clauses[I], UpperSQL);
    if ClausePos > 0 then begin
      // Go back to beginning of word
      while (ClausePos > 1) and (ResultSQL[ClausePos] <> ' ') do
        Dec(ClausePos);

      ResultSQL:= Copy(ResultSQL, 1, ClausePos - 1);
      UpperSQL:= UpperCase(ResultSQL); // Update uppercase version
    end;
  end;

  // Remove trailing semicolon
  ResultSQL:= Trim(ResultSQL);
  if (ResultSQL <> '') and (ResultSQL[Length(ResultSQL)] = ';') then
    ResultSQL:= Copy(ResultSQL, 1, Length(ResultSQL) - 1);

  Result:= ResultSQL;
end;

constructor TQueryDataProvider.Create(AQuery: TObject; DisableControles: Boolean);
begin
  inherited Create;
  fQuery:= TSQLQuery(AQuery);

  if DisableControles then
    fQuery.DisableControls
  else
    fQuery.EnableControls;

  FCurrentRecord:= 0;
end;

destructor TQueryDataProvider.Destroy;
begin
  // Ensure controls are re-enabled when provider is destroyed
  if Assigned(fQuery) then
    fQuery.EnableControls;

  inherited Destroy;
end;

procedure TQueryDataProvider.Open;
begin
  if not fQuery.Active then
    fQuery.Open;
  fCurrentRecord:= 0;
end;

procedure TQueryDataProvider.Close;
begin
  fCurrentRecord:= 0;
end;

function TQueryDataProvider.First: Boolean;
begin
  fQuery.First;
  fCurrentRecord:= 0;
  Result:= not fQuery.EOF;
end;

function TQueryDataProvider.Last : Boolean;
begin
  fQuery.Last;
  fCurrentRecord:= 0;
  Result:= not fQuery.EOF;
end;

function TQueryDataProvider.Next: Boolean;
begin
  fQuery.Next;
  Inc(fCurrentRecord);
  Result:= not fQuery.EOF;
end;

function TQueryDataProvider.EOF: Boolean;
begin
  Result:= fQuery.EOF;
end;

function TQueryDataProvider.GetFieldValue(const FieldName: string): Variant;
begin
  try
    if fQuery.FieldByName(FieldName).IsNull then
      Result:= Null
    else
      Result:= fQuery.FieldByName(FieldName).AsVariant;
  except
    on E: Exception do
      Result:= Null;
  end;
end;

function TQueryDataProvider.FieldExists(const FieldName: string): Boolean;
begin
  try
    Result:= fQuery.FindField(FieldName) <> nil;
  except
    Result:= False;
  end;
end;

function TQueryDataProvider.GetObjectType: string;
begin
  Result:= Trim(VarToStr(GetFieldValue('OBJECT_TYPE')));

  if Result = '' then
    Result:= 'ONBEKEND';  // Unknown
end;

function TQueryDataProvider.GetRecordCount: Integer;
begin
  Result:= fQuery.RecordCount;
end;

function TQueryDataProvider.GetAccurateRecordCount: Integer;
var
  CountQuery: TSQLQuery;
  MainSQL, CleanSQL: string;
begin
  Result:= 0;

  if not fQuery.Active or (fQuery.SQL.Text = '') then
    Exit;

  try
    // Get original SQL and delete comments
    MainSQL:= fQuery.SQL.Text;
    CleanSQL:= RemoveSQLComments(MainSQL);

    // Remove ORDER BY/LIMIT/OFFSET clauses for COUNT query
    CleanSQL:= RemoveQueryClauses(CleanSQL);

    // Trim and clean
    CleanSQL:= Trim(CleanSQL);

    if CleanSQL = '' then
      Exit;

    // Build COUNT query
    CountQuery:= TSQLQuery.Create(nil);
    try
      CountQuery.DataBase:= fQuery.DataBase;
      CountQuery.Transaction:= fQuery.Transaction;

      CountQuery.ReadOnly:= True;
      CountQuery.SQL.Text:= 'SELECT COUNT(*) as CNT FROM (' + CleanSQL + ')';

      CountQuery.Open;

      if not CountQuery.EOF then
        Result:= CountQuery.FieldByName('CNT').AsInteger;

      CountQuery.Close;
    finally
      CountQuery.Free;
    end;

  except
    on E: Exception do
    begin
      // Fallback to standard RecordCount
      { #todo : This should raise an exception, GetRecordCount is not reliable
        without calling Query.Last first (which is slow for large datasets) }
      Result:= GetRecordCount;
    end;
  end;
end;

function TQueryDataProvider.GetProviderType: string;
begin
  Result:= 'TQuery';
end;

end.
