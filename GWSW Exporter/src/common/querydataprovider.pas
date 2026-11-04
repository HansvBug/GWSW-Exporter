{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
unit QueryDataProvider;

interface

uses
  SysUtils, Classes, SQLDB, DB, variants, uIGWSWDataProvider;

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
    function Last: Boolean;

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
  N, i: Integer;
  InString: Boolean;
  InBlockComment: Boolean;
  InInlineComment: Boolean;
  C, NextC: Char;
begin
  Result:= '';
  N:= Length(SQL);
  i:= 1;
  InString:= False;
  InBlockComment:= False;
  InInlineComment:= False;

  while i <= N do
  begin
    C:= SQL[i];
    NextC:= #0;
    if i < N then
      NextC:= SQL[i + 1];

    { 1) Regelcommentaar: sla over tot het einde van de regel.
         Vervang het einde van de regel door één spatie zodat
         woordgrenzen behouden blijven. }
    if InInlineComment then begin
      if C in [#10, #13] then begin
        InInlineComment:= False;
        Result:= Result + ' ';
      end;
      Inc(i);
      Continue;
    end;

    { 2) Blokcommentaar: wacht op '*/'. Vervang het hele commentaar door één spatie. }
    if InBlockComment then begin
      if (C = '*') and (NextC = '/') then begin
        InBlockComment:= False;
        Result:= Result + ' ';
        Inc(i, 2);
      end
      else
        Inc(i);
      Continue;
    end;

    { 3) In een string-literal: alles tot en met de afsluitende quote
         letterlijk overnemen. '' is een escape voor één quote. }
    if InString then begin
      Result:= Result + C;
      if C = '''' then begin
        if NextC = '''' then begin
          Result:= Result + NextC;
          Inc(i, 2);
          Continue;
        end;
        InString:= False;
      end;
      Inc(i);
      Continue;
    end;

    { 4) Buiten alles: kijk of er een string of commentaar begint. }
    if C = '''' then begin
      InString:= True;
      Result:= Result + C;
      Inc(i);
      Continue;
    end;

    if (C = '-') and (NextC = '-') then begin
      InInlineComment:= True;
      Inc(i, 2);
      Continue;
    end;

    if (C = '/') and (NextC = '*') then begin
      InBlockComment:= True;
      Inc(i, 2);
      Continue;
    end;

    { 5) Gewoon teken: overnemen. }
    Result:= Result + C;
    Inc(i);
  end;

  { Witruimte normaliseren: tabs, CR, LF → spatie; reeksen → één spatie.
    Dit is functioneel vergelijkbaar met het oude "regels joinen met spatie",
    maar zonder de bijbehorende bugs. }
  Result:= StringReplace(Result, #9,  ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, #10, ' ', [rfReplaceAll]);
  while Pos('  ', Result) > 0 do
    Result:= StringReplace(Result, '  ', ' ', [rfReplaceAll]);

  Result:= Trim(Result);
end;

function TQueryDataProvider.RemoveQueryClauses(const SQL: string): string;

  function IsWordChar(C: Char): Boolean; inline;
  begin
    // Letters, cijfers, underscore: voldoende voor SQL-identifiers.
    Result:= (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;

  function MatchesClauseAt(const UpperSQL: string; Pos: Integer;
                           const Clause: string): Boolean;
  var
    k, NextPos: Integer;
  begin
    Result:= False;
    if Pos + Length(Clause) - 1 > Length(UpperSQL) then Exit;

    // Case-insensitieve vergelijking (UpperSQL is al uppercase).
    for k:= 1 to Length(Clause) do
      if UpperSQL[Pos + k - 1] <> Clause[k] then Exit;

    // Moet op een woordgrens beginnen.
    if (Pos > 1) and IsWordChar(UpperSQL[Pos - 1]) then Exit;

    // Moet op een woordgrens eindigen.
    NextPos:= Pos + Length(Clause);
    if (NextPos <= Length(UpperSQL)) and IsWordChar(UpperSQL[NextPos]) then Exit;

    Result:= True;
  end;

const
  Clauses: array[0..8] of string = (
    'ORDER BY', 'LIMIT', 'OFFSET', 'FETCH FIRST', 'FETCH NEXT',
    'TOP', 'ROWNUM', 'ROW_NUMBER()', 'FOR UPDATE'
    );
var
  UpperSQL: string;
  i, j, N, ClausePos: Integer;
  InString: Boolean;
  C: Char;
begin
  UpperSQL:= UpperCase(SQL);
  N:= Length(SQL);
  InString:= False;
  ClausePos:= 0;

  i:= 1;
  while i <= N do
  begin
    C:= SQL[i];

    if C = '''' then begin
      InString:= not InString;
      Inc(i);
      Continue;
    end;

    if not InString then begin
      for j:= 0 to High(Clauses) do
        if MatchesClauseAt(UpperSQL, i, Clauses[j]) then begin
          ClausePos:= i;
          Break;
        end;
      if ClausePos > 0 then Break;
    end;

    Inc(i);
  end;

  if ClausePos > 0 then
    Result:= Copy(SQL, 1, ClausePos - 1)
  else
    Result:= SQL;

  Result:= Trim(Result);
  if (Result <> '') and (Result[Length(Result)] = ';') then
    Result:= Trim(Copy(Result, 1, Length(Result) - 1));
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

function TQueryDataProvider.Last: Boolean;
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
var
  fld: TField;
begin
  fld:= fQuery.FindField(FieldName);   // geeft nil als niet gevonden, geen exception
  if fld = nil then
    Result:= Null
  else if fld.IsNull then
    Result:= Null
  else
    Result:= fld.AsVariant;
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
