{ Copyright ©2025-2026 Hans van Buggenum }
unit QueryDataProvider;

interface

uses
  SysUtils, Classes, SQLDB, variants, uIGWSWDataProvider;

type

  { TQueryDataProvider }
  TQueryDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    fQuery: TSQLQuery;
    fCurrentRecord: Integer;

    function RemoveSQLComments(const SQL: string): string;
    function RemoveQueryClauses(const SQL: string): string;
  public
    constructor Create(AQuery: TObject; DisableControles: Boolean = True);
    destructor Destroy; override;

    // IGWSWDataProvider methods
    procedure Open;
    procedure Close;
    function First: Boolean;
    function Last: Boolean;
    function Next: Boolean;
    function EOF: Boolean;

    function GetFieldValue(const FieldName: string): Variant;
    function FieldExists(const FieldName: string): Boolean;
    function GetObjectType: string;

    function GetRecordCount: Integer;
    function GetAccurateRecordCount: Integer;
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
      CommentPos := Pos('--', Line);
      if CommentPos > 0 then
        Line := Copy(Line, 1, CommentPos - 1);

      // Remove block comment /*... */
      while Pos('/*', Line) > 0 do begin
        CommentPos:= Pos('/*', Line);
        if Pos('*/', Line) > CommentPos then begin
          // Commentaar sluit op dezelfde regel
          Line:= Copy(Line, 1, CommentPos - 1) +
                 Copy(Line, Pos('*/', Line) + 2, MaxInt);
        end
        else begin
          // Commentaar gaat over meerdere regels
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
    for i := 0 to Lines.Count - 1 do begin
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
// List of clauses to remove
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
  // Zorg dat controls weer geactiveerd worden
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
    Result:= 'ONBEKEND';
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

    // Remove ORDER BY/LIMIT/OFFSET voor COUNT query
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
      // Fallback naar normale RecordCount
      { #todo : Hier moet een raise komen, Get recordcount is niet handig want resultaat is onjuist zolang er geen queryl.last is gedaan (traag) }
      Result:= GetRecordCount;
    end;
  end;
end;

function TQueryDataProvider.GetProviderType: string;
begin
  Result:= 'TQuery';
end;


end.
