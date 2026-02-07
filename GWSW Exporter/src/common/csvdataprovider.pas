{ Copyright ©2025-2026 Hans van Buggenum }
unit CSVDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, StrUtils, uIGWSWDataProvider,
  DB, csvdataset;

type
  { TCSVDataProvider
    Implements the IGWSWDataProvider interface for CSV file data access.
    Provides efficient CSV parsing with performance caching, automatic type detection,
    and integration with TDBGrid components through TCSVDataset. }
  TCSVDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    fCSVLines: TStringList;          // Raw text lines loaded from CSV file
    fFieldNames: TStringList;        // Column names (from header or generated as FIELD1, FIELD2...)
    fCurrentLine: Integer;           // Current record position (0-based index)
    fDelimiter: Char;                // Field delimiter character (default: ',')
    fQuoteChar: Char;                // Text qualifier character (default: '"')
    fHasHeader: Boolean;             // Indicates if first line contains column names
    fFileName: String;               // Full path to the CSV source file

    // Performance optimization cache
    fParsedLines: array of TStringList;  // Cache of parsed CSV lines (lazy initialization)
    fCacheInitialized: Boolean;          // Flag indicating cache array is initialized

    { Parses a CSV line into individual fields, respecting quoted text and escaped characters
      Parameters:
        Line: Raw CSV text line
      Returns:
        TStringList containing individual field values }
    function ParseCSVLine(const Line: string): TStringList;

    { Removes surrounding quote characters and trims whitespace from field names
      Parameters:
        Name: Raw field name potentially enclosed in quotes
      Returns:
        Cleaned field name suitable for comparison }
    function CleanFieldName(const Name: string): string;

    { Locates the zero-based index of a field by name (case-insensitive search)
      Parameters:
        FieldName: Name of the field to locate
      Returns:
        Field index or -1 if field does not exist }
    function GetFieldIndex(const FieldName: string): Integer;

    // Cache management methods
    { Initializes the line parsing cache (lazy initialization pattern) }
    procedure InitializeCache;

    { Retrieves a parsed line from cache, parsing on first access if necessary
      Parameters:
        LineIndex: Zero-based line number
      Returns:
        Parsed TStringList or nil if index is out of range }
    function GetCachedLine(LineIndex: Integer): TStringList;

    { Releases all cached parsed lines to free memory }
    procedure ClearCache;

    // Type conversion helpers
    { Attempts to convert a string value to appropriate numeric type (Integer or Float)
      Handles various number formats including scientific notation and European decimals
      Parameters:
        ValueStr: String representation of potential number
        NumValue: Output variant containing converted numeric value
      Returns:
        True if successful conversion to number, False if string is not numeric }
    function TryConvertToNumber(const ValueStr: string; out NumValue: Variant): Boolean;

  public
    { Creates a new CSV data provider instance and loads the specified file
      Parameters:
        FileName: Path to the CSV file (required)
        Delimiter: Field separator character (default: ',')
        QuoteChar: Text qualifier character (default: '"')
        HasHeader: Indicates if first line contains column names (default: True) }
    constructor Create(const FileName: string; Delimiter: Char = ','; QuoteChar: Char = '"'; HasHeader: Boolean = True);

    { Destroys the instance and releases all allocated resources }
    destructor Destroy; override;

    // IGWSWDataProvider interface implementation

    { Prepares the CSV file for reading: parses header (if present), initializes cache,
      and positions cursor at first data record }
    procedure Open;

    { Resets reading position to beginning but preserves cache for potential reuse }
    procedure Close;

    { Moves to the first data record (skips header line if HasHeader is True)
      Returns:
        True if successful (file contains data), False if file is empty }
    function First: Boolean;

    { Moves to the last data record in the file
      Returns:
        True if successful, False if file contains no data records }
    function Last: Boolean;

    { Advances to the next data record
      Returns:
        True if successful, False if already at end of file }
    function Next: Boolean;

    { Checks if cursor is positioned at end of file (no more records)
      Returns:
        True if no more records available, False otherwise }
    function EOF: Boolean;

    { Retrieves the value of a specified field from the current record
      Performs automatic type detection: returns numeric types when possible,
      otherwise returns string values. Removes surrounding quotes from values.
      Parameters:
        FieldName: Name of the field to retrieve
      Returns:
        Variant containing field value (numeric if convertible, otherwise string) }
    function GetFieldValue(const FieldName: string): Variant;

    { Checks if a field exists in the dataset
      Parameters:
        FieldName: Name of the field to check
      Returns:
        True if field exists, False otherwise }
    function FieldExists(const FieldName: string): Boolean;

    { Identifies object type by checking common column name variations
      Searches for columns named OBJECT_TYPE, OBJECTTYPE, or TYPE
      Returns:
        Object type string or 'ONBEKEND' (unknown) if not found }
    function GetObjectType: string;

    { Gets the number of data records in the CSV file
      Excludes header line from count if HasHeader is True
      Returns:
        Count of data records }
    function GetRecordCount: Integer;

    { Provides accurate record count (identical to GetRecordCount for CSV files)
      Returns:
        Count of data records }
    function GetAccurateRecordCount: Integer;

    { Identifies the data provider type
      Returns:
        Constant string 'CSV' }
    function GetProviderType: string;

    { Creates a TCSVDataset and TDataSource for integration with TDBGrid components
      Configures dataset with CSV parsing options and sets MaxIndexesCount to 100
      to enable sorting functionality through DbGridHelper
      Parameters:
        ADataSource: Output TDataSource object (caller assumes ownership)
        ADataSet: Output TCSVDataset object (caller assumes ownership)
      Returns:
        True if successful, False on error (file not found, parsing error, etc.)
      Note: There is a known memory leak in the current Pascal version when
            setting Active:=True on TCSVDataset
      This has been solved by including csvdataset.pas from the pascal fixes version in this project. }
    function LoadCSVData(out ADataSource: TObject; out ADataSet: TObject): Boolean;

    // Configuration properties
    property Delimiter: Char read fDelimiter write fDelimiter;    // Field separator character
    property QuoteChar: Char read fQuoteChar write fQuoteChar;    // Text qualifier character
    property HasHeader: Boolean read fHasHeader write fHasHeader; // Header presence flag
  end;

implementation

{ TCSVDataProvider }

function TCSVDataProvider.ParseCSVLine(const Line: string): TStringList;
var
  I: Integer;
  Len: Integer;
  InQuotes: Boolean;
  CurrentField: string;
  Ch: Char;
begin
  Result:= TStringList.Create;
  try
    InQuotes:= False;
    CurrentField:= '';
    I:= 1;
    Len:= Length(Line);

    while I <= Len do
    begin
      Ch:= Line[I];

      if Ch = fQuoteChar then
      begin
        if InQuotes and (I < Len) and (Line[I+1] = fQuoteChar) then
        begin
          // Escaped quote - add one quote to the field
          CurrentField:= CurrentField + Ch;
          Inc(I); // Skip the next quote character
        end;
        InQuotes:= not InQuotes;
      end
      else if (Ch = fDelimiter) and not InQuotes then
      begin
        // End of field
        Result.Add(CurrentField);
        CurrentField:= '';
      end
      else
      begin
        CurrentField:= CurrentField + Ch;
      end;

      Inc(I);
    end;

    // Add the last field
    Result.Add(CurrentField);
  except
    Result.Free;
    raise;
  end;
end;

function TCSVDataProvider.CleanFieldName(const Name: string): string;
begin
  // Remove quotes and trim
  Result:= Trim(Name);
  if (Length(Result) > 0) and (Result[1] = fQuoteChar) and
     (Result[Length(Result)] = fQuoteChar) then
    Result:= Copy(Result, 2, Length(Result) - 2);
  Result:= Trim(Result);
end;

function TCSVDataProvider.GetFieldIndex(const FieldName: string): Integer;
var
  CleanName: string;
  I: Integer;
begin
  CleanName:= CleanFieldName(FieldName);

  // Fast search using case-insensitive comparison
  for I:= 0 to fFieldNames.Count - 1 do
  begin
    if SameText(CleanFieldName(fFieldNames[I]), CleanName) then
    begin
      Result:= I;
      Exit;
    end;
  end;

  Result:= -1;
end;

// Cache management
procedure TCSVDataProvider.InitializeCache;
begin
  if fCacheInitialized or (fCSVLines = nil) then
    Exit;

  SetLength(fParsedLines, fCSVLines.Count);
  // Lazy initialization - parse only when needed
  fCacheInitialized:= True;
end;

function TCSVDataProvider.GetCachedLine(LineIndex: Integer): TStringList;
begin
  if not fCacheInitialized then
    InitializeCache;

  if (LineIndex < 0) or (LineIndex >= Length(fParsedLines)) then
    Exit(nil);

  if fParsedLines[LineIndex] = nil then
    fParsedLines[LineIndex]:= ParseCSVLine(fCSVLines[LineIndex]);

  Result:= fParsedLines[LineIndex];
end;

procedure TCSVDataProvider.ClearCache;
var
  i: Integer;
begin
  for i:= 0 to Length(fParsedLines) - 1 do
    if fParsedLines[i] <> nil then
      FreeAndNil(fParsedLines[i]);
  SetLength(fParsedLines, 0);
  fCacheInitialized:= False;
end;

function TCSVDataProvider.TryConvertToNumber(const ValueStr: string; out NumValue: Variant): Boolean;
var
  CleanStr: string;
  IntValue: Int64;
  FloatValue: Double;
  FS: TFormatSettings;
  HasDecimal: Boolean;
  DotPos: Integer;
begin
  Result:= False;
  NumValue:= Unassigned;

  if ValueStr = '' then
    Exit;

  // Quick check if value could potentially be a number
  CleanStr:= Trim(ValueStr);
  if CleanStr = '' then
    Exit;

  // Fast check for special cases
  if (CleanStr = '0') or (CleanStr = '-0') then
  begin
    NumValue:= 0;
    Result:= True;
    Exit;
  end;

  // Remove quotes if present
  if (Length(CleanStr) > 0) and (CleanStr[1] = fQuoteChar) and
     (CleanStr[Length(CleanStr)] = fQuoteChar) then
    CleanStr:= Copy(CleanStr, 2, Length(CleanStr) - 2);

  CleanStr:= Trim(CleanStr);
  if CleanStr = '' then
    Exit;

  // Remove thousand separators
  if FormatSettings.ThousandSeparator <> #0 then
    CleanStr:= StringReplace(CleanStr, FormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // Quick check for scientific notation
  if (Pos('e', LowerCase(CleanStr)) > 0) or (Pos('E', CleanStr) > 0) then
  begin
    FS:= DefaultFormatSettings;
    FS.DecimalSeparator:= '.';
    if TryStrToFloat(CleanStr, FloatValue, FS) then
    begin
      NumValue:= FloatValue;
      Result:= True;
    end;
    Exit;
  end;

  // Check for decimal point/comma
  HasDecimal:= (Pos('.', CleanStr) > 0) or (Pos(',', CleanStr) > 0);

  // Replace comma with dot for European notation
  if Pos(',', CleanStr) > 0 then
    CleanStr:= StringReplace(CleanStr, ',', '.', [rfReplaceAll]);

  // Prepare format settings
  FS:= DefaultFormatSettings;
  FS.DecimalSeparator:= '.';
  FS.ThousandSeparator:= #0;

  // Try as integer first if no decimal point
  if not HasDecimal then
  begin
    if TryStrToInt64(CleanStr, IntValue) then
    begin
      NumValue:= IntValue;
      Result:= True;
      Exit;
    end;
  end
  else
  begin
    // Has decimal, check if it ends with .0 or .00 etc.
    DotPos:= Pos('.', CleanStr);
    if DotPos > 0 then
    begin
      // Check if everything after the decimal point are only zeros
      if StrToIntDef(Copy(CleanStr, DotPos + 1, Length(CleanStr)), -1) = 0 then
      begin
        // It's a whole number like 256.0
        CleanStr:= Copy(CleanStr, 1, DotPos - 1);
        if TryStrToInt64(CleanStr, IntValue) then
        begin
          NumValue:= IntValue;
          Result:= True;
          Exit;
        end;
      end;
    end;
  end;

  // Try as floating point number
  if TryStrToFloat(CleanStr, FloatValue, FS) then
  begin
    // Check if it's actually a whole number
    if Abs(FloatValue - Trunc(FloatValue)) < 0.00000001 then
      NumValue:= Trunc(FloatValue)  // Return as integer
    else
      NumValue:= FloatValue;        // Return as float
    Result:= True;
  end;
end;

function TCSVDataProvider.GetFieldValue(const FieldName: string): Variant;
var
  FieldIndex: Integer;
  FieldValues: TStringList;
  ValueStr: string;
begin
  Result:= Null;

  if (fCurrentLine < 0) or (fCurrentLine >= fCSVLines.Count) then
    Exit;

  FieldIndex:= GetFieldIndex(FieldName);
  if FieldIndex = -1 then
    Exit;

  // Use cached line - this is the key performance optimization!
  FieldValues:= GetCachedLine(fCurrentLine);
  if FieldValues = nil then
    Exit;

  if FieldIndex < FieldValues.Count then
  begin
    ValueStr:= FieldValues[FieldIndex];

    // Quick check for empty string
    if ValueStr = '' then
      Exit;

    // Remove quotes if present at beginning/end
    if (Length(ValueStr) > 0) and (ValueStr[1] = fQuoteChar) and
       (ValueStr[Length(ValueStr)] = fQuoteChar) then
      ValueStr:= Copy(ValueStr, 2, Length(ValueStr) - 2);

    ValueStr:= Trim(ValueStr);

    if ValueStr = '' then
      Exit;

    // Try to convert to number
    if not TryConvertToNumber(ValueStr, Result) then
    begin
      // Not a number, return as string
      Result:= ValueStr;
    end;
  end;
end;

constructor TCSVDataProvider.Create(const FileName: string; Delimiter: Char; QuoteChar: Char; HasHeader: Boolean);
begin
  inherited Create;

  fCSVLines:= TStringList.Create;
  fFieldNames:= TStringList.Create;
  fCurrentLine:= 0;
  fDelimiter:= Delimiter;
  fQuoteChar:= QuoteChar;
  fHasHeader:= HasHeader;
  fCacheInitialized:= False;
  SetLength(fParsedLines, 0);
  fFileName:= FileName;
  try
    fCSVLines.LoadFromFile(FileName);
  except
    on E: Exception do
      raise Exception.Create('Error loading CSV file: ' + E.Message);
  end;
end;

destructor TCSVDataProvider.Destroy;
begin
  ClearCache;
  fCSVLines.Free;
  fFieldNames.Free;
  inherited Destroy;
end;

procedure TCSVDataProvider.Open;
var
  i: Integer;
  TempList: TStringList;
begin
  fCurrentLine:= 0;
  ClearCache; // Clear old cache

  if fHasHeader and (fCSVLines.Count > 0) then
  begin
    // Parse header line (cache for reuse)
    TempList:= ParseCSVLine(fCSVLines[0]);
    try
      fFieldNames.Assign(TempList);
    finally
      TempList.Free;
    end;
    fCurrentLine:= 1; // Start after header
  end
  else if fCSVLines.Count > 0 then
  begin
    // Generate field names (FIELD1, FIELD2, ...)
    fFieldNames.Clear;
    // Parse first line to determine number of fields
    TempList:= ParseCSVLine(fCSVLines[0]);
    try
      for i:= 0 to TempList.Count - 1 do
        fFieldNames.Add('FIELD' + IntToStr(i+1));
    finally
      TempList.Free;
    end;
  end;

  // Initialize cache
  InitializeCache;
end;

procedure TCSVDataProvider.Close;
begin
  fCurrentLine:= 0;
  // Keep cache intact for potential future Open calls
end;

function TCSVDataProvider.First: Boolean;
begin
  if fHasHeader then
    fCurrentLine:= 1
  else
    fCurrentLine:= 0;

  Result:= fCurrentLine < fCSVLines.Count;
end;

function TCSVDataProvider.Last: Boolean;
begin
  fCurrentLine:= fCSVLines.Count - 1;
  Result:= fCurrentLine >= 0;
  if fHasHeader and (fCurrentLine = 0) then
    Result:= False;
end;

function TCSVDataProvider.Next: Boolean;
begin
  if fCurrentLine < fCSVLines.Count - 1 then
  begin
    Inc(fCurrentLine);
    Result:= True;
  end
  else
    Result:= False;
end;

function TCSVDataProvider.EOF: Boolean;
begin
  Result:= fCurrentLine >= fCSVLines.Count;
  if fHasHeader and (fCurrentLine = 0) and (fCSVLines.Count > 0) then
    Result:= False;
end;

function TCSVDataProvider.FieldExists(const FieldName: string): Boolean;
begin
  Result:= GetFieldIndex(FieldName) >= 0;
end;

function TCSVDataProvider.GetObjectType: string;
begin
  Result:= '';

  // Try to get OBJECT_TYPE from CSV
  if FieldExists('OBJECT_TYPE') then
    Result:= Trim(VarToStr(GetFieldValue('OBJECT_TYPE')))
  else if FieldExists('OBJECTTYPE') then
    Result:= Trim(VarToStr(GetFieldValue('OBJECTTYPE')))
  else if FieldExists('TYPE') then
    Result:= Trim(VarToStr(GetFieldValue('TYPE')));

  if Result = '' then
    Result:= 'ONBEKEND';  // Unknown
end;

function TCSVDataProvider.GetRecordCount: Integer;
begin
  Result:= fCSVLines.Count;
  if fHasHeader and (Result > 0) then
    Dec(Result);
end;

function TCSVDataProvider.GetAccurateRecordCount: Integer;
begin
  Result:= GetRecordCount;
end;

function TCSVDataProvider.GetProviderType: string;
begin
  Result:= 'CSV';
end;

function TCSVDataProvider.LoadCSVData(out ADataSource: TObject; out ADataSet: TObject): Boolean;
var
  CSVDataSet: TCSVDataset = nil;
  lDataSource: TDataSource;
begin
  Result:= False;
  ADataSource:= nil;
  ADataSet:= nil;

  CSVDataSet:= TCSVDataset.Create(nil);
  CSVDataSet.MaxIndexesCount:= 100;  // Required to enable sorting functionality
  try
    // Configure TCSVDataset
    CSVDataSet.CSVOptions.FirstLineAsFieldNames:= fHasHeader;
    CSVDataSet.CSVOptions.Delimiter:= fDelimiter;
    CSVDataSet.CSVOptions.QuoteChar:= fQuoteChar;
    CSVDataSet.CSVOptions.IgnoreOuterWhitespace:= True;

    CSVDataSet.FileName:= fFileName;

    { Note: This triggers a known memory leak in current Pascal version
      This has been solved by including csvdataset.pas from the pascal fixes version in this project.
      --> So Activie:= true can be safely applied here
    }
    CSVDataSet.Active:= True;

    if CSVDataSet.FieldCount = 0 then
    begin
      CSVDataSet.Free;
      Exit;
    end;

    // Create DataSource
    lDataSource:= TDataSource.Create(nil);
    lDataSource.DataSet:= CSVDataSet;

    // Return as TObject
    ADataSource:= lDataSource;
    ADataSet:= CSVDataSet;

    Result:= True;

  except
    on E: Exception do
    begin
      // Cleanup on error
      if Assigned(lDataSource) then
        lDataSource.Free
      else if Assigned(CSVDataSet) then
        CSVDataSet.Free;

      ADataSource:= nil;
      ADataSet:= nil;
      Result:= False;
    end;
  end;
end;

end.
