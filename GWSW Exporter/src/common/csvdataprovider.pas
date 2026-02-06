{ Copyright ©2025-2026 Hans van Buggenum }
unit CSVDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, StrUtils, uIGWSWDataProvider,
  DB, csvdataset;

type
  { TCSVDataProvider }
  TCSVDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    fCSVLines: TStringList;
    fFieldNames: TStringList;
    fCurrentLine: Integer;
    fDelimiter: Char;
    fQuoteChar: Char;
    fHasHeader: Boolean;
    fFileName: String;

    // Cache voor performance
    fParsedLines: array of TStringList;  // Cache van geparsede regels
    fCacheInitialized: Boolean;
    fFieldIndexCache: TStringList;       // Cache voor field index lookups

    function ParseCSVLine(const Line: string): TStringList;
    function CleanFieldName(const Name: string): string;
    function GetFieldIndex(const FieldName: string): Integer;

    // Cache management
    procedure InitializeCache;
    function GetCachedLine(LineIndex: Integer): TStringList;
    procedure ClearCache;

    // Helper functions voor snellere conversie
    function TryConvertToNumber(const ValueStr: string; out NumValue: Variant): Boolean;
  public
    constructor Create(const FileName: string; Delimiter: Char = ','; QuoteChar: Char = '"'; HasHeader: Boolean = True);
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
    function LoadCSVData(out ADataSource: TObject; out ADataSet: TObject): Boolean;

    property Delimiter: Char read fDelimiter write fDelimiter;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    property HasHeader: Boolean read fHasHeader write fHasHeader;
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

  // Snel zoeken door case-insensitive compare
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
  // We initialiseren lazy - alleen wanneer nodig
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

  // Snel checken of het überhaupt een getal zou kunnen zijn
  CleanStr:= Trim(ValueStr);
  if CleanStr = '' then
    Exit;

  // Snel check op speciale gevallen
  if (CleanStr = '0') or (CleanStr = '-0') then
  begin
    NumValue:= 0;
    Result:= True;
    Exit;
  end;

  // Verwijder quotes als die er zijn
  if (Length(CleanStr) > 0) and (CleanStr[1] = fQuoteChar) and
     (CleanStr[Length(CleanStr)] = fQuoteChar) then
    CleanStr:= Copy(CleanStr, 2, Length(CleanStr) - 2);

  CleanStr:= Trim(CleanStr);
  if CleanStr = '' then
    Exit;

  // Verwijder duizendtalscheidingen
  if FormatSettings.ThousandSeparator <> #0 then
    CleanStr:= StringReplace(CleanStr, FormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // Snel check op wetenschappelijke notatie
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

  // Check op decimaal punt/komma
  HasDecimal:= (Pos('.', CleanStr) > 0) or (Pos(',', CleanStr) > 0);

  // Vervang komma door punt voor Europese notatie
  if Pos(',', CleanStr) > 0 then
    CleanStr:= StringReplace(CleanStr, ',', '.', [rfReplaceAll]);

  // Voorbereiden format settings
  FS:= DefaultFormatSettings;
  FS.DecimalSeparator:= '.';
  FS.ThousandSeparator:= #0;

  // Probeer eerst als integer als er geen decimaal is
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
    // Heeft decimaal, check of het eindigt op .0 of .00 etc.
    DotPos:= Pos('.', CleanStr);
    if DotPos > 0 then
    begin
      // Check of alles na de punt alleen nullen zijn
      if StrToIntDef(Copy(CleanStr, DotPos + 1, Length(CleanStr)), -1) = 0 then
      begin
        // Het is een geheel getal zoals 256.0
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

  // Probeer als float
  if TryStrToFloat(CleanStr, FloatValue, FS) then
  begin
    // Check of het eigenlijk een geheel getal is
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

  // Gebruik gecachte regel - dit is de belangrijkste optimalisatie!
  FieldValues:= GetCachedLine(fCurrentLine);
  if FieldValues = nil then
    Exit;

  if FieldIndex < FieldValues.Count then
  begin
    ValueStr:= FieldValues[FieldIndex];

    // Snel check op lege string
    if ValueStr = '' then
      Exit;

    // Verwijder quotes als die aan het begin/einde staan
    if (Length(ValueStr) > 0) and (ValueStr[1] = fQuoteChar) and
       (ValueStr[Length(ValueStr)] = fQuoteChar) then
      ValueStr:= Copy(ValueStr, 2, Length(ValueStr) - 2);

    ValueStr:= Trim(ValueStr);

    if ValueStr = '' then
      Exit;

    // Probeer te converteren naar getal
    if not TryConvertToNumber(ValueStr, Result) then
    begin
      // Geen getal, retourneer als string
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
      raise Exception.Create('Fout bij laden CSV bestand: ' + E.Message);
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
  ClearCache; // Clear oude cache

  if fHasHeader and (fCSVLines.Count > 0) then
  begin
    // Parse header line (cachen voor hergebruik)
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

  // Initialiseer cache
  InitializeCache;
end;

procedure TCSVDataProvider.Close;
begin
  fCurrentLine:= 0;
  // We houden de cache, want we kunnen opnieuw Open aanroepen
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
    Result:= 'ONBEKEND';
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
  CSVDataSet.MaxIndexesCount:= 100;  // !!!!!!!!!!!!!!!!!!! nodig om te kunnen sorteren
  try
    // Configureer TCSVDataset
    CSVDataSet.CSVOptions.FirstLineAsFieldNames:= fHasHeader;
    CSVDataSet.CSVOptions.Delimiter:= fDelimiter;
    CSVDataSet.CSVOptions.QuoteChar:= fQuoteChar;
    CSVDataSet.CSVOptions.IgnoreOuterWhitespace:= True;

    CSVDataSet.FileName:= fFileName;
    CSVDataSet.Active:= True;  // Dit raakt een bug die in de huidige pascal versie zit.   Door de set op actief te zetten wordt een geheugenlek gemaakt.

    if CSVDataSet.FieldCount = 0 then
    begin
      CSVDataSet.Free;
      Exit;
    end;

    // Creëer DataSource
    lDataSource:= TDataSource.Create(nil);
    lDataSource.DataSet:= CSVDataSet;

    // Return als TObject
    ADataSource:= lDataSource;
    ADataSet:= CSVDataSet;

    Result:= True;

  except
    on E: Exception do
    begin
      // Cleanup bij fout
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
