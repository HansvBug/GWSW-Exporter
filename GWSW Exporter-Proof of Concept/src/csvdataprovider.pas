{******************************************************************************
  MIT License

  Copyright (c) 2025 Hans van Buggenum

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
******************************************************************************}

unit CSVDataProvider;

interface

uses
  SysUtils, Classes, uIGWSWDataProvider;

type
  TCSVDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    FCSVData: TStringList;
    FFieldNames: TStringList;
    FCurrentRecord: Integer;
    FDelimiter: Char;
    procedure ParseCSVLine(const Line: string; Fields: TStrings);
  public
    constructor Create(const Filename: string; Delimiter: Char = ';');
    destructor Destroy; override;

    // IGWSWDataProvider methods
    procedure Open;
    procedure Close;
    function First: Boolean;
    function Next: Boolean;
    function EOF: Boolean;

    function GetFieldValue(const FieldName: string): Variant;
    function FieldExists(const FieldName: string): Boolean;
    function GetObjectType: string;

    function GetRecordCount: Integer;
    function GetProviderType: string;
  end;

implementation

constructor TCSVDataProvider.Create(const Filename: string; Delimiter: Char);
begin
  inherited Create;
  FCSVData := TStringList.Create;
  FFieldNames := TStringList.Create;
  FDelimiter := Delimiter;
  FCSVData.LoadFromFile(Filename);
end;

destructor TCSVDataProvider.Destroy;
begin
  FCSVData.Free;
  FFieldNames.Free;
  inherited Destroy;
end;

procedure TCSVDataProvider.ParseCSVLine(const Line: string; Fields: TStrings);
var
  I: Integer;
  Field: string;
  InQuotes: Boolean;
begin
  Fields.Clear;
  Field := '';
  InQuotes := False;

  for I := 1 to Length(Line) do
  begin
    if Line[I] = '"' then
      InQuotes := not InQuotes
    else if (Line[I] = FDelimiter) and not InQuotes then
    begin
      Fields.Add(Trim(Field));
      Field := '';
    end
    else
      Field := Field + Line[I];
  end;

  if Field <> '' then
    Fields.Add(Trim(Field));
end;

procedure TCSVDataProvider.Open;
var
  FieldLine: string;
  Fields: TStringList;
begin
  if FCSVData.Count > 0 then
  begin
    Fields := TStringList.Create;
    try
      FieldLine := FCSVData[0];
      ParseCSVLine(FieldLine, Fields);
      FFieldNames.Assign(Fields);
    finally
      Fields.Free;
    end;
  end;

  FCurrentRecord := 1;
end;

procedure TCSVDataProvider.Close;
begin
  FCurrentRecord := 0;
end;

function TCSVDataProvider.First: Boolean;
begin
  FCurrentRecord := 1;
  Result := FCurrentRecord < FCSVData.Count;
end;

function TCSVDataProvider.Next: Boolean;
begin
  Inc(FCurrentRecord);
  Result := FCurrentRecord < FCSVData.Count;
end;

function TCSVDataProvider.EOF: Boolean;
begin
  Result := FCurrentRecord >= FCSVData.Count;
end;

function TCSVDataProvider.GetFieldValue(const FieldName: string): Variant;
var
  FieldIndex: Integer;
  Fields: TStringList;
  Value: string;
begin
  FieldIndex := FFieldNames.IndexOf(FieldName);
  if (FieldIndex = -1) or (FCurrentRecord >= FCSVData.Count) then
    Result := Null
  else
  begin
    Fields := TStringList.Create;
    try
      ParseCSVLine(FCSVData[FCurrentRecord], Fields);
      if FieldIndex < Fields.Count then
      begin
        Value := Fields[FieldIndex];
        if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
          Value := Copy(Value, 2, Length(Value) - 2);
        Result := Value;
      end
      else
        Result := Null;
    finally
      Fields.Free;
    end;
  end;
end;

function TCSVDataProvider.FieldExists(const FieldName: string): Boolean;
begin
  Result := FFieldNames.IndexOf(FieldName) <> -1;
end;

function TCSVDataProvider.GetObjectType: string;
begin
  Result := GetFieldValue('OBJECT_TYPE');
end;

function TCSVDataProvider.GetRecordCount: Integer;
begin
  Result := FCSVData.Count - 1;
end;

function TCSVDataProvider.GetProviderType: string;
begin
  Result := 'CSV';
end;

end.
