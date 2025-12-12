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

unit QueryDataProvider;

interface

uses
  SysUtils, Classes, ZDataset, uIGWSWDataProvider;

type

  { TQueryDataProvider }

  TQueryDataProvider = class(TInterfacedObject, IGWSWDataProvider)
  private
    FQuery: TZQuery;
    FCurrentRecord: Integer;
  public
    constructor Create(AQuery: TZQuery);
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

constructor TQueryDataProvider.Create(AQuery : TZQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FCurrentRecord := 0;
end;

destructor TQueryDataProvider.Destroy;
begin
  inherited Destroy;
end;

procedure TQueryDataProvider.Open;
begin
  if not FQuery.Active then
    FQuery.Open;
  FCurrentRecord := 0;
end;

procedure TQueryDataProvider.Close;
begin
  FCurrentRecord := 0;
end;

function TQueryDataProvider.First: Boolean;
begin
  FQuery.First;
  FCurrentRecord := 0;
  Result := not FQuery.EOF;
end;

function TQueryDataProvider.Next: Boolean;
begin
  FQuery.Next;
  Inc(FCurrentRecord);
  Result := not FQuery.EOF;
end;

function TQueryDataProvider.EOF: Boolean;
begin
  Result := FQuery.EOF;
end;

function TQueryDataProvider.GetFieldValue(const FieldName: string): Variant;
begin
  try
    if FQuery.FieldByName(FieldName).IsNull then
      Result := Null
    else
      Result := FQuery.FieldByName(FieldName).Value;
  except
    on E: Exception do
      Result := Null;
  end;
end;

function TQueryDataProvider.FieldExists(const FieldName: string): Boolean;
begin
  try
    Result := FQuery.FindField(FieldName) <> nil;
  except
    Result := False;
  end;
end;

function TQueryDataProvider.GetObjectType: string;
begin
  Result := GetFieldValue('OBJECT_TYPE');

  if Result = '' then
    Result := 'ONBEKEND'; { #todo : Moet een melding naar de view sturen }
end;

function TQueryDataProvider.GetRecordCount: Integer;
begin
  Result := FQuery.RecordCount;
end;

function TQueryDataProvider.GetProviderType: string;
begin
  Result := 'TQuery';
end;

end.
