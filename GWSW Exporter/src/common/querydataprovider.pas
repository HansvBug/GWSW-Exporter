{ Copyright ©2025 Hans van Buggenum }
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
    constructor Create(AQuery: TObject);
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
    function GetProviderType: string;
  end;

implementation

constructor TQueryDataProvider.Create(AQuery : TObject);
begin
  inherited Create;
  FQuery:= TZQuery(AQuery);
  FCurrentRecord:= 0;
end;

destructor TQueryDataProvider.Destroy;
begin
  inherited Destroy;
end;

procedure TQueryDataProvider.Open;
begin
  if not FQuery.Active then
    FQuery.Open;
  FCurrentRecord:= 0;
end;

procedure TQueryDataProvider.Close;
begin
  FCurrentRecord:= 0;
end;

function TQueryDataProvider.First: Boolean;
begin
  FQuery.First;
  FCurrentRecord:= 0;
  Result:= not FQuery.EOF;
end;

function TQueryDataProvider.Last : Boolean;
begin
  FQuery.Last;
  FCurrentRecord:= 0;
  Result:= not FQuery.EOF;
end;

function TQueryDataProvider.Next: Boolean;
begin
  FQuery.Next;
  Inc(FCurrentRecord);
  Result:= not FQuery.EOF;
end;

function TQueryDataProvider.EOF: Boolean;
begin
  Result:= FQuery.EOF;
end;

function TQueryDataProvider.GetFieldValue(const FieldName: string): Variant;
begin
  try
    if FQuery.FieldByName(FieldName).IsNull then
      Result:= Null
    else
      Result:= FQuery.FieldByName(FieldName).Value;
  except
    on E: Exception do
      Result:= Null;
  end;
end;

function TQueryDataProvider.FieldExists(const FieldName: string): Boolean;
begin
  try
    Result:= FQuery.FindField(FieldName) <> nil;
  except
    Result:= False;
  end;
end;

function TQueryDataProvider.GetObjectType: string;
begin
  Result:= Trim(GetFieldValue('OBJECT_TYPE'));

  if Result = '' then
    Result:= 'ONBEKEND'; { #todo : Moet een melding naar de view sturen }
end;

function TQueryDataProvider.GetRecordCount: Integer;
begin
  Result:= FQuery.RecordCount;
end;

function TQueryDataProvider.GetProviderType: string;
begin
  Result:= 'TQuery';
end;

end.
