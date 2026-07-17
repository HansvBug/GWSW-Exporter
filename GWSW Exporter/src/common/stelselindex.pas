{ Copyright ©2026 Hans van Buggenum }
unit stelselIndex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  { TStelselIndex }
  TStelselIndex = class(TObject)
  private
    fParts: TFPHashList;  // Key: StelselGUID, Value: TStringList met object-URIs
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPart(const StelselGUID, ObjectURI: string);
    function GetParts(const StelselGUID: string): TStringList;
    procedure Clear;
    function Count: Integer;
  end;

implementation

{ TStelselIndex }

constructor TStelselIndex.Create;
begin
  inherited Create;
  fParts:= TFPHashList.Create;
end;

destructor TStelselIndex.Destroy;
var
  i: Integer;
begin
  for i:= 0 to fParts.Count - 1 do
    TStringList(fParts[i]).Free;
  fParts.Free;
  inherited Destroy;
end;

procedure TStelselIndex.AddPart(const StelselGUID, ObjectURI: string);
var
  List: TStringList;
begin
  if StelselGUID = '' then Exit;

  List:= TStringList(fParts.Find(StelselGUID));
  if List = nil then begin
    List:= TStringList.Create;
    fParts.Add(StelselGUID, List);
  end;
  List.Add(ObjectURI);
end;

function TStelselIndex.GetParts(const StelselGUID: string): TStringList;
begin
  Result:= TStringList(fParts.Find(StelselGUID));
end;

procedure TStelselIndex.Clear;
var
  i: Integer;
begin
  for i:= 0 to fParts.Count - 1 do
    TStringList(fParts[i]).Free;
    //TStringList(fParts[i]).Clear;
  fParts.Clear;
end;

function TStelselIndex.Count: Integer;
begin
  Result:= fParts.Count;
end;

end.
