{ Copyright ©2025 Hans van Buggenum }
unit MappingManager;

interface

uses
  SysUtils, Classes, Variants;

type
  TMappingType = (mtMateriaalPut, mtMateriaalLeiding, mtVormPut, mtVormLeiding, mtObjectType,
    mtWijzeInwinning, mtStatusFunctioneren, mtStelseltype, mtPutType, mtLeidingType,
    mtKolkType, mtPersleidingType);

  { TMappingManager }

  TMappingManager = class
  private
    fMappings: array[TMappingType] of TStringList;
    fMappingFile: string;
    procedure LoadMappingsFromODS(const AFileName: string);
    function GetMappingFile: string;
    procedure SetMappingFile(const Value: string);
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;

    function GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;

    property MappingFile: string read GetMappingFile write SetMappingFile;
  end;

implementation

uses
  fpspreadsheet, {%H-}fpsallformats; // Zonder fpsallformats werkt de export niet.

constructor TMappingManager.Create(const FileName : String);
var
  mt: TMappingType;
  DefaultMappingFile: string;
begin
  inherited Create;
  for mt:= Low(TMappingType) to High(TMappingType) do
    fMappings[mt]:= TStringList.Create;

  // Optioneel: probeer default mapping file te laden
  DefaultMappingFile:= FileName;
  if FileExists(DefaultMappingFile) then
    MappingFile:= DefaultMappingFile;
end;

destructor TMappingManager.Destroy;
var
  mt: TMappingType;
begin
  for mt:= Low(TMappingType) to High(TMappingType) do
    fMappings[mt].Free;
  inherited Destroy;
end;

procedure TMappingManager.LoadMappingsFromODS(const AFileName: string); { #note : Rename to LoadMappingsFromFile }
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  i: Integer;
  SourceValue, GWSWValue: string;
  MappingType: TMappingType;
  SheetName: string;
begin
  if not FileExists(AFileName) then
    raise Exception.Create('Mapping bestand niet gevonden: ' + AFileName);

  Workbook:= TsWorkbook.Create;
  try
    Workbook.ReadFromFile(AFileName);

    for MappingType:= Low(TMappingType) to High(TMappingType) do begin
      case MappingType of
        { #note : De werkbladnamen moeten intstelbaar worden }
        mtMateriaalPut      : SheetName := 'Materiaal_Put';      // wordt ook bij kolk gebruik. wellicht scheiden
        mtMateriaalLeiding  : SheetName := 'Materiaal_leiding';
        mtVormPut           : SheetName := 'Vorm_Put';           // wordt ook bij kolk gebruik. wellicht scheiden
        mtVormLeiding       : SheetName := 'Vorm_Leiding';
        mtStatusFunctioneren: SheetName := 'StatusFunctioneren';
        mtStelseltype       : SheetName := 'Stelseltype';
        mtObjectType        : SheetName := 'ObjectType';  //--->  Deze moet weg
        mtPutType           : SheetName := 'PutType';
        mtLeidingType       : SheetName := 'LeidingType';
        mtKolkType          : SheetName := 'KolkType';
        mtPersleidingType   : SheetName := 'LeidingType'; // LET OP. Voorlopig zo houden. Wellicht scheiden in het ods document.
        mtWijzeInwinning    : SheetName := 'WijzeInwinning'; // Wordt nog niet gebuikt
      end;

      fMappings[MappingType].Clear; // Bestaande mappings clearen

      Worksheet:= Workbook.GetWorksheetByName(SheetName);
      if Worksheet <> nil then begin
        for i:= 1 to Worksheet.GetLastRowIndex do
        begin
          SourceValue:= Trim(Worksheet.ReadAsText(i, 0));
          GWSWValue:= Trim(Worksheet.ReadAsText(i, 1));

          if (SourceValue <> '') and (GWSWValue <> '') then
            fMappings[MappingType].Values[UpperCase(SourceValue)] := GWSWValue;
        end;
      end;
    end;

  finally
    Workbook.Free;
  end;
end;

function TMappingManager.GetMappingFile: string;
begin
  Result:= fMappingFile;
end;

procedure TMappingManager.SetMappingFile(const Value: string);
var
  mt: TMappingType;
begin
  if fMappingFile <> Value then begin
    fMappingFile:= Value;
    if FileExists(Value) then
      LoadMappingsFromODS(Value)
    else begin
      // Log een waarschuwing maar ga niet crashen
      for mt:= Low(TMappingType) to High(TMappingType) do
        fMappings[mt].Clear;
    end;
  end;
end;

function TMappingManager.GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;
var
  RawValue: string;
begin
  // Controleer of mappings geladen zijn
  if fMappings[MappingType].Count = 0 then begin
    Result := 'gwsw:Onbekend';    { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
    Exit;
  end;

  if SourceValue = '' then
    Result:= 'gwsw:Onbekend'  { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
  else begin
    RawValue:= fMappings[MappingType].Values[UpperCase(SourceValue)];

    if RawValue = '' then
      Result:= 'gwsw:Onbekend' { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
    else if Pos(':', RawValue) > 0 then
      Result:= RawValue
    else
      Result:= 'gwsw:' + RawValue;
  end;
end;

end.

