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

unit MappingManager;

interface

uses
  SysUtils, Classes, Variants;

type
  TMappingType = (mtMateriaalPut, mtMateriaalLeiding, mtVormPut, mtVormLeiding, mtObjectType,
    mtWijzeInwinning, mtStatusFunctioneren, mtStelseltype, mtPutType, mtLeidingType,
    mtKolkType, mtPersleidingType);

  TMappingManager = class
  private
    FMappings: array[TMappingType] of TStringList;
    FMappingFile: string;
    procedure LoadMappingsFromODS(const AFileName: string);
    function GetMappingFile: string;
    procedure SetMappingFile(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;

    property MappingFile: string read GetMappingFile write SetMappingFile;
  end;

implementation

uses
  fpspreadsheet, {%H-}fpsallformats; // Zonder fpsallformats werkt de export niet.

constructor TMappingManager.Create;
var
  mt: TMappingType;
  DefaultMappingFile: string;
begin
  inherited Create;
  for mt := Low(TMappingType) to High(TMappingType) do
    FMappings[mt] := TStringList.Create;

  // Optioneel: probeer default mapping file te laden
  DefaultMappingFile := ExtractFilePath(ParamStr(0)) + 'DomainList' + PathDelim + 'GWSW161mappings.ods'; { #note : Moet intelbaar worden }
  if FileExists(DefaultMappingFile) then
    MappingFile := DefaultMappingFile;
end;

destructor TMappingManager.Destroy;
var
  mt: TMappingType;
begin
  for mt := Low(TMappingType) to High(TMappingType) do
    FMappings[mt].Free;
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

  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromFile(AFileName);

    for MappingType := Low(TMappingType) to High(TMappingType) do begin
      case MappingType of
        { #note : De werkbladnamen moeten intstelbaar worden }
        mtMateriaalPut      : SheetName := 'Materiaal_Put';
        mtMateriaalLeiding  : SheetName := 'Materiaal_leiding';
        mtVormPut           : SheetName := 'Vorm_Put';
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

      FMappings[MappingType].Clear; // Bestaande mappings clearen

      Worksheet := Workbook.GetWorksheetByName(SheetName);
      if Worksheet <> nil then begin
        for i := 1 to Worksheet.GetLastRowIndex do
        begin
          SourceValue := Trim(Worksheet.ReadAsText(i, 0));
          GWSWValue := Trim(Worksheet.ReadAsText(i, 1));

          if (SourceValue <> '') and (GWSWValue <> '') then
            FMappings[MappingType].Values[UpperCase(SourceValue)] := GWSWValue;
        end;
      end;
    end;

  finally
    Workbook.Free;
  end;
end;

function TMappingManager.GetMappingFile: string;
begin
  Result := FMappingFile;
end;

procedure TMappingManager.SetMappingFile(const Value: string);
var
  mt: TMappingType;
begin
  if FMappingFile <> Value then begin
    FMappingFile := Value;
    if FileExists(Value) then
      LoadMappingsFromODS(Value)
    else begin
      // Log een waarschuwing maar ga niet crashen
      for mt := Low(TMappingType) to High(TMappingType) do
        FMappings[mt].Clear;
    end;
  end;
end;

function TMappingManager.GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;
var
  RawValue: string;
begin
  // Controleer of mappings geladen zijn
  if FMappings[MappingType].Count = 0 then begin
    Result := 'gwsw:Onbekend';    { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
    Exit;
  end;

  if SourceValue = '' then
    Result := 'gwsw:Onbekend'  { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
  else begin
    RawValue := FMappings[MappingType].Values[UpperCase(SourceValue)];

    if RawValue = '' then
      Result := 'gwsw:Onbekend' { #todo : Melden in de view. Dan kan de bron eert worden nagekeken/aangepast. }
    else if Pos(':', RawValue) > 0 then
      Result := RawValue
    else
      Result := 'gwsw:' + RawValue;
  end;
end;

end.
