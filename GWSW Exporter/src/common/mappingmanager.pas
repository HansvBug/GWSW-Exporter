{ Copyright ©2025-2026 Hans van Buggenum }
unit MappingManager;

interface

uses
  SysUtils, Classes, Variants;

type
  TMappingType = (mtMateriaalPut, mtMateriaalLeiding, mtVormPut, mtVormLeiding, mtObjectType,
    mtStatusFunctioneren, mtStelseltype, mtPutType, mtLeidingType, mtKolkType,
     mtPersleidingType, mtFundering, mtWibonThema);

  { TMappingManager }
  TMappingManager = class
  private
    fMappings: array[TMappingType] of TStringList;
    fMappingFile: string;
    procedure LoadMappingsFromODS(const AFileName: string);
    function Get_MappingFile: string;
    procedure Set_MappingFile(const Value: string);
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;

    function GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;

    property MappingFile: string read Get_MappingFile write Set_MappingFile;
  end;

implementation

uses
  fpspreadsheet, {%H-}fpsallformats; // Without fpsallformats, the export will not work.

constructor TMappingManager.Create(const FileName : String);
var
  mt: TMappingType;
begin
  inherited Create;

  if FileExists(FileName) then begin
    for mt:= Low(TMappingType) to High(TMappingType) do
      fMappings[mt]:= TStringList.Create;

    MappingFile:= FileName
  end
  else
    raise Exception.Create('MappingFileNotFound' + '| (' + FileName + ')' );
end;

destructor TMappingManager.Destroy;
var
  mt: TMappingType;
begin
  for mt:= Low(TMappingType) to High(TMappingType) do
    fMappings[mt].Free;
  inherited Destroy;
end;

procedure TMappingManager.LoadMappingsFromODS(const AFileName: string);
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  i: Integer;
  SourceValue, GWSWValue: string;
  MappingType: TMappingType;
  SheetName: string;
begin
  if not FileExists(AFileName) then
    raise Exception.Create('MappingFileNotFound');

  Workbook:= TsWorkbook.Create;
  try
    Workbook.ReadFromFile(AFileName);

    for MappingType:= Low(TMappingType) to High(TMappingType) do begin
      case MappingType of
        { #note : De werkbladnamen moeten intstelbaar worden }
        mtMateriaalPut      : SheetName:= 'Materiaal_Put';      // Is also used at gully (kolk). Perhaps split.
        mtMateriaalLeiding  : SheetName:= 'Materiaal_leiding';
        mtVormPut           : SheetName:= 'Vorm_Put';           // Is also used at gully (kolk). Perhaps split.
        mtVormLeiding       : SheetName:= 'Vorm_Leiding';
        mtStatusFunctioneren: SheetName:= 'StatusFunctioneren';
        mtStelseltype       : SheetName:= 'Stelseltype';
        mtObjectType        : SheetName:= 'ObjectType';         // Is not (yet) used.
        mtPutType           : SheetName:= 'PutType';
        mtLeidingType       : SheetName:= 'LeidingType';
        mtKolkType          : SheetName:= 'KolkType';
        mtPersleidingType   : SheetName:= 'LeidingType';        // CAUTION. Keep it that way for the time being. Perhaps separate in the ods document.
        mtFundering         : Sheetname:= 'Fundering';          // Used by manhole and pipe.
        mtWibonThema        : Sheetname:= 'WibonThema';
      end;

      fMappings[MappingType].Clear; // Clear existing mappings

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

function TMappingManager.Get_MappingFile: string;
begin
  Result:= fMappingFile;
end;

procedure TMappingManager.Set_MappingFile(const Value: string);
var
  mt: TMappingType;
begin
  if fMappingFile <> Value then begin
    fMappingFile:= Value;
    if FileExists(Value) then
      LoadMappingsFromODS(Value)
    else begin
      for mt:= Low(TMappingType) to High(TMappingType) do
        fMappings[mt].Clear;

      raise Exception.Create('MappingFileNotFound');
    end;
  end;
end;

function TMappingManager.GetGWSWURI(MappingType: TMappingType; const SourceValue: string): string;
var
  RawValue: string;
begin
  // Check if mappings are loaded
  if fMappings[MappingType].Count = 0 then begin
    { #todo : Maapingstabel ontbreekt.  fouttype voor maken. }
    Result := '';
    Exit;
  end;

  if SourceValue = '' then
    //Result:= 'gwsw:Onbekend'
    Result:= ''
  else begin // een waarde gevonden die opgezocht gaat worden in de mappingslist.
    RawValue:= fMappings[MappingType].Values[UpperCase(SourceValue)];

    if RawValue = '' then
      Result:= 'Mapping_error'  // Value does not appear in the mapping list
    else if Pos(':', RawValue) > 0 then
      Result:= RawValue
    else
      Result:= 'gwsw:' + RawValue;
  end;
end;

end.

