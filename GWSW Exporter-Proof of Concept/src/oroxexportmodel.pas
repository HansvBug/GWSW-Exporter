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

unit OroxExportModel;

interface

uses
  SysUtils, Classes, TypInfo, Variants, Math, DateUtils,
  ZDataset,
  GWSWTypes, uIGWSWDataProvider, QueryDataProvider, CSVDataProvider,
  MappingManager;

type
  TProgressEvent = procedure(Current, Total: Integer) of object;

  { TOroxExportModel }

  TOroxExportModel = class
  private
    FOrganisatieNaam: String;

    FPutList: TList;
    FLeidingList: TList;
    FStelselList: TList;
    FKolkList: TList;
    FPersleidingList: TList;

    FOnProgress: TProgressEvent;
    FMappingManager: TMappingManager;

    function MapDatabaseToGWSW(DataProvider : IGWSWDataProvider; TotalRecords : Integer) : Boolean;
    function GenerateOroxTurtle(const FileName: string): Boolean;
    function CreateGMLPoint(X, Y, Z: Double): string;
    function CreateGMLLineString(const Points: array of Double): string;
    function WijzeInwinningToGWSW(Wijze: TGWSWWijzeInwinning): string;
    function CreateGMLLineStringForPersleiding(const Persleiding: PGWSWPersleiding): string;

    // Helper methods voor mapping
    function MapPutFromProvider(DataProvider: IGWSWDataProvider): PGWSWPut;
    function MapLeidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWLeiding;
    function MapStelselFromProvider(DataProvider: IGWSWDataProvider): PGWSWStelsel;
    function MapKolkFromProvider(DataProvider: IGWSWDataProvider): PGWSWKolk;
    function MapPersleidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWPersleiding;

    function ParseWKTPoint(const WKT: string; out X, Y, Z: Double): Boolean;
    procedure SplitString(const Input: string; const Delimiter: Char; Strings: TStrings);
    function ConvertWKTToGML(const WKT: string): string;
    function ConvertWKTToGMLLineString(const WKT: string): string;
    procedure ParseCoordinateStringForLineString(const CoordStr: string; out X, Y, Z: Double);
    function GetDateFromYearField(DataProvider: IGWSWDataProvider; const FieldName: string;
                                                DefaultDay: Integer = 1;
                                                DefaultMonth: Integer = 1): TDateTime;
    function DateToOroxFormat(DateValue: TDateTime): string;  // OROX/RDF heeft datum alstijd in formaat: YYYY-MM-DD
    procedure ParsePersleidingGeometry(Persleiding: PGWSWPersleiding);

    // Export helpers
    procedure ExportStelselToTurtle(SL: TStringList; const Stelsel: PGWSWStelsel);
    procedure ExportPutToTurtle(SL: TStringList; const Put: PGWSWPut);
    procedure ExportLeidingToTurtle(SL: TStringList; const Leiding: PGWSWLeiding);
    procedure ExportKolkToTurtle(SL: TStringList; const Kolk: PGWSWKolk);
    procedure ExportPersleidingToTurtle(SL: TStringList; const Persleiding: PGWSWPersleiding);
    procedure AddPutKenmerken(SL: TStringList; const Put: PGWSWPut);
    procedure AddPutOrientatie(SL: TStringList; const Put: PGWSWPut);
    procedure AddLeidingKenmerken(SL: TStringList; const Leiding: PGWSWLeiding);
    procedure AddKolkKenmerken(SL: TStringList; const Kolk: PGWSWKolk);
    procedure AddKolkOrientatie(SL: TStringList; const Kolk: PGWSWKolk);
    procedure AddPersleidingKenmerken(SL: TStringList; const Persleiding: PGWSWPersleiding);
    procedure ExportPersleidingOrientatie(SL: TStringList; const Persleiding: PGWSWPersleiding);

    // List management
    procedure ClearAllLists;
    procedure ClearPutList;
    procedure ClearLeidingList;
    procedure ClearStelselList;
    procedure ClearKolkList;
    procedure ClearPersleidingList;
    procedure DoProgress(Current, Total: Integer);

    { #todo : FloatToOrox aanpassen zodat het aantal decimalen als paramater mee gaat. }
    function FloatToOrox(Value: Double): string;  // Ongeacht de landinstelling, het Orox ttl bestand verwacht een "." als decimaal scheidingsteken. (en geen ",")
    function FloatToOrox2Dec(Value: Double): string;
    function FormatOrox(const FormatStr: string; const Args: array of const): string;// Ongeacht de landinstelling, het Orox ttl bestand verwacht een "." als decimaal scheidingsteken. (en geen ",")

    function IsValidFloat(Value: Double): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ExportToOrox(DataProvider: IGWSWDataProvider; const FileName: string);
    procedure ExportToOroxFromQuery(Query: TZQuery; const FileName: string);
    procedure ExportToOroxFromCSV(const CSVFilename, OutputFilename: string);

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OrganisatieNaam: string read FOrganisatieNaam write FOrganisatieNaam;
  end;

implementation

constructor TOroxExportModel.Create;
begin
  inherited;
  FPutList := TList.Create;
  FLeidingList := TList.Create;
  FStelselList := TList.Create;
  FKolkList := TList.Create;
  FPersleidingList := TList.Create;

  FMappingManager := TMappingManager.Create;
  // Stel default mapping file in
  { #todo : Moet instelbaar worden }
  FMappingManager.MappingFile := ExtractFilePath(ParamStr(0)) + 'DomainList' + PathDelim +  'GWSW161mappings.ods';
end;

destructor TOroxExportModel.Destroy;
begin
  ClearAllLists;  // Bestaande data eerst opruimen

  FMappingManager.Free;
  FPutList.Free;
  FLeidingList.Free;
  FStelselList.Free;
  FKolkList.Free;
  FPersleidingList.Free;
  inherited;
end;

procedure TOroxExportModel.DoProgress(Current, Total: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Current, Total);
end;

function TOroxExportModel.FloatToOrox(Value : Double) : string;
var
  TempStr: string;
begin
  // Formatteer altijd met 3 decimalen
  TempStr := Format('%.3f', [Value]);

  // STAP 1: Verwijder eerst duizendtal scheiding (bijv: 1.000,50 -> 1000,50)
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    TempStr := StringReplace(TempStr, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // STAP 2: Vervang dan decimaalscheiding door punt (bijv: 1000,50 -> 1000.50)
  TempStr := StringReplace(TempStr, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);

  Result := TempStr;
end;

function TOroxExportModel.FloatToOrox2Dec(Value : Double) : string;
// sommige getallen mogen maar 2 decimalen hebben.
{ #todo : Beter instelbaar maken: 1 functie FloatToOrox maken. }
var
  TempStr: string;
begin
  // Formatteer altijd met 3 decimalen
  TempStr := Format('%.2f', [Value]);

  // STAP 1: Verwijder eerst duizendtal scheiding (bijv: 1.000,50 -> 1000,50)
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    TempStr := StringReplace(TempStr, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // STAP 2: Vervang dan decimaalscheiding door punt (bijv: 1000,50 -> 1000.50)
  TempStr := StringReplace(TempStr, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);

  Result := TempStr;
end;

function TOroxExportModel.FormatOrox(const FormatStr : string; const Args : array of const) : string;
begin
  Result := Format(FormatStr, Args);

  // STAP 1: Verwijder eerst duizendtal scheiding
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    Result := StringReplace(Result, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // STAP 2: Vervang dan decimaalscheiding door punt
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
end;

function TOroxExportModel.IsValidFloat(Value : Double) : Boolean;
begin
  Result := not (IsNaN(Value) or IsInfinite(Value));    // Nan=Not a Number
end;

procedure TOroxExportModel.ExportToOrox(DataProvider: IGWSWDataProvider; const FileName: string);
var
  TotalRecords: Integer;
begin
  // Eerst aantal records bepalen voor voortgang
  DataProvider.Open;
  try
    TotalRecords := DataProvider.GetRecordCount;

    // Begin met voortgang (eerste helft voor mapping)
    DoProgress(0, TotalRecords * 2);  // Totaal = mapping + genereren

    if not MapDatabaseToGWSW(DataProvider, TotalRecords) then
      raise Exception.Create('Fout bij mapping data naar GWSW model');

  finally
    DataProvider.Close;
  end;

  if not GenerateOroxTurtle(FileName) then
    raise Exception.Create('Fout bij genereren Orox Turtle bestand');
end;

procedure TOroxExportModel.ExportToOroxFromQuery(Query : TZQuery;
  const FileName : string);
var
  DataProvider: IGWSWDataProvider;
begin
  DataProvider := TQueryDataProvider.Create(Query);
  Query.DisableControls;
    ExportToOrox(DataProvider, FileName);
  Query.EnableControls;
end;

procedure TOroxExportModel.ExportToOroxFromCSV(const CSVFilename, OutputFilename: string);
var
  DataProvider: IGWSWDataProvider;
begin
  { #todo : export naar ttl vanuit een csv is nog niet ingebouwd. }
  DataProvider := TCSVDataProvider.Create(CSVFilename);
  ExportToOrox(DataProvider, OutputFilename);
end;

function TOroxExportModel.MapDatabaseToGWSW(DataProvider: IGWSWDataProvider; TotalRecords: Integer): Boolean;
var
  ProcessedCount: Integer;
begin
  Result := False;
  try
    ClearAllLists;
    ProcessedCount := 0;

    DataProvider.Open;
    try
      if DataProvider.First then begin
        repeat
          case UpperCase(DataProvider.GetObjectType) of
            'STELSEL': begin
              FStelselList.Add(MapStelselFromProvider(DataProvider));
            end;
            'PUT': begin
              FPutList.Add(MapPutFromProvider(DataProvider));
            end;
            'LEIDING': begin
              FLeidingList.Add(MapLeidingFromProvider(DataProvider));
            end;
            'PERSLEIDING': begin
              FPersLeidingList.Add(MapPersleidingFromProvider(DataProvider));
            end;
            'KOLK': begin
              FKolkList.Add(MapKolkFromProvider(DataProvider));
            end
            else begin
              // Onbekend objecttype aangetroffen.
              { #todo : Moet een melding geven in de view }
              //tmp:= '';
            end;
          end;


        Inc(ProcessedCount);
        DoProgress(ProcessedCount, TotalRecords * 2);  // ← Direct update

        until not DataProvider.Next;
      end;
    finally
      DataProvider.Close;
    end;

    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Mapping fout: ' + E.Message);
  end;
end;

function TOroxExportModel.MapStelselFromProvider(DataProvider: IGWSWDataProvider): PGWSWStelsel;
var
  Stelsel: PGWSWStelsel;
  StelselTypeStr: string;
  TempValue: Variant;
begin
  New(Stelsel);

  // Initialiseer
  Stelsel^.GUID := VarToStrDef(DataProvider.GetFieldValue('GUID'), '');
  Stelsel^.aLabel := '';
  Stelsel^.HasParts := True;
  Stelsel^.StelselTypeUri := '';

  // Naam
  if DataProvider.FieldExists('NAAM') then begin
    TempValue := DataProvider.GetFieldValue('NAAM');
    if not VarIsNull(TempValue) then
      Stelsel^.aLabel := VarToStr(TempValue);
  end;

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    StelselTypeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Stelsel^.StelselTypeUri := FMappingManager.GetGWSWURI(mtStelseltype, StelselTypeStr);

    // DEBUG: Log indien nodig
    if Stelsel^.StelselTypeUri = 'gwsw:Onbekend' then
      ; // { #todo : Melden in view }
  end;

  Result := Stelsel;
end;

function TOroxExportModel.MapKolkFromProvider(DataProvider : IGWSWDataProvider) : PGWSWKolk;
var
  Kolk: PGWSWKolk;
  KolkTypeStr, MateriaalStr, VormStr: string;   { #todo : Kan weg. TempValue is voldoende }
  WKTGeometry: string;
  GeoX, GeoY, GeoZ: Double;
  TempValue: Variant;
begin
  New(Kolk);

  // Initialiseer basisvelden
  Kolk^.GUID := DataProvider.GetFieldValue('GUID');

  if DataProvider.FieldExists('NAAM') then begin
    TempValue := DataProvider.GetFieldValue('NAAM');
    if not VarIsNull(TempValue) then Kolk^.aLabel := TempValue;
  end;

  // Geometrie inlezen
  Kolk^.WKTGeometry := '';
  Kolk^.HasWKTGeometry := False;

  if DataProvider.FieldExists('WKT_GEOMETRY') or DataProvider.FieldExists('GEOMETRY') then begin
    if DataProvider.FieldExists('WKT_GEOMETRY') then
      WKTGeometry := DataProvider.GetFieldValue('WKT_GEOMETRY')
    else
      WKTGeometry := DataProvider.GetFieldValue('GEOMETRY');

    if WKTGeometry <> '' then begin
      Kolk^.WKTGeometry := WKTGeometry;
      Kolk^.HasWKTGeometry := True;

      // Parse ook naar X,Y,Z voor backward compatibility
      if ParseWKTPoint(WKTGeometry, GeoX, GeoY, GeoZ) then begin
        Kolk^.X := GeoX;
        Kolk^.Y := GeoY;
        Kolk^.Z := GeoZ;
        Kolk^.HasOrientation := (GeoX <> 0) and (GeoY <> 0);
      end
      else begin
        // Fallback naar individuele coordinaat velden
        // Mag hier nooit komen. Validatie vooraf maken
        TempValue := DataProvider.GetFieldValue('X');
        if not VarIsNull(TempValue) then Kolk^.X := TempValue else Kolk^.X := 0;

        TempValue := DataProvider.GetFieldValue('Y');
        if not VarIsNull(TempValue) then Kolk^.Y := TempValue else Kolk^.Y := 0;

        TempValue := DataProvider.GetFieldValue('Z');
        if not VarIsNull(TempValue) then Kolk^.Z := TempValue else Kolk^.Z := 0;

        Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);
      end;
    end;
  end
  else begin
    // Gebruik individuele coordinaat velden
    // Mag hiert nooit komen. Validatie vooraf maken
    TempValue := DataProvider.GetFieldValue('X');
    if not VarIsNull(TempValue) then Kolk^.X := TempValue else Kolk^.X := 0;

    TempValue := DataProvider.GetFieldValue('Y');
    if not VarIsNull(TempValue) then Kolk^.Y := TempValue else Kolk^.Y := 0;

    TempValue := DataProvider.GetFieldValue('Z');
    if not VarIsNull(TempValue) then Kolk^.Z := TempValue else Kolk^.Z := 0;

    Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);
  end;



  // Afmetingen met NULL checks
  if DataProvider.FieldExists('BREEDTE') then begin
    TempValue := DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(TempValue) then Kolk^.Breedte := TempValue;
  end;

  if DataProvider.FieldExists('LENGTE') then begin
    TempValue := DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(TempValue) then Kolk^.Lengte := TempValue;
  end;

  if DataProvider.FieldExists('HOOGTE') then begin
    TempValue := DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(TempValue) then Kolk^.Hoogte := TempValue;
  end;

  if DataProvider.FieldExists('DIAMETER') then begin
    TempValue := DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(TempValue) then Kolk^.Diameter := TempValue;
  end;

  // Materiaal mapping
  { #todo : Controle op field exists maken. Moet ook bij put, streng en stelsel }
  MateriaalStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
  Kolk^.MateriaalURI := FMappingManager.GetGWSWURI(mtMateriaalPut, MateriaalStr);

  // Vorm mapping
  VormStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
  Kolk^.VormURI := FMappingManager.GetGWSWURI(mtVormPut, VormStr);

  // Kolktype mapping
  KolkTypeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('KOLK_TYPE'), ''));
  if KolkTypeStr <> '' then
    Kolk^.KolkTypeURI := FMappingManager.GetGWSWURI(mtKolkType, KolkTypeStr)
  else
    Kolk^.KolkTypeURI := 'gwsw:Kolk'; // Default

  // Kolk-specifieke velden
  if DataProvider.FieldExists('WANDDIKTE') then begin
    TempValue := DataProvider.GetFieldValue('WANDDIKTE');
    if not VarIsNull(TempValue) then Kolk^.Wanddikte := TempValue;
  end;

  if DataProvider.FieldExists('TYPE_REINIGING') then begin
    TempValue := DataProvider.GetFieldValue('TYPE_REINIGING');
    if not VarIsNull(TempValue) then Kolk^.TypeReiniging := VarToStr(TempValue);
  end;

  Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);

  Result := Kolk;
end;

function TOroxExportModel.MapPersleidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWPersleiding;
var
  Persleiding: PGWSWPersleiding;
  PersleidingType, MateriaalStr, StatusStr: string;
  StelselTypeStr: string;
  TempValue: Variant;
  TempDate: TDateTime;
  WKTGeometry: Variant;  // Gebruik Variant zoals in andere functies
  WKTGeometryStr: string;
begin
  New(Persleiding);
  FillChar(Persleiding^, SizeOf(TGWSWPersleiding), 0);

  // Basis identificatie - PRECIES ZOALS MapLeidingFromProvider
  TempValue := DataProvider.GetFieldValue('GUID');
  if not VarIsNull(TempValue) then
    Persleiding^.GUID := VarToStr(TempValue);

  TempValue := DataProvider.GetFieldValue('NAAM');
  if not VarIsNull(TempValue) then
    Persleiding^.aLabel := VarToStr(TempValue);

  // Topologie - verbinding met putten
  TempValue := DataProvider.GetFieldValue('BEGINPUT_ID');
  if not VarIsNull(TempValue) then
    Persleiding^.BeginPutID := VarToStr(TempValue);

  TempValue := DataProvider.GetFieldValue('EINDPUT_ID');
  if not VarIsNull(TempValue) then
    Persleiding^.EindPutID := VarToStr(TempValue);

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then
  begin
    TempValue := DataProvider.GetFieldValue('STELSEL_ID');
    if not VarIsNull(TempValue) then
      Persleiding^.StelselID := VarToStr(TempValue);
  end;

  // Afmetingen met NULL checks - ZELFDE STIJL ALS MapLeidingFromProvider
  TempValue := DataProvider.GetFieldValue('LENGTE');
  if not VarIsNull(TempValue) then
    Persleiding^.Lengte := TempValue;

  if DataProvider.FieldExists('DIAMETER') then
  begin
    TempValue := DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(TempValue) then
      Persleiding^.Diameter := TempValue;
  end;

  // Materiaal mapping
  TempValue := DataProvider.GetFieldValue('MATERIAAL');
  if not VarIsNull(TempValue) then
  begin
    MateriaalStr := UpperCase(VarToStr(TempValue));
    Persleiding^.MateriaalURI := FMappingManager.GetGWSWURI(mtMateriaalLeiding, MateriaalStr);
  end;

  // Status functioneren
  TempValue := DataProvider.GetFieldValue('STATUS_FUNCTIONEREN');
  if not VarIsNull(TempValue) then
  begin
    StatusStr := UpperCase(VarToStr(TempValue));
    Persleiding^.StatusFunctionerenURI := FMappingManager.GetGWSWURI(mtStatusFunctioneren, StatusStr);
  end;

  // Stelsel type
  TempValue := DataProvider.GetFieldValue('STELSEL_TYPE');
  if not VarIsNull(TempValue) then
  begin
    StelselTypeStr := UpperCase(VarToStr(TempValue));
    Persleiding^.StelselURI := FMappingManager.GetGWSWURI(mtStelseltype, StelselTypeStr);
  end;

  // Stelselnaam
  TempValue := DataProvider.GetFieldValue('STELSEL_NAAM');
  if not VarIsNull(TempValue) then
    Persleiding^.Stelselnaam := VarToStr(TempValue);

  // Persleidingtype mapping
  TempValue := DataProvider.GetFieldValue('LEIDING_TYPE');
  if not VarIsNull(TempValue) then
  begin
    PersleidingType := UpperCase(VarToStr(TempValue));
    Persleiding^.PersleidingTypeURI := FMappingManager.GetGWSWURI(mtPersleidingType, PersleidingType);
  end;

  // Vorm mapping
  TempValue := UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
  Persleiding^.VormURI := FMappingManager.GetGWSWURI(mtVormLeiding, TempValue);  { #todo : Let op dit is gelijk aan de leiding (streng). Misschien scheiden }

  // Datum velden
  TempDate := GetDateFromYearField(DataProvider, 'BEGINDATUM');
  if TempDate <> 0 then
    Persleiding^.Begindatum := TempDate;

  // Geometrie inlezen - UNIFORME AANPAK ZOALS MapLeidingFromProvider
  Persleiding^.WKTGeometry := '';
  Persleiding^.HasWKTGeometry := False;
  Persleiding^.HasMultipleVertices := False;

  // PRECIES DEZELFDE LOGICA ALS MapLeidingFromProvider
  WKTGeometryStr := '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then
  begin
    WKTGeometry := DataProvider.GetFieldValue('WKT_GEOMETRY');
    if not VarIsNull(WKTGeometry) then
      WKTGeometryStr := Trim(VarToStr(WKTGeometry));
  end
  else if DataProvider.FieldExists('GEOMETRY') then
  begin
    WKTGeometry := DataProvider.GetFieldValue('GEOMETRY');
    if not VarIsNull(WKTGeometry) then
      WKTGeometryStr := Trim(VarToStr(WKTGeometry));
  end;

  // Controleer of we geldige WKT geometrie hebben - ZELFDE ALS MapLeidingFromProvider
  if WKTGeometryStr <> '' then
  begin
    Persleiding^.WKTGeometry := WKTGeometryStr;
    Persleiding^.HasWKTGeometry := True;

    // Extra: specifieke verwerking voor persleiding geometrie
    if Pos('LINESTRING', UpperCase(Persleiding^.WKTGeometry)) > 0 then
    begin
      ParsePersleidingGeometry(Persleiding);
    end;
  end;

  // Optioneel: coördinaten van begin- en eindput als fallback
  // ZELFDE STIJL ALS MapLeidingFromProvider
  if DataProvider.FieldExists('BEGINPUT_X') then
  begin
    TempValue := DataProvider.GetFieldValue('BEGINPUT_X');
    if not VarIsNull(TempValue) then
      Persleiding^.BeginPutX := TempValue;
  end;

  if DataProvider.FieldExists('BEGINPUT_Y') then
  begin
    TempValue := DataProvider.GetFieldValue('BEGINPUT_Y');
    if not VarIsNull(TempValue) then
      Persleiding^.BeginPutY := TempValue;
  end;

  if DataProvider.FieldExists('BEGINPUT_Z') then
  begin
    TempValue := DataProvider.GetFieldValue('BEGINPUT_Z');
    if not VarIsNull(TempValue) then
      Persleiding^.BeginPutZ := TempValue;
  end;

  if DataProvider.FieldExists('EINDPUT_X') then
  begin
    TempValue := DataProvider.GetFieldValue('EINDPUT_X');
    if not VarIsNull(TempValue) then
      Persleiding^.EindPutX := TempValue;
  end;

  if DataProvider.FieldExists('EINDPUT_Y') then
  begin
    TempValue := DataProvider.GetFieldValue('EINDPUT_Y');
    if not VarIsNull(TempValue) then
      Persleiding^.EindPutY := TempValue;
  end;

  if DataProvider.FieldExists('EINDPUT_Z') then
  begin
    TempValue := DataProvider.GetFieldValue('EINDPUT_Z');
    if not VarIsNull(TempValue) then
      Persleiding^.EindPutZ := TempValue;
  end;

  Result := Persleiding;
end;

function TOroxExportModel.ParseWKTPoint(const WKT: string; out X, Y, Z: Double): Boolean;
var
  CleanWKT, CoordStr: string;
  StartPos, EndPos: Integer;
  Coords: TStringList;
  OldDecimalSeparator: Char;
begin
  Result := False;
  X := 0; Y := 0; Z := 0;

  if Trim(WKT) = '' then
    Exit;

  CleanWKT := Trim(WKT);

  // Bewaar het oude decimaalscheidingsteken en wijzig tijdelijk naar punt
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';

    // POINT Z (x y z) - 3D met Z marker
    if Pos('POINT Z', UpperCase(CleanWKT)) = 1 then begin
      StartPos := Pos('(', CleanWKT);
      EndPos := Pos(')', CleanWKT);
      if (StartPos > 0) and (EndPos > StartPos) then begin
        CoordStr := Copy(CleanWKT, StartPos + 1, EndPos - StartPos - 1);
        CoordStr := Trim(CoordStr);

        Coords := TStringList.Create;
        try
          SplitString(CoordStr, ' ', Coords);

          if Coords.Count >= 3 then begin
            X := StrToFloatDef(Trim(Coords[0]), 0);
            Y := StrToFloatDef(Trim(Coords[1]), 0);
            Z := StrToFloatDef(Trim(Coords[2]), 0);
            Result := True;
          end;
        finally
          Coords.Free;
        end;
      end;
    end
    // POINT (x y z) - 3D zonder Z marker
    else if Pos('POINT', UpperCase(CleanWKT)) = 1 then begin
      StartPos := Pos('(', CleanWKT);
      EndPos := Pos(')', CleanWKT);
      if (StartPos > 0) and (EndPos > StartPos) then begin
        CoordStr := Copy(CleanWKT, StartPos + 1, EndPos - StartPos - 1);
        CoordStr := Trim(CoordStr);

        Coords := TStringList.Create;
        try
          SplitString(CoordStr, ' ', Coords);

          if Coords.Count >= 2 then begin
            X := StrToFloatDef(Trim(Coords[0]), 0);
            Y := StrToFloatDef(Trim(Coords[1]), 0);

            // Als er een derde coordinaat is
            if Coords.Count >= 3 then
              Z := StrToFloatDef(Trim(Coords[2]), 0)
            else
              Z := 0;

            Result := True;
          end;
        finally
          Coords.Free;
        end;
      end;
    end;
  finally
    // Zet het oude decimaalscheidingsteken terug
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TOroxExportModel.SplitString(const Input: string; const Delimiter: Char; Strings: TStrings);
var
  I, StartPos: Integer;
  Token: string;
begin
  Strings.Clear;
  StartPos := 1;

  for I := 1 to Length(Input) do begin
    if Input[I] = Delimiter then begin
      Token := Copy(Input, StartPos, I - StartPos);
      if Token <> '' then
        Strings.Add(Token);

      StartPos := I + 1;
    end;
  end;

  // Voeg de laatste token toe
  if StartPos <= Length(Input) then begin
    Token := Copy(Input, StartPos, Length(Input) - StartPos + 1);
    if Token <> '' then
      Strings.Add(Token);
  end;
end;

function TOroxExportModel.ConvertWKTToGML(const WKT : string) : string;
var
  X, Y, Z: Double;
begin
  if ParseWKTPoint(WKT, X, Y, Z) then begin
    // Gebruik bestaande CreateGMLPoint met de geparseerde coördinaten
    Result := CreateGMLPoint(X, Y, Z);
  end
  else begin
    // Fallback: gebruik CreateGMLPoint met default waarden
    Result := CreateGMLPoint(0, 0, 0);     { #todo : Waarschuwen in de view }
  end;
end;

function TOroxExportModel.ConvertWKTToGMLLineString(const WKT: string): string;
var
  Inner: string;
  i: Integer;
  PointTokens, CoordTokens: TStringList;
  pTok: string;
  StartPos, EndPos: Integer;
  x, y, z: Double;
  FS: TFormatSettings;
  PosList: string;

  function TrimAndCollapseSpaces(const S: string): string;
  var
    tmp: string;
  begin
    tmp := Trim(S);
    while Pos('  ', tmp) > 0 do
      tmp := StringReplace(tmp, '  ', ' ', [rfReplaceAll]);
    Result := tmp;
  end;

begin
  Result := '';

  if Trim(WKT) = '' then Exit;

  StartPos := Pos('(', WKT);
  EndPos := LastDelimiter(')', WKT);
  if (StartPos = 0) or (EndPos <= StartPos) then Exit;

  Inner := Copy(WKT, StartPos + 1, EndPos - StartPos - 1);
  Inner := Trim(Inner);

  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;   // belangrijk: duizendtallen mogen niet herkend worden

  PointTokens := TStringList.Create;
  CoordTokens := TStringList.Create;
  try
    // punten splitsen
    PointTokens.StrictDelimiter := True;
    PointTokens.Delimiter := ',';
    PointTokens.DelimitedText := Inner;

    PosList := '';

    for i := 0 to PointTokens.Count - 1 do begin
      pTok := TrimAndCollapseSpaces(PointTokens[i]);
      if pTok = '' then Continue;

      // tabs → spatie
      pTok := StringReplace(pTok, #9, ' ', [rfReplaceAll]);
      pTok := TrimAndCollapseSpaces(pTok);

      CoordTokens.Clear;
      ExtractStrings([' '], [], PChar(pTok), CoordTokens);

      x := 0; y := 0; z := 0;

      if CoordTokens.Count >= 1 then
        TryStrToFloat(CoordTokens[0], x, FS);

      if CoordTokens.Count >= 2 then
        TryStrToFloat(CoordTokens[1], y, FS);

      if CoordTokens.Count >= 3 then
        TryStrToFloat(CoordTokens[2], z, FS);

      if PosList <> '' then
        PosList := PosList + ' ';

      PosList :=
        PosList +
        FormatFloat('0.000', x, FS) + ' ' +
        FormatFloat('0.000', y, FS) + ' ' +
        FormatFloat('0.000', z, FS);
    end;

    if PosList = '' then Exit;

    Result :=
      '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\">' +
      '<gml:posList srsDimension=\"3\">' + PosList + '</gml:posList>' +
      '</gml:LineString>';

  finally
    PointTokens.Free;
    CoordTokens.Free;
  end;
end;

procedure TOroxExportModel.ParseCoordinateStringForLineString(const CoordStr: string; out X, Y, Z: Double);
var
  Coords: TStringList;
  CleanCoordStr: string;
  OldDecimalSeparator: Char;
begin
  X := 0; Y := 0; Z := 0;

  // Vervang ALLE comma's door punten - dit is de cruciale stap
  CleanCoordStr := StringReplace(CoordStr, ',', '.', [rfReplaceAll]);

  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';

    Coords := TStringList.Create;
    try
      // Gebruik dezelfde SplitString methode als voor putten
      SplitString(Trim(CleanCoordStr), ' ', Coords);

      if Coords.Count >= 2 then begin
        X := StrToFloatDef(Trim(Coords[0]), 0);
        Y := StrToFloatDef(Trim(Coords[1]), 0);

        // Voor LineString: als er een derde coordinaat is, gebruik die als Z
        if Coords.Count >= 3 then
          Z := StrToFloatDef(Trim(Coords[2]), 0)
        else
          Z := 0; // Geen Z coordinaat, gebruik 0
      end;
    finally
      Coords.Free;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TOroxExportModel.GetDateFromYearField(
  DataProvider : IGWSWDataProvider; const FieldName : string;
  DefaultDay : Integer; DefaultMonth : Integer) : TDateTime;
var
  TempValue: Variant;
  YearStr: string;
  YearInt: Integer;
begin
  // Default waarde (0 betekent "geen geldige datum")
  Result := 0;

  TempValue := DataProvider.GetFieldValue(FieldName);
  if VarIsNull(TempValue) or VarIsEmpty(TempValue) then
    Exit;

  YearStr := Trim(VarToStr(TempValue));

  if YearStr = '' then
    Exit;

  // Probeer om te zetten naar integer
  if not TryStrToInt(YearStr, YearInt) then
    Exit;

  // Valideer jaar (instelbare grenzen)
  if (YearInt < 1900) or (YearInt > YearOf(Date) + 10) then
    Exit;  { #todo : Wel een melding inbouwen }

  // Maak datum
  try
    Result := EncodeDate(YearInt, DefaultMonth, DefaultDay);
  except
    on E: EConvertError do
      Result := 0;  // Ongeldige datum
  end;
end;

function TOroxExportModel.DateToOroxFormat(DateValue : TDateTime) : string;
begin
  if DateValue = 0 then
    Result := ''
  else
    Result := FormatDateTime('yyyy-mm-dd', DateValue);
end;

procedure TOroxExportModel.ParsePersleidingGeometry(Persleiding : PGWSWPersleiding);
var
  WKT: string;
  StartPos, EndPos: Integer;
  InnerStr, CoordStr: string;
  PointTokens, CoordTokens: TStringList;
  i: Integer;
  FS: TFormatSettings;
  x, y, z: Double;
begin
  WKT := UpperCase(Persleiding^.WKTGeometry);

  if Pos('LINESTRING', WKT) = 0 then
    Exit;

  // Zoek de coördinaten string
  StartPos := Pos('(', WKT);
  EndPos := LastDelimiter(')', WKT);

  if (StartPos = 0) or (EndPos <= StartPos) then
    Exit;

  InnerStr := Copy(Persleiding^.WKTGeometry, StartPos + 1, EndPos - StartPos - 1);
  InnerStr := Trim(InnerStr);

  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;

  PointTokens := TStringList.Create;
  CoordTokens := TStringList.Create;
  try
    // Punten splitsen op komma
    PointTokens.StrictDelimiter := True;
    PointTokens.Delimiter := ',';
    PointTokens.DelimitedText := InnerStr;

    // Bereken aantal vertices
    SetLength(Persleiding^.Vertices, PointTokens.Count);

    for i := 0 to PointTokens.Count - 1 do begin
      CoordStr := Trim(PointTokens[i]);

      // Verwijder dubbele spaties en tabs
      while Pos('  ', CoordStr) > 0 do
        CoordStr := StringReplace(CoordStr, '  ', ' ', [rfReplaceAll]);
      CoordStr := StringReplace(CoordStr, #9, ' ', [rfReplaceAll]);

      // Split coordinaat
      CoordTokens.Clear;
      ExtractStrings([' '], [], PChar(CoordStr), CoordTokens);

      x := 0; y := 0; z := 0;

      if CoordTokens.Count >= 1 then
        TryStrToFloat(CoordTokens[0], x, FS);

      if CoordTokens.Count >= 2 then
        TryStrToFloat(CoordTokens[1], y, FS);

      if CoordTokens.Count >= 3 then
        TryStrToFloat(CoordTokens[2], z, FS);

      // Sla vertex op
      Persleiding^.Vertices[i][0] := x;
      Persleiding^.Vertices[i][1] := y;
      Persleiding^.Vertices[i][2] := z;
    end;

    Persleiding^.HasMultipleVertices := PointTokens.Count > 2;

  finally
    PointTokens.Free;
    CoordTokens.Free;
  end;
end;

function TOroxExportModel.MapPutFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPut;
var
  Put: PGWSWPut;
  PutType, MateriaalStr, VormStr, WijzeStr, StelselNaamStr, WKTGeometry: string;
  StelselTypeStr: String;
  GeoX, GeoY, GeoZ: Double;
  TempValue: Variant;
  TempDate: TDateTime;
begin
  New(Put);

  // Initialiseer nieuwe velden
  Put^.WKTGeometry := '';
  Put^.HasWKTGeometry := False;

  Put^.GUID := DataProvider.GetFieldValue('GUID');

  if DataProvider.FieldExists('NAAM') then begin
    TempValue := DataProvider.GetFieldValue('NAAM');
    if not VarIsNull(TempValue) then Put^.aLabel := TempValue;
  end;

  // Eerst proberen WKT geometrie te gebruiken
  if DataProvider.FieldExists('WKT_GEOMETRY') or DataProvider.FieldExists('GEOMETRY') then begin
    if DataProvider.FieldExists('WKT_GEOMETRY') then
      WKTGeometry := DataProvider.GetFieldValue('WKT_GEOMETRY')
    else
      WKTGeometry := DataProvider.GetFieldValue('GEOMETRY');

    if WKTGeometry <> '' then begin
      Put^.WKTGeometry := WKTGeometry;
      Put^.HasWKTGeometry := True;

      // Parse ook naar X,Y,Z voor backward compatibility
      if ParseWKTPoint(WKTGeometry, GeoX, GeoY, GeoZ) then begin
        Put^.X := GeoX;
        Put^.Y := GeoY;
        Put^.Z := GeoZ;
        Put^.HasOrientation := (GeoX <> 0) and (GeoY <> 0);
      end
      else begin
        // Fallback naar individuele coordinaat velden
        // Mag hiert nooit komen. Validatie vooraf maken
        TempValue := DataProvider.GetFieldValue('X');
        if not VarIsNull(TempValue) then Put^.X := TempValue else Put^.X := 0;

        TempValue := DataProvider.GetFieldValue('Y');
        if not VarIsNull(TempValue) then Put^.Y := TempValue else Put^.Y := 0;

        TempValue := DataProvider.GetFieldValue('Z');
        if not VarIsNull(TempValue) then Put^.Z := TempValue else Put^.Z := 0;

        Put^.HasOrientation := (Put^.X <> 0) and (Put^.Y <> 0);
      end;
    end;
  end
  else begin
    // Gebruik individuele coordinaat velden
      // Mag hiert nooit komen. Validatie vooraf maken
    TempValue := DataProvider.GetFieldValue('X');
    if not VarIsNull(TempValue) then Put^.X := TempValue else Put^.X := 0;

    TempValue := DataProvider.GetFieldValue('Y');
    if not VarIsNull(TempValue) then Put^.Y := TempValue else Put^.Y := 0;

    TempValue := DataProvider.GetFieldValue('Z');
    if not VarIsNull(TempValue) then Put^.Z := TempValue else Put^.Z := 0;

    Put^.HasOrientation := (Put^.X <> 0) and (Put^.Y <> 0);
  end;

  // Afmetingen met NULL checks
  { #todo : Breedte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('BREEDTE') then      { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
  begin
    TempValue := DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(TempValue) then Put^.Breedte := TempValue;
  end;

  { #todo : Lengte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('LENGTE') then begin
    TempValue := DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(TempValue) then Put^.Lengte := TempValue;
  end;

  { #todo : Hoogte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('HOOGTE') then begin
    TempValue := DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(TempValue) then
      Put^.Hoogte := TempValue;
  end;

  // Materiaal mapping

  MateriaalStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
  Put^.MateriaalURI := FMappingManager.GetGWSWURI(mtMateriaalPut, MateriaalStr);

  // Vorm mapping
  VormStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
  Put^.VormURI := FMappingManager.GetGWSWURI(mtvormPut, VormStr);

  // Puttype mapping
  PutType := UpperCase(VarToStrDef(DataProvider.GetFieldValue('PUT_TYPE'), ''));
  Put^.PutTypeUri := FMappingManager.GetGWSWURI(mtPutType, PutType);

  // Hoogte waarden met NULL checks
  TempValue := DataProvider.GetFieldValue('MAAIVELD');
  if not VarIsNull(TempValue) then
    Put^.Maaiveldhoogte := TempValue;
  //else
    //Put^.Maaiveldhoogte := 0;

  TempValue := DataProvider.GetFieldValue('GRONDWATER');
  if not VarIsNull(TempValue) then Put^.Grondwaterstand := TempValue else Put^.Grondwaterstand := 0;  { #todo : Wordt niet gebruikt. }

  { #todo : Aanlegjaar wordt hier omgezet naar een datum.
    Moet worden, eerst kijken of het een jaartal of een datum is. als jaartal dan door met huidige opzet anders datum omzetten naar TDate.
  }
  TempDate := GetDateFromYearField(DataProvider, 'BEGINDATUM');
  if TempDate <> 0 then
    Put^.Begindatum := TempDate;

  // Wijze van inwinning
  { #todo : Moet naar extern mappingsbestand }
  { #note : Wordt niet gebruikt ! }
  WijzeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('WIJZE_INWINNING'), ''));
  if WijzeStr = 'GEMETEN' then Put^.WijzeInwinning := wiGemeten
  else if WijzeStr = 'BEREKEND' then Put^.WijzeInwinning := wiBerekend
  else if WijzeStr = 'GESCHAT' then Put^.WijzeInwinning := wiGeschat
  else if WijzeStr = 'ONTWERP' then Put^.WijzeInwinning := wiOntwerp
  else Put^.WijzeInwinning := wiOnbekend;

  // Stelsel relatie
  if DataProvider.FieldExists('STELSEL_ID') then
    Put^.StelselID := VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '')
  else
    Put^.StelselID := '';

  // stelsel uit de mapping ophalen
  StelselNaamStr := VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
  Put^.Stelselnaam:= StelselNaamStr;

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    StelselTypeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Put^.StelselURI := FMappingManager.GetGWSWURI(mtStelseltype, StelselTypeStr);
  end else
    Put^.StelselURI := '';

  Put^.HasOrientation := (Put^.X <> 0) and (Put^.Y <> 0);

  Result := Put;
end;

function TOroxExportModel.MapLeidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWLeiding;
var
  Leiding: PGWSWLeiding;
  LeidingType, MateriaalStr, VormStr, StatusStr, WijzeStr, WKTGeometry: string;
  StelselTypeStr, StelselNaamStr: string;
  TempValue: Variant;
  TempDate: TDateTime;
begin
  New(Leiding);
  FillChar(Leiding^, SizeOf(TGWSWLeiding), 0);

  // Basis identificatie
  Leiding^.GUID := VarToStrDef(DataProvider.GetFieldValue('GUID'), '');
  Leiding^.aLabel := VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');

  // Topologie - verbinding met putten
  Leiding^.BeginPutID := VarToStrDef(DataProvider.GetFieldValue('BEGINPUT_ID'), '');
  Leiding^.EindPutID := VarToStrDef(DataProvider.GetFieldValue('EINDPUT_ID'), '');

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then
    Leiding^.StelselID := VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '')
  else
    Leiding^.StelselID := '';

  // Afmetingen met NULL checks
  TempValue := DataProvider.GetFieldValue('LENGTE');

  if not VarIsNull(TempValue) then Leiding^.Lengte := TempValue;

  if DataProvider.FieldExists('BREEDTE') then begin{ #todo : Deze controle overal gaan toevoegen. }
    TempValue := DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(TempValue) then Leiding^.Breedte := TempValue;
  end;

  if DataProvider.FieldExists('HOOGTE') then begin
    TempValue := DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(TempValue) then
      Leiding^.Hoogte := TempValue;
  end;

  if DataProvider.FieldExists('DIAMETER') then
  begin
    TempValue := DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(TempValue) then Leiding^.Diameter := TempValue;
  end;
  //else
    //Leiding^.Diameter := 0;

  // Materiaal mapping
  MateriaalStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
  Leiding^.MateriaalURI := FMappingManager.GetGWSWURI(mtMateriaalLeiding, MateriaalStr);

  // Vorm mapping
  VormStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
  Leiding^.VormURI := FMappingManager.GetGWSWURI(mtVormLeiding, VormStr);

  // status functioneren
  StatusStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('STATUS_FUNCTIONEREN'), ''));
  Leiding^.StatusFunctionerenURI := FMappingManager.GetGWSWURI(mtStatusFunctioneren, StatusStr);

  // Stelsel type
  StelselTypeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
  Leiding^.StelselURI := FMappingManager.GetGWSWURI(mtStelseltype, StelselTypeStr);

  // stelselnaam (vanuit gv_stelsel) { #todo : Wordt nog niet opgepakt in de export ??? }
  StelselNaamStr := VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
  Leiding^.Stelselnaam:= StelselNaamStr;

  // Leidingtype mapping
  { #todo : Moeet naar extern mappingsbestand }
  LeidingType := UpperCase(VarToStrDef(DataProvider.GetFieldValue('LEIDING_TYPE'), ''));
  Leiding^.LeidingTypeURI := FMappingManager.GetGWSWURI(mtLeidingType, LeidingType);

  // BOB waarden
  TempValue := DataProvider.GetFieldValue('BOB_BEGIN');
  if not VarIsNull(TempValue) then
    Leiding^.BobBegin := TempValue
  else
    Leiding^.BobBegin := 0;

  TempValue := DataProvider.GetFieldValue('BOB_EIND');
  if not VarIsNull(TempValue) then
    Leiding^.BobEind := TempValue
  else
    Leiding^.BobEind := 0;

  // Datum velden
  TempDate := GetDateFromYearField(DataProvider, 'BEGINDATUM');
  if TempDate <> 0 then
    Leiding^.Begindatum := TempDate;

  // Wijze van inwinning
  { #todo : Moeet naar extern mappingsbestand }
  WijzeStr := UpperCase(VarToStrDef(DataProvider.GetFieldValue('WIJZE_INWINNING'), ''));
  if WijzeStr = 'GEMETEN' then
    Leiding^.WijzeInwinning := wiGemeten
  else if WijzeStr = 'BEREKEND' then
    Leiding^.WijzeInwinning := wiBerekend
  else if WijzeStr = 'GESCHAT' then
    Leiding^.WijzeInwinning := wiGeschat
  else if WijzeStr = 'ONTWERP' then
    Leiding^.WijzeInwinning := wiOntwerp
  else
    Leiding^.WijzeInwinning := wiOnbekend;

  //Geometrie inlezen
  Leiding^.WKTGeometry := '';
  Leiding^.HasWKTGeometry := False;
  Leiding^.HasMultipleVertices := False;

  // Probeer verschillende mogelijke veldnamen voor geometrie
  WKTGeometry := '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then
    WKTGeometry := DataProvider.GetFieldValue('WKT_GEOMETRY')
{  else if DataProvider.FieldExists('GEOMETRY') then
    WKTGeometry := DataProvider.GetFieldValue('GEOMETRY')
  else if DataProvider.FieldExists('WKT') then
    WKTGeometry := DataProvider.GetFieldValue('WKT')
  else if DataProvider.FieldExists('SHAPE') then
    WKTGeometry := DataProvider.GetFieldValue('SHAPE')};

  // Controleer of we geldige WKT geometrie hebben
  if not VarIsNull(WKTGeometry) and (Trim(VarToStr(WKTGeometry)) <> '') then
  begin
    Leiding^.WKTGeometry := Trim(VarToStr(WKTGeometry));
    Leiding^.HasWKTGeometry := True;
  end;

  // Optioneel: probeer coördinaten van begin- en eindput in te lezen als backup
  if DataProvider.FieldExists('BEGINPUT_X') and DataProvider.FieldExists('BEGINPUT_Y') then begin
    TempValue := DataProvider.GetFieldValue('BEGINPUT_X');
    if not VarIsNull(TempValue) then Leiding^.BeginPutX := TempValue;

    TempValue := DataProvider.GetFieldValue('BEGINPUT_Y');
    if not VarIsNull(TempValue) then Leiding^.BeginPutY := TempValue;

    TempValue := DataProvider.GetFieldValue('BEGINPUT_Z');
    if not VarIsNull(TempValue) then Leiding^.BeginPutZ := TempValue;
  end;

  if DataProvider.FieldExists('EINDPUT_X') and DataProvider.FieldExists('EINDPUT_Y') then begin
    TempValue := DataProvider.GetFieldValue('EINDPUT_X');
    if not VarIsNull(TempValue) then Leiding^.EindPutX := TempValue;

    TempValue := DataProvider.GetFieldValue('EINDPUT_Y');
    if not VarIsNull(TempValue) then Leiding^.EindPutY := TempValue;

    TempValue := DataProvider.GetFieldValue('EINDPUT_Z');
    if not VarIsNull(TempValue) then Leiding^.EindPutZ := TempValue;
  end;

  Result := Leiding;
end;

function TOroxExportModel.GenerateOroxTurtle(const FileName: string): Boolean;
var
  SL: TStringList;
  i, TotalObjects, ObjectsProcessed: Integer;
  DummyStelsel: PGWSWStelsel;
begin
  Result := False;
  SL := TStringList.Create;  // de specificaties schrijven utf 8 voor. (nodig voor é ë etc.)
  try
    // Eerst een dummy stelsel toevoegen aan de lijst voor objecten zonder stelsel
    // Nodig voor de kolken. Is niet voldoende/goed om de validatie meldingen te voorkomen. Nakijken.
    { #todo : Moet anders? beter? Er blijven validatie meldingen bij de kolken bestaan. }
    New(DummyStelsel);
    DummyStelsel^.GUID := 'DUMMY_STELSEL';
    DummyStelsel^.aLabel := 'Dummy stelsel voor objecten zonder stelsel';
    DummyStelsel^.HasParts := True;
    DummyStelsel^.StelselTypeUri := 'gwsw:GemengdStelsel';
    FStelselList.Add(DummyStelsel);

    // Bereken totaal aantal te verwerken objecten
    TotalObjects := FStelselList.Count + FPutList.Count + FLeidingList.Count + FKolkList.Count + FPersleidingList.Count;
    ObjectsProcessed := 0;
    // Start voortgang op 0 (eerste helft was mapping, tweede helft is genereren)
    DoProgress(TotalObjects, TotalObjects * 2);  // ← Dit zou op 50% moeten staan

    SL.WriteBOM := False; { #todo : Uitzoeken. }

    // Prefixes conform GWSW-OroX specificatie
    SL.Add('@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .');
    SL.Add('@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .');
    SL.Add('@prefix owl:     <http://www.w3.org/2002/07/owl#> .');
    SL.Add('@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .');
    SL.Add('@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .');
    SL.Add('@prefix geo:     <http://www.opengis.net/ont/geosparql#> .');
    SL.Add('@prefix gwsw:    <http://data.gwsw.nl/1.6/totaal/> .');
    SL.Add('@prefix :        <http://sparql.gwsw.nl/repositories/' + FOrganisatieNaam + '#> .');  // Dit komt voor de URI staan!
    SL.Add('');
    SL.Add('# gwsw-exporter, een export tool in aanbouw.');
    SL.Add('# Exportdatum: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SL.Add('');
    SL.Add('');

    // Export Stelsels
    for i := 0 to FStelselList.Count - 1 do begin
      ExportStelselToTurtle(SL, PGWSWStelsel(FStelselList[i]));
      Inc(ObjectsProcessed);
      DoProgress(TotalObjects + ObjectsProcessed, TotalObjects * 2);
    end;

    // Export Putten
    for i := 0 to FPutList.Count - 1 do begin
      ExportPutToTurtle(SL, PGWSWPut(FPutList[i]));
      Inc(ObjectsProcessed);
      DoProgress(TotalObjects + ObjectsProcessed, TotalObjects * 2)
    end;

    // Export Kolken
    for i := 0 to FKolkList.Count - 1 do begin
      ExportKolkToTurtle(SL, PGWSWKolk(FKolkList[i]));
      Inc(ObjectsProcessed);
      DoProgress(TotalObjects + ObjectsProcessed, TotalObjects * 2);
    end;

    // Export Leidingen
    for i := 0 to FLeidingList.Count - 1 do begin
      ExportLeidingToTurtle(SL, PGWSWLeiding(FLeidingList[i]));
      Inc(ObjectsProcessed);
      DoProgress(TotalObjects + ObjectsProcessed, TotalObjects * 2);
    end;

    // Export Persleidingen
    for i := 0 to FPersleidingList.Count - 1 do begin
      ExportPersleidingToTurtle(SL, PGWSWPersleiding(FPersleidingList[i]));
      Inc(ObjectsProcessed);
      DoProgress(TotalObjects + ObjectsProcessed, TotalObjects * 2);
    end;

    // Einde van genereren
    DoProgress(TotalObjects * 2, TotalObjects * 2);

    // Opslaan
    SL.SaveToFile(FileName);
    Result := True;

  finally
    SL.Free;
  end;
end;

procedure TOroxExportModel.ExportStelselToTurtle(SL: TStringList; const Stelsel: PGWSWStelsel);
var
  i: Integer;
  Put: PGWSWPut;
  Leiding: PGWSWLeiding;
  HasParts: Boolean;
  PartsList: TStringList;
  StelselTypeUri: string;
  Kolk: PGWSWKolk;
  Persleiding: PGWSWPersleiding;
begin
  // Controleer of stelsel een geldige URI heeft
  if Stelsel^.StelselTypeUri = '' then
    StelselTypeUri := 'gwsw:OnbekendStelsel'
  else
    StelselTypeUri := Stelsel^.StelselTypeUri;

  // Stelsel definitie
  SL.Add(':' + Stelsel^.GUID);
  SL.Add('  rdf:type      ' + StelselTypeUri + ' ;');

  // Label (naam)
  if Stelsel^.aLabel <> '' then
    SL.Add('  rdfs:label    "' + Stelsel^.aLabel + '" ;')
  else
    SL.Add('  rdfs:label    "Onbekend stelsel" ;');

  // Voeg stelsel specifieke kenmerken toe
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WijzeVanInwinning ; ' +
         'gwsw:hasReference ' + WijzeInwinningToGWSW(wiGemeten) + ' ] ;');

  // Zoek onderdelen van dit stelsel
  HasParts := False;
  PartsList := TStringList.Create;
  try
    // Putten die bij dit stelsel horen
    for i := 0 to FPutList.Count - 1 do begin
      Put := PGWSWPut(FPutList[i]);
      if Put^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Put^.GUID);
        HasParts := True;
      end;
    end;

    // Leidingen die bij dit stelsel horen
    for i := 0 to FLeidingList.Count - 1 do begin
      Leiding := PGWSWLeiding(FLeidingList[i]);
      if Leiding^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Leiding^.GUID);
        HasParts := True;
      end;
    end;

    for i := 0 to FPersleidingList.Count - 1 do begin
      Persleiding := PGWSWPersleiding(FPersleidingList[i]);
      if Persleiding^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Persleiding^.GUID);
        HasParts := True;
      end;
    end;

    // Kolken aan dummy stelsel toekennen
    // test.....
    // zou de validatie foutmelding moeten oplossen    (Knooppunt (orientatie van Put, Bouwwerk, Compartiment, Hulpstuk, Aansluitpunt) heeft geen verbinding)
    // ttl lijkt er goed uit te zien maar de validatie geeft nog steeds de knooppunt foutmelding
    if Stelsel^.GUID = 'DUMMY_STELSEL' then begin
      for i := 0 to FKolkList.Count - 1 do begin
        Kolk := PGWSWKolk(FKolkList[i]);
        // Voeg ALLE kolken toe aan DUMMY_STELSEL
        PartsList.Add('    :' + Kolk^.GUID);
        HasParts := True;
      end;
    end;

    // Voeg hasPart relaties toe indien er onderdelen zijn
    if HasParts and (PartsList.Count > 0) then begin
      SL.Add('  gwsw:hasPart');
      for i := 0 to PartsList.Count - 1 do begin
        if i < PartsList.Count - 1 then
          SL.Add(PartsList[i] + ' ,')
        else
          SL.Add(PartsList[i] + ' .');
      end;
    end else begin
      // Geen delen, sluit af met .
      if SL.Count > 0 then begin
        if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
          SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
        else
          SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
      end;
    end;

  finally
    PartsList.Free;
  end;

  SL.Add('');
end;

procedure TOroxExportModel.ExportPutToTurtle(SL: TStringList; const Put: PGWSWPut);
begin
  SL.Add(':' + Put^.GUID);
  SL.Add('  rdf:type      ' + Put^.PutTypeUri + ' ;');
  SL.Add('  rdfs:label    "' + Put^.aLabel + '" ;');

  // EERSTE ORIENTATIE TOEVOEGING =  Voegt de relatie toe van de put naar zijn oriëntatie
  if Put^.HasOrientation or Put^.HasWKTGeometry then
    SL.Add('  gwsw:hasAspect  :' + Put^.GUID + '_ori ;');

  // Stelsel relatie toevoegen
  if Put^.StelselID <> '' then
    SL.Add('  gwsw:isPartOf   :' + Put^.StelselID + ' ;');

  AddPutKenmerken(SL, Put);

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // TWEEDE ORIENTATIE TOEVOEGING - DIT IS DE DEFINITIE VAN DE ORIENTATIE ZELF
  if Put^.HasOrientation or Put^.HasWKTGeometry then
    AddPutOrientatie(SL, Put);
end;

procedure TOroxExportModel.AddPutKenmerken(SL: TStringList; const Put: PGWSWPut);
begin
  if not IsNan(Put^.Breedte) and (Put^.Breedte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Breedte) + ' ] ;');

  if not IsNan(Put^.Lengte) and (Put^.Lengte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Lengte) + ' ] ;');

  if not IsNan(Put^.Hoogte) and (Put^.Hoogte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Hoogte) + ' ] ;');

  if Put^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalPut ; ' +
          'gwsw:hasReference ' + Put^.MateriaalURI + ' ] ;');

  if not IsNaN(Put^.Grondwaterstand) then  // and (Put^.Grondwaterstand <> 0)
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Grondwaterniveau ; gwsw:hasValue ' + FloatToOrox(Put^.Grondwaterstand) + ' ] ;');

  if Put^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormPut ; ' +
         'gwsw:hasReference ' + Put^.VormURI + ' ] ;');

  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WijzeVanInwinning ; ' +
         'gwsw:hasReference ' + WijzeInwinningToGWSW(Put^.WijzeInwinning) + ' ] ;');
end;

procedure TOroxExportModel.AddPutOrientatie(SL: TStringList; const Put: PGWSWPut);
var
  GMLString: string;
begin
  SL.Add(':' + Put^.GUID + '_ori');
  SL.Add('  rdf:type      gwsw:Putorientatie ;');
  SL.Add('  gwsw:hasAspect  [');
  SL.Add('    rdf:type      gwsw:Punt ;');

  // Gebruik WKT geometrie als beschikbaar, anders CreateGMLPoint
  if Put^.HasWKTGeometry and (Put^.WKTGeometry <> '') then begin
    GMLString := ConvertWKTToGML(Put^.WKTGeometry);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral');
  end
  else begin
    SL.Add('    gwsw:hasValue "' + CreateGMLPoint(Put^.X, Put^.Y, Put^.Z) + '"^^geo:gmlLiteral');
  end;

  SL.Add('  ] ;');

  // NIEUW: Voeg maaiveldorientatie toe als maaiveldhoogte bekend is
  if not IsNaN(Put^.Maaiveldhoogte) and (Put^.Maaiveldhoogte <> 0) then begin
    SL.Add('  gwsw:hasConnection  _:' + Put^.GUID + '_maaiveld_ori ;');
  end;

  SL.Add('.');
  SL.Add('');

  // Voeg maaiveldorientatie toe
  if not IsNaN(Put^.Maaiveldhoogte) then begin  // and (Put^.Maaiveldhoogte <> 0)
    SL.Add('_:' + Put^.GUID + '_maaiveld_ori');
    SL.Add('  rdf:type                      gwsw:Maaiveldorientatie ;');
    SL.Add('  gwsw:hasAspect                [');
    SL.Add('    rdf:type                      gwsw:Maaiveldhoogte ;');
    SL.Add('    gwsw:hasValue                 "' + FloatToOrox2Dec(Put^.Maaiveldhoogte) + '"^^xsd:decimal ;');

    // Optioneel: voeg inwinning toe als die beschikbaar is
    { #todo : Nog verder bekijken. }
    if Put^.WijzeInwinning <> wiOnbekend then begin
      SL.Add('    gwsw:hasAspect                [');
      SL.Add('      rdf:type                      gwsw:WijzeVanInwinning ;');
      SL.Add('      gwsw:hasReference             ' + WijzeInwinningToGWSW(Put^.WijzeInwinning));
      SL.Add('    ]');
    end;

    SL.Add('  ] .');
    SL.Add('');
  end;
end;

procedure TOroxExportModel.ExportLeidingToTurtle(SL: TStringList; const Leiding: PGWSWLeiding);
var
  GMLString: string;
  Points: array of Double = Nil;
begin
  SL.Add(':' + Leiding^.GUID);
  SL.Add('  rdf:type      ' + Leiding^.LeidingTypeUri + ' ;');
  SL.Add('  rdfs:label    "' + Leiding^.aLabel + '" ;');

  // Stelsel relatie toevoegen - gebruik isPartOf voor de compositie
  if Leiding^.StelselID <> '' then
    SL.Add('  gwsw:isPartOf  :' + Leiding^.StelselID + ' ;');

  SL.Add('  gwsw:hasAspect  :' + Leiding^.GUID + '_ori ;');

  AddLeidingKenmerken(SL, Leiding);  // ZONDER BOB waarden

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Leiding oriëntatie MET BOB waarden genest in de eindpunten
  SL.Add(':' + Leiding^.GUID + '_ori');
  SL.Add('  rdf:type        gwsw:Leidingorientatie ;');

  // Beginpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:BeginpuntLeiding ;');
  if not IsNaN(Leiding^.BobBegin) and (Leiding^.BobBegin <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobBeginpuntLeiding ; gwsw:hasValue ' + FloatToOrox2Dec(Leiding^.BobBegin) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Leiding^.BeginPutID + '_ori ] ;');

  // Eindpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:EindpuntLeiding ;');
  if not IsNaN(Leiding^.BobEind) and (Leiding^.BobEind <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobEindpuntLeiding ; gwsw:hasValue ' + FloatToOrox2Dec(Leiding^.BobEind) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Leiding^.EindPutID + '_ori ] ;');

  // Geometrie
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Lijn ;');

  if Leiding^.HasWKTGeometry and (Leiding^.WKTGeometry <> '') then begin
    GMLString := ConvertWKTToGMLLineString(Leiding^.WKTGeometry);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end
  else begin
    // Fallback geometrie op basis van begin- en eindput coördinaten
    SetLength(Points, 6);
    Points[0] := Leiding^.BeginPutX;
    Points[1] := Leiding^.BeginPutY;
    Points[2] := Leiding^.BeginPutZ;
    Points[3] := Leiding^.EindPutX;
    Points[4] := Leiding^.EindPutY;
    Points[5] := Leiding^.EindPutZ;
    SL.Add('    gwsw:hasValue "' + CreateGMLLineString(Points) + '"^^geo:gmlLiteral ] ;');
  end;

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');
end;

procedure TOroxExportModel.ExportKolkToTurtle(SL : TStringList; const Kolk : PGWSWKolk);
begin
  SL.Add(':' + Kolk^.GUID);
  SL.Add('  rdf:type      ' + Kolk^.KolkTypeUri + ' ;');
  SL.Add('  rdfs:label    "' + Kolk^.aLabel + '" ;');

  // test met Dummy. (kijken of de melding: "Knooppunt (orientatie van Put, Bouwwerk, Compartiment, Hulpstuk, Aansluitpunt) heeft geen verbinding" voorkomen kan worden
  SL.Add('  gwsw:isPartOf   :DUMMY_STELSEL ;');  // Fallback naar dummy


  // Oriëntatie toevoegen
  if Kolk^.HasOrientation or Kolk^.HasWKTGeometry then
    SL.Add('  gwsw:hasAspect  :' + Kolk^.GUID + '_ori ;');

  AddKolkKenmerken(SL, Kolk);

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Oriëntatie definitie toevoegen
  if Kolk^.HasOrientation or Kolk^.HasWKTGeometry then
    AddKolkOrientatie(SL, Kolk);
end;

procedure TOroxExportModel.ExportPersleidingToTurtle(SL : TStringList; const Persleiding : PGWSWPersleiding);
begin
  SL.Add(':' + Persleiding^.GUID);
  SL.Add('  rdf:type      ' + Persleiding^.PersleidingTypeURI + ' ;');
  SL.Add('  rdfs:label    "' + Persleiding^.aLabel + '" ;');

  // Stelsel relatie
  if Persleiding^.StelselID <> '' then
    SL.Add('  gwsw:isPartOf  :' + Persleiding^.StelselID + ' ;');

  SL.Add('  gwsw:hasAspect  :' + Persleiding^.GUID + '_ori ;');

  AddPersleidingKenmerken(SL, Persleiding);

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Persleiding oriëntatie
  ExportPersleidingOrientatie(SL, Persleiding);
end;

procedure TOroxExportModel.AddLeidingKenmerken(SL: TStringList; const Leiding: PGWSWLeiding);
begin
  if Leiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox2Dec(Leiding^.Lengte) + ' ] ;');

  if Leiding^.Diameter > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterLeiding ; gwsw:hasValue ' +
           IntToStr(Leiding^.Diameter) + ' ] ;');

  if Leiding^.Breedte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedteLeiding ; gwsw:hasValue ' +
           IntToStr(Leiding^.Breedte) + ' ] ;');

  if Leiding^.Hoogte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogteLeiding ; gwsw:hasValue ' +
           IntToStr(Leiding^.Hoogte) + ' ] ;');

  if Leiding^.MateriaalURI <> '' then ;
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalLeiding ; ' +
         'gwsw:hasReference ' + Leiding^.MateriaalURI + ' ] ;');

  if Leiding^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormLeiding ; ' +
         'gwsw:hasReference ' + Leiding^.VormURI + ' ] ;');

  if Leiding^.StatusFunctionerenURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:StatusFunctioneren ; ' +
         'gwsw:hasReference ' + Leiding^.StatusFunctionerenURI + ' ] ;');

  if Leiding^.Begindatum <> 0 then
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
         'gwsw:hasValue "' + DateToOroxFormat(Leiding^.Begindatum) + '"^^xsd:date ] ;'); // OROX/RDF heeft datum alstijd in formaat: YYYY-MM-DD
end;

procedure TOroxExportModel.AddKolkKenmerken(SL : TStringList; const Kolk : PGWSWKolk);
begin
  // Afmetingen
  if not IsNan(Kolk^.Breedte) and (Kolk^.Breedte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Breedte) + ' ] ;');

  if not IsNan(Kolk^.Lengte) and (Kolk^.Lengte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Lengte) + ' ] ;');

  if not IsNan(Kolk^.Hoogte) and (Kolk^.Hoogte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Hoogte) + ' ] ;');

  // Diameter voor ronde kolken
  if not IsNan(Kolk^.Diameter) and (Kolk^.Diameter > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterPut ; gwsw:hasValue ' +
           IntToStr(Kolk^.Diameter) + ' ] ;');

  // Materiaal
  if Kolk^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalPut ; ' +
          'gwsw:hasReference ' + Kolk^.MateriaalURI + ' ] ;');   // =  MateriaalPut volgens de ontologie ?

  // Vorm
  if Kolk^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormPut ; ' +
         'gwsw:hasReference ' + Kolk^.VormURI + ' ] ;');

  // Kolk-specifieke kenmerken
  if not IsNan(Kolk^.Wanddikte) and (Kolk^.Wanddikte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WanddiktePut ; gwsw:hasValue ' +
           IntToStr(Kolk^.Wanddikte) + ' ] ;');

  if Kolk^.TypeReiniging <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:TypeReinigingKolk ; ' +
           'gwsw:hasValue "' + Kolk^.TypeReiniging + '" ] ;');
end;

procedure TOroxExportModel.AddKolkOrientatie(SL: TStringList; const Kolk: PGWSWKolk);
var
  GMLString: string;
begin
  SL.Add(':' + Kolk^.GUID + '_ori');
  SL.Add('  rdf:type      gwsw:Putorientatie ;');
  SL.Add('  gwsw:hasAspect  [');
  SL.Add('    rdf:type      gwsw:Punt ;');

  if Kolk^.HasWKTGeometry and (Kolk^.WKTGeometry <> '') then begin
    GMLString := ConvertWKTToGML(Kolk^.WKTGeometry);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral');
  end
  else begin
    SL.Add('    gwsw:hasValue "' + CreateGMLPoint(Kolk^.X, Kolk^.Y, Kolk^.Z) + '"^^geo:gmlLiteral');
  end;

  SL.Add('  ] ;');

  // NIEUW: Voeg een hasConnection toe (zoals bij putten)
  // Maak een dummy maaiveldorientatie voor de kolk
  SL.Add('  gwsw:hasConnection  _:' + Kolk^.GUID + '_maaiveld_ori ;');

  SL.Add('.');
  SL.Add('');

  // Voeg de dummy maaiveldorientatie toe
  SL.Add('_:' + Kolk^.GUID + '_maaiveld_ori');
  SL.Add('  rdf:type                      gwsw:Maaiveldorientatie ;');
  SL.Add('  gwsw:hasAspect                [');
  SL.Add('    rdf:type                      gwsw:Maaiveldhoogte ;');
  // Gebruik de Z-coördinaat als maaiveldhoogte, of een standaardwaarde
  if not IsNaN(Kolk^.Z) and (Kolk^.Z <> 0) then
    SL.Add('    gwsw:hasValue                 "' + FloatToOrox2Dec(Kolk^.Z) + '"^^xsd:decimal')
  else
    SL.Add('    gwsw:hasValue                 "0.00"^^xsd:decimal');
  SL.Add('  ] .');
  SL.Add('');
end;

procedure TOroxExportModel.AddPersleidingKenmerken(SL : TStringList; const Persleiding : PGWSWPersleiding);
begin
  // Basis kenmerken
  if Persleiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox2Dec(Persleiding^.Lengte) + ' ] ;');

  if Persleiding^.Diameter > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterLeiding ; gwsw:hasValue ' +
           IntToStr(Persleiding^.Diameter) + ' ] ;');

  if Persleiding^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalLeiding ; ' +
         'gwsw:hasReference ' + Persleiding^.MateriaalURI + ' ] ;');

  if Persleiding^.StatusFunctionerenURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:StatusFunctioneren ; ' +
         'gwsw:hasReference ' + Persleiding^.StatusFunctionerenURI + ' ] ;');

  if Persleiding^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormLeiding ; ' +
         'gwsw:hasReference ' + Persleiding^.VormURI + ' ] ;');

  if Persleiding^.Begindatum <> 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
         'gwsw:hasValue "' + DateToOroxFormat(Persleiding^.Begindatum) + '"^^xsd:date ] ;');
end;

procedure TOroxExportModel.ExportPersleidingOrientatie(SL : TStringList; const Persleiding : PGWSWPersleiding);
var
  GMLString: string;
begin
  SL.Add(':' + Persleiding^.GUID + '_ori');
  SL.Add('  rdf:type        gwsw:Leidingorientatie ;');

  // Beginpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:BeginpuntLeiding ;');
  if not IsNaN(Persleiding^.BobBegin) and (Persleiding^.BobBegin <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobBeginpuntLeiding ; gwsw:hasValue ' +
           FloatToOrox2Dec(Persleiding^.BobBegin) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Persleiding^.BeginPutID + '_ori ] ;');

  // Eindpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:EindpuntLeiding ;');
  if not IsNaN(Persleiding^.BobEind) and (Persleiding^.BobEind <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobEindpuntLeiding ; gwsw:hasValue ' +
           FloatToOrox2Dec(Persleiding^.BobEind) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Persleiding^.EindPutID + '_ori ] ;');

  // Geometrie - speciaal voor persleiding met meerdere vertices
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Lijn ;');

  if Persleiding^.HasWKTGeometry and (Persleiding^.WKTGeometry <> '') then begin
    if Persleiding^.HasMultipleVertices then
      GMLString := ConvertWKTToGMLLineString(Persleiding^.WKTGeometry)  // Complexe geometrie
    else
      GMLString := CreateGMLLineStringForPersleiding(Persleiding);  // Eenvoudige geometrie

    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end
  else begin
    // Fallback geometrie
    GMLString := CreateGMLLineStringForPersleiding(Persleiding);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end;

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1] := Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1] := SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');
end;

procedure TOroxExportModel.ClearAllLists;
begin
  ClearPutList;
  ClearLeidingList;
  ClearStelselList;
  ClearKolkList;
  ClearPersleidingList;
end;

procedure TOroxExportModel.ClearPutList;
var
  i: Integer;
  Put: PGWSWPut;
begin
  for i := 0 to FPutList.Count - 1 do begin
    Put := PGWSWPut(FPutList[i]);
    Dispose(Put);
  end;
  FPutList.Clear;
end;

procedure TOroxExportModel.ClearLeidingList;
var
  i: Integer;
  Leiding: PGWSWLeiding;
begin
  for i := 0 to FLeidingList.Count - 1 do begin
    Leiding := PGWSWLeiding(FLeidingList[i]);
    Dispose(Leiding);
  end;
  FLeidingList.Clear;
end;

procedure TOroxExportModel.ClearStelselList;
var
  i: Integer;
  Stelsel: PGWSWStelsel;
begin
  for i := 0 to FStelselList.Count - 1 do begin
    Stelsel := PGWSWStelsel(FStelselList[i]);
    Dispose(Stelsel);
  end;
  FStelselList.Clear;
end;

procedure TOroxExportModel.ClearKolkList;
var
  i: Integer;
  Kolk: PGWSWKolk;
begin
  for i := 0 to FKolkList.Count - 1 do begin
    Kolk := PGWSWKolk(FKolkList[i]);
    Dispose(Kolk);
  end;
  FKolkList.Clear;
end;

procedure TOroxExportModel.ClearPersleidingList;
var
  i: Integer;
  Persleiding: PGWSWPersleiding;
begin
  for i := 0 to FPersleidingList.Count - 1 do begin
    Persleiding := PGWSWPersleiding(FPersleidingList[i]);
    Dispose(Persleiding);
  end;
  FPersleidingList.Clear;
end;

function TOroxExportModel.WijzeInwinningToGWSW(Wijze: TGWSWWijzeInwinning): string;
begin
  case Wijze of
    wiGemeten: Result := 'gwsw:Ingemeten';
    wiBerekend: Result := 'gwsw:Berekend';
    wiGeschat: Result := 'gwsw:Geschat';
    wiOntwerp: Result := 'gwsw:Ontwerp';
  else
    Result := 'gwsw:Onbekend';
  end;
end;

function TOroxExportModel.CreateGMLLineStringForPersleiding(const Persleiding : PGWSWPersleiding) : string;
var
  PosList: string;
  i: Integer;
  srsDimension: string;
  HasZValues: Boolean;
begin
  // Bepaal of we Z-waarden hebben
  HasZValues := False;
  if Length(Persleiding^.Vertices) > 0 then
    HasZValues := (Persleiding^.Vertices[0][2] <> 0);

  if HasZValues then
    srsDimension := '3'
  else
    srsDimension := '2';

  // Bouw de posList op basis van vertices
  PosList := '';
  for i := 0 to Length(Persleiding^.Vertices) - 1 do begin
    if PosList <> '' then
      PosList := PosList + ' ';

    if HasZValues then
      PosList := PosList +
        FloatToOrox(Persleiding^.Vertices[i][0]) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][1]) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][2])
    else
      PosList := PosList +
        FloatToOrox(Persleiding^.Vertices[i][0]) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][1]);
  end;

  // Als er geen vertices zijn, gebruik begin- en eindpunten
  if PosList = '' then begin
    if HasZValues then
      PosList :=
        FloatToOrox(Persleiding^.BeginPutX) + ' ' +
        FloatToOrox(Persleiding^.BeginPutY) + ' ' +
        FloatToOrox(Persleiding^.BeginPutZ) + ' ' +
        FloatToOrox(Persleiding^.EindPutX) + ' ' +
        FloatToOrox(Persleiding^.EindPutY) + ' ' +
        FloatToOrox(Persleiding^.EindPutZ)
    else
      PosList :=
        FloatToOrox(Persleiding^.BeginPutX) + ' ' +
        FloatToOrox(Persleiding^.BeginPutY) + ' ' +
        FloatToOrox(Persleiding^.EindPutX) + ' ' +
        FloatToOrox(Persleiding^.EindPutY);
  end;

  Result := '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"' +
             srsDimension + '\">' + PosList + '</gml:posList></gml:LineString>';
end;

function TOroxExportModel.CreateGMLPoint(X, Y, Z: Double): string;
var
  srsDimension: string;
  coordString: string;
begin
  if Z <> 0 then
    srsDimension := '3'
  else
    srsDimension := '2';

  coordString := FormatOrox('%.3f %.3f %.3f', [X, Y, Z]);

  Result := '<gml:Point xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:pos srsDimension=\"' + srsDimension + '\">' +
            coordString +
            '</gml:pos></gml:Point>';
end;

function TOroxExportModel.CreateGMLLineString(const Points: array of Double): string;
var
  I: Integer;
  PosList: string;
  srsDimension: string;
  HasZValues: Boolean;
begin
  PosList := '';
  HasZValues := False;

  if Length(Points) = 0 then begin
    Result := '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"2\">0 0 10 10</gml:posList></gml:LineString>';
    Exit;
  end;

  HasZValues := (Length(Points) mod 3 = 0);

  if HasZValues then
    srsDimension := '3'
  else
    srsDimension := '2';

  for I := 0 to (Length(Points) div 3) - 1 do begin
    if PosList <> '' then
      PosList := PosList + ' ';

    if HasZValues then
      PosList := PosList + FloatToOrox(Points[I*3]) + ' ' +
                         FloatToOrox(Points[I*3+1]) + ' ' +
                         FloatToOrox(Points[I*3+2])
    else
      PosList := PosList + FloatToOrox(Points[I*3]) + ' ' +
                         FloatToOrox(Points[I*3+1]);
  end;

  Result := '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"' +
             srsDimension + '\">' + PosList +
            '</gml:posList></gml:LineString>';
end;

end.
