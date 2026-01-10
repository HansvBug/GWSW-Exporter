{ Copyright ©2025-2026 Hans van Buggenum }
unit OroxExport;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, Variants, Math, DateUtils, StrUtils,
  model.intf, uIGWSWDataProvider, GWSWTypes, MappingManager,
  exportprogressreporter.intf, GWSWValidationConfig, GWSWValidation;

type
  { TOroxExport }
  TOroxExport= class(TObject)
    private
      fDataProvider: IGWSWDataProvider;
      fFileName: String;
      fOrganizationName: String;
      fProgressReporter: IExportProgressReporter;
      fTotalRecords: Integer;
      fCurrentRecord: Integer;

      fPutList: TList;
      fLeidingList: TList;
      fStelselList: TList;
      fKolkList: TList;
      fPersleidingList: TList;
      fMappingManager: TMappingManager;

      procedure ClearAllLists;
      procedure ClearPutList;
      procedure ClearLeidingList;
      procedure ClearStelselList;
      procedure ClearKolkList;
      procedure ClearPersleidingList;
      function MapDatabaseToGWSW(DataProvider : IGWSWDataProvider; TotalRecords : Integer) : Boolean;  // Map de BOR data naar de GWSW domeinlijsten
      function GenerateOroxTurtle(const FileName, Version : string) : Boolean;  // Bouw de ttl op
      function MapStelselFromProvider(DataProvider: IGWSWDataProvider): PGWSWStelsel;
      function InitializeStelsel(var Stelsel: PGWSWStelsel): Boolean;
      function MapPutFromProvider(DataProvider: IGWSWDataProvider): PGWSWPut;
      function InitializePut(var Put : PGWSWPut) : Boolean;
      function MapLeidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWLeiding;
      function InitializeLeiding(var Leiding : PGWSWLeiding) : Boolean;
      function MapPersleidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWPersleiding;
      function InitializePersleiding(var Persleiding : PGWSWPersleiding) : Boolean;
      function MapKolkFromProvider(DataProvider: IGWSWDataProvider): PGWSWKolk;
      function InitializeKolk(var Kolk : PGWSWKolk) : Boolean;

      function ParseWKTPoint(const WKT: string; out X, Y, Z: Double): Boolean;
      function GetDateFromYearField(DataProvider: IGWSWDataProvider; const FieldName: string;
                                                DefaultDay: Integer = 1;
                                                DefaultMonth: Integer = 1): TDateTime;
      procedure SplitString(const Input: string; const Delimiter: Char; Strings: TStrings);
      function DateToOroxFormat(DateValue: TDateTime): string;  // OROX/RDF has date as time in format: YYYY-MM-DD
      function ConvertWKTToGML(const WKT: string): string;
      function ConvertWKTToGMLLineString(const WKT: string): string;
      function CreateGMLPoint(X, Y, Z: Double): string;
      function CreateGMLLineString(const Points: array of Double): string;
      procedure ParsePersleidingGeometry(Persleiding: PGWSWPersleiding);
      function CreateGMLLineStringForPersleiding(const Persleiding: PGWSWPersleiding): string;


      procedure AddPutKenmerken(SL: TStringList; const Put: PGWSWPut);
      procedure AddPutOrientatie(SL: TStringList; const Put: PGWSWPut);
      procedure AddLeidingKenmerken(SL: TStringList; const Leiding: PGWSWLeiding);
      procedure AddPersleidingKenmerken(SL: TStringList; const Persleiding: PGWSWPersleiding);
      procedure ExportPersleidingOrientatie(SL: TStringList; const Persleiding: PGWSWPersleiding);
      procedure AddKolkKenmerken(SL: TStringList; const Kolk: PGWSWKolk);
      procedure AddKolkOrientatie(SL: TStringList; const Kolk: PGWSWKolk);

      function FloatToOrox(Value : Double; Decimals : Integer) : String; // Ongeacht de landinstelling, het Orox ttl bestand verwacht een "." als decimaal scheidingsteken. (en geen ",")
      function FormatOrox(const FormatStr: string; const Args: array of const): string;// Regardless of the locale, the Orox ttl file expects a "." as a decimal separator. (and not ",")

      procedure ExportStelselToTurtle(SL: TStringList; const Stelsel: PGWSWStelsel);
      procedure ExportPutToTurtle(SL: TStringList; const Put: PGWSWPut);
      procedure ExportLeidingToTurtle(SL: TStringList; const Leiding: PGWSWLeiding);
      procedure ExportPersleidingToTurtle(SL: TStringList; const Persleiding: PGWSWPersleiding);
      procedure ExportKolkToTurtle(SL: TStringList; const Kolk: PGWSWKolk);
      //exportToTurtle helper
      function TryDifferentDateFormats(const ValueStr: string; out DateVal: TDateTime): Boolean;

      procedure ReportProgress(const Msg: string);
      procedure ReportError(const ErrMsg: string; const ErrorType: Integer = 0; const Guid: string = '');
      procedure UpdateProgressCount(Current, Total: Integer);


    public
      constructor Create(ADataProvider : IGWSWDataProvider; const FileName, OrganizationName, MappingsFile : String; AProgressReporter: IExportProgressReporter);
      destructor Destroy; override;
      procedure ExportToOrox(const GWSWversion: String);

  end;

const Default_Z_Value = -9999; // De rioned server controleert op: Kenmerk Z_coordinaat - waarde wijkt af (min=-20,max=300)

implementation
uses common.consts;

{ TOroxExport }

procedure TOroxExport.ClearAllLists;
begin
  ClearPutList;
  ClearLeidingList;
  ClearStelselList;
  ClearKolkList;
  ClearPersleidingList;
end;

procedure TOroxExport.ClearPutList;
var
  i: Integer;
  Put: PGWSWPut;
begin
  for i:= 0 to fPutList.Count - 1 do begin
    Put:= PGWSWPut(fPutList[i]);
    Dispose(Put);
  end;
  fPutList.Clear;
end;

procedure TOroxExport.ClearLeidingList;
var
  i: Integer;
  Leiding: PGWSWLeiding;
begin
  for i:= 0 to fLeidingList.Count - 1 do begin
    Leiding:= PGWSWLeiding(fLeidingList[i]);
    Dispose(Leiding);
  end;
  fLeidingList.Clear;
end;

procedure TOroxExport.ClearStelselList;
var
  i: Integer;
  Stelsel: PGWSWStelsel;
begin
  for i:= 0 to fStelselList.Count - 1 do begin
    Stelsel:= PGWSWStelsel(fStelselList[i]);
    Dispose(Stelsel);
  end;
  fStelselList.Clear;
end;

procedure TOroxExport.ClearKolkList;
var
  i: Integer;
  Kolk: PGWSWKolk;
begin
  for i:= 0 to fKolkList.Count - 1 do begin
    Kolk:= PGWSWKolk(fKolkList[i]);
    Dispose(Kolk);
  end;
  fKolkList.Clear;
end;

procedure TOroxExport.ClearPersleidingList;
var
  i: Integer;
  Persleiding: PGWSWPersleiding;
begin
  for i:= 0 to fPersleidingList.Count - 1 do begin
    Persleiding:= PGWSWPersleiding(fPersleidingList[i]);
    Dispose(Persleiding);
  end;
  fPersleidingList.Clear;
end;

function TOroxExport.MapDatabaseToGWSW(DataProvider : IGWSWDataProvider; TotalRecords : Integer) : Boolean;
var
  Stelsel, Put, Leiding, Persleiding, Kolk: Boolean;
begin
  Result:= False;
  Stelsel:= False;
  Put:= False;
  Leiding:= False;
  Persleiding:= False;
  Kolk:= False;
  // Initialiseer progress tracking
  fTotalRecords:= TotalRecords;
  fCurrentRecord:= 0;

  try
    ClearAllLists;
    DataProvider.Open;
    try
      if DataProvider.First then begin
        repeat
          Inc(fCurrentRecord);

          // Update progress every 100 records or at milestones
          // Mapping is time-consuming. So update the gui less and update the progress bar per 100 records
          if (fCurrentRecord mod 100 = 0) or (fCurrentRecord = 1) or (fCurrentRecord = TotalRecords) then
            UpdateProgressCount(fCurrentRecord, TotalRecords);

          case UpperCase(DataProvider.GetObjectType) of
            'STELSEL': begin
              if not Stelsel then begin
                Stelsel:= True;  // Show only once.
                ReportProgress('MappingSewerSystem');  // Mapping Stelsel...
              end;
              FStelselList.Add(MapStelselFromProvider(DataProvider));
            end;
            'PUT': begin
              if not Put then begin
                Put:= True;
                ReportProgress('MappingManhole'); // Mapping Put...
              end;
              FPutList.Add(MapPutFromProvider(DataProvider));
            end;
            'LEIDING': begin
              if not Leiding then begin
                Leiding:= True;
                ReportProgress('MappingPipeline');  // Mapping Leiding...
              end;
              FLeidingList.Add(MapLeidingFromProvider(DataProvider));
            end;
            'PERSLEIDING': begin
              if not Persleiding then begin
                Persleiding:= True;
                ReportProgress('MappingMechanicalPipeline');
              end;
              FPersLeidingList.Add(MapPersleidingFromProvider(DataProvider));
            end;
            'KOLK': begin
              if not Kolk then begin
                Kolk:= True;
                ReportProgress('MappingGully');
              end;
              FKolkList.Add(MapKolkFromProvider(DataProvider));
            end
            else begin
              ReportError('Onbekend Object type aangetroffen.');   { #todo : Taalinstelling }
              ReportError('Dit is: ' + UpperCase(DataProvider.GetObjectType));
            end;
          end;

        until not DataProvider.Next;
      end;
    finally
      DataProvider.Close;
    end;

    Result:= True;
  except
    on E: Exception do
      raise Exception.Create('Mapping fout: ' + E.Message);   { #todo : Moet naar de view. } { #todo : Taalinstelling }
  end;
end;

function TOroxExport.GenerateOroxTurtle(const FileName, Version : string) : Boolean;
var
  SL: TStringList;
  i: Integer;
  TotalObjects, CurrentObject: Integer;
begin
  Result:= False;

  // Bereken totaal aantal objecten
  TotalObjects:= fStelselList.Count + fPutList.Count + fLeidingList.Count +
                 fPersleidingList.Count + fKolkList.Count;
  CurrentObject:= 0;

  SL:= TStringList.Create;  // de specificaties schrijven utf 8 voor. (nodig voor é ë etc.)
  try

    SL.WriteBOM:= False; { #todo : Uitzoeken. }

    // Prefixes conform GWSW-OroX specificatie.
    SL.Add('@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .');
    SL.Add('@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .');
    SL.Add('@prefix owl:     <http://www.w3.org/2002/07/owl#> .');
    SL.Add('@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .');
    SL.Add('@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .');
    SL.Add('@prefix geo:     <http://www.opengis.net/ont/geosparql#> .');
    SL.Add('@prefix gwsw:    <http://data.gwsw.nl/'+ Version + '/totaal/> .');

    if fOrganizationName <> '' then  // fOrganizationName is now mandatory
      SL.Add('@prefix :        <http://sparql.gwsw.nl/repositories/' + fOrganizationName + '#> .')  // Dit komt voor de URI staan!
    else
      SL.Add('@prefix :        <http://sparql.gwsw.nl/repositories/#> .');  // Dit komt voor de URI staan!

    SL.Add('');
    SL.Add('# GWSW-Exporter. (https://codeberg.org/HansvBug/GWSW-Exporter)');
    SL.Add('# Exportdatum: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SL.Add('# ' + fOrganizationName );
    SL.Add('# ##############################################################');
    SL.Add('');
    SL.Add('');

    // Export Stelsels
    for i:= 0 to FStelselList.Count - 1 do begin
      ExportStelselToTurtle(SL, PGWSWStelsel(FStelselList[i]));
      Inc(CurrentObject);
      // The export is relatively fast so the gui may update more often here than during the mapping and we update the progress bar with 50 records
      if (CurrentObject mod 100 = 0) and (TotalObjects > 0) then
        UpdateProgressCount(CurrentObject, TotalObjects);
    end;

    // Export Putten. Als putten zonder leidingen worden geëxporteerd dan geeft de balidatie een fout. het "netwerk" is dan niet goed.
     for i:= 0 to FPutList.Count - 1 do begin
       ExportPutToTurtle(SL, PGWSWPut(FPutList[i]));
       Inc(CurrentObject);
       if (CurrentObject mod 100 = 0) and (TotalObjects > 0) then
         UpdateProgressCount(CurrentObject, TotalObjects);
     end;

     // Export Leidingen
     for i:= 0 to FLeidingList.Count - 1 do begin
       ExportLeidingToTurtle(SL, PGWSWLeiding(FLeidingList[i]));
       Inc(CurrentObject);
       if (CurrentObject mod 100 = 0) and (TotalObjects > 0) then
         UpdateProgressCount(CurrentObject, TotalObjects);
     end;

     // Export Persleidingen
     for i:= 0 to FPersleidingList.Count - 1 do begin
       ExportPersleidingToTurtle(SL, PGWSWPersleiding(FPersleidingList[i]));
       Inc(CurrentObject);
       if (CurrentObject mod 100 = 0) and (TotalObjects > 0) then
         UpdateProgressCount(CurrentObject, TotalObjects);
     end;

     // Export Kolken
     for i:= 0 to FKolkList.Count - 1 do begin
       ExportKolkToTurtle(SL, PGWSWKolk(FKolkList[i]));
       Inc(CurrentObject);
       if (CurrentObject mod 100 = 0) and (TotalObjects > 0) then
         UpdateProgressCount(CurrentObject, TotalObjects);
     end;

    if TotalObjects > 0 then
      UpdateProgressCount(TotalObjects, TotalObjects);

    // Opslaan
    ReportProgress('Opslaan bestand...');
    SL.SaveToFile(FileName);
    Result:= True;

  finally
    SL.Free;
  end;
end;

function TOroxExport.MapStelselFromProvider(DataProvider : IGWSWDataProvider) : PGWSWStelsel;
var
  Stelsel: PGWSWStelsel;
  lValue: Variant;
begin
  New(Stelsel);

  // Stop als InitializeStelsel faalt
  if not InitializeStelsel(Stelsel) then
  begin
    Dispose(Stelsel);
    ReportError('Stelsel: Initialisatie mislukt!');
    Exit(nil);
  end;

  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');
    if lValue <> '' then
      Stelsel^.GUID:= lValue
    else
      ReportError('Stelsel: "GUID" is leeg.', eetFieldIsEmpty, Stelsel^.GUID);
  end
  else
    ReportError('Stelsel: "Guid" veld ontbreekt.', eetFieldIsMissing, '');

  // Korte naam (STELSELCODE)
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Stelsel^.aLabel:= lValue
    else
      ReportError('Stelsel: "Naam" is leeg.', eetFieldIsEmpty, Stelsel^.GUID);
  end
  else
    ReportError('Stelsel: "Naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Lange naam  (STELSELNAAM)
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Stelsel^.StelselNaam:= lValue
    else
      ReportError('Stelsel: "Stelselnaam" is leeg.', eetFieldIsEmpty, Stelsel^.GUID);
  end
  else
    ReportError('Stelsel: "Stelsel naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Als er geen korte naam is, gebruik dan de lange naam voor aLabel
  if (Stelsel^.aLabel= '') and (Stelsel^.StelselNaam <> '') then
    Stelsel^.aLabel:= Stelsel^.StelselNaam;

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));   // Default is "Onbekend"
    Stelsel^.StelselTypeUri:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);

    if Stelsel^.StelselTypeUri = '' then begin
      Stelsel^.StelselTypeUri:= '""';
      ReportError('StelseltypeIsMissing', eetFieldIsEmpty, Stelsel^.GUID);
    end
    else if Stelsel^.StelselTypeUri = 'Mapping_error' then begin
      Stelsel^.StelselTypeUri:= '""';
      ReportError('Stelsel: "Stelseltype" veld niet conform gwsw domeinlijst.', eetMapping, Stelsel^.GUID);
    end
  end
  else
    ReportError('Stelsel: "Stelseltype" veld ontbreekt.', eetFieldIsMissing, '');

  Result:= Stelsel;
end;

function TOroxExport.InitializeStelsel(var Stelsel : PGWSWStelsel) : Boolean;
begin
  Result:= False;
  if Stelsel = nil then Exit;

  FillChar(Stelsel^, SizeOf(TGWSWStelsel), 0);
  Stelsel^.GUID:= '';
  Stelsel^.aLabel:= '';
  Stelsel^.StelselNaam:= '';
  Stelsel^.StelselTypeUri:= '';
  Stelsel^.HasParts:= True;

  Result:= True;
end;

function TOroxExport.MapPutFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPut;
var
  Put: PGWSWPut;
  lVal: TGWSWValidationResult;
  WKTGeometry: string;
  GeoX, GeoY, GeoZ: Double;
  lValue: Variant;
  lDate: TDateTime;
  tmp: String;
begin
  // Misschien ... optie die aangeeft dat als er ook maar 1 mapping fout is dan wordt de put niet meegenomen
  // dan wordt het iets van
{  try
   except
    on E: Exception do
    begin
      ReportError('Put mapping fout: ' + E.Message);
      Dispose(Put);
      Result:= nil;
    end;
  end;}

  New(Put);
  if not InitializePut(Put) then begin
    Dispose(Put);
    ReportError('Put: Initialisatie mislukt!');
    Exit(nil);
  end;

  // Initialiseer nieuwe velden
  Put^.WKTGeometry:= '';
  Put^.HasWKTGeometry:= False;
  Put^.StelselID:= '';

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Put^.GUID:= lValue
    else
      ReportError('Put: "GUID" is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "GUID" veld ontbreekt.', eetFieldIsMissing, '');  // Can never happen but still...

  // Naam (putcode)
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Put^.aLabel:= lValue
    else begin
      // Put^.aLabel:= '';  = dubbel met   VarToStrDef
      ReportError('Put: "Naam" is leeg.', eetFieldIsEmpty, Put^.GUID);
      // ernstig, dit is de putcode die dan in BOR ontbreekt
    end
  end
  else
  ReportError('Put: "Naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Geometrie
  if DataProvider.FieldExists('WKT_GEOMETRY') then begin
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

    if not VarIsNull(WKTGeometry) and (Trim(VarToStr(WKTGeometry)) <> '') then begin
      Put^.WKTGeometry:= Trim(VarToStr(WKTGeometry));
      Put^.HasWKTGeometry:= True;

      // Parse ook naar X,Y,Z voor backward compatibility
      if ParseWKTPoint(WKTGeometry, GeoX, GeoY, GeoZ) then begin
        Put^.X:= GeoX;
        Put^.Y:= GeoY;
        Put^.Z:= GeoZ;
        Put^.HasOrientation:= (GeoX <> 0) and (GeoY <> 0);
      end
    end
    else begin
      ReportError('Put: "WKT-Geometry" is leeg. De put wordt niet in het exportbestand gezet.', eetFatal, Put^.GUID);
      Dispose(Put);
      Result:= nil;
      Exit;
    end
  end
  else begin
    ReportError('Put: "WKT_Geometry" veld ontbreekt.', eetFatal, '');
    Dispose(Put);
    Result:= nil;
    Exit;
  end;

  // Breedte
  if DataProvider.FieldExists('BREEDTE') then begin     { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then begin
      Put^.Breedte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePutWidth(Put);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Put^.GUID);
    end
    else
      ReportError('Put: "Breedte" is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Breedte" veld ontbreekt.', eetFieldIsMissing, '');

  // Lengte
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then begin
      Put^.Lengte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePutLength(Put);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Put^.GUID);
    end
    else
      ReportError('Put: "Lengte" veld is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Lengte" veld ontbreekt.', eetFieldIsMissing, '');

  // Hoogte
  if DataProvider.FieldExists('HOOGTE') then begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then begin
      Put^.Hoogte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePutHeight(Put);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Put^.GUID);
    end
    else
      ReportError('Put: "Hoogte" veld is leeg.' , eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Hoogte" veld ontbreekt.', eetFieldIsMissing, '');

  // Diameter
  if DataProvider.FieldExists('DIAMETER') then begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then begin
      Put^.Diameter:= VarAsType(lValue, varInteger);

      lVal:= ValidatePutDiameter(Put);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Put^.GUID);
    end
    else
      ReportError('Put: "Diameter" veld is leeg.' , eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Diameter" veld ontbreekt.', eetFieldIsMissing, '');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Put^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalPut, lValue);

    if Put^.MateriaalURI = '' then
      ReportError('Put: "Materiaal" veld is leeg.', eetFieldIsEmpty, Put^.GUID)
    else if Put^.MateriaalURI = 'Mapping_error' then begin
      Put^.MateriaalURI:= '';
      ReportError('Put: "Materiaal" veld niet conform gwsw domeinlijst.', eetMapping, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Materiaal" veld ontbreekt.', eetFieldIsMissing, '');

  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Put^.VormURI:= fMappingManager.GetGWSWURI(mtvormPut, lValue);
    if Put^.VormURI = '' then
      ReportError('Put: "Vorm" veld is leeg.', eetFieldIsEmpty, Put^.GUID)
    else if Put^.VormURI = 'Mapping_error' then begin
      Put^.VormURI:= '';
      ReportError('Put: "Vorm" veld niet conform gwsw domeinlijst.', eetMapping, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Vorm" veld ontbreekt.', eetFieldIsMissing, '');

  // Fundering mapping

  if DataProvider.FieldExists('FUNDERING') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('FUNDERING'), ''));
    Put^.FunderingUri:= fMappingManager.GetGWSWURI(mtFundering, lValue);

    if Put^.FunderingUri = '' then
      ReportError('Put: "Fundering" veld is leeg.', eetFieldIsEmpty, Put^.GUID)
    else if Put^.FunderingUri = 'Mapping_error' then begin
      Put^.FunderingUri:= '';
      ReportError('Put: "Fundering" veld niet conform gwsw domeinlijst.', eetMapping, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Fundering" veld ontbreekt.', eetFieldIsMissing, '');

  // Puttype mapping
  if DataProvider.FieldExists('PUT_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('PUT_TYPE'), ''));
    Put^.PutTypeUri:= fMappingManager.GetGWSWURI(mtPutType, lValue);
    if Put^.PutTypeUri = '' then begin
      Put^.PutTypeUri:= '""';  // Zonder de "" geeft de gwsw validatie een error en ishet bestand niet bruikbaar.
      ReportError('PuttypeIsMissing', eetFieldIsEmpty, Put^.GUID)
    end
    else if Put^.PutTypeUri = 'Mapping_error' then begin
      Put^.PutTypeUri:= '""';  // Zonder de "" geeft de gwsw validatie een error en ishet bestand niet bruikbaar.
      ReportError('Put: "Put type" veld niet conform gwsw domeinlijst.', eetMapping, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Puttype" veld ontbreekt.', eetFieldIsMissing, '');

  // Maaiveld
  if DataProvider.FieldExists('MAAIVELD') then begin { #todo : MAAIVELD hernoemen naar MAAIVELD_HOOGTE }
    lValue:= DataProvider.GetFieldValue('MAAIVELD');
    if not VarIsNull(lValue) then begin
      Put^.Maaiveldhoogte:= lValue;

      lVal:= ValidatePutZCoord(Put);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Put^.GUID);
    end
    else
      ReportError('Put: "Maaiveldhoogte" veld is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Maaiveld" veld ontbreekt.', eetFieldIsMissing, '');

  // Begindatum. Wordt momenteel in BOR niet gebruikt daar voor in de plaats: aanlegjaar omzetten naar een datum.
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Put^.Begindatum:= lDate
    else begin
      Put^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      ReportError('Put: "Begindatum" veld is leeg.', eetFieldIsEmpty, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Begindatum" veld ontbreekt.', eetFieldIsMissing, '');

  // Einddatum
  if DataProvider.FieldExists('EINDDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'EINDDATUM');
    if lDate <> 0 then
      Put^.Einddatum:= lDate
    else begin
      Put^.Einddatum:= 0;
      ReportError('Put: "einddatum" veld is leeg.', eetFieldIsEmpty, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Einddatum" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel relatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Put^.StelselID:= lValue
    else
      ReportError('Put: "Stelsel id" is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Stelsel id" veld ontbreekt.', eetFieldIsMissing, '');

  // stelsel uit de mapping ophalen
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Put^.Stelselnaam:= lValue
    else
      ReportError('Put: "Stelsel_naam" veld is leeg.', eetFieldIsEmpty, Put^.GUID);
  end
  else
    ReportError('Put: "Stelsel naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Put^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);

    if Put^.StelselURI = '' then
      ReportError('Put: "Stelsel type" veld is leeg.', eetFieldIsEmpty, Put^.GUID)
    else if Put^.StelselURI = 'Mapping_error' then begin
      Put^.StelselURI:= '';
      ReportError('Put: "Stelsel type" veld niet conform gwsw domeinlijst.', eetMapping, Put^.GUID);
    end
  end
  else
    ReportError('Put: "Stelseltype" veld ontbreekt.', eetFieldIsMissing, '');

  Put^.HasOrientation:= (not IsNan(Put^.X)) and (not IsNan(Put^.Y)) and
                     (Put^.X <> 0) and (Put^.Y <> 0);

  Result:= Put;
end;

function TOroxExport.InitializePut(var Put : PGWSWPut) : Boolean;
begin
  Result:= False;
  if Put = nil then Exit;

  FillChar(Put^, SizeOf(TGWSWPut), 0);  // Zet alle velden op 0/nil/leeg

  // String velden die niet automatisch geïnitialiseerd worden
   Put^.WKTGeometry:= '';
   Put^.GUID:= '';
   Put^.aLabel:= '';
   Put^.StelselID:= '';
   Put^.Stelselnaam:= '';
   Put^.MateriaalURI:= '';
   Put^.VormURI:= '';
   Put^.PutTypeUri:= '';
   Put^.StelselURI:= '';

   // Numerieke velden op NaN of default waarden
   Put^.Maaiveldhoogte:= NaN;
   Put^.Begindatum:= 0;
   Put^.Einddatum:= 0;

   Put^.X:= NaN;
   Put^.Y:= NaN;
   Put^.Z:= Default_Z_Value;  // Gebruik dezelfde default als in je constante
   Put^.HasWKTGeometry:= False;
   Put^.HasOrientation:= False;

   Result:= True;
end;

function TOroxExport.MapLeidingFromProvider(DataProvider : IGWSWDataProvider) : PGWSWLeiding;
var
  Leiding: PGWSWLeiding;
  lVal: TGWSWValidationResult;
  WKTGeometry: string;
  lValue: Variant;
  lDate: TDateTime;
begin
  New(Leiding);

  if not InitializeLeiding(Leiding) then begin
    Dispose(Leiding);
    ReportError('Leiding: Initialisatie mislukt!');
    Exit(nil);
  end;

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Leiding^.GUID:= lValue
    else
      ReportError('Leiding: "GUID" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Guid" veld ontbreekt.', eetFieldIsMissing, '');  // Can never happen but still...

  // Naam
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Leiding^.aLabel:= lValue
    else
      ReportError('Leiding: "Naam" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Topologie - verbinding met putten
  if DataProvider.FieldExists('BEGINPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('BEGINPUT_ID'), '');
    if lValue <> '' then
      Leiding^.BeginPutID:= lValue
    else
      ReportError('Leiding: "Beginput_id" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Beginput id" veld ontbreekt.', eetFieldIsMissing, '');

  if DataProvider.FieldExists('EINDPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('EINDPUT_ID'), '');
    if lValue <> '' then
      Leiding^.EindPutID:= lValue
    else
      ReportError('Leiding: "Eindput_id" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Eindput id" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Leiding^.StelselID:= lValue
    else
      ReportError('Leiding: "Stelsel_id" is leeg', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Stelsel id" veld ontbreekt.', eetFieldIsMissing, '');

  // Lengte
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then begin
      Leiding^.Lengte:= VarAsType(lValue, varInteger);

      lVal:= ValidateLeidingLength(Leiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Leiding^.GUID);
    end
    else
      ReportError('Leiding: "Lengte" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Lengte" veld ontbreekt.', eetFieldIsMissing, '');

  // Breedte
  if DataProvider.FieldExists('BREEDTE') then begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then begin
      Leiding^.Breedte:= VarAsType(lValue, varInteger);

      lVal:= ValidateLeidingWidth(Leiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Leiding^.GUID);
    end
    else
      ReportError('Leiding: "Breedte" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Breedte" veld ontbreekt.', eetFieldIsMissing, '');

  // Hoogte
  if DataProvider.FieldExists('HOOGTE') then
  begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then begin
      Leiding^.Hoogte:= VarAsType(lValue, varInteger);

      lVal:= ValidateLeidingHeight(Leiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Leiding^.GUID);
    end
    else
      ReportError('Leiding: "Hoogte" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Hoogte" veld ontbreekt.', eetFieldIsMissing, '');

  // Diameter
  if DataProvider.FieldExists('DIAMETER') then
  begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then begin
      Leiding^.Diameter:= VarAsType(lValue, varInteger);

      lVal:= ValidateLeidingDiameter(Leiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Leiding^.GUID);
    end
    else
      ReportError('Leiding: "Diameter" is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Diameter" veld ontbreekt.', eetFieldIsMissing, '');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Leiding^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalLeiding, lValue);

    if Leiding^.MateriaalURI = '' then
      ReportError('Leiding: "Materiaal" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID)

    else if Leiding^.MateriaalURI = 'Mapping_error' then begin
      Leiding^.MateriaalURI:= '';
      ReportError('Leiding: "Materiaal" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Materiaal" veld ontbreekt.', eetFieldIsMissing, '');

  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Leiding^.VormURI:= fMappingManager.GetGWSWURI(mtVormLeiding, lValue);
    if Leiding^.VormURI = '' then
      ReportError('Leiding: "Vorm" veld is leeg', eetFieldIsEmpty, Leiding^.GUID)
    else if Leiding^.VormURI = 'Mapping_error' then begin
      Leiding^.VormURI:= '';
      ReportError('Leiding: "Vorm" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Vorm" veld ontbreekt.', eetFieldIsMissing, '');

  // Fundering mapping
  if DataProvider.FieldExists('FUNDERING') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('FUNDERING'), ''));
    Leiding^.FunderingUri:= fMappingManager.GetGWSWURI(mtFundering, lValue);
    if Leiding^.FunderingUri = '' then
      ReportError('Leiding: "Fundering" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID)
    else if Leiding^.FunderingUri = 'Mapping_error' then begin
      Leiding^.FunderingUri:= '';
      ReportError('Leiding: "Fundering" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Fundering" veld ontbreekt.', eetFieldIsMissing, '');

  // status functioneren
  if DataProvider.FieldExists('STATUS_FUNCTIONEREN') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STATUS_FUNCTIONEREN'), ''));
    Leiding^.StatusFunctionerenURI:= fMappingManager.GetGWSWURI(mtStatusFunctioneren, lValue);
    if Leiding^.StatusFunctionerenURI = '' then
    ReportError('Leiding: "Status functioneren" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID)
    else if Leiding^.StatusFunctionerenURI = 'Mapping_error' then begin
      Leiding^.StatusFunctionerenURI:= '';
      ReportError('Leiding: "Status functioneren" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Status functioneren" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel type
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Leiding^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);

    if Leiding^.StelselURI = '' then
      ReportError('Leiding: "Stelsel type" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID)
    else if Leiding^.StelselURI = 'Mapping_error' then begin
      Leiding^.StelselURI:= '';
      ReportError('Leiding: "Stelsel type" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Stelseltype" veld ontbreekt.', eetFieldIsMissing, '');

  // stelselnaam (vanuit gv_stelsel)
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Leiding^.Stelselnaam:= lValue
    else
      ReportError('Leiding: "Stelsel_naam" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Stelsel naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Leidingtype mapping
  if DataProvider.FieldExists('LEIDING_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('LEIDING_TYPE'), ''));
    Leiding^.LeidingTypeURI:= fMappingManager.GetGWSWURI(mtLeidingType, lValue);
    if Leiding^.LeidingTypeURI = '' then begin
      Leiding^.LeidingTypeURI:= '""';
      ReportError('LeidingtypeIsMissing', eetFieldIsEmpty, Leiding^.GUID)
    end
    else if Leiding^.LeidingTypeURI = 'Mapping_error' then begin
      Leiding^.LeidingTypeURI:= '""';
      ReportError('Leiding: "Leiding type" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Leiding type" veld ontbreekt.', eetFieldIsMissing, '');

  // WibonThema mapping
  if DataProvider.FieldExists('WIBON_THEMA') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('WIBON_THEMA'), ''));
    Leiding^.WibonUri:= fMappingManager.GetGWSWURI(mtWibonThema, lValue);
    if Leiding^.WibonUri = '' then begin
      ReportError('Leiding: "Wion thema" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID)
    end
    else if Leiding^.WibonUri = 'Mapping_error' then begin
      Leiding^.WibonUri := '';
      ReportError('Leiding: "Wion thema" veld niet conform gwsw domeinlijst.', eetMapping, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Wion thema" veld ontbreekt.', eetFieldIsMissing, '');

  // BOB waarden
  if DataProvider.FieldExists('BOB_BEGIN') then begin
    lValue:= DataProvider.GetFieldValue('BOB_BEGIN');
    if not VarIsNull(lValue) then
      Leiding^.BobBegin:= lValue
    else
      ReportError('Leiding: "Bob begin" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Bob begin" veld ontbreekt.', eetFieldIsMissing, '');

  if DataProvider.FieldExists('BOB_EIND') then begin
    lValue:= DataProvider.GetFieldValue('BOB_EIND');
    if not VarIsNull(lValue) then
      Leiding^.BobEind:= lValue
    else
      ReportError('Leiding: "Bob eind" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
  end
  else
    ReportError('Leiding: "Bob eind" veld ontbreekt.', eetFieldIsMissing, '');

  // Begindatum. Wordt momenteel in BOR niet gebruikt daar voor in de plaats: aanlegjaar omzetten naar een datum.
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Leiding^.Begindatum:= lDate
    else begin
      Leiding^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      ReportError('Leiding: "Begindatum" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Begindatum" veld ontbreekt.', eetFieldIsMissing, '');

  // Einddatum
  if DataProvider.FieldExists('EINDDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'EINDDATUM');
    if lDate <> 0 then
      Leiding^.Einddatum:= lDate
    else begin
      Leiding^.Einddatum:= 0;
      ReportError('Leiding: "einddatum" veld is leeg.', eetFieldIsEmpty, Leiding^.GUID);
    end
  end
  else
    ReportError('Leiding: "Einddatum" veld ontbreekt.', eetFieldIsMissing, '');

  // Reading geometry
  Leiding^.WKTGeometry:= '';
  Leiding^.HasWKTGeometry:= False;
  Leiding^.HasMultipleVertices:= False;

  WKTGeometry:= '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then begin
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

    if not VarIsNull(WKTGeometry) and (Trim(VarToStr(WKTGeometry)) <> '') then begin
      Leiding^.WKTGeometry:= Trim(VarToStr(WKTGeometry));
      Leiding^.HasWKTGeometry:= True;
    end
    else begin
      ReportError('Leiding: "WKT_Geometry" is leeg. Leiding wordt niet in het exportbestand gezet.', eetFatal, Leiding^.GUID);
      Dispose(Leiding);
      Result:= nil;
      Exit;
    end;
  end
  else begin
    ReportError('Leiding: "WKT_Geometry" veld ontbreekt.', eetFatal, '');
    Dispose(Leiding);
    Result:= nil;
    Exit;
  end;

  Result:= Leiding;
end;

function TOroxExport.InitializeLeiding(var Leiding : PGWSWLeiding) : Boolean;
begin
  Result:= False;
  if Leiding = nil then Exit;

  FillChar(Leiding^, SizeOf(TGWSWLeiding), 0);  // Zet alle velden op 0/nil/leeg


  // String velden
  Leiding^.GUID:= '';
  Leiding^.aLabel:= '';
  Leiding^.BeginPutID:= '';
  Leiding^.EindPutID:= '';
  Leiding^.StelselID:= '';
  Leiding^.Stelselnaam:= '';
  Leiding^.WKTGeometry:= '';

  // URI velden
  Leiding^.MateriaalURI:= '';
  Leiding^.VormURI:= '';
  Leiding^.StatusFunctionerenURI:= '';
  Leiding^.StelselURI:= '';
  Leiding^.LeidingTypeURI:= '';
  Leiding^.WibonUri:= '';

  Leiding^.Lengte:= NaN;
  Leiding^.BobBegin:= Nan;
  Leiding^.BobEind:= Nan;
  Leiding^.Begindatum:= 0;

  // Boolean/Enum velden
  Leiding^.HasWKTGeometry:= False;
  Leiding^.HasMultipleVertices:= False;

  Result:= True;
end;

function TOroxExport.MapPersleidingFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPersleiding;
var
  Persleiding: PGWSWPersleiding;
  lVal: TGWSWValidationResult;
  lValue: Variant;
  lDate: TDateTime;
  WKTGeometry: Variant;  // Gebruik Variant zoals in andere functies
  WKTGeometryStr: string;
begin
  New(Persleiding);
  if not InitializePersleiding(Persleiding) then begin
    Dispose(Persleiding);
    ReportError('Persleiding: Initialisatie mislukt!');
    Exit(nil);
  end;

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Persleiding^.GUID:= lValue
    else
      ReportError('Leiding: "GUID" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Leiding: "Guid" veld ontbreekt.', eetFieldIsMissing, '');   // Can never happen but still...

  // Naam
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Persleiding^.aLabel:= lValue
    else begin  // Persleidingen hebben geen naam. object_guid als naam opvoeren.
      if Persleiding^.GUID <> '' then
        Persleiding^.aLabel:= Persleiding^.GUID
      else
        ReportError('Persleiding: "Naam" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Topologie - verbinding met putten
  if DataProvider.FieldExists('BEGINPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('BEGINPUT_ID'), '');
    if lValue <> '' then
      Persleiding^.BeginPutID:= lValue
    else
      ReportError('Persleiding: "Beginput id" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Beginput id" veld ontbreekt.', eetFieldIsMissing, '');

  if DataProvider.FieldExists('EINDPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('EINDPUT_ID'), '');
    if lValue <> '' then
      Persleiding^.EindPutID:= lValue
    else
      ReportError('Persleiding: "Eindput id" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Eindput id" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Persleiding^.StelselID:= lValue
    else
      ReportError('Persleiding: "Stelsel_id" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Stelsel id" veld ontbreekt.', eetFieldIsMissing, '');

  // Lengte
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then begin
      Persleiding^.Lengte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePersleidingLength(Persleiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Persleiding^.GUID);
    end
    else
      ReportError('Persleiding: "Lengte" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Lengte" veld ontbreekt.', eetFieldIsMissing, '');

  // Diameter
  if DataProvider.FieldExists('DIAMETER') then
  begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then begin
      Persleiding^.Diameter:= VarAsType(lValue, varInteger);

      lVal:= ValidatePersleidingDiameter(Persleiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Persleiding^.GUID);
    end
    else
      ReportError('Persleiding: "Diameter" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Diameter" veld ontbreekt.', eetFieldIsMissing, '');

  // Breedte
  if DataProvider.FieldExists('BREEDTE') then begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then begin
      Persleiding^.Breedte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePersleidingWidth(Persleiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Persleiding^.GUID);
    end
    else
      ReportError('Persleiding: "Breedte" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Breedte" veld ontbreekt.', eetFieldIsMissing, '');

  // Hoogte
  if DataProvider.FieldExists('HOOGTE') then
  begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then begin
      Persleiding^.Hoogte:= VarAsType(lValue, varInteger);

      lVal:= ValidatePersleidingHeight(Persleiding);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Persleiding^.GUID);
    end
    else
      ReportError('Persleiding: "Hoogte" is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Hoogte" veld ontbreekt.', eetFieldIsMissing, '');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Persleiding^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalLeiding, lValue);

    if Persleiding^.MateriaalURI = '' then
      ReportError('Persleiding: "Materiaal" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID)

    else if Persleiding^.MateriaalURI = 'Mapping_error' then begin
      Persleiding^.MateriaalURI:= '';
      ReportError('Persleiding: "Materiaal" veld niet conform gwsw domeinlijst.', eetMapping, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Materiaal" veld ontbreekt.', eetFieldIsMissing, '');

  // Status functioneren
  if DataProvider.FieldExists('STATUS_FUNCTIONEREN') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STATUS_FUNCTIONEREN'), ''));
    Persleiding^.StatusFunctionerenURI:= fMappingManager.GetGWSWURI(mtStatusFunctioneren, lValue);

    if Persleiding^.StatusFunctionerenURI = '' then
    ReportError('Persleiding: "Status functioneren" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID)
    else if Persleiding^.StatusFunctionerenURI = 'Mapping_error' then begin
      Persleiding^.StatusFunctionerenURI:= '';
      ReportError('Persleiding: "Status functioneren" veld niet conform gwsw domeinlijst.', eetMapping, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Status functioneren" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelsel type
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Persleiding^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);

    if Persleiding^.StelselURI = '' then
      ReportError('Persleiding: "Stelsel type" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID)
    else if Persleiding^.StelselURI = 'Mapping_error' then begin
      Persleiding^.StelselURI:= '""';
      ReportError('Persleiding: "Stelsel type" veld niet conform gwsw domeinlijst.', eetMapping, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Stelseltype" veld ontbreekt.', eetFieldIsMissing, '');

  // Stelselnaam
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Persleiding^.Stelselnaam:= lValue
    else
      ReportError('Persleiding: "Stelsel_naam" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
  end
  else
    ReportError('Persleiding: "Stelsel naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Persleidingtype mapping
  if DataProvider.FieldExists('LEIDING_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('LEIDING_TYPE'), ''));
    Persleiding^.PersleidingTypeURI:= fMappingManager.GetGWSWURI(mtPersleidingType, lValue);
    if Persleiding^.PersleidingTypeURI = '' then begin
      Persleiding^.PersleidingTypeURI:= '""';
      ReportError('PersleidingtypeIsMissing', eetFieldIsEmpty, Persleiding^.GUID)
    end
    else if Persleiding^.PersleidingTypeURI = 'Mapping_error' then begin
      Persleiding^.PersleidingTypeURI:= '""';
      ReportError('Persleiding: "Leiding type" veld niet conform gwsw domeinlijst.', eetMapping, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Leiding type" veld ontbreekt.', eetFieldIsMissing, '');

  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Persleiding^.VormURI:= fMappingManager.GetGWSWURI(mtVormLeiding, lValue);
    if Persleiding^.VormURI = '' then
      ReportError('Persleiding: "Vorm" veld is leeg', eetFieldIsEmpty, Persleiding^.GUID)
    else if Persleiding^.VormURI = 'Mapping_error' then begin
      Persleiding^.VormURI:= '';
      ReportError('Persleiding: "Vorm" veld niet conform gwsw domeinlijst.', eetMapping, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Vorm" veld ontbreekt.', eetFieldIsMissing, '');

  // Datum velden
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Persleiding^.Begindatum:= lDate
    else begin
      Persleiding^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      ReportError('Persleiding: "Begindatum" veld is leeg.', eetFieldIsEmpty, Persleiding^.GUID);
    end
  end
  else
    ReportError('Persleiding: "Begindatum" veld ontbreekt.', eetFieldIsMissing, '');


  Persleiding^.WKTGeometry:= '';
  Persleiding^.HasWKTGeometry:= False;
  Persleiding^.HasMultipleVertices:= False;

  WKTGeometryStr:= '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then begin
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

    if not VarIsNull(WKTGeometry) and (Trim(VarToStr(WKTGeometry)) <> '') then begin
      WKTGeometryStr:= Trim(VarToStr(WKTGeometry));
      Persleiding^.WKTGeometry:= WKTGeometryStr;
      Persleiding^.HasWKTGeometry:= True;

      // Extra: specifieke verwerking voor persleiding geometrie
      if Pos('LINESTRING', UpperCase(Persleiding^.WKTGeometry)) > 0 then
      begin
        ParsePersleidingGeometry(Persleiding);
      end;

    end
    else begin
      ReportError('Leiding: "WKT_Geometry" is leeg. Leiding wordt niet in het exportbestand gezet.', eetFatal, Persleiding^.GUID);
      Dispose(Persleiding);
      Result:= nil;
      Exit;
    end;
  end
  else begin
    ReportError('Persleiding: "WKT_Geometry" veld ontbreekt.', eetFatal, '');
    Dispose(Persleiding);
    Result:= nil;
    Exit;
  end;

  Result:= Persleiding;
end;

function TOroxExport.InitializePersleiding(var Persleiding : PGWSWPersleiding) : Boolean;
begin
  Result:= False;
  if Persleiding = nil then Exit;

  FillChar(Persleiding^, SizeOf(TGWSWPersleiding), 0);

  // String velden
  Persleiding^.GUID:= '';
  Persleiding^.aLabel:= '';
  Persleiding^.BeginPutID:= '';
  Persleiding^.EindPutID:= '';
  Persleiding^.StelselID:= '';
  Persleiding^.Stelselnaam:= '';
  Persleiding^.WKTGeometry:= '';

  // URI velden
  Persleiding^.MateriaalURI:= '';
  Persleiding^.StatusFunctionerenURI:= '';
  Persleiding^.StelselURI:= '';
  Persleiding^.PersleidingTypeURI:= '';
  Persleiding^.VormURI:= '';

  Persleiding^.Lengte:= NaN;
  Persleiding^.Begindatum:= NaN;

  // Boolean velden
  Persleiding^.HasWKTGeometry:= False;
  Persleiding^.HasMultipleVertices:= False;

  Result:= True;
end;

function TOroxExport.MapKolkFromProvider(DataProvider : IGWSWDataProvider) : PGWSWKolk;
var
  Kolk: PGWSWKolk;
  lVal: TGWSWValidationResult;
  WKTGeometry: string;
  GeoX, GeoY, GeoZ: Double;
  lValue: Variant;
begin
  New(Kolk);
  if not InitializeKolk(Kolk) then
  begin
    Dispose(Kolk);
    ReportError('Kolk: Initialisatie mislukt!');
    Exit(nil);
  end;

  // Guid
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Kolk^.GUID:= lValue
    else
      ReportError('Kolk: "GUID" is leeg.', eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Guid" veld ontbreekt.', eetFieldIsMissing, '');  // Can never happen but still...

  // Naam
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Kolk^.aLabel:= lValue
    else begin
      ReportError('Kolk: "Naam" is leeg.', eetFieldIsEmpty, Kolk^.GUID);
    end
  end
  else
    ReportError('Kolk: "Naam" veld ontbreekt.', eetFieldIsMissing, '');

  // Geometrie inlezen
  Kolk^.WKTGeometry:= '';
  Kolk^.HasWKTGeometry:= False;

  if DataProvider.FieldExists('WKT_GEOMETRY') then begin
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

    if WKTGeometry <> '' then begin
      Kolk^.WKTGeometry:= Trim(VarToStr(WKTGeometry));;
      Kolk^.HasWKTGeometry:= True;

      // Parse ook naar X,Y,Z voor backward compatibility
      if ParseWKTPoint(WKTGeometry, GeoX, GeoY, GeoZ) then begin
        Kolk^.X:= GeoX;
        Kolk^.Y:= GeoY;
        Kolk^.Z:= GeoZ;
        Kolk^.HasOrientation:= (GeoX <> 0) and (GeoY <> 0);
      end
    end
    else begin
      ReportError('Kolk: "WKT-Geometry" is leeg. De kolk wordt niet in het exportbestand gezet.', eetFatal, Kolk^.GUID);
      Dispose(Kolk);
      Result:= nil;
      Exit;
    end;
  end
  else begin
    ReportError('Kolk: "WKT_Geometry" veld ontbreekt.', eetFatal, '');
    Dispose(Kolk);
    Result:= nil;
    Exit;
  end;

  // Breedte
  if DataProvider.FieldExists('BREEDTE') then      { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
  begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then begin
      Kolk^.Breedte:= VarAsType(lValue, varInteger);

      lVal:= ValidateKolkWidth(Kolk);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Kolk^.GUID);
    end
    else
      ReportError('Kolk: "Breedte" is leeg.', eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Breedte" veld ontbreekt.', eetFieldIsMissing, '');

  // Lengte
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then begin
      Kolk^.Lengte:= VarAsType(lValue, varInteger);

      lVal:= ValidateKolkLength(Kolk);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Kolk^.GUID);
    end
    else
      ReportError('Kolk: "Lengte" veld is leeg.', eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Lengte" veld ontbreekt.', eetFieldIsMissing, '');

  // Hoogte
  if DataProvider.FieldExists('HOOGTE') then begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then begin
      Kolk^.Hoogte:= VarAsType(lValue, varInteger);

      lVal:= ValidateKolkHeight(Kolk);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Kolk^.GUID);
    end
    else
      ReportError('Kolk: "Hoogte" veld is leeg.' , eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Hoogte" veld ontbreekt.', eetFieldIsMissing, '');

  // Diameter
  if DataProvider.FieldExists('DIAMETER') then begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then begin
      Kolk^.Diameter:= VarAsType(lValue, varInteger);

      lVal:= ValidateKolkDiameter(Kolk);  // Validate
      if not lVal.IsValid then ReportError(lVal.ErrorMsg, eetValueOutOfRange, Kolk^.GUID);
    end
    else
      ReportError('Kolk: "Diameter" veld is leeg.' , eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Diameter" veld ontbreekt.', eetFieldIsMissing, '');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Kolk^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalPut, lValue);

    if Kolk^.MateriaalURI = '' then
      ReportError('Kolk: "Materiaal" veld is leeg.', eetFieldIsEmpty, Kolk^.GUID)

    else if Kolk^.MateriaalURI = 'Mapping_error' then begin
      Kolk^.MateriaalURI:= '';
      ReportError('Kolk: "Materiaal" veld niet conform gwsw domeinlijst.', eetMapping, Kolk^.GUID);
    end
  end
  else
    ReportError('Kolk: "Materiaal" veld ontbreekt.', eetFieldIsMissing, '');

  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Kolk^.VormURI:= fMappingManager.GetGWSWURI(mtvormPut, lValue);
    if Kolk^.VormURI = '' then
      ReportError('Kolk: "Vorm" veld is leeg.', eetFieldIsEmpty, Kolk^.GUID)
    else if Kolk^.VormURI = 'Mapping_error' then begin
      Kolk^.VormURI:= '';
      ReportError('Kolk: "Vorm" veld niet conform gwsw domeinlijst.', eetMapping, Kolk^.GUID);
    end
  end
  else
    ReportError('Kolk: "Vorm" veld ontbreekt.', eetFieldIsMissing, '');

  // Kolktype mapping
  if DataProvider.FieldExists('KOLK_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('KOLK_TYPE'), ''));
    Kolk^.KolkTypeUri:= fMappingManager.GetGWSWURI(mtKolkType, lValue);
    if Kolk^.KolkTypeUri = '' then begin
      Kolk^.KolkTypeUri:= '""';
      ReportError('KolktypeIsMissing', eetFieldIsEmpty, Kolk^.GUID)
    end
    else if Kolk^.KolkTypeUri = 'Mapping_error' then begin
      Kolk^.KolkTypeUri:= '""';
      ReportError('Kolk: "Kolktype" veld niet conform gwsw domeinlijst.', eetMapping, Kolk^.GUID);
    end
  end
  else
    ReportError('Kolk: "Kolktype" veld ontbreekt.', eetFieldIsMissing, '');

  // Kolk-specifieke velden
  if DataProvider.FieldExists('WANDDIKTE') then begin
    lValue:= DataProvider.GetFieldValue('WANDDIKTE');
    if not VarIsNull(lValue) then
      Kolk^.Wanddikte:= lValue
    else
      ReportError('Kolk: "Wanddikte" veld is leeg.', eetFieldIsEmpty, Kolk^.GUID);
  end
  else
    ReportError('Kolk: "Wanddikte" veld ontbreekt.', eetFieldIsMissing, '');

  Kolk^.HasOrientation:= (Kolk^.X <> 0) and (Kolk^.Y <> 0);

  Result:= Kolk;
end;

function TOroxExport.InitializeKolk(var Kolk : PGWSWKolk) : Boolean;
begin
  Result:= False;
  if Kolk = nil then Exit;

  FillChar(Kolk^, SizeOf(TGWSWKolk), 0);

  Kolk^.GUID:= '';
  Kolk^.aLabel:= '';
  Kolk^.WKTGeometry:= '';

  // URI velden
  Kolk^.MateriaalURI:= '';
  Kolk^.VormURI:= '';
  Kolk^.KolkTypeUri:= '';

  // Boolean velden
  Kolk^.HasWKTGeometry:= False;
  Kolk^.HasOrientation:= False;

  Result:= True;
end;

function TOroxExport.ParseWKTPoint(const WKT : string; out X, Y, Z : Double) : Boolean;
var
  CleanWKT, CoordStr: string;
  StartPos, EndPos: Integer;
  Coords: TStringList;
  OldDecimalSeparator: Char;
begin
  Result:= False;
  X:= 0;
  Y:= 0;
  Z:= 0;

  if Trim(WKT) = '' then
    Exit;

  CleanWKT:= Trim(WKT);

  // Keep the old decimal separator and temporarily change to point
  OldDecimalSeparator:= FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:= '.';

    // POINT Z (x y z) - 3D met Z marker
    if Pos('POINT Z', UpperCase(CleanWKT)) = 1 then begin
      StartPos:= Pos('(', CleanWKT);
      EndPos:= Pos(')', CleanWKT);
      if (StartPos > 0) and (EndPos > StartPos) then begin
        CoordStr:= Copy(CleanWKT, StartPos + 1, EndPos - StartPos - 1);
        CoordStr:= Trim(CoordStr);

        Coords:= TStringList.Create;
        try
          SplitString(CoordStr, ' ', Coords);

          if Coords.Count >= 3 then begin
            X:= StrToFloatDef(Trim(Coords[0]), 0);
            Y:= StrToFloatDef(Trim(Coords[1]), 0);
            Z:= StrToFloatDef(Trim(Coords[2]), 0);
            Result:= True;
          end;
        finally
          Coords.Free;
        end;
      end;
    end
    // POINT (x y z) - 3D zonder Z marker
    else if Pos('POINT', UpperCase(CleanWKT)) = 1 then begin
      StartPos:= Pos('(', CleanWKT);
      EndPos:= Pos(')', CleanWKT);
      if (StartPos > 0) and (EndPos > StartPos) then begin
        CoordStr:= Copy(CleanWKT, StartPos + 1, EndPos - StartPos - 1);
        CoordStr:= Trim(CoordStr);

        Coords:= TStringList.Create;
        try
          SplitString(CoordStr, ' ', Coords);

          if Coords.Count >= 2 then begin
            X:= StrToFloatDef(Trim(Coords[0]), 0);
            Y:= StrToFloatDef(Trim(Coords[1]), 0);

            // Als er een derde coordinaat is
            if Coords.Count >= 3 then
              Z:= StrToFloatDef(Trim(Coords[2]), 0)
            else
              Z:= 0;

            Result:= True;
          end;
        finally
          Coords.Free;
        end;
      end;
    end;
  finally
    // Reset the old decimal separator
    FormatSettings.DecimalSeparator:= OldDecimalSeparator;
  end;
end;

function TOroxExport.GetDateFromYearField(DataProvider: IGWSWDataProvider;
  const FieldName: string; DefaultDay: Integer; DefaultMonth: Integer): TDateTime;
var
  lValue: Variant;
  ValueStr: string;
  YearInt: Integer;
  DateVal: TDateTime;
begin
  // Default value (0 means "no valid date")
  Result:= 0;

  lValue:= DataProvider.GetFieldValue(FieldName);
  if VarIsNull(lValue) or VarIsEmpty(lValue) then
    Exit;

  ValueStr:= Trim(VarToStr(lValue));
  if ValueStr = '' then
    Exit;

  { 1) Eerst: is het al een echte datum? }
  // Probeer eerst als datum+tijd (voor strings zoals '31-12-2025 15:42:02')
  if TryStrToDateTime(ValueStr, DateVal) then
  begin
    Result:= DateVal;
    Exit;
  end;

  { 2) Probeer als alleen datum }
  if TryStrToDate(ValueStr, DateVal) then
  begin
    Result:= DateVal;
    Exit;
  end;

  { 3) Probeer met specifieke formaten als de locale conversie faalt }
  if TryDifferentDateFormats(ValueStr, DateVal) then
  begin
    Result:= DateVal;
    Exit;
  end;

  { 4) Zo niet: probeer het als jaartal }
  if not TryStrToInt(ValueStr, YearInt) then
    Exit;

  { 5) Maak datum van jaartal }
  try
    Result:= EncodeDate(YearInt, DefaultMonth, DefaultDay);
  except
    on E: EConvertError do
    begin
      ReportError('Ongeldige datum.');
      Result:= 0; // Ongeldige datum
    end;
  end;
end;

procedure TOroxExport.SplitString(const Input : string; const Delimiter : Char; Strings : TStrings);
var
  i, StartPos: Integer;
  Token: string;
begin
  Strings.Clear;
  StartPos:= 1;

  for i:= 1 to Length(Input) do begin
    if Input[i] = Delimiter then begin
      Token:= Copy(Input, StartPos, i - StartPos);
      if Token <> '' then
        Strings.Add(Token);

      StartPos:= i + 1;
    end;
  end;

  // Voeg de laatste token toe
  if StartPos <= Length(Input) then begin
    Token:= Copy(Input, StartPos, Length(Input) - StartPos + 1);
    if Token <> '' then
      Strings.Add(Token);
  end;
end;

function TOroxExport.DateToOroxFormat(DateValue : TDateTime) : string;
begin
  if DateValue = 0 then
    Result:= ''
  else
    Result:= FormatDateTime('yyyy-mm-dd', DateValue);
end;

function TOroxExport.ConvertWKTToGML(const WKT : string) : string;
var
  X, Y, Z: Double;
begin
  Z:= Default_Z_Value; // Give Z a large negative default value. then there is no Z and otherwise there is a Z value
  if ParseWKTPoint(WKT, X, Y, Z) then begin
    // Use existing CreateGMLPoint with the parsed coordinates
    Result:= CreateGMLPoint(X, Y, Z);
  end
  else begin
    // Fallback: Use CreateGMLPoint with default values    --> Kan eigenlijk niet
    Result:= CreateGMLPoint(0, 0, 0);
    ReportError('Let op onjuiste geometrie getroffen. omgezet naar default waarden x=0, y=0, z=0'); { #todo : Taalinstelling }
    { #note : Mag niet voorkomen. Er moet vooraf op geom gecontrleerd gaan worden }
  end;
end;

function TOroxExport.CreateGMLPoint(X, Y, Z : Double) : string;
var
  srsDimension: string;
  coordString: string;
begin
  if Z > Default_Z_Value then
    srsDimension:= '3'
  else
    srsDimension:= '2';

  coordString:= FormatOrox('%.3f %.3f %.3f', [X, Y, Z]);

  Result:= '<gml:Point xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:pos srsDimension=\"' + srsDimension + '\">' +
            coordString +
            '</gml:pos></gml:Point>';
end;

function TOroxExport.CreateGMLLineString(const Points : array of Double) : string;
var
  I: Integer;
  PosList: string;
  srsDimension: string;
  HasZValues: Boolean;
begin
  PosList:= '';
  HasZValues:= False;

  if Length(Points) = 0 then begin
    Result:= '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"2\">0 0 10 10</gml:posList></gml:LineString>';
    Exit;
  end;

  HasZValues:= (Length(Points) mod 3 = 0);

  if HasZValues then
    srsDimension:= '3'
  else
    srsDimension:= '2';

  for I:= 0 to (Length(Points) div 3) - 1 do begin
    if PosList <> '' then
      PosList:= PosList + ' ';

    if HasZValues then
      PosList:= PosList + FloatToOrox(Points[I*3], 3) + ' ' +
                         FloatToOrox(Points[I*3+1], 3) + ' ' +
                         FloatToOrox(Points[I*3+2], 3)
    else
      PosList:= PosList + FloatToOrox(Points[I*3], 3) + ' ' +
                         FloatToOrox(Points[I*3+1], 3);
  end;

  Result:= '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"' +
             srsDimension + '\">' + PosList +
            '</gml:posList></gml:LineString>';
end;

procedure TOroxExport.ParsePersleidingGeometry(Persleiding : PGWSWPersleiding);
var
  WKT: string;
  StartPos, EndPos: Integer;
  InnerStr, CoordStr: string;
  PointTokens, CoordTokens: TStringList;
  i: Integer;
  FS: TFormatSettings;
  x, y, z: Double;
begin
  WKT:= UpperCase(Persleiding^.WKTGeometry);

  if Pos('LINESTRING', WKT) = 0 then
    Exit;

  // Find the coordinates string
  StartPos:= Pos('(', WKT);
  EndPos:= LastDelimiter(')', WKT);

  if (StartPos = 0) or (EndPos <= StartPos) then
    Exit;

  InnerStr:= Copy(Persleiding^.WKTGeometry, StartPos + 1, EndPos - StartPos - 1);
  InnerStr:= Trim(InnerStr);

  FS.DecimalSeparator:= '.';
  FS.ThousandSeparator:= #0;

  PointTokens:= TStringList.Create;
  CoordTokens:= TStringList.Create;
  try
    // Split points by comma
    PointTokens.StrictDelimiter:= True;
    PointTokens.Delimiter:= ',';
    PointTokens.DelimitedText:= InnerStr;

    // Calculate number of vertices
    SetLength(Persleiding^.Vertices, PointTokens.Count);

    for i:= 0 to PointTokens.Count - 1 do begin
      CoordStr:= Trim(PointTokens[i]);

      // Remove double spaces and tabs
      while Pos('  ', CoordStr) > 0 do
        CoordStr:= StringReplace(CoordStr, '  ', ' ', [rfReplaceAll]);
      CoordStr:= StringReplace(CoordStr, #9, ' ', [rfReplaceAll]);

      // Split coordinate
      CoordTokens.Clear;
      ExtractStrings([' '], [], PChar(CoordStr), CoordTokens);

      x:= 0;
      y:= 0;
      z:= 0;

      if CoordTokens.Count >= 1 then
        TryStrToFloat(CoordTokens[0], x, FS);

      if CoordTokens.Count >= 2 then
        TryStrToFloat(CoordTokens[1], y, FS);

      if CoordTokens.Count >= 3 then
        TryStrToFloat(CoordTokens[2], z, FS);

      // Sla vertex op
      Persleiding^.Vertices[i][0]:= x;
      Persleiding^.Vertices[i][1]:= y;
      Persleiding^.Vertices[i][2]:= z;
    end;

    Persleiding^.HasMultipleVertices:= PointTokens.Count > 2;

  finally
    PointTokens.Free;
    CoordTokens.Free;
  end;
end;

function TOroxExport.CreateGMLLineStringForPersleiding(const Persleiding : PGWSWPersleiding) : string;
var
  PosList: string;
  i: Integer;
  srsDimension: string;
  HasZValues: Boolean;
begin
  // Bepaal of we Z-waarden hebben
  HasZValues:= False;
  if Length(Persleiding^.Vertices) > 0 then
    HasZValues:= (Persleiding^.Vertices[0][2] <> 0);

  if HasZValues then
    srsDimension:= '3'
  else
    srsDimension:= '2';

  // Bouw de posList op basis van vertices
  PosList:= '';
  for i:= 0 to Length(Persleiding^.Vertices) - 1 do begin
    if PosList <> '' then
      PosList:= PosList + ' ';

    if HasZValues then
      PosList:= PosList +
        FloatToOrox(Persleiding^.Vertices[i][0], 3) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][1], 3) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][2], 3)
    else
      PosList:= PosList +
        FloatToOrox(Persleiding^.Vertices[i][0], 3) + ' ' +
        FloatToOrox(Persleiding^.Vertices[i][1], 3);
  end;

  // Als er geen vertices zijn, gebruik begin- en eindpunten
  if PosList = '' then begin
    if HasZValues then
      PosList:=
        FloatToOrox(Persleiding^.BeginPutX, 3) + ' ' +
        FloatToOrox(Persleiding^.BeginPutY, 3) + ' ' +
        FloatToOrox(Persleiding^.BeginPutZ, 3) + ' ' +
        FloatToOrox(Persleiding^.EindPutX, 3) + ' ' +
        FloatToOrox(Persleiding^.EindPutY, 3) + ' ' +
        FloatToOrox(Persleiding^.EindPutZ, 3)
    else
      PosList:=
        FloatToOrox(Persleiding^.BeginPutX, 3) + ' ' +
        FloatToOrox(Persleiding^.BeginPutY, 3) + ' ' +
        FloatToOrox(Persleiding^.EindPutX, 3) + ' ' +
        FloatToOrox(Persleiding^.EindPutY, 3);
  end;

  Result:= '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\"><gml:posList srsDimension=\"' +
             srsDimension + '\">' + PosList + '</gml:posList></gml:LineString>';
end;

function TOroxExport.ConvertWKTToGMLLineString(const WKT : string) : string;
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
    tmp:= Trim(S);
    while Pos('  ', tmp) > 0 do
      tmp:= StringReplace(tmp, '  ', ' ', [rfReplaceAll]);
    Result:= tmp;
  end;

begin
  Result:= '';

  if Trim(WKT) = '' then Exit;

  StartPos:= Pos('(', WKT);
  EndPos:= LastDelimiter(')', WKT);
  if (StartPos = 0) or (EndPos <= StartPos) then Exit;

  Inner:= Copy(WKT, StartPos + 1, EndPos - StartPos - 1);
  Inner:= Trim(Inner);

  FS.DecimalSeparator:= '.';
  FS.ThousandSeparator:= #0;   // belangrijk: duizendtallen mogen niet herkend worden

  PointTokens:= TStringList.Create;
  CoordTokens:= TStringList.Create;
  try
    // punten splitsen
    PointTokens.StrictDelimiter:= True;
    PointTokens.Delimiter:= ',';
    PointTokens.DelimitedText:= Inner;

    PosList:= '';

    for i:= 0 to PointTokens.Count - 1 do begin
      pTok:= TrimAndCollapseSpaces(PointTokens[i]);
      if pTok = '' then Continue;

      // tabs → spatie
      pTok:= StringReplace(pTok, #9, ' ', [rfReplaceAll]);
      pTok:= TrimAndCollapseSpaces(pTok);

      CoordTokens.Clear;
      ExtractStrings([' '], [], PChar(pTok), CoordTokens);

      x:= 0; y:= 0; z:= 0;

      if CoordTokens.Count >= 1 then
        TryStrToFloat(CoordTokens[0], x, FS);

      if CoordTokens.Count >= 2 then
        TryStrToFloat(CoordTokens[1], y, FS);

      if CoordTokens.Count >= 3 then
        TryStrToFloat(CoordTokens[2], z, FS);

      if PosList <> '' then
        PosList:= PosList + ' ';

      PosList:=
        PosList +
        FormatFloat('0.000', x, FS) + ' ' +
        FormatFloat('0.000', y, FS) + ' ' +
        FormatFloat('0.000', z, FS);
    end;

    if PosList = '' then Exit;

    Result:=
      '<gml:LineString xmlns:gml=\"http://www.opengis.net/gml/3.2\">' +
      '<gml:posList srsDimension=\"3\">' + PosList + '</gml:posList>' +
      '</gml:LineString>';

  finally
    PointTokens.Free;
    CoordTokens.Free;
  end;
end;

procedure TOroxExport.AddPutKenmerken(SL : TStringList; const Put : PGWSWPut);
var
  tmp: String;
begin
  // Als puttype contains gemaal dan een aantal velden anders in de ttl zetten
  if not IsNan(Put^.Breedte) and (Put^.Breedte > 0) then begin
    if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedtePut ; gwsw:hasValue ' +
             FloatToOrox(Put^.Breedte, 3) + ' ] ;');
    end
    else
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedteBouwwerk ; gwsw:hasValue ' +
             FloatToOrox(Put^.Breedte, 0) + ' ] ;'); // Bij gemalen geen decimalen
  end;

  if not IsNan(Put^.Lengte) and (Put^.Lengte > 0) then begin
    if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengtePut ; gwsw:hasValue ' +
             FloatToOrox(Put^.Lengte, 3) + ' ] ;');
    end
    else
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteBouwwerk ; gwsw:hasValue ' +
             FloatToOrox(Put^.Lengte, 0) + ' ] ;');  // Bij gemalen geen decimalen

  end;

  if not IsNan(Put^.Hoogte) and (Put^.Hoogte > 0) then begin
    if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogtePut ; gwsw:hasValue ' +
             FloatToOrox(Put^.Hoogte, 3) + ' ] ;');
    end
    else
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogteBouwwerk ; gwsw:hasValue ' +
             FloatToOrox(Put^.Hoogte, 0) + ' ] ;');  // Bij gemalen geen decimalen
  end;

  if not IsNan(Put^.Diameter) and (Put^.Diameter > 0) then begin
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterPut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Diameter, 0) + ' ] ;');
  end;

  if Put^.MateriaalURI <> '' then begin
    if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalPut ; ' +
             'gwsw:hasReference ' + Put^.MateriaalURI + ' ] ;');
    end
    else
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalBouwwerk ; ' +
             'gwsw:hasReference ' + Put^.MateriaalURI + ' ] ;');
  end;

  if Put^.VormURI <> '' then begin
    if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
      SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormPut ; ' +
             'gwsw:hasReference ' + Put^.VormURI + ' ] ;');
    end
    else
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormBouwwerk ; ' +
           'gwsw:hasReference ' + Put^.VormURI + ' ] ;');
  end;

  // Fundering


  if Put^.FunderingUri <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Fundering ; ' +
           'gwsw:hasReference ' + Put^.FunderingUri + ' ] ;');

  if Put^.Begindatum > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
           'gwsw:hasValue "' + DateToOroxFormat(Put^.Begindatum) + '"^^xsd:date ] ;');

  if Put^.Einddatum > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Einddatum ; ' +
           'gwsw:hasValue "' + DateToOroxFormat(Put^.Einddatum) + '"^^xsd:date ] ;');
end;

procedure TOroxExport.AddPutOrientatie(SL : TStringList; const Put : PGWSWPut);
var
  GMLString: string;
begin
  SL.Add(':' + Put^.GUID + '_ori');
  SL.Add('  rdf:type      gwsw:Putorientatie ;');
  SL.Add('  gwsw:hasAspect  [');
  SL.Add('    rdf:type      gwsw:Punt ;');

  // Gebruik WKT geometrie als beschikbaar, anders CreateGMLPoint
  if Put^.HasWKTGeometry and (Put^.WKTGeometry <> '') then begin
    GMLString:= ConvertWKTToGML(Put^.WKTGeometry);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral');
  end
  else begin
    SL.Add('    gwsw:hasValue "' + CreateGMLPoint(Put^.X, Put^.Y, Put^.Z) + '"^^geo:gmlLiteral');
  end;

  SL.Add('  ] ;');

  if not AnsiContainsText(Put^.PutTypeUri, 'gemaal') then begin
    if not IsNaN(Put^.Maaiveldhoogte) and (Put^.Maaiveldhoogte <> 0) then begin
      SL.Add('  gwsw:hasConnection  _:' + Put^.GUID + '_maaiveld_ori ');
    end;

    SL.Add('.');
    SL.Add('');

    if not IsNaN(Put^.Maaiveldhoogte) then begin  // and (Put^.Maaiveldhoogte <> 0)
      SL.Add('_:' + Put^.GUID + '_maaiveld_ori');
      SL.Add('  rdf:type                      gwsw:Maaiveldorientatie ;');
      SL.Add('  gwsw:hasAspect                [');
      SL.Add('    rdf:type                      gwsw:Maaiveldhoogte ;');
      SL.Add('    gwsw:hasValue                 "' + FloatToOrox(Put^.Maaiveldhoogte, 2) + '"^^xsd:decimal ;');

      SL.Add('  ] .');
      SL.Add('');
    end;
  end
  else begin
    SL.Add('.');
    SL.Add('');
  end;

  { #todo : Moet anders bij de gemalen !!! }
  // Voeg maaiveldorientatie toe

end;

procedure TOroxExport.AddLeidingKenmerken(SL : TStringList; const Leiding : PGWSWLeiding);
var
  tmp: String;
begin
  if Leiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Lengte, 2) + ' ] ;');

  if Leiding^.Diameter > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Diameter, 0) + ' ] ;');

  if Leiding^.Breedte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Breedte, 0) + ' ] ;');

  if Leiding^.Hoogte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Hoogte, 0) + ' ] ;');  // GWSW schrijft 0 decimalen voor. Moet een heel getal in mm zijn.

  if Leiding^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalLeiding ; ' +
         'gwsw:hasReference ' + Leiding^.MateriaalURI + ' ] ;');

  if Leiding^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormLeiding ; ' +
         'gwsw:hasReference ' + Leiding^.VormURI + ' ] ;');

  // Funndering
  if Leiding^.FunderingUri <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Fundering ; ' +
           'gwsw:hasReference ' + Leiding^.FunderingUri + ' ] ;');

  // Wibon thema { #todo : WIBONThema: geeft geen fout maar wordt ook niet verwerkt? }
  if Leiding^.WibonUri <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WIBONThema ; ' +
           'gwsw:hasReference ' + Leiding^.WibonUri + ' ] ;');

  if Leiding^.StatusFunctionerenURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:StatusFunctioneren ; ' +
         'gwsw:hasReference ' + Leiding^.StatusFunctionerenURI + ' ] ;');

  if Leiding^.Begindatum <> 0 then
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
         'gwsw:hasValue "' + DateToOroxFormat(Leiding^.Begindatum) + '"^^xsd:date ] ;'); // OROX/RDF heeft datum alstijd in formaat: YYYY-MM-DD

  if Leiding^.Einddatum > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Einddatum ; ' +
           'gwsw:hasValue "' + DateToOroxFormat(Leiding^.Einddatum) + '"^^xsd:date ] ;');
end;

procedure TOroxExport.AddPersleidingKenmerken(SL : TStringList; const Persleiding : PGWSWPersleiding);
begin
  if Persleiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.Lengte, 2) + ' ] ;');

  if Persleiding^.Diameter > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.Diameter, 0) + ' ] ;');

  if Persleiding^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalLeiding ; ' +
         'gwsw:hasReference ' + Persleiding^.MateriaalURI + ' ] ;');

  if Persleiding^.StatusFunctionerenURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:StatusFunctioneren ; ' +
         'gwsw:hasReference ' + Persleiding^.StatusFunctionerenURI + ' ] ;');

  if Persleiding^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormLeiding ; ' +
         'gwsw:hasReference ' + Persleiding^.VormURI + ' ] ;');

  if Persleiding^.Hoogte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogteLeiding ; gwsw:hasValue ' +
         FloatToOrox(Persleiding^.Hoogte, 0) + ' ] ;');  // GWSW schrijft 0 decimalen voor. Moet een heel getal in mm zijn.

  if Persleiding^.Begindatum <> 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
         'gwsw:hasValue "' + DateToOroxFormat(Persleiding^.Begindatum) + '"^^xsd:date ] ;');
end;

procedure TOroxExport.ExportPersleidingOrientatie(SL : TStringList; const Persleiding : PGWSWPersleiding);
var
  GMLString: string;
begin
  SL.Add(':' + Persleiding^.GUID + '_ori');
  SL.Add('  rdf:type        gwsw:Leidingorientatie ;');

  // Beginpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:BeginpuntLeiding ;');
  if not IsNaN(Persleiding^.BobBegin)  then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobBeginpuntLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.BobBegin, 2) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Persleiding^.BeginPutID + '_ori ] ;');

  // Eindpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:EindpuntLeiding ;');
  if not IsNaN(Persleiding^.BobEind) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobEindpuntLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.BobEind, 2) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Persleiding^.EindPutID + '_ori ] ;');

  // Geometrie - speciaal voor persleiding met meerdere vertices
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Lijn ;');

  if Persleiding^.HasWKTGeometry and (Persleiding^.WKTGeometry <> '') then begin
    if Persleiding^.HasMultipleVertices then
      GMLString:= ConvertWKTToGMLLineString(Persleiding^.WKTGeometry)  // Complexe geometrie
    else
      GMLString:= CreateGMLLineStringForPersleiding(Persleiding);  // Eenvoudige geometrie

    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end
  else begin
    // Fallback geometrie
    GMLString:= CreateGMLLineStringForPersleiding(Persleiding);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end;

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');
end;

procedure TOroxExport.AddKolkKenmerken(SL : TStringList; const Kolk : PGWSWKolk);
begin
  // Afmetingen
  if not IsNan(Kolk^.Breedte) and (Kolk^.Breedte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Breedte, 3) + ' ] ;');

  if not IsNan(Kolk^.Lengte) and (Kolk^.Lengte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Lengte, 3) + ' ] ;');

  if not IsNan(Kolk^.Hoogte) and (Kolk^.Hoogte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogtePut ; gwsw:hasValue ' +
           FloatToOrox(Kolk^.Hoogte, 3) + ' ] ;');

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
end;

procedure TOroxExport.AddKolkOrientatie(SL : TStringList; const Kolk : PGWSWKolk);
var
  GMLString: string;
begin
  SL.Add(':' + Kolk^.GUID + '_ori');
  SL.Add('  rdf:type      gwsw:Putorientatie ;');
  SL.Add('  gwsw:hasAspect  [');
  SL.Add('    rdf:type      gwsw:Punt ;');

  if Kolk^.HasWKTGeometry and (Kolk^.WKTGeometry <> '') then begin
    GMLString:= ConvertWKTToGML(Kolk^.WKTGeometry);
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
    SL.Add('    gwsw:hasValue                 "' + FloatToOrox(Kolk^.Z, 2) + '"^^xsd:decimal')
  else
    SL.Add('    gwsw:hasValue                 "0.00"^^xsd:decimal');
  SL.Add('  ] .');
  SL.Add('');
end;

function TOroxExport.FormatOrox(const FormatStr : string; const Args : array of const) : string;
begin
  Result:= Format(FormatStr, Args);

  // STEP 1: First remove a thousand separation
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    Result:= StringReplace(Result, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  // STEP 2: Then replace decimal separation with dot
  Result:= StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
end;

function TOroxExport.FloatToOrox(Value : Double; Decimals : Integer) : String;
var
  FormatStr: string;
begin
  // Bouw formatstring zoals %.2f, %.3f, etc.
  FormatStr:= '%.' + IntToStr(Decimals) + 'f';

  Result:= Format(FormatStr, [Value]);

  // Verwijder duizendtallen
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    Result:= StringReplace(
      Result,
      DefaultFormatSettings.ThousandSeparator,
      '',
      [rfReplaceAll]
    );

  // Zet decimaal naar punt
  Result:= StringReplace(
    Result,
    DefaultFormatSettings.DecimalSeparator,
    '.',
    [rfReplaceAll]
  );
end;

procedure TOroxExport.ExportStelselToTurtle(SL : TStringList; const Stelsel : PGWSWStelsel);
var
  i: Integer;
  Put: PGWSWPut;
  Leiding: PGWSWLeiding;
  HasParts: Boolean;
  PartsList: TStringList;
  Persleiding: PGWSWPersleiding;
begin
  // Stelsel definitie
  SL.Add(':' + Stelsel^.GUID);
  SL.Add('  rdf:type      ' + Stelsel^.StelselTypeUri + ' ;');

  // Label (naam)
  if Stelsel^.aLabel <> '' then
    SL.Add('  rdfs:label    "' + Stelsel^.aLabel + '" ;')
  else
    SL.Add('  rdfs:label    "Onbekend stelsel" ;');

  // Zoek onderdelen van dit stelsel
  HasParts:= False;
  PartsList:= TStringList.Create;
  try
    // Putten die bij dit stelsel horen
    for i:= 0 to FPutList.Count - 1 do begin
      Put:= PGWSWPut(FPutList[i]);
      if Put^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Put^.GUID);
        HasParts:= True;
      end;
    end;

    // Leidingen die bij dit stelsel horen
    for i:= 0 to FLeidingList.Count - 1 do begin
      Leiding:= PGWSWLeiding(FLeidingList[i]);
      if Leiding^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Leiding^.GUID);
        HasParts:= True;
      end;
    end;

    for i:= 0 to FPersleidingList.Count - 1 do begin
      Persleiding:= PGWSWPersleiding(FPersleidingList[i]);
      if Persleiding^.StelselID = Stelsel^.GUID then begin
        PartsList.Add('    :' + Persleiding^.GUID);
        HasParts:= True;
      end;
    end;

    // Voeg hasPart relaties toe indien er onderdelen zijn
    if HasParts and (PartsList.Count > 0) then begin
      SL.Add('  gwsw:hasPart');
      for i:= 0 to PartsList.Count - 1 do begin
        if i < PartsList.Count - 1 then
          SL.Add(PartsList[i] + ' ,')
        else
          SL.Add(PartsList[i] + ' .');
      end;
    end else begin
      // Geen delen, sluit af met .
      if SL.Count > 0 then begin
        if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
          SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
        else
          SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
      end;
    end;

  finally
    PartsList.Free;
  end;

  SL.Add('');
end;

procedure TOroxExport.ExportPutToTurtle(SL : TStringList; const Put : PGWSWPut);
begin
  SL.Add(':' + Put^.GUID);
  SL.Add('  rdf:type      ' + Put^.PutTypeUri + ' ;');
  SL.Add('  rdfs:label    "' + Put^.aLabel + '" ;');

  // Voeg de relatie toe van de put naar zijn oriëntatie
  if Put^.HasOrientation or Put^.HasWKTGeometry then
    SL.Add('  gwsw:hasAspect  :' + Put^.GUID + '_ori ;');

  // Stelsel relatie toevoegen
  if Put^.StelselID <> '' then
    SL.Add('  gwsw:isPartOf   :' + Put^.StelselID + ' ;');

  AddPutKenmerken(SL, Put);

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // TWEEDE ORIENTATIE TOEVOEGING - DIT IS DE DEFINITIE VAN DE ORIENTATIE ZELF
  if Put^.HasOrientation or Put^.HasWKTGeometry then
    AddPutOrientatie(SL, Put);
end;

procedure TOroxExport.ExportLeidingToTurtle(SL : TStringList; const Leiding : PGWSWLeiding);
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
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Leiding oriëntatie MET BOB waarden genest in de eindpunten
  SL.Add(':' + Leiding^.GUID + '_ori');
  SL.Add('  rdf:type        gwsw:Leidingorientatie ;');

  // Beginpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:BeginpuntLeiding ;');
  if not IsNaN(Leiding^.BobBegin) and (Leiding^.BobBegin <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobBeginpuntLeiding ; gwsw:hasValue ' + FloatToOrox(Leiding^.BobBegin, 2) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Leiding^.BeginPutID + '_ori ] ;');

  // Eindpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:EindpuntLeiding ;');
  if not IsNaN(Leiding^.BobEind) and (Leiding^.BobEind <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobEindpuntLeiding ; gwsw:hasValue ' + FloatToOrox(Leiding^.BobEind, 2) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Leiding^.EindPutID + '_ori ] ;');

  // Geometrie
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Lijn ;');

  if Leiding^.HasWKTGeometry and (Leiding^.WKTGeometry <> '') then begin
    GMLString:= ConvertWKTToGMLLineString(Leiding^.WKTGeometry);
    SL.Add('    gwsw:hasValue "' + GMLString + '"^^geo:gmlLiteral ] ;');
  end
  else begin
    // Fallback geometry based on start and end well coordinates
    SetLength(Points, 6);
    Points[0]:= Leiding^.BeginPutX;
    Points[1]:= Leiding^.BeginPutY;
    Points[2]:= Leiding^.BeginPutZ;
    Points[3]:= Leiding^.EindPutX;
    Points[4]:= Leiding^.EindPutY;
    Points[5]:= Leiding^.EindPutZ;
    SL.Add('    gwsw:hasValue "' + CreateGMLLineString(Points) + '"^^geo:gmlLiteral ] ;');
  end;

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');
end;

procedure TOroxExport.ExportPersleidingToTurtle(SL : TStringList; const Persleiding : PGWSWPersleiding);
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
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Persleiding oriëntatie
  ExportPersleidingOrientatie(SL, Persleiding);
end;

procedure TOroxExport.ExportKolkToTurtle(SL : TStringList; const Kolk : PGWSWKolk);
begin
  SL.Add(':' + Kolk^.GUID);
  SL.Add('  rdf:type      ' + Kolk^.KolkTypeUri + ' ;');
  SL.Add('  rdfs:label    "' + Kolk^.aLabel + '" ;');

  // Oriëntatie toevoegen
  if Kolk^.HasOrientation or Kolk^.HasWKTGeometry then
    SL.Add('  gwsw:hasAspect  :' + Kolk^.GUID + '_ori ;');

  AddKolkKenmerken(SL, Kolk);

  // Verwijder laatste ; en vervang door .
  if SL.Count > 0 then begin
    if Copy(SL[SL.Count - 1], Length(SL[SL.Count - 1]), 1) = ';' then
      SL[SL.Count - 1]:= Copy(SL[SL.Count - 1], 1, Length(SL[SL.Count - 1]) - 1) + ' .'
    else
      SL[SL.Count - 1]:= SL[SL.Count - 1] + ' .';
  end;

  SL.Add('');

  // Oriëntatie definitie toevoegen
  if Kolk^.HasOrientation or Kolk^.HasWKTGeometry then
    AddKolkOrientatie(SL, Kolk);
end;

function TOroxExport.TryDifferentDateFormats(const ValueStr: string; out DateVal: TDateTime): Boolean;
var
  FmtSettings: TFormatSettings;
begin
  Result:= False;

  // Maak format settings voor Europese formaten
  FmtSettings:= DefaultFormatSettings;

  // Probeer verschillende Europese datumformaten

  // Formaat: DD-MM-YYYY (of DD-MM-YYYY HH:MM:SS)
  FmtSettings.ShortDateFormat:= 'DD-MM-YYYY';
  FmtSettings.DateSeparator:= '-';
  if TryStrToDateTime(ValueStr, DateVal, FmtSettings) then
  begin
    Result:= True;
    Exit;
  end;

  // Formaat: DD/MM/YYYY
  FmtSettings.ShortDateFormat:= 'DD/MM/YYYY';
  FmtSettings.DateSeparator:= '/';
  if TryStrToDateTime(ValueStr, DateVal, FmtSettings) then
  begin
    Result:= True;
    Exit;
  end;

  // Formaat: YYYY-MM-DD (ISO)
  FmtSettings.ShortDateFormat:= 'YYYY-MM-DD';
  FmtSettings.DateSeparator:= '-';
  if TryStrToDateTime(ValueStr, DateVal, FmtSettings) then
  begin
    Result:= True;
    Exit;
  end;

  // Probeer ook zonder tijd component
  FmtSettings.ShortDateFormat:= 'DD-MM-YYYY';
  FmtSettings.DateSeparator:= '-';
  if TryStrToDate(ValueStr, DateVal, FmtSettings) then
  begin
    Result:= True;
    Exit;
  end;
end;

procedure TOroxExport.ReportProgress(const Msg : string);
begin
  if Assigned(fProgressReporter) then
  begin
    fProgressReporter.ReportProgressMsg(Msg);
    // Als we bezig zijn met records verwerken, stuur ook de progress count
    if fTotalRecords > 0 then
      fProgressReporter.ReportProgressCount(fCurrentRecord, fTotalRecords);
  end;

{  if Assigned(fProgressReporter) then
    fProgressReporter.ReportProgressMsg(Msg);}
end;

procedure TOroxExport.ReportError(const ErrMsg: string; const ErrorType: Integer; const Guid: string);
begin
  if Assigned(fProgressReporter) then begin
    fProgressReporter.ReportError(ErrMsg, ErrorType, Guid);
  end;
end;

procedure TOroxExport.UpdateProgressCount(Current, Total : Integer);
begin
  if Assigned(fProgressReporter) then
    fProgressReporter.ReportProgressCount(Current, Total);
end;

constructor TOroxExport.Create(ADataProvider : IGWSWDataProvider;
  const FileName, OrganizationName, MappingsFile : String;
  AProgressReporter : IExportProgressReporter);
begin
  inherited Create;

  fDataProvider:= ADataProvider;
  fFileName:= FileName;
  fOrganizationName:= OrganizationName;
  fProgressReporter:= AProgressReporter;

  fPutList:= TList.Create;
  fLeidingList:= TList.Create;
  fStelselList:= TList.Create;
  fKolkList:= TList.Create;
  fPersleidingList:= TList.Create;

  fMappingManager:= TMappingManager.Create(MappingsFile);
end;

destructor TOroxExport.Destroy;
begin
  ClearAllLists;
  fMappingManager.Free;
  FPutList.Free;
  FLeidingList.Free;
  FStelselList.Free;
  FKolkList.Free;
  FPersleidingList.Free;

  inherited Destroy;
end;

procedure TOroxExport.ExportToOrox(const GWSWversion : String);
var
  TotalRecords: Integer;
begin
  ReportProgress('Export gestart...');
  // Retrieve validation data
  LoadGWSWConfig(GWSW_versie_16, GWSWversion);  // get the GWSW version. Used with validation
  InitValidationConfig;  // Initialize global config in validation unit

  fDataProvider.Open;
  try
    TotalRecords:= fDataProvider.GetAccurateRecordCount; // Determine the number of records. Needed to show progress
    fDataProvider.First;
    ReportProgress(Format('%d records gevonden', [TotalRecords]));
    UpdateProgressCount(0, TotalRecords); // Initialize progress

    // Mapping the BOR domain value to the GWSW domain value
    ReportProgress('Mapping data naar GWSW model...');

    if not MapDatabaseToGWSW(fDataProvider, TotalRecords) then begin
      ReportError('Fout bij mapping data naar GWSW model');
      Exit;
    end;

    { #todo : Taalinstellig }
    { #todo : Let op stel dat een record wordt geweigert als 1 van de attributen niet voldoet dan klopt deze telling niet meer.  }
    ReportProgress(Format('%d leidingen gemapt', [fLeidingList.Count]));
    ReportProgress(Format('%d putten gemapt', [fPutList.Count]));
    ReportProgress(Format('%d stelsel gemapt', [fStelselList.Count]));
    ReportProgress(Format('%d mechanische leidingen gemapt', [fPersleidingList.Count]));
    ReportProgress(Format('%d kolken gemapt', [fKolkList.Count]));

    // Prepare and save TTL file
    ReportProgress('Genereren Turtle bestand...');

    // Reset voor generatie fase
    fTotalRecords:= 0;
    fCurrentRecord:= 0;

    if not GenerateOroxTurtle(fFileName, GWSWversion) then begin
      ReportError('Fout bij genereren Orox Turtle bestand');
      Exit;
    end;
    ReportProgress('Export succesvol voltooid!');
    UpdateProgressCount(TotalRecords, TotalRecords);
  except
      on E: Exception do
      begin
        ReportError('Onverwachte fout: ' + E.Message);
      end;
    end;
end;

end.

