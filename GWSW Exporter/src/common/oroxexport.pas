{ Copyright ©2025 Hans van Buggenum }
unit OroxExport;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, Variants, Math, DateUtils,
  model.intf, uIGWSWDataProvider, GWSWTypes, MappingManager,
  exportprogressreporter.intf;

type
  { TOroxExport }
  TOroxExport= class(TObject)
    private
      fDataProvider: IGWSWDataProvider;
      fFileName: String;
      fOrganizationName: String;
      fProgressReporter: IExportProgressReporter;
      fDisableErrorReport: Boolean;

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
      function GenerateOroxTurtle(const FileName: string): Boolean;  // Bouw de ttl op
      function MapStelselFromProvider(DataProvider: IGWSWDataProvider): PGWSWStelsel;
      function MapPutFromProvider(DataProvider: IGWSWDataProvider): PGWSWPut;
      function MapLeidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWLeiding;
      function MapPersleidingFromProvider(DataProvider: IGWSWDataProvider): PGWSWPersleiding;
      function MapKolkFromProvider(DataProvider: IGWSWDataProvider): PGWSWKolk;

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
      function WijzeInwinningToGWSW(Wijze: TGWSWWijzeInwinning): string;

      procedure ReportProgress(const Msg: string);
      procedure ReportError(const ErrMsg: string);
    public
      constructor Create(ADataProvider : IGWSWDataProvider; const FileName,
        OrganizationName, MappingsFile : String; AProgressReporter : IExportProgressReporter);
      destructor Destroy; override;
      procedure ExportToOrox(DisableErrorReport: Boolean);
  end;

const Default_Z_Value = -9999; // De rioned server controleert op: Kenmerk Z_coordinaat - waarde wijkt af (min=-20,max=300)

implementation

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
  Stelsel, Put, Leiding: Boolean;
begin
  Result:= False;
  Stelsel:= False;
  Put:= False;
  Leiding:= False;

  try
    ClearAllLists;

    DataProvider.Open;
    try
      if DataProvider.First then begin
        repeat
          case UpperCase(DataProvider.GetObjectType) of
            'STELSEL': begin
              if not Stelsel then begin
                Stelsel:= True;
                ReportProgress('Mapping Stelsel...');
              end;
              FStelselList.Add(MapStelselFromProvider(DataProvider));
            end;
            'PUT': begin
              if not Put then begin
                Put:= True;
                ReportProgress('Mapping Put...');
              end;
              FPutList.Add(MapPutFromProvider(DataProvider));
            end;
            'LEIDING': begin
              if not Leiding then begin
                Leiding:= True;
                ReportProgress('Mapping Leiding...');
              end;
              FLeidingList.Add(MapLeidingFromProvider(DataProvider));
            end;
            'PERSLEIDING': begin
              FPersLeidingList.Add(MapPersleidingFromProvider(DataProvider));
            end;
            'KOLK': begin
              FKolkList.Add(MapKolkFromProvider(DataProvider));
            end
            else begin
              ReportError('Onbekend Object type aangetroffen.');
              ReportError('Dit is: ' + UpperCase(DataProvider.GetObjectType))
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
      raise Exception.Create('Mapping fout: ' + E.Message);   { #todo : Moet naar de view. }
  end;
end;

function TOroxExport.GenerateOroxTurtle(const FileName : string) : Boolean;
var
  SL: TStringList;
  i: Integer;
  DummyStelsel: PGWSWStelsel;
begin
  Result:= False;
  SL:= TStringList.Create;  // de specificaties schrijven utf 8 voor. (nodig voor é ë etc.)
  try
    // Eerst een dummy stelsel toevoegen aan de lijst voor objecten zonder stelsel
    // Nodig voor de kolken. Is niet voldoende/goed om de validatie meldingen te voorkomen. Nakijken.
{    New(DummyStelsel);
    DummyStelsel^.GUID:= 'DUMMY_STELSEL';
    DummyStelsel^.aLabel:= 'Dummy stelsel voor objecten zonder stelsel';
    DummyStelsel^.HasParts:= True;
    DummyStelsel^.StelselTypeUri:= 'gwsw:GemengdStelsel';
    FStelselList.Add(DummyStelsel);
    }

    SL.WriteBOM:= False; { #todo : Uitzoeken. }

    // Prefixes conform GWSW-OroX specificatie
    SL.Add('@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .');
    SL.Add('@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .');
    SL.Add('@prefix owl:     <http://www.w3.org/2002/07/owl#> .');
    SL.Add('@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .');
    SL.Add('@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .');
    SL.Add('@prefix geo:     <http://www.opengis.net/ont/geosparql#> .');
    SL.Add('@prefix gwsw:    <http://data.gwsw.nl/1.6/totaal/> .');

    if fOrganizationName <> '' then
      SL.Add('@prefix :        <http://sparql.gwsw.nl/repositories/' + fOrganizationName + '#> .')  // Dit komt voor de URI staan!
    else
      SL.Add('@prefix :        <http://sparql.gwsw.nl/repositories/#> .');  // Dit komt voor de URI staan!

    SL.Add('');
    SL.Add('# gwsw-exporter, een export tool in aanbouw.');
    SL.Add('# Exportdatum: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SL.Add('');
    SL.Add('');

    // Export Stelsels
    for i:= 0 to FStelselList.Count - 1 do begin
      ExportStelselToTurtle(SL, PGWSWStelsel(FStelselList[i]));
    end;

    // Export Putten. Als putten zonder leidingen worden geëxporteerd dan geeft de balidatie een fout. het "netwerk" is dan niet goed.
     for i:= 0 to FPutList.Count - 1 do begin
       ExportPutToTurtle(SL, PGWSWPut(FPutList[i]));
     end;

     // Export Leidingen
     for i:= 0 to FLeidingList.Count - 1 do begin
       ExportLeidingToTurtle(SL, PGWSWLeiding(FLeidingList[i]));
     end;

     // Export Persleidingen
     for i:= 0 to FPersleidingList.Count - 1 do begin
       ExportPersleidingToTurtle(SL, PGWSWPersleiding(FPersleidingList[i]));
     end;

     // Export Kolken
     for i := 0 to FKolkList.Count - 1 do begin
       ExportKolkToTurtle(SL, PGWSWKolk(FKolkList[i]));
     end;

    // Opslaan
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

  // Initialiseer
  Stelsel^.aLabel:= '';
  Stelsel^.HasParts:= True;
  Stelsel^.StelselTypeUri:= '';

  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');
    if lValue <> '' then
      Stelsel^.GUID:= lValue
    else
      ReportError('Stelsel: "GUID" is leeg');
  end
  else
    ReportError('Stelsel: GUID veld ontbreekt');


  // Korte naam (STELSELCODE)
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Stelsel^.aLabel:= lValue
    else begin
      Stelsel^.aLabel:= '';
      if not fDisableErrorReport then ReportError('Stelsel: "Naam" is leeg' + ' (' + Stelsel^.GUID + ')');
    end
  end
  else
    ReportError('Stelsel: "Naam" veld ontbreekt.');

  // Lange naam  (STELSELNAAM)
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Stelsel^.StelselNaam:= lValue
    else
      if not fDisableErrorReport then ReportError('Stelsel: "Stelsel_naam" is leeg' + ' (' + Stelsel^.GUID + ')');
  end
  else
    ReportError('Stelsel: "Stelsel_naam" ontbreekt.');

  // Als er geen korte naam is, gebruik dan de lange naam voor aLabel
  if (Stelsel^.aLabel= '') and (Stelsel^.StelselNaam <> '') then
    Stelsel^.aLabel:= Stelsel^.StelselNaam;

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));   // Default is "Onbekend"
    Stelsel^.StelselTypeUri:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);

    if Stelsel^.StelselTypeUri = 'gwsw:Onbekend' then begin
      Stelsel^.StelselTypeUri:= '""';  { #todo : Default "gwsw:onbekend" vanuit DataProvider.GetFieldValue() is niet handig }
      if not fDisableErrorReport then ReportError('Stelsel: "Stelsel_type" is leeg' + ' (' + Stelsel^.GUID + ')');

      if lValue = '' then
         if not fDisableErrorReport then ReportError('Stelsel: "Stelsel_type" is leeg' + ' (' + Stelsel^.GUID + ')');
    end
  end
  else
    ReportError('Stelsel: "Stelsel_type" ontbreekt.');

  Result:= Stelsel;
end;

function TOroxExport.MapPutFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPut;
var
  Put: PGWSWPut;
  WKTGeometry: string;
  GeoX, GeoY, GeoZ: Double;
  lValue: Variant;
  lDate: TDateTime;
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

  // Initialiseer nieuwe velden
  Put^.WKTGeometry:= '';
  Put^.HasWKTGeometry:= False;

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Put^.GUID:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "GUID" is leeg' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: GUID veld ontbreekt');  // Can never happen but still...

  // Naam
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Put^.aLabel:= lValue
    else begin
      Put^.aLabel:= '';
      if not fDisableErrorReport then ReportError('Put: "Naam" is leeg' + ' (' + Put^.GUID + ')');
      if not fDisableErrorReport then ReportError('Put: "Naam" is leeg, Dit is de PUTCODE die ontbreekt!!!');
      // ernstig, dit is de putcode die dan in BOR ontbreekt
    end
  end
  else
    ReportError('Put: "Naam" veld ontbreekt.');

  // Eerst proberen WKT geometrie te gebruiken
  if DataProvider.FieldExists('WKT_GEOMETRY') then begin
      WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

    if WKTGeometry <> '' then begin
      Put^.WKTGeometry:= WKTGeometry;
      Put^.HasWKTGeometry:= True;

      // Parse ook naar X,Y,Z voor backward compatibility
      if ParseWKTPoint(WKTGeometry, GeoX, GeoY, GeoZ) then begin
        Put^.X:= GeoX;
        Put^.Y:= GeoY;
        Put^.Z:= GeoZ;
        Put^.HasOrientation:= (GeoX <> 0) and (GeoY <> 0);
      end
      else begin
        // Fallback naar individuele coordinaat velden
        // Mag hiert nooit komen. Validatie vooraf maken
        lValue:= DataProvider.GetFieldValue('X');
        if not VarIsNull(lValue) then Put^.X:= lValue;

        lValue:= DataProvider.GetFieldValue('Y');
        if not VarIsNull(lValue) then Put^.Y:= lValue;

        lValue:= DataProvider.GetFieldValue('Z');
        if not VarIsNull(lValue) then Put^.Z:= lValue;

        Put^.HasOrientation:= (Put^.X <> 0) and (Put^.Y <> 0);
      end;
    end
    else
      ReportError('Put: "WKT-Geometry" is leeg.' + ' (' + Put^.GUID + ')');
  end
  else begin
    ReportError('Put: "WKT_Geometry" veld ontbreekt.');
  end;

  // Afmetingen met NULL checks
  { #todo : Breedte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('BREEDTE') then      { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
  begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then
      Put^.Breedte:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Breedte" is leeg.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Breedte" veld ontbreekt.');

  { #todo : Lengte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then
      Put^.Lengte:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Lengte" veld is leeg.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Lengte" veld ontbreekt.');

  { #todo : Hoogte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('HOOGTE') then begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then
      Put^.Hoogte:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Hoogte" veld is leeg.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Hoogte" veld ontbreekt');

  // Diameter (noodzakelijk?...)
  if DataProvider.FieldExists('DIAMETER') then begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then
      Put^.Diameter:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Diameter" veld is leeg.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Diameter" veld ontbreekt');


  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Put^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalPut, lValue);
    if Put^.MateriaalURI = '' then
      if not fDisableErrorReport then ReportError('Put: "Materiaal" veld is leeg' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Materiaal" veld ontbreekt.');

  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Put^.VormURI:= fMappingManager.GetGWSWURI(mtvormPut, lValue);
    if Put^.VormURI = '' then
      if not fDisableErrorReport then ReportError('Put: "Vorm" veld is leeg' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Vorm" veld ontbreekt.');

  // Puttype mapping
  if DataProvider.FieldExists('PUT_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('PUT_TYPE'), ''));
    Put^.PutTypeUri:= fMappingManager.GetGWSWURI(mtPutType, lValue);
    if Put^.PutTypeUri = '' then begin
      Put^.PutTypeUri:= 'gwsw:Onbekend';  // Put^.PutTypeUri mag niet leeg zijn.
      if not fDisableErrorReport then ReportError('Put: "Put_type" veld is leeg' + ' (' + Put^.GUID + ')');
    end;
  end
  else
    ReportError('Put: "Put_type" veld ontbreekt.');

  // Maaiveld
  if DataProvider.FieldExists('MAAIVELD') then begin
    lValue:= DataProvider.GetFieldValue('MAAIVELD');
    if not VarIsNull(lValue) then
      Put^.Maaiveldhoogte:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Maaiveld" veld is leeg' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Maaiveld" veld ontbreekt.');

  // Begindatum. Wordt momenteel in BOR niet gebruikt. aanlegjaar omzetten naar een datum.
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Put^.Begindatum:= lDate
    else begin
      Put^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      if not fDisableErrorReport then ReportError('Put: "Begindatum" veld is leeg' + ' (' + Put^.GUID + ')');
    end
  end
  else
    ReportError('Put: "Begindatum" veld ontbreekt.');

  // Wijze van inwinning
  { #todo : Moet naar extern mappingsbestand }
  { #note : Wordt niet gebruikt ! }
  lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('WIJZE_INWINNING'), ''));
  if lValue = 'GEMETEN' then Put^.WijzeInwinning:= wiGemeten
  else if lValue = 'BEREKEND' then Put^.WijzeInwinning:= wiBerekend
  else if lValue = 'GESCHAT' then Put^.WijzeInwinning:= wiGeschat
  else if lValue = 'ONTWERP' then Put^.WijzeInwinning:= wiOntwerp
  else Put^.WijzeInwinning:= wiOnbekend;

  // Stelsel relatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Put^.StelselID:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Stelsel_id" is leeg' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Stelsel_id" veld ontbreekt.');

  // stelsel uit de mapping ophalen
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Put^.Stelselnaam:= lValue
    else
      if not fDisableErrorReport then ReportError('Put: "Stelsel_naam" veld ontbreekt.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Stelsel_naam" veld ontbreekt.');


  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Put^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);
    if Put^.StelselURI = '' then
      if not fDisableErrorReport then ReportError('Put: "Stelsel_type" veld ontbreekt.' + ' (' + Put^.GUID + ')');
  end
  else
    ReportError('Put: "Stelsel_type" veld ontbreekt.');

  Put^.HasOrientation:= (Put^.X <> 0) and (Put^.Y <> 0);

  Result:= Put;
end;

function TOroxExport.MapLeidingFromProvider(DataProvider : IGWSWDataProvider) : PGWSWLeiding;
var
  Leiding: PGWSWLeiding;
  WKTGeometry: string;
  lValue: Variant;
  lDate: TDateTime;
begin
  New(Leiding);
  FillChar(Leiding^, SizeOf(TGWSWLeiding), 0);

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Leiding^.GUID:= lValue
    else
      ReportError('Leiding: "GUID" is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: GUID veld ontbreekt');  // Can never happen but still...

  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Leiding^.aLabel:= lValue
    else begin
      Leiding^.aLabel:= '';
      if not fDisableErrorReport then ReportError('Leiding: "Naam" is leeg' + ' (' + Leiding^.GUID + ')');
      if not fDisableErrorReport then ReportError('Leiding: "Naam" is leeg, Dit is de Strengcode die ontbreekt!!!');
      // ernstig, dit is de strengcode die dan in BOR ontbreekt
    end
  end
  else
    ReportError('Leiding: "Naam" veld ontbreekt.');

  // Topologie - verbinding met putten
  if DataProvider.FieldExists('BEGINPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('BEGINPUT_ID'), '');
    if lValue <> '' then
      Leiding^.BeginPutID:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Beginput_id" is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Beginput_id" veld ontbreekt.');

  if DataProvider.FieldExists('EINDPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('EINDPUT_ID'), '');
    if lValue <> '' then
      Leiding^.EindPutID:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Eindput_id" is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Eindput_id" veld ontbreekt.');

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Leiding^.StelselID:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Stelsel_id" is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Stelsel_id" veld ontbreekt.');

  // Afmetingen met NULL checks
  if DataProvider.FieldExists('LENGTE') then begin { #todo : Lengte mag nooit negatief zijn. Controle maken. }
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then
      Leiding^.Lengte:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Lengte" veld is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Lengte" veld ontbreekt.');

  //Breedte
  { #todo : Breedte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('BREEDTE') then      { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
  begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then
      Leiding^.Breedte:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Breedte" is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Breedte" veld ontbreekt.');

  // Hoogte
  if DataProvider.FieldExists('HOOGTE') then
  begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then
      Leiding^.Hoogte:= lValue  // moet 0 decimalen hebben (is in [mm])
    else
      if not fDisableErrorReport then ReportError('Leiding: "Hoogte" is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Hoogte" veld ontbreekt.');

  // Diameter
  if DataProvider.FieldExists('DIAMETER') then
  begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then
      Leiding^.Diameter:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Diameter" is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Diameter" veld ontbreekt.');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Leiding^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalLeiding, lValue);

    if Leiding^.MateriaalURI = '' then
      if not fDisableErrorReport then ReportError('Leiding: "Materiaal" veld is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Materiaal" veld ontbreekt.');


  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Leiding^.VormURI:= fMappingManager.GetGWSWURI(mtVormLeiding, lValue);
    if Leiding^.VormURI = '' then
      if not fDisableErrorReport then ReportError('Leiding: "Vorm" veld is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Vorm" veld ontbreekt.');

  // status functioneren
  if DataProvider.FieldExists('STATUS_FUNCTIONEREN') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STATUS_FUNCTIONEREN'), ''));
    Leiding^.StatusFunctionerenURI:= fMappingManager.GetGWSWURI(mtStatusFunctioneren, lValue);
    if Leiding^.StatusFunctionerenURI = '' then
      if not fDisableErrorReport then ReportError('Leiding: "Status functioneren" veld is leeg' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Status functioneren" veld ontbreekt.');



  // Stelsel type
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Leiding^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);
    if Leiding^.StelselURI = '' then
      if not fDisableErrorReport then ReportError('Leiding: "Stelsel_type" veld ontbreekt.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Stelsel_type" veld ontbreekt.');

  // stelselnaam (vanuit gv_stelsel) { #todo : Wordt nog niet opgepakt in de export ??? }
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Leiding^.Stelselnaam:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Stelsel_naam" veld ontbreekt.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Stelsel_naam" veld ontbreekt.');

  // Leidingtype mapping
  if DataProvider.FieldExists('LEIDING_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('LEIDING_TYPE'), ''));
    Leiding^.LeidingTypeURI:= fMappingManager.GetGWSWURI(mtLeidingType, lValue);
    if Leiding^.LeidingTypeURI = '' then begin
      Leiding^.LeidingTypeURI:= 'gwsw:Onbekend';  // Leiding^.LeidingTypeURI mag niet leeg zijn.
      if not fDisableErrorReport then ReportError('Leiding: "Leiding_type" veld is leeg' + ' (' + Leiding^.GUID + ')');
    end;
  end
  else
    ReportError('Leiding: "Leiding_type" veld ontbreekt.');

  // BOB waarden
  if DataProvider.FieldExists('BOB_BEGIN') then begin
    lValue:= DataProvider.GetFieldValue('BOB_BEGIN');
    if not VarIsNull(lValue) then
      Leiding^.BobBegin:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Bob begin" veld is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Bob begin" veld ontbreekt');

  if DataProvider.FieldExists('BOB_EIND') then begin
    lValue:= DataProvider.GetFieldValue('BOB_EIND');
    if not VarIsNull(lValue) then
      Leiding^.BobEind:= lValue
    else
      if not fDisableErrorReport then ReportError('Leiding: "Bob eind" veld is leeg.' + ' (' + Leiding^.GUID + ')');
  end
  else
    ReportError('Leiding: "Bob eind" veld ontbreekt');


  // Begindatum. Wordt momenteel in BOR niet gebruikt. aanlegjaar omzetten naar een datum.
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Leiding^.Begindatum:= lDate
    else begin
      Leiding^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      if not fDisableErrorReport then ReportError('Leiding: "Begindatum" veld is leeg' + ' (' + Leiding^.GUID + ')');
    end
  end
  else
    ReportError('Leiding: "Begindatum" veld ontbreekt.');


  // Wijze van inwinning
  { #todo : Moeet naar extern mappingsbestand }
  lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('WIJZE_INWINNING'), ''));
  if lValue = 'GEMETEN' then
    Leiding^.WijzeInwinning:= wiGemeten
  else if lValue = 'BEREKEND' then
    Leiding^.WijzeInwinning:= wiBerekend
  else if lValue = 'GESCHAT' then
    Leiding^.WijzeInwinning:= wiGeschat
  else if lValue = 'ONTWERP' then
    Leiding^.WijzeInwinning:= wiOntwerp
  else
    Leiding^.WijzeInwinning:= wiOnbekend;

  // Reading geometry
  Leiding^.WKTGeometry:= '';
  Leiding^.HasWKTGeometry:= False;
  Leiding^.HasMultipleVertices:= False;

  WKTGeometry:= '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');

  // Check if we have valid WKT geometry
  if not VarIsNull(WKTGeometry) and (Trim(VarToStr(WKTGeometry)) <> '') then
  begin
    Leiding^.WKTGeometry:= Trim(VarToStr(WKTGeometry));
    Leiding^.HasWKTGeometry:= True;
  end;

  // Optional: try to read coordinates of the start and end well as a backup
  if DataProvider.FieldExists('BEGINPUT_X') and DataProvider.FieldExists('BEGINPUT_Y') then begin
    lValue:= DataProvider.GetFieldValue('BEGINPUT_X');
    if not VarIsNull(lValue) then Leiding^.BeginPutX:= lValue;

    lValue:= DataProvider.GetFieldValue('BEGINPUT_Y');
    if not VarIsNull(lValue) then Leiding^.BeginPutY:= lValue;

    lValue:= DataProvider.GetFieldValue('BEGINPUT_Z');
    if not VarIsNull(lValue) then Leiding^.BeginPutZ:= lValue;
  end;

  if DataProvider.FieldExists('EINDPUT_X') and DataProvider.FieldExists('EINDPUT_Y') then begin
    lValue:= DataProvider.GetFieldValue('EINDPUT_X');
    if not VarIsNull(lValue) then Leiding^.EindPutX:= lValue;

    lValue:= DataProvider.GetFieldValue('EINDPUT_Y');
    if not VarIsNull(lValue) then Leiding^.EindPutY:= lValue;

    lValue:= DataProvider.GetFieldValue('EINDPUT_Z');
    if not VarIsNull(lValue) then Leiding^.EindPutZ:= lValue;
  end;

  Result:= Leiding;
end;

function TOroxExport.MapPersleidingFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPersleiding;
var
  Persleiding: PGWSWPersleiding;
  lValue: Variant;
  lDate: TDateTime;
  WKTGeometry: Variant;  // Gebruik Variant zoals in andere functies
  WKTGeometryStr: string;
begin
  New(Persleiding);
//  FillChar(Persleiding^, SizeOf(TGWSWPersleiding), 0);

  // GUID
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Persleiding^.GUID:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "GUID" is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: GUID veld ontbreekt');  // Can never happen but still...



  // Persleidingen hebben geen naam. object_guid als naam opvoeren.
  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Persleiding^.aLabel:= lValue
    else begin
      if Persleiding^.GUID <> '' then
        Persleiding^.aLabel:= Persleiding^.GUID
      else
        if not fDisableErrorReport then ReportError('Persleiding: "Naam" is leeg' + ' (' + Persleiding^.GUID + ')');
    end
  end
  else
    ReportError('Persleiding: "Naam" veld ontbreekt.');


  // Topologie - verbinding met putten
  if DataProvider.FieldExists('BEGINPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('BEGINPUT_ID'), '');
    if lValue <> '' then
      Persleiding^.BeginPutID:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Beginput_id" is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Beginput_id" veld ontbreekt.');

  if DataProvider.FieldExists('EINDPUT_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('EINDPUT_ID'), '');
    if lValue <> '' then
      Persleiding^.EindPutID:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Eindput_id" is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Beginput_id" veld ontbreekt.');

  // Stelsel informatie
  if DataProvider.FieldExists('STELSEL_ID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_ID'), '');
    if lValue <> '' then
      Persleiding^.StelselID:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Stelsel_id" is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Stelsel_id" veld ontbreekt.');

  { #todo : Lengte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then
      Persleiding^.Lengte:= lValue;
      if Persleiding^.Lengte > 1000 then
        ReportError('Persleiding: "Lengte" veld is te groot. volgens GWSW: min=1,max=1000.  Lengte = ' + FloatToStr(Persleiding^.Lengte))
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Lengte" veld is leeg.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Lengte" veld ontbreekt.');

  if DataProvider.FieldExists('DIAMETER') then
  begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then
      Persleiding^.Diameter:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Diameter" is leeg.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Diameter" veld ontbreekt.');


  if DataProvider.FieldExists('BREEDTE') then
  begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then
      Persleiding^.Breedte:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Breedte" is leeg.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Breedte" veld ontbreekt.');


  if DataProvider.FieldExists('HOOGTE') then
  begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then
      Persleiding^.Hoogte:= lValue  // moet 0 decimalen hebben (is in [mm])
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Hoogte" is leeg.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Hoogte" veld ontbreekt.');

  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Persleiding^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalLeiding, lValue);
    if Persleiding^.MateriaalURI = '' then
      if not fDisableErrorReport then ReportError('Persleiding: "Materiaal" veld is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Materiaal" veld ontbreekt.');

  // Status functioneren
  if DataProvider.FieldExists('STATUS_FUNCTIONEREN') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STATUS_FUNCTIONEREN'), ''));
    Persleiding^.StatusFunctionerenURI:= fMappingManager.GetGWSWURI(mtStatusFunctioneren, lValue);
    if Persleiding^.StatusFunctionerenURI = '' then
      if not fDisableErrorReport then ReportError('Persleiding: "Status functioneren" veld is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Status functioneren" veld ontbreekt.');

  // Stelsel type
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Persleiding^.StelselURI:= fMappingManager.GetGWSWURI(mtStelseltype, lValue);
    if Persleiding^.StelselURI = '' then
      if not fDisableErrorReport then ReportError('Persleiding: "Stelsel_type" veld ontbreekt.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Stelsel_type" veld ontbreekt.');

  // Stelselnaam
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('STELSEL_NAAM'), '');
    if lValue <> '' then
      Persleiding^.Stelselnaam:= lValue
    else
      if not fDisableErrorReport then ReportError('Persleiding: "Stelsel_naam" veld ontbreekt.' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Stelsel_naam" veld ontbreekt.');

  // Persleidingtype mapping
  if DataProvider.FieldExists('LEIDING_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('LEIDING_TYPE'), ''));
    Persleiding^.PersleidingTypeURI:= fMappingManager.GetGWSWURI(mtPersleidingType, lValue);
    if Persleiding^.PersleidingTypeURI = '' then begin
      Persleiding^.PersleidingTypeURI:= 'gwsw:Onbekend';  // Leiding^.LeidingTypeURI mag niet leeg zijn.
      if not fDisableErrorReport then ReportError('Persleiding: "Leiding_type" veld is leeg' + ' (' + Persleiding^.GUID + ')');
    end;
  end
  else
    ReportError('Persleiding: "Leiding_type" veld ontbreekt.');


  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));
    Persleiding^.VormURI:= fMappingManager.GetGWSWURI(mtVormLeiding, lValue);  { #todo : Let op dit is gelijk aan de leiding (streng). Misschien scheiden }
    if Persleiding^.VormURI = '' then
      if not fDisableErrorReport then ReportError('Persleiding: "Vorm" veld is leeg' + ' (' + Persleiding^.GUID + ')');
  end
  else
    ReportError('Persleiding: "Vorm" veld ontbreekt.');

  // Datum velden
  if DataProvider.FieldExists('BEGINDATUM') then begin
    lDate:= GetDateFromYearField(DataProvider, 'BEGINDATUM');
    if lDate <> 0 then
      Persleiding^.Begindatum:= lDate
    else begin
      Persleiding^.Begindatum:= 0; // mag NIET weg.  --> Dit wordt verder op afgevangen. Moet mischien al hier beter opgezet worden.
      if not fDisableErrorReport then ReportError('Persleiding: "Begindatum" veld is leeg' + ' (' + Persleiding^.GUID + ')');
    end
  end
  else
    ReportError('Persleiding: "Begindatum" veld ontbreekt.');


  // Geometrie inlezen - UNIFORME AANPAK ZOALS MapLeidingFromProvider
  Persleiding^.WKTGeometry:= '';
  Persleiding^.HasWKTGeometry:= False;
  Persleiding^.HasMultipleVertices:= False;

  // PRECIES DEZELFDE LOGICA ALS MapLeidingFromProvider
  WKTGeometryStr:= '';

  if DataProvider.FieldExists('WKT_GEOMETRY') then
  begin
    WKTGeometry:= DataProvider.GetFieldValue('WKT_GEOMETRY');
    if not VarIsNull(WKTGeometry) then
      WKTGeometryStr:= Trim(VarToStr(WKTGeometry));
  end
  else if DataProvider.FieldExists('GEOMETRY') then
  begin
    WKTGeometry:= DataProvider.GetFieldValue('GEOMETRY');
    if not VarIsNull(WKTGeometry) then
      WKTGeometryStr:= Trim(VarToStr(WKTGeometry));
  end;

  // Controleer of we geldige WKT geometrie hebben - ZELFDE ALS MapLeidingFromProvider
  if WKTGeometryStr <> '' then
  begin
    Persleiding^.WKTGeometry:= WKTGeometryStr;
    Persleiding^.HasWKTGeometry:= True;

    // Extra: specifieke verwerking voor persleiding geometrie
    if Pos('LINESTRING', UpperCase(Persleiding^.WKTGeometry)) > 0 then
    begin
      ParsePersleidingGeometry(Persleiding);
    end;
  end;

  // Optioneel: coördinaten van begin- en eindput als fallback
  if DataProvider.FieldExists('BEGINPUT_X') then
  begin
    lValue:= DataProvider.GetFieldValue('BEGINPUT_X');
    if not VarIsNull(lValue) then
      Persleiding^.BeginPutX:= lValue;
  end;

  if DataProvider.FieldExists('BEGINPUT_Y') then
  begin
    lValue:= DataProvider.GetFieldValue('BEGINPUT_Y');
    if not VarIsNull(lValue) then
      Persleiding^.BeginPutY:= lValue;
  end;

  if DataProvider.FieldExists('BEGINPUT_Z') then
  begin
    lValue:= DataProvider.GetFieldValue('BEGINPUT_Z');
    if not VarIsNull(lValue) then
      Persleiding^.BeginPutZ:= lValue;
  end;

  if DataProvider.FieldExists('EINDPUT_X') then
  begin
    lValue:= DataProvider.GetFieldValue('EINDPUT_X');
    if not VarIsNull(lValue) then
      Persleiding^.EindPutX:= lValue;
  end;

  if DataProvider.FieldExists('EINDPUT_Y') then
  begin
    lValue:= DataProvider.GetFieldValue('EINDPUT_Y');
    if not VarIsNull(lValue) then
      Persleiding^.EindPutY:= lValue;
  end;

  if DataProvider.FieldExists('EINDPUT_Z') then
  begin
    lValue:= DataProvider.GetFieldValue('EINDPUT_Z');
    if not VarIsNull(lValue) then
      Persleiding^.EindPutZ:= lValue;
  end;

  Result:= Persleiding;
end;

function TOroxExport.MapKolkFromProvider(DataProvider : IGWSWDataProvider) : PGWSWKolk;
var
  Kolk: PGWSWKolk;
  KolkTypeStr, MateriaalStr, VormStr: string;   { #todo : Kan weg. TempValue is voldoende }
  WKTGeometry: string;
  GeoX, GeoY, GeoZ: Double;
  lValue: Variant;
begin
  New(Kolk);

  // Guid
  if DataProvider.FieldExists('GUID') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('GUID'), '');  // It is a string!
    if lValue <> '' then
      Kolk^.GUID:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "GUID" is leeg' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: GUID veld ontbreekt');  // Can never happen but still...


  if DataProvider.FieldExists('NAAM') then begin
    lValue:= VarToStrDef(DataProvider.GetFieldValue('NAAM'), '');
    if lValue <> '' then
      Kolk^.aLabel:= lValue
    else begin
      Kolk^.aLabel:= '';
      if not fDisableErrorReport then ReportError('Kolk: "Naam" is leeg' + ' (' + Kolk^.GUID + ')');
    end
  end
  else
    ReportError('Kolk: "Naam" veld ontbreekt.');

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
        lValue := DataProvider.GetFieldValue('X');
        if not VarIsNull(lValue) then Kolk^.X := lValue else Kolk^.X := 0;

        lValue := DataProvider.GetFieldValue('Y');
        if not VarIsNull(lValue) then Kolk^.Y := lValue else Kolk^.Y := 0;

        lValue := DataProvider.GetFieldValue('Z');
        if not VarIsNull(lValue) then Kolk^.Z := lValue else Kolk^.Z := 0;

        Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);
      end;
    end;
  end
  else begin
    // Gebruik individuele coordinaat velden
    // Mag hiert nooit komen. Validatie vooraf maken
    lValue := DataProvider.GetFieldValue('X');
    if not VarIsNull(lValue) then Kolk^.X := lValue else Kolk^.X := 0;

    lValue := DataProvider.GetFieldValue('Y');
    if not VarIsNull(lValue) then Kolk^.Y := lValue else Kolk^.Y := 0;

    lValue := DataProvider.GetFieldValue('Z');
    if not VarIsNull(lValue) then Kolk^.Z := lValue else Kolk^.Z := 0;

    Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);
  end;

  // Afmetingen met NULL checks
  { #todo : Breedte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('BREEDTE') then      { #todo : Dit soort naamgevingen  zoals breedte, hoogte, etc moeten naar een const. }
  begin
    lValue:= DataProvider.GetFieldValue('BREEDTE');
    if not VarIsNull(lValue) then
      Kolk^.Breedte:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "Breedte" is leeg.' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Breedte" veld ontbreekt.');

  // Lengte
  { #todo : Lengte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('LENGTE') then begin
    lValue:= DataProvider.GetFieldValue('LENGTE');
    if not VarIsNull(lValue) then
      Kolk^.Lengte:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "Lengte" veld is leeg.' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Lengte" veld ontbreekt.');

  // Hoogte
  { #todo : Hoogte mag nooit negatief zijn. Controle maken. }
  if DataProvider.FieldExists('HOOGTE') then begin
    lValue:= DataProvider.GetFieldValue('HOOGTE');
    if not VarIsNull(lValue) then
      Kolk^.Hoogte:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "Hoogte" veld is leeg.' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Hoogte" veld ontbreekt');

  // DIAMETER
  if DataProvider.FieldExists('DIAMETER') then begin
    lValue:= DataProvider.GetFieldValue('DIAMETER');
    if not VarIsNull(lValue) then
      Kolk^.Diameter:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "Diameter" veld is leeg.' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Diameter" veld ontbreekt');


  // Materiaal mapping
  if DataProvider.FieldExists('MATERIAAL') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('MATERIAAL'), ''));
    Kolk^.MateriaalURI:= fMappingManager.GetGWSWURI(mtMateriaalPut, lValue);  { #todo : Let op, Materiaal is gelijk aan material put. onderscheid maken }
    if Kolk^.MateriaalURI = '' then
      if not fDisableErrorReport then ReportError('Kolk: "Materiaal" veld is leeg' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Materiaal" veld ontbreekt.');


  // Vorm mapping
  if DataProvider.FieldExists('VORM') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('VORM'), ''));  { #todo : Let op, vorm is gelijk aan put.misschien obnderscheid maken }
    Kolk^.VormURI:= fMappingManager.GetGWSWURI(mtvormPut, lValue);
    if Kolk^.VormURI = '' then
      if not fDisableErrorReport then ReportError('Kolk: "Vorm" veld is leeg' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Vorm" veld ontbreekt.');

  // Kolktype mapping
  if DataProvider.FieldExists('KOLK_TYPE') then begin
    lValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('KOLK_TYPE'), ''));
    Kolk^.KolkTypeUri:= fMappingManager.GetGWSWURI(mtKolkType, lValue);
    if Kolk^.KolkTypeUri = '' then begin
      Kolk^.KolkTypeUri:= 'gwsw:Kolk';  { #note : Deafult = Kolk }
      if not fDisableErrorReport then ReportError('Kolk: "Kolk_type" veld is leeg' + ' (' + Kolk^.GUID + ')');
    end;
  end
  else
    ReportError('Kolk: "Put_type" veld ontbreekt.');

  // Kolk-specifieke velden
  if DataProvider.FieldExists('WANDDIKTE') then begin
    lValue:= DataProvider.GetFieldValue('WANDDIKTE');
    if not VarIsNull(lValue) then
      Kolk^.Wanddikte:= lValue
    else
      if not fDisableErrorReport then ReportError('Kolk: "Wanddikte" veld is leeg.' + ' (' + Kolk^.GUID + ')');
  end
  else
    ReportError('Kolk: "Wanddikte" veld ontbreekt');

  Kolk^.HasOrientation := (Kolk^.X <> 0) and (Kolk^.Y <> 0);

  Result := Kolk;
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

function TOroxExport.GetDateFromYearField(DataProvider : IGWSWDataProvider;
         const FieldName : string; DefaultDay : Integer; DefaultMonth : Integer) : TDateTime;
var
  lValue: Variant;
  YearStr: string;
  YearInt: Integer;
begin
  // Default value (0 means "no valid date")
  Result:= 0;

  lValue:= DataProvider.GetFieldValue(FieldName);
  if VarIsNull(lValue) or VarIsEmpty(lValue) then
    Exit;

  YearStr:= Trim(VarToStr(lValue));

  if YearStr = '' then
    Exit;

  // Try to convert to integrity
  if not TryStrToInt(YearStr, YearInt) then
    Exit;

  // Validate year (adjustable limits) { #todo : Optioneel maken. }  EN op zoek vooraf. 1800 is erg oud
  if (YearInt < 1800) or (YearInt > YearOf(Date) + 10) then
    Exit;  { #todo : To build in a notification }

  // Create Date
  try
    Result:= EncodeDate(YearInt, DefaultMonth, DefaultDay);
  except
    on E: EConvertError do
      Result:= 0;  // Invalid date  { #todo : Moet naar de view }
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
begin
  if not IsNan(Put^.Breedte) and (Put^.Breedte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Breedte, 3) + ' ] ;');

  if not IsNan(Put^.Lengte) and (Put^.Lengte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Lengte, 3) + ' ] ;');

  if not IsNan(Put^.Hoogte) and (Put^.Hoogte > 0) then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogtePut ; gwsw:hasValue ' +
           FloatToOrox(Put^.Hoogte, 3) + ' ] ;');

  if Put^.MateriaalURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:MateriaalPut ; ' +
           'gwsw:hasReference ' + Put^.MateriaalURI + ' ] ;');

  if Put^.VormURI <> '' then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:VormPut ; ' +
           'gwsw:hasReference ' + Put^.VormURI + ' ] ;');

  if Put^.Begindatum > 0 then
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:Begindatum ; ' +
         'gwsw:hasValue "' + DateToOroxFormat(Put^.Begindatum) + '"^^xsd:date ] ;');

  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WijzeVanInwinning ; ' +
         'gwsw:hasReference ' + WijzeInwinningToGWSW(Put^.WijzeInwinning) + ' ] ;');
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
    SL.Add('    gwsw:hasValue                 "' + FloatToOrox(Put^.Maaiveldhoogte, 2) + '"^^xsd:decimal ;');

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

procedure TOroxExport.AddLeidingKenmerken(SL : TStringList; const Leiding : PGWSWLeiding);
begin
  if Leiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Lengte, 2) + ' ] ;');

  if Leiding^.Diameter > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:DiameterLeiding ; gwsw:hasValue ' +
           IntToStr(Leiding^.Diameter) + ' ] ;');

  if Leiding^.Breedte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:BreedteLeiding ; gwsw:hasValue ' +
           IntToStr(Leiding^.Breedte) + ' ] ;');

  if Leiding^.Hoogte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:HoogteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Leiding^.Hoogte, 0) + ' ] ;');  // GWSW schrijft 0 decimalen voor. Moet een heel getal in mm zijn.

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

procedure TOroxExport.AddPersleidingKenmerken(SL : TStringList; const Persleiding : PGWSWPersleiding);
begin
  if Persleiding^.Lengte > 0 then
    SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:LengteLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.Lengte, 2) + ' ] ;');

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
  if not IsNaN(Persleiding^.BobBegin) and (Persleiding^.BobBegin <> 0) then
    SL.Add('    gwsw:hasAspect  [ rdf:type gwsw:BobBeginpuntLeiding ; gwsw:hasValue ' +
           FloatToOrox(Persleiding^.BobBegin, 2) + ' ] ;');
  SL.Add('    gwsw:hasConnection  :' + Persleiding^.BeginPutID + '_ori ] ;');

  // Eindpunt met BOB waarde
  SL.Add('  gwsw:hasPart    [ rdf:type gwsw:EindpuntLeiding ;');
  if not IsNaN(Persleiding^.BobEind) and (Persleiding^.BobEind <> 0) then
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

function TOroxExport.FloatToOrox(Value : Double; Decimals : Integer
  ) : String;
var
  FormatStr: string;
begin
  // Bouw formatstring zoals %.2f, %.3f, etc.
  FormatStr := '%.' + IntToStr(Decimals) + 'f';

  Result := Format(FormatStr, [Value]);

  // Verwijder duizendtallen
  if DefaultFormatSettings.ThousandSeparator <> #0 then
    Result := StringReplace(
      Result,
      DefaultFormatSettings.ThousandSeparator,
      '',
      [rfReplaceAll]
    );

  // Zet decimaal naar punt
  Result := StringReplace(
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
  Kolk: PGWSWKolk;
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

  // Voeg stelsel specifieke kenmerken toe
  SL.Add('  gwsw:hasAspect  [ rdf:type gwsw:WijzeVanInwinning ; ' +
         'gwsw:hasReference ' + WijzeInwinningToGWSW(wiGemeten) + ' ] ;');

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

    // Kolken aan dummy stelsel toekennen
    // test.....
    // zou de validatie foutmelding moeten oplossen    (Knooppunt (orientatie van Put, Bouwwerk, Compartiment, Hulpstuk, Aansluitpunt) heeft geen verbinding)
    // ttl lijkt er goed uit te zien maar de validatie geeft nog steeds de knooppunt foutmelding
{    if Stelsel^.GUID = 'DUMMY_STELSEL' then begin
      for i:= 0 to FKolkList.Count - 1 do begin
        Kolk:= PGWSWKolk(FKolkList[i]);
        // Voeg ALLE kolken toe aan DUMMY_STELSEL
        PartsList.Add('    :' + Kolk^.GUID);
        HasParts:= True;
      end;
    end;}

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

  // test met Dummy. (kijken of de melding: "Knooppunt (orientatie van Put, Bouwwerk, Compartiment, Hulpstuk, Aansluitpunt) heeft geen verbinding" voorkomen kan worden
//  SL.Add('  gwsw:isPartOf   :DUMMY_STELSEL ;');  // Fallback naar dummy


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

function TOroxExport.WijzeInwinningToGWSW(Wijze : TGWSWWijzeInwinning) : string;
begin

  case Wijze of
    wiGemeten: Result:= 'gwsw:Ingemeten';
    wiBerekend: Result:= 'gwsw:Berekend';
    wiGeschat: Result:= 'gwsw:Geschat';
    wiOntwerp: Result:= 'gwsw:Ontwerp';
  else
    Result:= 'gwsw:Onbekend';
  end;
end;

procedure TOroxExport.ReportProgress(const Msg : string);
begin
  if Assigned(fProgressReporter) then
    fProgressReporter.ReportProgress(Msg);
end;

procedure TOroxExport.ReportError(const ErrMsg : string);
begin
  if Assigned(fProgressReporter) then
    fProgressReporter.ReportError(ErrMsg);
end;

constructor TOroxExport.Create(ADataProvider : IGWSWDataProvider; const FileName, OrganizationName, MappingsFile : String;
         AProgressReporter: IExportProgressReporter);
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

procedure TOroxExport.ExportToOrox(DisableErrorReport : Boolean);
var
  TotalRecords: Integer;
begin
  ReportProgress('Export gestart...');
  fDisableErrorReport:= DisableErrorReport;
  fDataProvider.Open;
  try
    fDataProvider.Last;
    fDataProvider.First;
    TotalRecords:= fDataProvider.GetRecordCount;
    ReportProgress(Format('%d records gevonden', [TotalRecords]));

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
    if not GenerateOroxTurtle(fFileName) then begin
      ReportError('Fout bij genereren Orox Turtle bestand');
      Exit;
    end;
    ReportProgress('Export succesvol voltooid!');
  except
      on E: Exception do
      begin
        ReportError('Onverwachte fout: ' + E.Message);
      end;
    end;
end;

end.

