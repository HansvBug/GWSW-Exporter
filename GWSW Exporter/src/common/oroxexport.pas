{ Copyright ©2025 Hans van Buggenum }
unit OroxExport;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, Variants, Math, DateUtils,
  model.intf, uIGWSWDataProvider, QueryDataProvider, GWSWTypes, MappingManager;

type

  { TOroxExport }
  TOroxExport= class(TObject)
    private
      fDataProvider: IGWSWDataProvider;
      fFileName: String;
      fOrganizationName: String;

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

      procedure ExportStelselToTurtle(SL: TStringList; const Stelsel: PGWSWStelsel);
      //exportToTurtle helper
      function WijzeInwinningToGWSW(Wijze: TGWSWWijzeInwinning): string;

    public
      constructor Create(ADataProvider : IGWSWDataProvider; const FileName,
        OrganizationName, MappingsFile : String);
      destructor Destroy; override;
      procedure ExportToOrox;
  end;



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
begin
  Result:= False;
  try
    ClearAllLists;

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
//              FLeidingList.Add(MapLeidingFromProvider(DataProvider));
            end;
            'PERSLEIDING': begin
//              FPersLeidingList.Add(MapPersleidingFromProvider(DataProvider));
            end;
            'KOLK': begin
//              FKolkList.Add(MapKolkFromProvider(DataProvider));
            end
            else begin
              // Onbekend objecttype aangetroffen.
              { #todo : Moet een melding geven in de view }
              //tmp:= '';
            end;
          end;

        until not DataProvider.Next;
      end;
    finally
      DataProvider.Close;
    end;

    Result := True;
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
  Result := False;
  SL := TStringList.Create;  // de specificaties schrijven utf 8 voor. (nodig voor é ë etc.)
  try
    // Eerst een dummy stelsel toevoegen aan de lijst voor objecten zonder stelsel
    // Nodig voor de kolken. Is niet voldoende/goed om de validatie meldingen te voorkomen. Nakijken.
    { #todo : Moet anders? beter? Er blijven validatie meldingen bij de kolken bestaan. }
    New(DummyStelsel);
    DummyStelsel^.GUID:= 'DUMMY_STELSEL';
    DummyStelsel^.aLabel:= 'Dummy stelsel voor objecten zonder stelsel';
    DummyStelsel^.HasParts:= True;
    DummyStelsel^.StelselTypeUri:= 'gwsw:GemengdStelsel';
    FStelselList.Add(DummyStelsel);

    SL.WriteBOM := False; { #todo : Uitzoeken. }

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

    // Opslaan
    SL.SaveToFile(FileName);
    Result := True;

  finally
    SL.Free;
  end;
end;

function TOroxExport.MapStelselFromProvider(DataProvider : IGWSWDataProvider) : PGWSWStelsel;
var
  Stelsel: PGWSWStelsel;
  TempValue: Variant;
begin
  New(Stelsel);

  // Initialiseer
  Stelsel^.GUID := VarToStrDef(DataProvider.GetFieldValue('GUID'), '');
  Stelsel^.aLabel := '';
  Stelsel^.HasParts := True;
  Stelsel^.StelselTypeUri := '';

  // Korte naam (STELSELCODE)
  if DataProvider.FieldExists('NAAM') then begin
    TempValue := DataProvider.GetFieldValue('NAAM');
    if not VarIsNull(TempValue) then
      Stelsel^.aLabel := VarToStr(TempValue);
  end;

  // Lange naam  (STELSELNAAM)
  if DataProvider.FieldExists('STELSEL_NAAM') then begin
    TempValue := DataProvider.GetFieldValue('STELSEL_NAAM');
    if not VarIsNull(TempValue) then
      Stelsel^.StelselNaam := VarToStr(TempValue);
  end;

  // Als er geen korte naam is, gebruik dan de lange naam voor aLabel
  if (Stelsel^.aLabel = '') and (Stelsel^.StelselNaam <> '') then
    Stelsel^.aLabel := Stelsel^.StelselNaam;

  // Stelsel type mapping
  if DataProvider.FieldExists('STELSEL_TYPE') then begin
    TempValue:= UpperCase(VarToStrDef(DataProvider.GetFieldValue('STELSEL_TYPE'), ''));
    Stelsel^.StelselTypeUri := FMappingManager.GetGWSWURI(mtStelseltype, TempValue);
    // DEBUG: Log indien nodig
    if Stelsel^.StelselTypeUri = 'gwsw:Onbekend' then begin
      // Melden in de view
      Stelsel^.StelselTypeUri:= '""';  // Geen gwsw:Onbekend gebruiken. Dat wordt door de shacl-meting als Violation gemerkt. (https://apps.gwsw.nl/item_validate_shacl)
    end;
  end;

  Result := Stelsel;
end;

function TOroxExport.MapPutFromProvider(DataProvider : IGWSWDataProvider) : PGWSWPut;
begin

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

function TOroxExport.WijzeInwinningToGWSW(Wijze : TGWSWWijzeInwinning) : string;
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

constructor TOroxExport.Create(ADataProvider : IGWSWDataProvider; const FileName, OrganizationName, MappingsFile : String);
begin
  inherited Create;

  fDataProvider:= ADataProvider;
  fFileName:= FileName;
  fOrganizationName:= OrganizationName;

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

procedure TOroxExport.ExportToOrox;
var
  TotalRecords: Integer;
begin
  fDataProvider.Open;
  try
    TotalRecords := fDataProvider.GetRecordCount;

    // Mapping the BOR domain value to the GWSW domain value
    if not MapDatabaseToGWSW(fDataProvider, TotalRecords) then
      raise Exception.Create('Fout bij mapping data naar GWSW model');  { #todo : Moet terug naar de view. }

    // Prepare and save TTL file
    if not GenerateOroxTurtle(fFileName) then
      raise Exception.Create('Fout bij genereren Orox Turtle bestand'); { #todo : Moet terug naar de view. }

  finally
    //...
  end;
end;

end.

