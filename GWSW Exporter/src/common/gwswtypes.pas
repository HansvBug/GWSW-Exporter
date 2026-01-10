{ Copyright ©2025-2026 Hans van Buggenum }
unit GWSWTypes;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

type
  // Pointer types voor TList per object
  PGWSWPut = ^TGWSWPut;
  PGWSWLeiding = ^TGWSWLeiding;
  PGWSWStelsel = ^TGWSWStelsel;
  PGWSWKolk = ^TGWSWKolk;
  PGWSWPersleiding = ^TGWSWPersleiding;

  // Rioolput
  TGWSWPut = record
    GUID: String;  // OBJECT_GUID
    aLabel: String;
    PutTypeUri: String;
    Breedte: Integer;  // in mm
    Lengte: Integer;   // in mm
    Hoogte: Integer;   // in mm
    Diameter: Integer;
    MateriaalURI: String;
    VormURI: String;
    FunderingUri: String;
    Maaiveldhoogte: Double;
    Begindatum: TDateTime;
    Einddatum: TDateTime;
    StelselID: String;  //
    StelselURI: String;
    Stelselnaam: String;
    // Topologie
    X: Double;
    Y: Double;
    Z: Double;
    HasOrientation: Boolean;
    WKTGeometry: String;  // Bewaar de originele WKT
    HasWKTGeometry: Boolean; // Flag om aan te geven of WKT beschikbaar is
  end;

  // Rioolstreng / vrijvervalleiding
  TGWSWLeiding = record
  GUID: String;  // OBJECT_GUID
  aLabel: String;
  LeidingTypeURI: String;
  BeginPutID: String;
  EindPutID: String;
  Lengte: Double;
  Breedte: Integer;
  Hoogte: Integer;
  Diameter: Integer;
  MateriaalURI: String;
  VormURI: String;
  FunderingUri: String;
  StatusFunctionerenURI: String;
  WibonUri: String;
  Begindatum: TDate;  // Let op dit is aanleg jaar en dan omgezet naar 1 januari.
  Einddatum: TDateTime;
  BobBegin: Double;
  BobEind: Double;
  HasMultipleVertices: Boolean;
  Vertices: array of array[0..2] of Double;
  StelselID: String;  //
  StelselURI: String;
  Stelselnaam: String;  // Naam van het stelsel voor rdfs:label

  // Topologie
  WKTGeometry: String;
  HasWKTGeometry: Boolean;
  BeginPutX, BeginPutY, BeginPutZ: Double;
  EindPutX, EindPutY, EindPutZ: Double;
end;

  // Stelsel type
  TGWSWStelsel = record
    GUID: String;           // OBJECT_GUID
    aLabel: String;         // STELSELCODE
    StelselNaam: String;    // STELSELNAAM
    StelselTypeUri: String;
    HasParts: Boolean;
  end;

  // Kolk
  TGWSWKolk = record
     GUID: String;
     aLabel: String;
     KolkTypeUri: String;
     Breedte: Integer;
     Lengte: Integer;
     Hoogte: Integer;
     Diameter: Integer;
     MateriaalURI: String;
     VormURI: String;
     Wanddikte: Integer;
     // Topologie
     X: Double;
     Y: Double;
     Z: Double;
     HasOrientation: Boolean;
     WKTGeometry: String;
     HasWKTGeometry: Boolean;
   end;

  // Persleiding
  TGWSWPersleiding = record
      GUID: String;  // OBJECT_GUID
      aLabel: String;
      PersleidingTypeURI: String;
      VormURI: String;
      BeginPutID: String;
      EindPutID: String;
      Lengte: Double;
      Diameter: Integer;
      Breedte: Integer;
      Hoogte: Integer;
      MateriaalURI: String;
      StatusFunctionerenURI: String;
      Begindatum: TDate;
      BobBegin: Double;
      BobEind: Double;

      // Geometrie specifiek voor persleiding (kan meerdere vertices hebben)
      HasMultipleVertices: Boolean;
      Vertices: array of array[0..2] of Double;  // X, Y, Z voor elk vertex
      WKTGeometry: String;
      HasWKTGeometry: Boolean;

      // Topologie coördinaten
      BeginPutX, BeginPutY, BeginPutZ: Double;
      EindPutX, EindPutY, EindPutZ: Double;

      // Stelsel relatie
      StelselID: String;
      StelselURI: String;
      Stelselnaam: String;
    end;

  // Validate
  TGWSWConfig = record
    PutMinBreedte: Integer;
    PutMaxBreedte: Integer;
  end;

implementation


end.
