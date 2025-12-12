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

unit GWSWTypes;

interface

type
  // Wijze van inwinning ==> Nog opzoeken en implementeren  EN in extern bestand zetten
  TGWSWWijzeInwinning = (wiGemeten, wiBerekend, wiGeschat, wiOntwerp, wiOnbekend);

  // Pointer types voor TList per object
  PGWSWPut = ^TGWSWPut;
  PGWSWLeiding = ^TGWSWLeiding;
  PGWSWStelsel = ^TGWSWStelsel;
  PGWSWKolk = ^TGWSWKolk;
  PGWSWPersleiding = ^TGWSWPersleiding;

  // Rioolput
  TGWSWPut = record
    GUID: string;  // OBJECT_GUID
    aLabel: string;
    PutTypeUri: String;
    Breedte: Integer;  // in mm
    Lengte: Integer;   // in mm
    Hoogte: Integer;   // in mm
    MateriaalURI: string;
    VormURI: string;
    Maaiveldhoogte: Double;
    Grondwaterstand: Double;
    Begindatum: TDateTime;
    WijzeInwinning: TGWSWWijzeInwinning;
    StelselID: string;  //
    StelselURI: string;
    Stelselnaam: string;
    // Topologie
    X: Double;
    Y: Double;
    Z: Double;
    HasOrientation: Boolean;
    WKTGeometry: string;  // Bewaar de originele WKT
    HasWKTGeometry: Boolean; // Flag om aan te geven of WKT beschikbaar is
  end;

  // Rioolstreng / vrijvervalleiding
  TGWSWLeiding = record
  GUID: string;  // OBJECT_GUID
  aLabel: string;
  LeidingTypeURI: String;
  BeginPutID: string;
  EindPutID: string;
  Lengte: Double;
  Breedte: Integer;
  Hoogte: Integer;
  Diameter: Integer;
  MateriaalURI: string;
  VormURI: string;
  StatusFunctionerenURI: string;
  Begindatum: TDate;  // Let op dit is aanleg jaar en dan omgezet naar 1 januari.
  BobBegin: Double;
  BobEind: Double;
  WijzeInwinning: TGWSWWijzeInwinning;
  HasMultipleVertices: Boolean;
  Vertices: array of array[0..2] of Double;
  StelselID: string;  //
  StelselURI: string;
  Stelselnaam: string;  // Naam van het stelsel voor rdfs:label
  // Topologie
  WKTGeometry: string;
  HasWKTGeometry: Boolean;
  BeginPutX, BeginPutY, BeginPutZ: Double;
  EindPutX, EindPutY, EindPutZ: Double;
end;

  // Stelsel type
  TGWSWStelsel = record
    GUID: string;  // OBJECT_GUID
    aLabel: string;
    StelselTypeUri: string;
    HasParts: Boolean;
  end;

  // Kolk
  TGWSWKolk = record
     GUID: string;
     aLabel: string;
     KolkTypeUri: String;
     Breedte: Integer;
     Lengte: Integer;
     Hoogte: Integer;
     Diameter: Integer;
     MateriaalURI: string;
     VormURI: string;
     Wanddikte: Integer;
     TypeReiniging: string;
     // Topologie
     X: Double;
     Y: Double;
     Z: Double;
     HasOrientation: Boolean;
     WKTGeometry: string;
     HasWKTGeometry: Boolean;
   end;

  // Persleiding
  TGWSWPersleiding = record
      GUID: string;  // OBJECT_GUID
      aLabel: string;
      PersleidingTypeURI: String;
      VormURI: String;
      BeginPutID: string;
      EindPutID: string;
      Lengte: Double;
      Diameter: Integer;
      MateriaalURI: string;
      StatusFunctionerenURI: string;
      Begindatum: TDate;
      BobBegin: Double;
      BobEind: Double;
      WijzeInwinning: TGWSWWijzeInwinning;

      // Geometrie specifiek voor persleiding (kan meerdere vertices hebben)
      HasMultipleVertices: Boolean;
      Vertices: array of array[0..2] of Double;  // X, Y, Z voor elk vertex
      WKTGeometry: string;
      HasWKTGeometry: Boolean;

      // Topologie coördinaten
      BeginPutX, BeginPutY, BeginPutZ: Double;
      EindPutX, EindPutY, EindPutZ: Double;

      // Stelsel relatie
      StelselID: string;
      StelselURI: string;
      Stelselnaam: string;
    end;

implementation

end.
