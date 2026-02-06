{ Copyright ©2025-2026 Hans van Buggenum }
unit ExportConfig;

{$mode objfpc}{$H+}

interface

type
  TExportConfig = record
    // Put attributen
    IncludePutLengte,
    IncludePutBreedte,
    IncludePutHoogte,
    IncludePutDiameter,
    IncludePutVorm,
    IncludePutMateriaal,
    IncludePutFundering,
    IncludePutMaaiveldhoogte,
    IncludePutBegindatum,
    IncludePutEinddatum: Boolean;

    // Leiding attributen
    IncludeLeidingLengte,
    IncludeLeidingBreedte,
    IncludeLeidingHoogte,
    IncludeLeidingDiameter,
    IncludeLeidingVorm,
    IncludeLeidingMateriaal,
    IncludeLeidingFundering,
    IncludeLeidingBobBegin,
    IncludeLeidingBobEind,
    IncludeLeidingStatusFunctioneren,
    IncludeLeidingWIBONThema,
    IncludeLeidingBegindatum,
    IncludeLeidingEinddatum: Boolean;

    // Persleiding attributen
    IncludePersleidingLengte,
    IncludePersleidingHoogte,
    IncludePersleidingDiameter,
    IncludePersleidingVorm,
    IncludePersleidingMateriaal,
    IncludePersleidingStatusFunctioneren,
    IncludePersleidingBegindatum,
    IncludePersleidingEinddatum,
    IncludePersleidingBobBegin,
    IncludePersleidingBobEind: Boolean;

    // Kolk attributen
    IncludeKolkLengte,
    IncludeKolkBreedte,
    IncludeKolkHoogte,
    IncludeKolkDiameter,
    IncludeKolkVorm,
    IncludeKolkMateriaal,
    IncludeKolkWanddikte,
    IncludeKolkBegindatum,
    IncludeKolkEinddatum: Boolean;

    // Object types
//    IncludePutten: Boolean;
//    IncludeLeidingen: Boolean;
//    IncludePersleidingen: Boolean;
//    IncludeKolken: Boolean;
//    IncludeStelsels: Boolean;

    // Default waarden
  end;

implementation



end.
