{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
unit ExportConfig;

{$mode objfpc}{$H+}

interface

uses
  model.base;

type
  { TExportConfig — value-type view op TSettingsRec.
    Wordt door TOroxExport als parameter gebruikt (read-only).
    De enige bron van waarheid is TSettingsRec; deze record wordt
    uitsluitend gevuld via ExportConfigFromSettings(). }
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
    IncludeLeidingEinddatum,
    IncludeLeidingVerbindingstype: Boolean;

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
  end;

{ Conversie-functie: TSettingsRec → TExportConfig.
  Alle aanroepers (TSettings.GetExportConfig, TOroxExport, tests)
  gebruiken deze, zodat er één plek is om een nieuwe export-setting
  toe te voegen. }
function ExportConfigFromSettings(const S: TSettingsRec): TExportConfig;

implementation

function ExportConfigFromSettings(const S: TSettingsRec): TExportConfig;
begin
  // Put
  Result.IncludePutLengte:= S.setIncludePutLengte;
  Result.IncludePutBreedte:= S.setIncludePutBreedte;
  Result.IncludePutHoogte:= S.setIncludePutHoogte;
  Result.IncludePutDiameter:= S.setIncludePutDiameter;
  Result.IncludePutVorm:= S.setIncludePutVorm;
  Result.IncludePutMateriaal:= S.setIncludePutMateriaal;
  Result.IncludePutFundering:= S.setIncludePutFundering;
  Result.IncludePutMaaiveldhoogte:= S.setIncludePutMaaiveldhoogte;
  Result.IncludePutBegindatum:= S.setIncludePutBegindatum;
  Result.IncludePutEinddatum:= S.setIncludePutEinddatum;

  // Leiding
  Result.IncludeLeidingLengte:= S.setIncludeLeidingLengte;
  Result.IncludeLeidingBreedte:= S.setIncludeLeidingBreedte;
  Result.IncludeLeidingHoogte:= S.setIncludeLeidingHoogte;
  Result.IncludeLeidingDiameter:= S.setIncludeLeidingDiameter;
  Result.IncludeLeidingVorm:= S.setIncludeLeidingVorm;
  Result.IncludeLeidingMateriaal:= S.setIncludeLeidingMateriaal;
  Result.IncludeLeidingFundering:= S.setIncludeLeidingFundering;
  Result.IncludeLeidingBobBegin:= S.setIncludeLeidingBobBegin;
  Result.IncludeLeidingBobEind:= S.setIncludeLeidingBobEind;
  Result.IncludeLeidingStatusFunctioneren:= S.setIncludeLeidingStatusFunctioneren;
  Result.IncludeLeidingWIBONThema:= S.setIncludeLeidingWIBONThema;
  Result.IncludeLeidingBegindatum:= S.setIncludeLeidingBegindatum;
  Result.IncludeLeidingEinddatum:= S.setIncludeLeidingEinddatum;
  Result.IncludeLeidingVerbindingstype:= S.setIncludeLeidingVerbindingstype;

  // Persleiding
  Result.IncludePersleidingLengte:= S.setIncludePersleidingLengte;
  Result.IncludePersleidingHoogte:= S.setIncludePersleidingHoogte;
  Result.IncludePersleidingDiameter:= S.setIncludePersleidingDiameter;
  Result.IncludePersleidingVorm:= S.setIncludePersleidingVorm;
  Result.IncludePersleidingMateriaal:= S.setIncludePersleidingMateriaal;
  Result.IncludePersleidingStatusFunctioneren:= S.setIncludePersleidingStatusFunctioneren;
  Result.IncludePersleidingBegindatum:= S.setIncludePersleidingBegindatum;
  Result.IncludePersleidingEinddatum:= S.setIncludePersleidingEinddatum;
  Result.IncludePersleidingBobBegin:= S.setIncludePersleidingBobBegin;
  Result.IncludePersleidingBobEind:= S.setIncludePersleidingBobEind;

  // Kolk
  Result.IncludeKolkLengte:= S.setIncludeKolkLengte;
  Result.IncludeKolkBreedte:= S.setIncludeKolkBreedte;
  Result.IncludeKolkHoogte:= S.setIncludeKolkHoogte;
  Result.IncludeKolkDiameter:= S.setIncludeKolkDiameter;
  Result.IncludeKolkVorm:= S.setIncludeKolkVorm;
  Result.IncludeKolkMateriaal:= S.setIncludeKolkMateriaal;
  Result.IncludeKolkWanddikte:= S.setIncludeKolkWanddikte;
  Result.IncludeKolkBegindatum:= S.setIncludeKolkBegindatum;
  Result.IncludeKolkEinddatum:= S.setIncludeKolkEinddatum;
end;

end.
