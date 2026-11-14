{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, ExportConfig, model.base;

type
  { TSettings — Application Configuration Manager

    Een nieuwe setting toevoegen:
      1. Voeg veld toe aan TSettingsRec (model.base.pas)
      2. Voeg één Read*-regel toe in TSettings.Load
      3. Voeg één Write*-regel toe in TSettings.Save
  }
  TSettings = class(TObject)
  private
    fAllSettings: TSettingsRec;
    function GetExportConfig: TExportConfig;
    procedure ResetToDefaults;
  public
    constructor Create; overload;
    constructor Create(const aFile: string); overload;
    destructor Destroy; override;

    { Primaire API — enige I/O }
    procedure Load;
    function  Save: Boolean;
    procedure WriteSetting(const SettingName, Section, AValue: String);  { TODO : Write single setting moet nog een keer weg.  }

    { Form state }
    procedure StoreFormState;
    procedure ReadFormState;

    { Nieuwe primaire state }
    property AllSettings: TSettingsRec read fAllSettings write fAllSettings;
    property ExportConfig: TExportConfig read GetExportConfig;

    property SettingsFile: String read fAllSettings.setSettingsFile write fAllSettings.setSettingsFile;
    property FormName: String read fAllSettings.setFrmName write fAllSettings.setFrmName;
    property AppName: String read fAllSettings.setApplicationName write fAllSettings.setApplicationName;
    property AppVersion: String read fAllSettings.setApplicationVersion write fAllSettings.setApplicationVersion;
    property AppBuildDate: String read fAllSettings.setApplicationBuildDate write fAllSettings.setApplicationBuildDate;
    property Succes: Boolean read fAllSettings.setSucces write fAllSettings.setSucces;
  end;

implementation

{ TSettings }

constructor TSettings.Create;
begin
  inherited Create;
  ResetToDefaults;
end;

constructor TSettings.Create(const aFile: string);
begin
  Create;
  fAllSettings.setSettingsFile:= aFile;
end;

destructor TSettings.Destroy;
begin
  inherited Destroy;
end;

function TSettings.GetExportConfig: TExportConfig;
begin
  // Afgeleide view op fAllSettings — geen aparte state meer
  Result.IncludePutLengte:= fAllSettings.setIncludePutLengte;
  Result.IncludePutBreedte:= fAllSettings.setIncludePutBreedte;
  Result.IncludePutHoogte:= fAllSettings.setIncludePutHoogte;
  Result.IncludePutDiameter:= fAllSettings.setIncludePutDiameter;
  Result.IncludePutVorm:= fAllSettings.setIncludePutVorm;
  Result.IncludePutMateriaal:= fAllSettings.setIncludePutMateriaal;
  Result.IncludePutFundering:= fAllSettings.setIncludePutFundering;
  Result.IncludePutMaaiveldhoogte:= fAllSettings.setIncludePutMaaiveldhoogte;
  Result.IncludePutBegindatum:= fAllSettings.setIncludePutBegindatum;
  Result.IncludePutEinddatum:= fAllSettings.setIncludePutEinddatum;

  Result.IncludeLeidingLengte:= fAllSettings.setIncludeLeidingLengte;
  Result.IncludeLeidingBreedte:= fAllSettings.setIncludeLeidingBreedte;
  Result.IncludeLeidingHoogte:= fAllSettings.setIncludeLeidingHoogte;
  Result.IncludeLeidingDiameter:= fAllSettings.setIncludeLeidingDiameter;
  Result.IncludeLeidingVorm:= fAllSettings.setIncludeLeidingVorm;
  Result.IncludeLeidingMateriaal:= fAllSettings.setIncludeLeidingMateriaal;
  Result.IncludeLeidingFundering:= fAllSettings.setIncludeLeidingFundering;
  Result.IncludeLeidingStatusFunctioneren:= fAllSettings.setIncludeLeidingStatusFunctioneren;
  Result.IncludeLeidingWIBONThema:= fAllSettings.setIncludeLeidingWIBONThema;
  Result.IncludeLeidingBegindatum:= fAllSettings.setIncludeLeidingBegindatum;
  Result.IncludeLeidingEinddatum:= fAllSettings.setIncludeLeidingEinddatum;
  Result.IncludeLeidingBobBegin:= fAllSettings.setIncludeLeidingBobBegin;
  Result.IncludeLeidingBobEind:= fAllSettings.setIncludeLeidingBobEind;
  Result.IncludeLeidingVerbindingstype:= fAllSettings.setIncludeLeidingVerbindingstype;

  Result.IncludePersleidingLengte:= fAllSettings.setIncludePersleidingLengte;
  Result.IncludePersleidingHoogte:= fAllSettings.setIncludePersleidingHoogte;
  Result.IncludePersleidingDiameter:= fAllSettings.setIncludePersleidingDiameter;
  Result.IncludePersleidingVorm:= fAllSettings.setIncludePersleidingVorm;
  Result.IncludePersleidingMateriaal:= fAllSettings.setIncludePersleidingMateriaal;
  Result.IncludePersleidingStatusFunctioneren:= fAllSettings.setIncludePersleidingStatusFunctioneren;
  Result.IncludePersleidingBegindatum:= fAllSettings.setIncludePersleidingBegindatum;
  Result.IncludePersleidingEinddatum:= fAllSettings.setIncludePersleidingEinddatum;
  Result.IncludePersleidingBobBegin:= fAllSettings.setIncludePersleidingBobBegin;
  Result.IncludePersleidingBobEind:= fAllSettings.setIncludePersleidingBobEind;

  Result.IncludeKolkLengte:= fAllSettings.setIncludeKolkLengte;
  Result.IncludeKolkBreedte:= fAllSettings.setIncludeKolkBreedte;
  Result.IncludeKolkHoogte:= fAllSettings.setIncludeKolkHoogte;
  Result.IncludeKolkDiameter:= fAllSettings.setIncludeKolkDiameter;
  Result.IncludeKolkVorm:= fAllSettings.setIncludeKolkVorm;
  Result.IncludeKolkMateriaal:= fAllSettings.setIncludeKolkMateriaal;
  Result.IncludeKolkWanddikte:= fAllSettings.setIncludeKolkWanddikte;
  Result.IncludeKolkBegindatum:= fAllSettings.setIncludeKolkBegindatum;
  Result.IncludeKolkEinddatum:= fAllSettings.setIncludeKolkEinddatum;
end;

procedure TSettings.ResetToDefaults;
begin
  // Algemeen
  fAllSettings.setActivateLogging:= True;
  fAllSettings.setAppendLogging:= True;
  fAllSettings.setLanguage:= 'en';
  fAllSettings.setSucces:= False;

  // GWSW / paden
  fAllSettings.setGWSWVersion:= '1.7.0';   { TODO : Moet een const worden }
  fAllSettings.setMappingFile:= '';
  fAllSettings.setSqlFileLocation:= '';
  fAllSettings.setLastUsedOrganization:= '';

  // Layout (voorkomt 0-waarden bij eerste run zonder ini)
  fAllSettings.setSplitterDataSettings:= 300;
  fAllSettings.setSplitterdataGrid:= 400;
  fAllSettings.setSplitterMemos:= 500;

  // UI-gedrag
  fAllSettings.setDbGridRowHighlight:= True;
  fAllSettings.setAskToOpenExportFile:= True;
  fAllSettings.setKeepLastOrganization:= True;

  // Rapportage
  fAllSettings.setRapportMappingError:= True;
  fAllSettings.setRapportFatalError:= True;
  fAllSettings.setRapportFieldIsEmpty:= False;
  fAllSettings.setRapportFieldIsMissing:= True;
  fAllSettings.setRapportOutOfRange:= True;

  // Export — Put
  fAllSettings.setIncludePutLengte:= True;
  fAllSettings.setIncludePutBreedte:= True;
  fAllSettings.setIncludePutHoogte:= True;
  fAllSettings.setIncludePutDiameter:= True;
  fAllSettings.setIncludePutVorm:= True;
  fAllSettings.setIncludePutMateriaal:= True;
  fAllSettings.setIncludePutFundering:= True;
  fAllSettings.setIncludePutBegindatum:= True;
  fAllSettings.setIncludePutEinddatum:= False;
  fAllSettings.setIncludePutMaaiveldhoogte:= True;

  // Export — Leiding
  fAllSettings.setIncludeLeidingLengte:= True;
  fAllSettings.setIncludeLeidingBreedte:= True;
  fAllSettings.setIncludeLeidingHoogte:= True;
  fAllSettings.setIncludeLeidingDiameter:= True;
  fAllSettings.setIncludeLeidingVorm:= True;
  fAllSettings.setIncludeLeidingMateriaal:= True;
  fAllSettings.setIncludeLeidingFundering:= True;
  fAllSettings.setIncludeLeidingStatusFunctioneren:= True;
  fAllSettings.setIncludeLeidingWIBONThema:= False;
  fAllSettings.setIncludeLeidingBegindatum:= True;
  fAllSettings.setIncludeLeidingEinddatum:= False;
  fAllSettings.setIncludeLeidingBobBegin:= True;
  fAllSettings.setIncludeLeidingBobEind:= True;
  fAllSettings.setIncludeLeidingVerbindingstype:= True;

  // Export — Persleiding
  fAllSettings.setIncludePersleidingLengte:= True;
  fAllSettings.setIncludePersleidingHoogte:= True;
  fAllSettings.setIncludePersleidingDiameter:= True;
  fAllSettings.setIncludePersleidingVorm:= True;
  fAllSettings.setIncludePersleidingMateriaal:= True;
  fAllSettings.setIncludePersleidingStatusFunctioneren:= True;
  fAllSettings.setIncludePersleidingBegindatum:= True;
  fAllSettings.setIncludePersleidingEinddatum:= False;
  fAllSettings.setIncludePersleidingBobBegin:= True;
  fAllSettings.setIncludePersleidingBobEind:= True;

  // Export — Kolk
  fAllSettings.setIncludeKolkLengte:= True;
  fAllSettings.setIncludeKolkBreedte:= True;
  fAllSettings.setIncludeKolkHoogte:= True;
  fAllSettings.setIncludeKolkDiameter:= True;
  fAllSettings.setIncludeKolkVorm:= True;
  fAllSettings.setIncludeKolkMateriaal:= True;
  fAllSettings.setIncludeKolkWanddikte:= False;
  fAllSettings.setIncludeKolkBegindatum:= True;
  fAllSettings.setIncludeKolkEinddatum:= False;
end;

procedure TSettings.Load;
var
  lIni: TIniFile;
begin
  fAllSettings.setSucces:= False;

  if (fAllSettings.setSettingsFile = '') or
     (not FileExists(fAllSettings.setSettingsFile)) then
    Exit;

  lIni:= TIniFile.Create(fAllSettings.setSettingsFile);
  try
    // Algemeen
    fAllSettings.setActivateLogging:= lIni.ReadBool  ('Configure', 'ActivateLogging', True);
    fAllSettings.setAppendLogging:= lIni.ReadBool  ('Configure', 'AppendLogFile', True);
    fAllSettings.setLanguage:= lIni.ReadString('Configure', 'Language', 'en');
    fAllSettings.setMappingFile:= lIni.ReadString('Configure', 'MappingFile', '');
    fAllSettings.setGWSWVersion:= lIni.ReadString('Configure', 'GWSWVersion', '1.6');
    fAllSettings.setSqlFileLocation:= lIni.ReadString('Configure', 'SQLfileLocation', '');
    fAllSettings.setSplitterDataSettings:= lIni.ReadInteger('Configure', 'SplitterDataSettings', 300);
    fAllSettings.setSplitterdataGrid:= lIni.ReadInteger('Configure', 'SplitterdataGrid', 400);
    fAllSettings.setSplitterMemos:= lIni.ReadInteger('Configure', 'SplitterMemos', 500);
    fAllSettings.setDbGridRowHighlight:= lIni.ReadBool('Configure', 'DbGridRowHighlight', True);
    fAllSettings.setAskToOpenExportFile:= lIni.ReadBool('Configure', 'AskToOpenExportFile', True);
    fAllSettings.setKeepLastOrganization:= lIni.ReadBool('Configure', 'KeepLastOrganization', True);
    fAllSettings.setLastUsedOrganization:= lIni.ReadString('Configure', 'LastUsedOrganization', '');

    fAllSettings.setRapportMappingError:= lIni.ReadBool('Configure', 'RapportMappingError', True);
    fAllSettings.setRapportFatalError:= lIni.ReadBool('Configure', 'RapportFatalError', True);
    fAllSettings.setRapportFieldIsEmpty:= lIni.ReadBool('Configure', 'RapportFieldIsEmpty', False);
    fAllSettings.setRapportFieldIsMissing:= lIni.ReadBool('Configure', 'RapportFieldIsMissing', True);
    fAllSettings.setRapportOutOfRange:= lIni.ReadBool('Configure', 'RapportOutOfRange', True);

    // Export — Put
    fAllSettings.setIncludePutLengte:= lIni.ReadBool('Export', 'IncludePutLengte', True);
    fAllSettings.setIncludePutBreedte:= lIni.ReadBool('Export', 'IncludePutBreedte', True);
    fAllSettings.setIncludePutHoogte:= lIni.ReadBool('Export', 'IncludePutHoogte', True);
    fAllSettings.setIncludePutDiameter:= lIni.ReadBool('Export', 'IncludePutDiameter', True);
    fAllSettings.setIncludePutVorm:= lIni.ReadBool('Export', 'IncludePutVorm', True);
    fAllSettings.setIncludePutMateriaal:= lIni.ReadBool('Export', 'IncludePutMateriaal', True);
    fAllSettings.setIncludePutFundering:= lIni.ReadBool('Export', 'IncludePutFundering', False);
    fAllSettings.setIncludePutBegindatum:= lIni.ReadBool('Export', 'IncludePutBegindatum', True);
    fAllSettings.setIncludePutEinddatum:= lIni.ReadBool('Export', 'IncludePutEinddatum', False);
    fAllSettings.setIncludePutMaaiveldhoogte:= lIni.ReadBool('Export', 'IncludePutMaaiveldhoogte', True);

    // Export — Leiding
    fAllSettings.setIncludeLeidingLengte:= lIni.ReadBool('Export', 'IncludeLeidingLengte', True);
    fAllSettings.setIncludeLeidingBreedte:= lIni.ReadBool('Export', 'IncludeLeidingBreedte', True);
    fAllSettings.setIncludeLeidingHoogte:= lIni.ReadBool('Export', 'IncludeLeidingHoogte', True);
    fAllSettings.setIncludeLeidingDiameter:= lIni.ReadBool('Export', 'IncludeLeidingDiameter', True);
    fAllSettings.setIncludeLeidingVorm:= lIni.ReadBool('Export', 'IncludeLeidingVorm', True);
    fAllSettings.setIncludeLeidingMateriaal:= lIni.ReadBool('Export', 'IncludeLeidingMateriaal', True);
    fAllSettings.setIncludeLeidingFundering:= lIni.ReadBool('Export', 'IncludeLeidingFundering', True);
    fAllSettings.setIncludeLeidingStatusFunctioneren:=lIni.ReadBool('Export', 'IncludeLeidingStatusFunctioneren', True);
    fAllSettings.setIncludeLeidingWIBONThema:= lIni.ReadBool('Export', 'IncludeLeidingWIBONThema', False);
    fAllSettings.setIncludeLeidingBegindatum:= lIni.ReadBool('Export', 'IncludeLeidingBegindatum', True);
    fAllSettings.setIncludeLeidingEinddatum:= lIni.ReadBool('Export', 'IncludeLeidingEinddatum', False);
    fAllSettings.setIncludeLeidingBobBegin:= lIni.ReadBool('Export', 'IncludeLeidingBobBegin', True);
    fAllSettings.setIncludeLeidingBobEind:= lIni.ReadBool('Export', 'IncludeLeidingBobEind', True);
    fAllSettings.setIncludeLeidingVerbindingstype:= lIni.ReadBool('Export', 'IncludeLeidingVerbindingstype', True);

    // Export — Persleiding
    fAllSettings.setIncludePersleidingLengte:= lIni.ReadBool('Export', 'IncludePersleidingLengte', True);
    fAllSettings.setIncludePersleidingHoogte:= lIni.ReadBool('Export', 'IncludePersleidingHoogte', True);
    fAllSettings.setIncludePersleidingDiameter:= lIni.ReadBool('Export', 'IncludePersleidingDiameter', True);
    fAllSettings.setIncludePersleidingVorm:= lIni.ReadBool('Export', 'IncludePersleidingVorm', True);
    fAllSettings.setIncludePersleidingMateriaal:= lIni.ReadBool('Export', 'IncludePersleidingMateriaal', True);
    fAllSettings.setIncludePersleidingStatusFunctioneren:=lIni.ReadBool('Export', 'IncludePersleidingStatusFunctioneren', True);
    fAllSettings.setIncludePersleidingBegindatum:= lIni.ReadBool('Export', 'IncludePersleidingBegindatum', True);
    fAllSettings.setIncludePersleidingEinddatum:= lIni.ReadBool('Export', 'IncludePersleidingEinddatum', False);
    fAllSettings.setIncludePersleidingBobBegin:= lIni.ReadBool('Export', 'IncludePersleidingBobBegin', False);
    fAllSettings.setIncludePersleidingBobEind:= lIni.ReadBool('Export', 'IncludePersleidingBobEind', False);

    // Export — Kolk
    fAllSettings.setIncludeKolkLengte:= lIni.ReadBool('Export', 'IncludeKolkLengte', True);
    fAllSettings.setIncludeKolkBreedte:= lIni.ReadBool('Export', 'IncludeKolkBreedte', True);
    fAllSettings.setIncludeKolkHoogte:= lIni.ReadBool('Export', 'IncludeKolkHoogte', True);
    fAllSettings.setIncludeKolkDiameter:= lIni.ReadBool('Export', 'IncludeKolkDiameter', True);
    fAllSettings.setIncludeKolkVorm:= lIni.ReadBool('Export', 'IncludeKolkVorm', True);
    fAllSettings.setIncludeKolkMateriaal:= lIni.ReadBool('Export', 'IncludeKolkMateriaal', True);
    fAllSettings.setIncludeKolkWanddikte:= lIni.ReadBool('Export', 'IncludeKolkWanddikte', False);
    fAllSettings.setIncludeKolkBegindatum:= lIni.ReadBool('Export', 'IncludeKolkBegindatum', True);
    fAllSettings.setIncludeKolkEinddatum:= lIni.ReadBool('Export', 'IncludeKolkEinddatum', False);

    // Application-info (in de ini, maar niet in het Configure/Export-blok)
    fAllSettings.setApplicationName:= lIni.ReadString('Application', 'Name', fAllSettings.setApplicationName);
    fAllSettings.setApplicationVersion:= lIni.ReadString('Application', 'Version', fAllSettings.setApplicationVersion);
    fAllSettings.setApplicationBuildDate:= lIni.ReadString('Application', 'Build date', fAllSettings.setApplicationBuildDate);

    fAllSettings.setSucces:= True;
  finally
    lIni.Free;
  end;
end;

function TSettings.Save: Boolean;
var
  lIni: TIniFile;
begin
  Result:= False;
  if fAllSettings.setSettingsFile = '' then Exit;

  lIni:= TIniFile.Create(fAllSettings.setSettingsFile);
  try
    // Application-info
    lIni.WriteString('Application', 'Name', fAllSettings.setApplicationName);
    lIni.WriteString('Application', 'Version', fAllSettings.setApplicationVersion);
    lIni.WriteString('Application', 'Build date', fAllSettings.setApplicationBuildDate);

    // Configure
    lIni.WriteBool('Configure', 'ActivateLogging', fAllSettings.setActivateLogging);
    lIni.WriteBool('Configure', 'AppendLogFile', fAllSettings.setAppendLogging);
    if fAllSettings.setLanguage <> '' then
      lIni.WriteString('Configure', 'Language', fAllSettings.setLanguage);

    lIni.WriteString('Configure', 'SQLfileLocation', fAllSettings.setSqlFileLocation);
    lIni.WriteBool('Configure', 'DbGridRowHighlight', fAllSettings.setDbGridRowHighlight);
    lIni.WriteBool('Configure', 'AskToOpenExportFile', fAllSettings.setAskToOpenExportFile);
    lIni.WriteBool('Configure', 'KeepLastOrganization', fAllSettings.setKeepLastOrganization);
    lIni.WriteBool('Configure', 'RapportMappingError', fAllSettings.setRapportMappingError);
    lIni.WriteBool('Configure', 'RapportFatalError', fAllSettings.setRapportFatalError);
    lIni.WriteBool('Configure', 'RapportFieldIsEmpty', fAllSettings.setRapportFieldIsEmpty);
    lIni.WriteBool('Configure', 'RapportFieldIsMissing', fAllSettings.setRapportFieldIsMissing);
    lIni.WriteBool('Configure', 'RapportOutOfRange', fAllSettings.setRapportOutOfRange);
    lIni.WriteString('Configure', 'GWSWVersion', fAllSettings.setGWSWVersion);
    lIni.WriteString('Configure', 'MappingFile', fAllSettings.setMappingFile);

    // Export — Put
    lIni.WriteBool('Export', 'IncludePutVorm', fAllSettings.setIncludePutVorm);
    lIni.WriteBool('Export', 'IncludePutLengte', fAllSettings.setIncludePutLengte);
    lIni.WriteBool('Export', 'IncludePutBreedte', fAllSettings.setIncludePutBreedte);
    lIni.WriteBool('Export', 'IncludePutHoogte', fAllSettings.setIncludePutHoogte);
    lIni.WriteBool('Export', 'IncludePutMateriaal', fAllSettings.setIncludePutMateriaal);
    lIni.WriteBool('Export', 'IncludePutFundering', fAllSettings.setIncludePutFundering);
    lIni.WriteBool('Export', 'IncludePutBegindatum', fAllSettings.setIncludePutBegindatum);
    lIni.WriteBool('Export', 'IncludePutEinddatum', fAllSettings.setIncludePutEinddatum);
    lIni.WriteBool('Export', 'IncludePutDiameter', fAllSettings.setIncludePutDiameter);
    lIni.WriteBool('Export', 'IncludePutMaaiveldhoogte', fAllSettings.setIncludePutMaaiveldhoogte);

    // Export — Leiding
    lIni.WriteBool('Export', 'IncludeLeidingLengte', fAllSettings.setIncludeLeidingLengte);
    lIni.WriteBool('Export', 'IncludeLeidingBreedte', fAllSettings.setIncludeLeidingBreedte);
    lIni.WriteBool('Export', 'IncludeLeidingHoogte', fAllSettings.setIncludeLeidingHoogte);
    lIni.WriteBool('Export', 'IncludeLeidingDiameter', fAllSettings.setIncludeLeidingDiameter);
    lIni.WriteBool('Export', 'IncludeLeidingVorm', fAllSettings.setIncludeLeidingVorm);
    lIni.WriteBool('Export', 'IncludeLeidingMateriaal', fAllSettings.setIncludeLeidingMateriaal);
    lIni.WriteBool('Export', 'IncludeLeidingFundering', fAllSettings.setIncludeLeidingFundering);
    lIni.WriteBool('Export', 'IncludeLeidingStatusFunctioneren', fAllSettings.setIncludeLeidingStatusFunctioneren);
    lIni.WriteBool('Export', 'IncludeLeidingWIBONThema', fAllSettings.setIncludeLeidingWIBONThema);
    lIni.WriteBool('Export', 'IncludeLeidingBegindatum', fAllSettings.setIncludeLeidingBegindatum);
    lIni.WriteBool('Export', 'IncludeLeidingEinddatum', fAllSettings.setIncludeLeidingEinddatum);
    lIni.WriteBool('Export', 'IncludeLeidingBobBegin', fAllSettings.setIncludeLeidingBobBegin);
    lIni.WriteBool('Export', 'IncludeLeidingBobEind', fAllSettings.setIncludeLeidingBobEind);
    lIni.WriteBool('Export', 'IncludeLeidingVerbindingstype', fAllSettings.setIncludeLeidingVerbindingstype);

    // Export — Persleiding
    lIni.WriteBool('Export', 'IncludePersleidingLengte', fAllSettings.setIncludePersleidingLengte);
    lIni.WriteBool('Export', 'IncludePersleidingHoogte', fAllSettings.setIncludePersleidingHoogte);
    lIni.WriteBool('Export', 'IncludePersleidingDiameter', fAllSettings.setIncludePersleidingDiameter);
    lIni.WriteBool('Export', 'IncludePersleidingVorm', fAllSettings.setIncludePersleidingVorm);
    lIni.WriteBool('Export', 'IncludePersleidingMateriaal', fAllSettings.setIncludePersleidingMateriaal);
    lIni.WriteBool('Export', 'IncludePersleidingStatusFunctioneren', fAllSettings.setIncludePersleidingStatusFunctioneren);
    lIni.WriteBool('Export', 'IncludePersleidingBegindatum', fAllSettings.setIncludePersleidingBegindatum);
    lIni.WriteBool('Export', 'IncludePersleidingEinddatum', fAllSettings.setIncludePersleidingEinddatum);
    lIni.WriteBool('Export', 'IncludePersleidingBobBegin', fAllSettings.setIncludePersleidingBobBegin);
    lIni.WriteBool('Export', 'IncludePersleidingBobEind', fAllSettings.setIncludePersleidingBobEind);

    // Export — Kolk
    lIni.WriteBool('Export', 'IncludeKolkLengte', fAllSettings.setIncludeKolkLengte);
    lIni.WriteBool('Export', 'IncludeKolkBreedte', fAllSettings.setIncludeKolkBreedte);
    lIni.WriteBool('Export', 'IncludeKolkHoogte', fAllSettings.setIncludeKolkHoogte);
    lIni.WriteBool('Export', 'IncludeKolkDiameter', fAllSettings.setIncludeKolkDiameter);
    lIni.WriteBool('Export', 'IncludeKolkVorm', fAllSettings.setIncludeKolkVorm);
    lIni.WriteBool('Export', 'IncludeKolkMateriaal', fAllSettings.setIncludeKolkMateriaal);
    lIni.WriteBool('Export', 'IncludeKolkWanddikte', fAllSettings.setIncludeKolkWanddikte);
    lIni.WriteBool('Export', 'IncludeKolkBegindatum', fAllSettings.setIncludeKolkBegindatum);
    lIni.WriteBool('Export', 'IncludeKolkEinddatum', fAllSettings.setIncludeKolkEinddatum);

    lIni.UpdateFile;
    fAllSettings.setSucces:= True;
    Result:= True;
  finally
    lIni.Free;
  end;
end;

procedure TSettings.WriteSetting(const SettingName, Section, AValue: String);
begin
  fAllSettings.setSucces:= False;
  if not FileExists(fAllSettings.setSettingsFile) then Exit;

  with TIniFile.Create(fAllSettings.setSettingsFile) do
  try
    if (AValue <> 'True') and (AValue <> 'False') and (AValue <> '-1') and (AValue <> '0') then
      WriteString(Section, SettingName, AValue)
    else if (AValue = 'True') or (AValue = '-1') then
      WriteString(Section, SettingName, '1')
    else if (AValue = 'False') or (AValue = '0') then
      WriteString(Section, SettingName, '0');

    UpdateFile;
    fAllSettings.setSucces:= True;
  finally
    Free;
  end;
end;

procedure TSettings.StoreFormState;
begin
  fAllSettings.setSucces:= False;
  if not FileExists(fAllSettings.setSettingsFile) then Exit;

  with TIniFile.Create(fAllSettings.setSettingsFile) do
  try
    WriteInteger('Position', fAllSettings.setFrmName + '_Windowstate', fAllSettings.setFrmWindowState);
    WriteInteger('Position', fAllSettings.setFrmName + '_Top', fAllSettings.setFrmTop);
    WriteInteger('Position', fAllSettings.setFrmName + '_Left', fAllSettings.setFrmLeft);
    WriteInteger('Position', fAllSettings.setFrmName + '_Height', fAllSettings.setFrmHeight);
    WriteInteger('Position', fAllSettings.setFrmName + '_Width', fAllSettings.setFrmWidth);
    WriteInteger('Position', fAllSettings.setFrmName + '_RestoredTop', fAllSettings.setFrmRestoredTop);
    WriteInteger('Position', fAllSettings.setFrmName + '_RestoredLeft', fAllSettings.setFrmRestoredLeft);
    WriteInteger('Position', fAllSettings.setFrmName + '_RestoredHeight', fAllSettings.setFrmRestoredHeight);
    WriteInteger('Position', fAllSettings.setFrmName + '_RestoredWidth', fAllSettings.setFrmRestoredWidth);
    UpdateFile;
    fAllSettings.setSucces:= True;
  finally
    Free;
  end;
end;

procedure TSettings.ReadFormState;
begin
  if not FileExists(fAllSettings.setSettingsFile) then begin  // set deafult values
    if fAllSettings.setFrmName = 'view.main' then begin
      fAllSettings.setFrmWindowState:= 0;
      fAllSettings.setFrmTop:= 10;
      fAllSettings.setFrmLeft:= 10;
      fAllSettings.setFrmHeight:= 800;
      fAllSettings.setFrmWidth:= 1400;
    end
    else if fAllSettings.setFrmName = 'view.configure' then begin
      fAllSettings.setFrmWindowState:= 0;
      fAllSettings.setFrmTop:= 10;
      fAllSettings.setFrmLeft:= 10;
      fAllSettings.setFrmHeight:= 630;
      fAllSettings.setFrmWidth:= 1090;
    end;

    Exit;
  end;

  with TIniFile.Create(fAllSettings.setSettingsFile) do
  try
    fAllSettings.setFrmWindowState:= ReadInteger('Position', fAllSettings.setFrmName + '_Windowstate', 0);
    fAllSettings.setFrmTop:= ReadInteger('Position', fAllSettings.setFrmName + '_Top', 10);
    fAllSettings.setFrmLeft:= ReadInteger('Position', fAllSettings.setFrmName + '_Left', 10);
    fAllSettings.setFrmHeight:= ReadInteger('Position', fAllSettings.setFrmName + '_Height', 500);
    fAllSettings.setFrmWidth:= ReadInteger('Position', fAllSettings.setFrmName + '_Width', 1400);
    fAllSettings.setFrmRestoredTop:= ReadInteger('Position', fAllSettings.setFrmName + '_RestoredTop', 10);
    fAllSettings.setFrmRestoredLeft:= ReadInteger('Position', fAllSettings.setFrmName + '_RestoredLeft', 10);
    fAllSettings.setFrmRestoredHeight:= ReadInteger('Position', fAllSettings.setFrmName + '_RestoredHeight', 800);
    fAllSettings.setFrmRestoredWidth:= ReadInteger('Position', fAllSettings.setFrmName + '_RestoredWidth', 1400);
  finally
    Free;
  end;
end;

end.
