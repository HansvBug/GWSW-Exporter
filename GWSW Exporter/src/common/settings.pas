{ Copyright ©2025-2026 Hans van Buggenum }
{
  Settings.pas - Application Configuration Manager

  Purpose:
  This unit provides centralized management of application settings, including:
  - Application metadata (name, version, build date)
  - User preferences (language, logging settings)
  - Form position and state persistence
  - INI file-based configuration storage

  Key Features:
  - Read/write application settings from/to INI file
  - Store and restore form positions and states
  - Manage application metadata
  - Thread-safe configuration operations
  - Default value handling for missing settings

  Usage:
  1. Create TSettings instance
  2. Set SettingsFile property
  3. Call ReadFile to load settings
  4. Access properties as needed
  5. Call WriteSettings to persist changes
}

unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, ExportConfig;

type
  { TSettings }
  { Manages application configuration and form state persistence. }
  TSettings = class(TObject)
  private
    fExportConfig: TExportConfig;

    { Configuration fields. }
    fAppendLogFile: Boolean;
    fAskToOpenExportFile : Boolean;
    fDbGridRowHighlight : Boolean;
    fGWSWVersion: String;
    fIncludeKolkBegindatum: Boolean;
    fIncludeKolkBreedte: Boolean;
    fIncludeKolkDiameter: Boolean;
    fIncludeKolkEinddatum: Boolean;
    fIncludeKolkHoogte: Boolean;
    fIncludeKolkLengte: Boolean;
    fIncludeKolkMateriaal: Boolean;
    fIncludeKolkVorm: Boolean;
    fIncludeKolkWanddikte: Boolean;
    fIncludeLeidingBegindatum: Boolean;
    fIncludeLeidingBobBegin: Boolean;
    fIncludeLeidingBobEind: Boolean;
    fIncludeLeidingBreedte: Boolean;
    fIncludeLeidingDiameter: Boolean;
    fIncludeLeidingEinddatum: Boolean;
    fIncludeLeidingFundering: Boolean;
    fIncludeLeidingHoogte: Boolean;
    fIncludeLeidingLengte: Boolean;
    fIncludeLeidingMateriaal: Boolean;
    fIncludeLeidingStatusFunctioneren: Boolean;
    fIncludeLeidingVorm: Boolean;
    fIncludeLeidingWIBONThema: Boolean;
    fIncludePersleidingBegindatum: Boolean;
    fIncludePersleidingBobBegin: Boolean;
    fIncludePersleidingBobEind: Boolean;
    fIncludePersleidingDiameter: Boolean;
    fIncludePersleidingEinddatum: Boolean;
    fIncludePersleidingHoogte: Boolean;
    fIncludePersleidingLengte: Boolean;
    fIncludePersleidingMateriaal: Boolean;
    fIncludePersleidingStatusFunctioneren: Boolean;
    fIncludePersleidingVorm: Boolean;
    fIncludePutBegindatum: Boolean;
    fIncludePutBreedte: Boolean;
    fIncludePutDiameter: Boolean;
    fIncludePutEinddatum: Boolean;
    fIncludePutFundering: Boolean;
    fIncludePutHoogte: Boolean;
    fIncludePutLengte: Boolean;
    fIncludePutMaaiveldhoogte: Boolean;
    fIncludePutMateriaal: Boolean;
    fKeepLastOrganization: Boolean;
    fLanguage: String;
    fLastUsedOrganization: String;
    fIncludePutVorm: Boolean;
    fMappingFile : String;
    fRapportFatalError: Boolean;
    fRapportFieldIsEmpty: Boolean;
    fRapportFieldIsMissing: Boolean;
    fRapportMappingError: Boolean;
    fRapportOutOfRange: Boolean;
    fSettingsFile: String;
    fAppName: String;
    fAppVersion: String;
    fAppBuildDate: String;
    fActivateLogging: Boolean;
    fSplitterDataSettings,
    fSplitterdataGrid,
    fSplitterMemos: Integer;
    fSqlFileLocation: String;
    fSucces: Boolean;

    { Form state fields. }
    fFrmName: String;
    fFrmWindowstate: Integer;
    fFrmTop: Integer;
    fFrmLeft: Integer;
    fFrmHeight: Integer;
    fFrmWidth: Integer;
    fFrmRestoredTop: Integer;
    fFrmRestoredLeft: Integer;
    fFrmRestoredHeight: Integer;
    fFrmRestoredWidth: Integer;

    { Property access methods. }
    function GetExportConfig: TExportConfig;
    function get_AppBuildDate: String;
    function get_AppName: String;
    function get_AppVersion: String;
    procedure set_AppBuildDate(AValue: String);
    procedure set_AppName(AValue: String);
    procedure set_AppVersion(AValue: String);
  public
    { Lifecycle management. }
    constructor Create; overload;
    destructor Destroy; override;

    { Configuration operations. }
    procedure ReadFile;
    procedure WriteSettings;
    procedure WriteSetting(const SettingName, Section, AValue: String);

    { Form state management. }
    procedure StoreFormState;
    procedure ReadFormState;

    procedure LoadExportConfigFromIni;

    { Application properties. }
    property SettingsFile: String read fSettingsFile write fSettingsFile;
    property AppName: String read get_AppName write set_AppName;
    property AppVersion: String read get_AppVersion write set_AppVersion;
    property AppBuildDate: String read get_AppBuildDate write set_AppBuildDate;
    { Form state properties. }
    property FormName: String read fFrmName write FFrmName;
    property FormWindowstate: Integer read fFrmWindowstate write fFrmWindowstate;
    property FormTop: Integer read fFrmTop write fFrmTop;
    property FormLeft: Integer read fFrmLeft write fFrmLeft;
    property FormHeight: Integer read fFrmHeight write fFrmHeight;
    property FormWidth: Integer read fFrmWidth write fFrmWidth;
    property FormRestoredTop: Integer read fFrmRestoredTop write fFrmRestoredTop;
    property FormRestoredLeft: Integer read fFrmRestoredLeft write fFrmRestoredLeft;
    property FormRestoredHeight: Integer read fFrmRestoredHeight write fFrmRestoredHeight;
    property FormRestoredWidth: Integer read fFrmRestoredWidth write fFrmRestoredWidth;
    { User preferences. }
    property Language: String read fLanguage write fLanguage;
    property ActivateLogging: Boolean read fActivateLogging write fActivateLogging;
    property AppendLogFile: Boolean read fAppendLogFile write fAppendLogFile;
    property MappingFile: String read fMappingFile write fMappingFile;
    property SqlFileLocation: String read fSqlFileLocation write fSqlFileLocation;
    property SplitterDataSettings: Integer read fSplitterDataSettings write fSplitterDataSettings;
    property SplitterDataGrid: Integer read fSplitterDataGrid write fSplitterDataGrid;
    property SplitterMemos: Integer read fSplitterMemos write fSplitterMemos;
    property DbGridRowHighlight: Boolean read fDbGridRowHighlight write fDbGridRowHighlight;
    property AskToOpenExportFile: Boolean read fAskToOpenExportFile write fAskToOpenExportFile;
    property KeepLastOrganization: Boolean read fKeepLastOrganization write fKeepLastOrganization;
    property LastUsedOrganization: String read fLastUsedOrganization write fLastUsedOrganization;

    property RapportMappingError: Boolean read fRapportMappingError write fRapportMappingError;
    property RapportFatalError: Boolean read fRapportFatalError write fRapportFatalError;
    property RapportFieldIsEmpty: Boolean read fRapportFieldIsEmpty write fRapportFieldIsEmpty;
    property RapportFieldIsMissing: Boolean read fRapportFieldIsMissing write fRapportFieldIsMissing;
    property RapportOutOfRange: Boolean read fRapportOutOfRange write fRapportOutOfRange;
    property GWSWVersion: String read fGWSWVersion write fGWSWVersion;
    // Export settings
    // Put
    Property IncludePutLengte: Boolean read fIncludePutLengte write fIncludePutLengte;
    Property IncludePutBreedte: Boolean read fIncludePutBreedte write fIncludePutBreedte;
    Property IncludePutHoogte: Boolean read fIncludePutHoogte write fIncludePutHoogte;
    Property IncludePutDiameter: Boolean read fIncludePutDiameter write fIncludePutDiameter;
    Property IncludePutVorm: Boolean read fIncludePutVorm write fIncludePutVorm;
    Property IncludePutMateriaal: Boolean read fIncludePutMateriaal write fIncludePutMateriaal;
    Property IncludePutFundering: Boolean read fIncludePutFundering write fIncludePutFundering;
    Property IncludePutBegindatum: Boolean read fIncludePutBegindatum write fIncludePutBegindatum;
    Property IncludePutEinddatum: Boolean read fIncludePutEinddatum write fIncludePutEinddatum;
    property IncludePutMaaiveldhoogte: Boolean read fIncludePutMaaiveldhoogte write fIncludePutMaaiveldhoogte;
    // Leiding
    property IncludeLeidingLengte: Boolean read fIncludeLeidingLengte write fIncludeLeidingLengte;
    property IncludeLeidingBreedte: Boolean read fIncludeLeidingBreedte write fIncludeLeidingBreedte;
    property IncludeLeidingHoogte: Boolean read fIncludeLeidingHoogte write fIncludeLeidingHoogte;
    property IncludeLeidingDiameter: Boolean read fIncludeLeidingDiameter write fIncludeLeidingDiameter;
    property IncludeLeidingVorm: Boolean read fIncludeLeidingVorm write fIncludeLeidingVorm;
    property IncludeLeidingMateriaal: Boolean read fIncludeLeidingMateriaal write fIncludeLeidingMateriaal;
    property IncludeLeidingFundering: Boolean read fIncludeLeidingFundering write fIncludeLeidingFundering;
    property IncludeLeidingStatusFunctioneren: Boolean read fIncludeLeidingStatusFunctioneren write fIncludeLeidingStatusFunctioneren;
    property IncludeLeidingWIBONThema: Boolean read fIncludeLeidingWIBONThema write fIncludeLeidingWIBONThema;
    property IncludeLeidingBegindatum: Boolean read fIncludeLeidingBegindatum write fIncludeLeidingBegindatum;
    property IncludeLeidingEinddatum: Boolean read fIncludeLeidingEinddatum write fIncludeLeidingEinddatum;
    property IncludeLeidingBobBegin: Boolean read fIncludeLeidingBobBegin write fIncludeLeidingBobBegin;
    property IncludeLeidingBobEind: Boolean read fIncludeLeidingBobEind write fIncludeLeidingBobEind;
    // Persleding
    property IncludePersleidingLengte: Boolean read fIncludePersleidingLengte write fIncludePersleidingLengte;
    property IncludePersleidingHoogte: Boolean read fIncludePersleidingHoogte write fIncludePersleidingHoogte;
    property IncludePersleidingDiameter: Boolean read fIncludePersleidingDiameter write fIncludePersleidingDiameter;
    property IncludePersleidingVorm: Boolean read fIncludePersleidingVorm write fIncludePersleidingVorm;
    property IncludePersleidingMateriaal: Boolean read fIncludePersleidingMateriaal write fIncludePersleidingMateriaal;
    property IncludePersleidingStatusFunctioneren: Boolean read fIncludePersleidingStatusFunctioneren write fIncludePersleidingStatusFunctioneren;
    property IncludePersleidingBegindatum: Boolean read fIncludePersleidingBegindatum write fIncludePersleidingBegindatum;
    property IncludePersleidingEinddatum: Boolean read fIncludePersleidingEinddatum write fIncludePersleidingEinddatum;
    property IncludePersleidingBobBegin: Boolean read fIncludePersleidingBobBegin write fIncludePersleidingBobBegin;
    property IncludePersleidingBobEind: Boolean read fIncludePersleidingBobEind write fIncludePersleidingBobEind;
    // Kolk
    property IncludeKolkLengte: Boolean read fIncludeKolkLengte write fIncludeKolkLengte;
    property IncludeKolkBreedte: Boolean read fIncludeKolkBreedte write fIncludeKolkBreedte;
    property IncludeKolkHoogte: Boolean read fIncludeKolkHoogte write fIncludeKolkHoogte;
    property IncludeKolkDiameter: Boolean read fIncludeKolkDiameter write fIncludeKolkDiameter;
    property IncludeKolkVorm: Boolean read fIncludeKolkVorm write fIncludeKolkVorm;
    property IncludeKolkMateriaal: Boolean read fIncludeKolkMateriaal write fIncludeKolkMateriaal;
    property IncludeKolkWanddikte: Boolean read fIncludeKolkWanddikte write fIncludeKolkWanddikte;
    property IncludeKolkBegindatum: Boolean read fIncludeKolkBegindatum write fIncludeKolkBegindatum;
    property IncludeKolkEinddatum: Boolean read fIncludeKolkEinddatum write fIncludeKolkEinddatum;

    { Operation status. }
    property Succes: Boolean read fSucces write fSucces;
    // Export settings
    property ExportConfig: TExportConfig read GetExportConfig;
  end;

implementation

{ TSettings }
constructor TSettings.Create;
begin
  inherited Create;
  // Initialize default values.
  fActivateLogging:= True;
  fAppendLogFile:= True;
  fLanguage:= 'en';
  fSucces:= False;
end;

destructor TSettings.Destroy;
begin
  // Cleanup if needed.
  inherited Destroy;
end;

function TSettings.get_AppBuildDate: String;
begin
  Result:= fAppBuildDate;
end;

function TSettings.GetExportConfig: TExportConfig;
begin
  Result:= fExportConfig;
end;

procedure TSettings.set_AppBuildDate(AValue: String);
begin
  fAppBuildDate:= AValue;
end;

function TSettings.get_AppName: String;
begin
  Result:= fAppName;
end;

procedure TSettings.set_AppName(AValue: String);
begin
  fAppName:= AValue;
end;

function TSettings.get_AppVersion: String;
begin
  Result:= fAppVersion;
end;

procedure TSettings.set_AppVersion(AValue: String);
begin
  fAppVersion:= AValue;
end;

procedure TSettings.StoreFormState;
begin
  Succes:= False;
  if FileExists(fSettingsFile) then
  begin
    with TIniFile.Create(fSettingsFile) do
    try
      WriteInteger('Position', fFrmName + '_Windowstate', fFrmWindowstate);
      WriteInteger('Position', fFrmName + '_Top', fFrmTop);
      WriteInteger('Position', fFrmName + '_Left', fFrmLeft);
      WriteInteger('Position', fFrmName + '_Height', fFrmHeight);
      WriteInteger('Position', fFrmName + '_Width', fFrmWidth);
      WriteInteger('Position', fFrmName + '_RestoredTop', fFrmRestoredTop);
      WriteInteger('Position', fFrmName + '_RestoredLeft', fFrmRestoredLeft);
      WriteInteger('Position', fFrmName + '_RestoredHeight', fFrmRestoredHeight);
      WriteInteger('Position', fFrmName + '_RestoredWidth', fFrmRestoredWidth);
      UpdateFile;
      Succes:= True;
    finally
      Free;
    end;
  end;
end;

procedure TSettings.ReadFormState;
begin
  if FileExists(fSettingsFile) then
  begin
    with TIniFile.Create(fSettingsFile) do
    try
      FormWindowstate:= ReadInteger('Position', FormName + '_Windowstate', 0);
      FormTop:= ReadInteger('Position', FormName + '_Top', 10);
      FormLeft:= ReadInteger('Position', FormName + '_Left', 10);
      FormHeight:= ReadInteger('Position', FormName + '_Height', 400);
      FormWidth:= ReadInteger('Position', FormName + '_Width', 800);
      FormRestoredTop:= ReadInteger('Position', FormName + '_RestoredTop', 10);
      FormRestoredLeft:= ReadInteger('Position', FormName + '_RestoredLeft', 10);
      FormRestoredHeight:= ReadInteger('Position', FormName + '_RestoredHeight', 400);
      FormRestoredWidth:= ReadInteger('Position', FormName + '_RestoredWidth', 800);
    finally
      Free;
    end;
  end;
end;

procedure TSettings.LoadExportConfigFromIni;
var
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(FSettingsFile);
  try
    // Put attributen
    fExportConfig.IncludePutVorm:= Ini.ReadBool('Export', 'IncludePutVorm', True);
    fExportConfig.IncludePutMateriaal:= Ini.ReadBool('Export', 'IncludePutMateriaal', True);
    fExportConfig.IncludePutFundering:= Ini.ReadBool('Export', 'IncludePutFundering', True);
    fExportConfig.IncludePutDiameter:= Ini.ReadBool('Export', 'IncludePutDiameter', True);
    fExportConfig.IncludePutBreedte:= Ini.ReadBool('Export', 'IncludePutBreedte', True);
    fExportConfig.IncludePutLengte:= Ini.ReadBool('Export', 'IncludePutLengte', True);
    fExportConfig.IncludePutHoogte:= Ini.ReadBool('Export', 'IncludePutHoogte', True);
    fExportConfig.IncludePutMaaiveldhoogte:= Ini.ReadBool('Export', 'IncludePutMaaiveldhoogte', True);
    fExportConfig.IncludePutBegindatum:= Ini.ReadBool('Export', 'IncludePutBegindatum', True);
    fExportConfig.IncludePutEinddatum:= Ini.ReadBool('Export', 'IncludePutEinddatum', False);
    // Leiding attributen
    fExportConfig.IncludeLeidingLengte:= Ini.ReadBool('Export', 'IncludeLeidingLengte', True);
    fExportConfig.IncludeLeidingBreedte:= Ini.ReadBool('Export', 'IncludeLeidingBreedte', True);
    fExportConfig.IncludeLeidingHoogte:= Ini.ReadBool('Export', 'IncludeLeidingHoogte', True);
    fExportConfig.IncludeLeidingDiameter:= Ini.ReadBool('Export', 'IncludeLeidingDiameter', True);
    fExportConfig.IncludeLeidingVorm:= Ini.ReadBool('Export', 'IncludeLeidingVorm', True);
    fExportConfig.IncludeLeidingMateriaal:= Ini.ReadBool('Export', 'IncludeLeidingMateriaal', True);
    fExportConfig.IncludeLeidingFundering:= Ini.ReadBool('Export', 'IncludeLeidingFundering', True);
    fExportConfig.IncludeLeidingStatusFunctioneren:= Ini.ReadBool('Export', 'IncludeLeidingStatusFunctioneren', True);
    fExportConfig.IncludeLeidingWIBONThema:= Ini.ReadBool('Export', 'IncludeLeidingWIBONThema', True);
    fExportConfig.IncludeLeidingBegindatum:= Ini.ReadBool('Export', 'IncludeLeidingBegindatum', True);
    fExportConfig.IncludeLeidingEinddatum:= Ini.ReadBool('Export', 'IncludeLeidingEinddatum', False);
    fExportConfig.IncludeLeidingBobBegin:= Ini.ReadBool('Export', 'IncludeLeidingBobBegin', True);
    fExportConfig.IncludeLeidingBobEind:= Ini.ReadBool('Export', 'IncludeLeidingBobEind', False);
    // Persleiding attributen
    fExportConfig.IncludePersleidingLengte:= Ini.ReadBool('Export', 'IncludePersleidingLengte', True);
    fExportConfig.IncludePersleidingHoogte:= Ini.ReadBool('Export', 'IncludePersleidingHoogte', True);
    fExportConfig.IncludePersleidingDiameter:= Ini.ReadBool('Export', 'IncludePersleidingDiameter', True);
    fExportConfig.IncludePersleidingVorm:= Ini.ReadBool('Export', 'IncludePersleidingVorm', True);
    fExportConfig.IncludePersleidingMateriaal:= Ini.ReadBool('Export', 'IncludePersleidingMateriaal', True);
    fExportConfig.IncludePersleidingStatusFunctioneren:= Ini.ReadBool('Export', 'IncludePersleidingStatusFunctioneren', True);
    fExportConfig.IncludePersleidingBegindatum:= Ini.ReadBool('Export', 'IncludePersleidingBegindatum', True);
    fExportConfig.IncludePersleidingEinddatum:= Ini.ReadBool('Export', 'IncludePersleidingEinddatum', False);
    fExportConfig.IncludePersleidingBobBegin:= Ini.ReadBool('Export', 'IncludePersleidingBobBegin', False);
    fExportConfig.IncludePersleidingBobEind:= Ini.ReadBool('Export', 'IncludePersleidingBobEind', False);

    // Kolk attributen
    fExportConfig.IncludeKolkLengte:= Ini.ReadBool('Export', 'IncludeKolkLengte', True);
    fExportConfig.IncludeKolkBreedte:= Ini.ReadBool('Export', 'IncludeKolkBreedte', True);
    fExportConfig.IncludeKolkHoogte:= Ini.ReadBool('Export', 'IncludeKolkHoogte', True);
    fExportConfig.IncludeKolkDiameter:= Ini.ReadBool('Export', 'IncludeKolkDiameter', True);
    fExportConfig.IncludeKolkVorm:= Ini.ReadBool('Export', 'IncludeKolkVorm', True);
    fExportConfig.IncludeKolkMateriaal:= Ini.ReadBool('Export', 'IncludeKolkMateriaal', True);
    fExportConfig.IncludeKolkWanddikte:= Ini.ReadBool('Export', 'IncludeKolkWanddikte', False);
    fExportConfig.IncludeKolkBegindatum:= Ini.ReadBool('Export', 'IncludeKolkBegindatum', True);
    fExportConfig.IncludeKolkEinddatum:= Ini.ReadBool('Export', 'IncludeKolkEinddatum', False);


    // Object types  Beter dan alleen de query. { #todo : Aanzet. Nog inbouwen }
//    fExportConfig.IncludePutten:= Ini.ReadBool('Export', 'IncludePutten', True);
//    fExportConfig.IncludeLeidingen:= Ini.ReadBool('Export', 'IncludeLeidingen', True);
//    fExportConfig.IncludePersleidingen:= Ini.ReadBool('Export', 'IncludePersleidingen', True);
//    fExportConfig.IncludeKolken:= Ini.ReadBool('Export', 'IncludeKolken', True);
//    fExportConfig.IncludeStelsels:= Ini.ReadBool('Export', 'IncludeStelsels', True);
  finally
    Ini.Free;
  end;
end;

procedure TSettings.ReadFile;
begin
  with TIniFile.Create(fSettingsFile) do
  try
    Succes:= False;
    if FileExists(fSettingsFile) then begin
      // Read existing settings.
      ActivateLogging:= ReadBool('Configure', 'ActivateLogging', True);
      AppendLogFile:= ReadBool('Configure', 'AppendLogFile', True);
      Language:= ReadString('Configure', 'Language', 'en');
      MappingFile:= ReadString('Configure', 'MappingFile', '');
      GWSWVersion:= ReadString('Configure', 'GWSWVersion', 'GWSW_versie_16');  // Start with version: GWSW_versie_16
      SqlFileLocation:= ReadString('Configure', 'SQLfileLocation', '');
      SplitterDataSettings:= ReadInteger('Configure', 'SplitterDataSettings', 300);
      SplitterDataGrid:= ReadInteger('Configure', 'SplitterdataGrid', 400);
      SplitterMemos:= ReadInteger('Configure', 'SplitterMemos', 500);
      DbGridRowHighlight:= ReadBool('Configure', 'DbGridRowHighlight', True);
      AskToOpenExportFile:= ReadBool('Configure', 'AskToOpenExportFile', True);
      KeepLastOrganization:= ReadBool('Configure', 'KeepLastOrganization', True);
      LastUsedOrganization:= ReadString('Configure', 'LastUsedOrganization', '');

      RapportMappingError:= ReadBool('Configure', 'RapportMappingError', True);
      RapportFatalError:= ReadBool('Configure', 'RapportFatalError', True);
      RapportFieldIsEmpty:= ReadBool('Configure', 'RapportFieldIsEmpty', False);
      RapportFieldIsMissing:= ReadBool('Configure', 'RapportFieldIsMissing', True);
      RapportOutOfRange:= ReadBool('Configure', 'RapportOutOfRange', True);

      // Export settings
      // Put
      IncludePutLengte:= ReadBool('Export', 'IncludePutLengte', True);
      IncludePutBreedte:= ReadBool('Export', 'IncludePutBreedte', True);
      IncludePutHoogte:= ReadBool('Export', 'IncludePutHoogte', True);
      IncludePutDiameter:= ReadBool('Export', 'IncludePutDiameter', True);
      IncludePutVorm:= ReadBool('Export', 'IncludePutVorm', True);
      IncludePutMateriaal:= ReadBool('Export', 'IncludePutMateriaal', True);
      IncludePutFundering:= ReadBool('Export', 'IncludePutFundering', False);
      IncludePutBegindatum:= ReadBool('Export', 'IncludePutBegindatum', True);
      IncludePutEinddatum:= ReadBool('Export', 'IncludePutEinddatum', False);
      IncludePutMaaiveldhoogte:= ReadBool('Export', 'IncludePutMaaiveldhoogte', True);
      // Leiding
      IncludeLeidingLengte:= ReadBool('Export', 'IncludeLeidingLengte', True);
      IncludeLeidingBreedte:= ReadBool('Export', 'IncludeLeidingBreedte', True);
      IncludeLeidingHoogte:= ReadBool('Export', 'IncludeLeidingHoogte', True);
      IncludeLeidingDiameter:= ReadBool('Export', 'IncludeLeidingDiameter', True);
      IncludeLeidingVorm:= ReadBool('Export', 'IncludeLeidingVorm', True);
      IncludeLeidingMateriaal:= ReadBool('Export', 'IncludeLeidingMateriaal', True);
      IncludeLeidingFundering:= ReadBool('Export', 'IncludeLeidingFundering', True);
      IncludeLeidingStatusFunctioneren:= ReadBool('Export', 'IncludeLeidingStatusFunctioneren', True);
      IncludeLeidingWIBONThema:= ReadBool('Export', 'IncludeLeidingWIBONThema', False);
      IncludeLeidingBegindatum:= ReadBool('Export', 'IncludeLeidingBegindatum', True);
      IncludeLeidingEinddatum:= ReadBool('Export', 'IncludeLeidingEinddatum', False);
      IncludeLeidingBobBegin:= ReadBool('Export', 'IncludeLeidingBobBegin', True);
      IncludeLeidingBobEind:= ReadBool('Export', 'IncludeLeidingBobEind', True);
      // Persleiding
      IncludePersleidingLengte:= ReadBool('Export', 'IncludePersleidingLengte', True);
      IncludePersleidingHoogte:= ReadBool('Export', 'IncludePersleidingHoogte', True);
      IncludePersleidingDiameter:= ReadBool('Export', 'IncludePersleidingDiameter', True);
      IncludePersleidingVorm:= ReadBool('Export', 'IncludePersleidingVorm', True);
      IncludePersleidingMateriaal:= ReadBool('Export', 'IncludePersleidingMateriaal', True);
      IncludePersleidingStatusFunctioneren:= ReadBool('Export', 'IncludePersleidingStatusFunctioneren', True);
      IncludePersleidingBegindatum:= ReadBool('Export', 'IncludePersleidingBegindatum', True);
      IncludePersleidingEinddatum:= ReadBool('Export', 'IncludePersleidingEinddatum', False);
      IncludePersleidingBobBegin:= ReadBool('Export', 'IncludePersleidingBobBegin', False);
      IncludePersleidingBobEind:= ReadBool('Export', 'IncludePersleidingBobEind', False);
      // Kolk
      IncludeKolkLengte:= ReadBool('Export', 'IncludeKolkLengte', True);
      IncludeKolkBreedte:= ReadBool('Export', 'IncludeKolkBreedte', True);
      IncludeKolkHoogte:= ReadBool('Export', 'IncludeKolkHoogte', True);
      IncludeKolkDiameter:= ReadBool('Export', 'IncludeKolkDiameter', True);
      IncludeKolkVorm:= ReadBool('Export', 'IncludeKolkVorm', True);
      IncludeKolkMateriaal:= ReadBool('Export', 'IncludeKolkMateriaal', True);
      IncludeKolkWanddikte:= ReadBool('Export', 'IncludeKolkWanddikte', False);
      IncludeKolkBegindatum:= ReadBool('Export', 'IncludeKolkBegindatum', True);
      IncludeKolkEinddatum:= ReadBool('Export', 'IncludeKolkEinddatum', False);
      // new setting...

      Succes:= True;
    end
    else begin  // Never gets here. The settings file is created in the afterconstruct of view.main
      // Create new settings file with defaults.
      WriteString('Application', 'Name', AppName);
      WriteString('Application', 'Version', AppVersion);
      WriteString('Application', 'Build date', AppBuilddate);

      // Default configuration.
      WriteBool('Configure', 'ActivateLogging', True);
      WriteBool('Configure', 'AppendLogFile', True);
      WriteString('Configure', 'Language', 'en');

      Succes:= True;
    end;
  finally
    Free;
  end;
end;

procedure TSettings.WriteSettings;
begin
  Succes:= False;
  with TIniFile.Create(fSettingsFile) do
  try
    // Write application info.
    WriteString('Application', 'Name', AppName);
    WriteString('Application', 'Version', AppVersion);
    WriteString('Application', 'Build date', AppBuilddate);

    // Write configuration.
    WriteBool('Configure', 'ActivateLogging', ActivateLogging);
    WriteBool('Configure', 'AppendLogFile', AppendLogFile);
    if Language <> '' then
      WriteString('Configure', 'Language', Language);

    //... new settings
    WriteString('Configure', 'SQLfileLocation', SqlFileLocation);
    WriteBool('Configure', 'DbGridRowHighlight', DbGridRowHighlight);
    WriteBool('Configure', 'AskToOpenExportFile', AskToOpenExportFile);
    WriteBool('Configure', 'KeepLastOrganization', KeepLastOrganization);

    WriteBool('Configure', 'RapportMappingError', RapportMappingError);
    WriteBool('Configure', 'RapportFatalError', RapportFatalError);
    WriteBool('Configure', 'RapportFieldIsEmpty', RapportFieldIsEmpty);
    WriteBool('Configure', 'RapportFieldIsMissing', RapportFieldIsMissing);
    WriteBool('Configure', 'RapportOutOfRange', RapportOutOfRange);
    WriteString('Configure', 'GWSWVersion', 'GWSW_versie_16');
    WriteString('Configure', 'MappingFile', MappingFile);

    UpdateFile;
    Succes:= True;
  finally
    Free;
  end;
end;

procedure TSettings.WriteSetting(const SettingName, Section, AValue: String);
begin
  Succes:= False;
  if FileExists(fSettingsFile) then
  begin
    with TIniFile.Create(fSettingsFile) do
    try
      if (AValue <> 'True') and (AValue <> 'False') and (AValue <> '-1') and (AValue <> '0') then begin
        WriteString(Section, SettingName, AValue);
      end
      else if  (AValue = 'True') or (AValue = '-1')  then begin
        WriteString(Section, SettingName, '1');
      end
      else if (AValue = 'False') or (AValue = '0') then begin
        WriteString(Section, SettingName, '0');
      end;

      UpdateFile;
      Succes:= True;
    finally
      Free;
    end;
  end;
end;

end.
