{ Copyright ©2025 Hans van Buggenum }
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
  Classes, SysUtils, IniFiles;

type
  { TSettings }
  { Manages application configuration and form state persistence. }
  TSettings = class(TObject)
  private
    { Configuration fields. }
    fAppendLogFile: Boolean;
    fLanguage: String;
    fSettingsFile: String;
    fAppName: String;
    fAppVersion: String;
    fAppBuildDate: String;
    fActivateLogging: Boolean;
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
    procedure WriteSetting(const SettingName, AValue: String);

    { Form state management. }
    procedure StoreFormState;
    procedure ReadFormState;

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

    { Operation status. }
    property Succes: Boolean read fSucces write fSucces;
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
      Succes := True;
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

procedure TSettings.ReadFile;
begin
  with TIniFile.Create(fSettingsFile) do
  try
    Succes := False;
    if FileExists(fSettingsFile) then begin
      // Read existing settings.
      ActivateLogging:= ReadBool('Configure', 'ActivateLogging', True);
      AppendLogFile:= ReadBool('Configure', 'AppendLogFile', True);
      Language:= ReadString('Configure', 'Language', 'en');
      // new setting...

      Succes:= True;
    end
    else begin
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
    UpdateFile;
    Succes:= True;
  finally
    Free;
  end;
end;

procedure TSettings.WriteSetting(const SettingName, AValue: String);
begin
  Succes:= False;
  if FileExists(fSettingsFile) then
  begin
    with TIniFile.Create(fSettingsFile) do
    try
      if (AValue <> 'True') and (AValue <> 'False') then begin
        WriteString('Configure', SettingName, AValue);
      end
      else if  AValue = 'True' then begin
        WriteString('Configure', SettingName, '1');
      end
      else if AValue = 'False' then begin
        WriteString('Configure', SettingName, '0');
      end;

      UpdateFile;
      Succes:= True;
    finally
      Free;
    end;
  end;
end;

end.
