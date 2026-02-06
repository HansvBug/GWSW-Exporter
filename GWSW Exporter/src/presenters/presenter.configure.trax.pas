{ Copyright ©2025-2026 Hans van Buggenum }
unit presenter.configure.trax;
{$mode objfpc}{$H+}
{.$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, {istrlist,} model.intf, model.base{,
  exp.dirinfo}; /// <experimental> ///

(* this unit is by design, for specialized transactions... can have siblings :o)    *)
(* the 'transaction-manager now uses a transaction-factory, which means: to use it  *)
(* correctly, you have to 'register' your transaction class in correlation with the *)
(* 'modreason' at unit startup, i.e.: 'initialization' part  --  see at the bottom. *)

{ -----------------------------------------------------------------------------}

type

  { TSettingsConfigTrx }
  TSettingsConfigTrx = class(TTransaction, ITrxExec)
    private
      fActivateLogging: Boolean;
      fAppendLogging: Boolean;
      fAskToOpenExportFile : Boolean;
      fDbGridRowHighlight : Boolean;
      fKeepLastOrganization: Boolean;
      fLanguage: String;

      fAppName: String;
      fAppVersion: String;
      fAppBuildDate: String;
      fFormHeight: Integer;
      fFormLeft: Integer;
      fFormName: String;
      fFormTop,
      fFormWidth,
      fFormWindowstate,
      fFrmRestoredHeight,
      fFrmRestoredLeft,
      fFrmRestoredTop,
      fFrmRestoredWidth: Integer;
      fMappingFile: String;
      fRapportFatalError: Boolean;
      fRapportFieldIsEmpty: Boolean;
      fRapportFieldIsMissing: Boolean;
      fRapportMappingError: Boolean;
      fRapportOutOfRange: Boolean;
      fRead: Boolean;
      fReadFrmState: Boolean;
      fSettingsFile: String;
      fSqlFileLocation : String;
      fSuccess: boolean;
      fWrite: Boolean;
      fStoreFrmState: Boolean;
    public
      function Execute(aMgr : ITransactionManager) : Boolean;
      property Success: boolean read fSuccess write fSuccess;
      property SettingsLocationAndFileName: String read fSettingsFile write fSettingsFile;
      property ReadSettings: Boolean read fRead write fRead;     // If true then read the settingsfile.
      property WriteSettings: Boolean read fWrite write fWrite;  // If true then write the settingsfile.
      property ReadFormState: Boolean read fReadFrmState write fReadFrmState;
      property StoreFormState: Boolean read fStoreFrmState write fStoreFrmState;

      property AppName: String read fAppName write fAppName;
      property AppVersion: String read fAppVersion write fAppVersion;
      property AppBuildDate: String read fAppBuildDate write fAppBuildDate;

      property ActivateLogging: Boolean read fActivateLogging write fActivateLogging;  // Enable or disbable Logging.
      property AppendLogging: Boolean read fAppendLogging write fAppendLogging;        // Append new logging to the log file. Or when disabled, delete previous logging and start again.
      property Language: String read fLanguage write fLanguage;                        // The choosen Language.

      property FormName: String read fFormName write fFormName;                        // Determines which settings are saved.
      property FormWindowstate: Integer read fFormWindowstate write fFormWindowstate;
      property FormTop: Integer read fFormTop write fFormTop;
      property FormLeft: Integer read fFormLeft write fFormLeft;
      property FormHeight: Integer read fFormHeight write fFormHeight;
      property FormWidth: Integer read fFormWidth write fFormWidth;
      property FormRestoredTop: Integer read fFrmRestoredTop write fFrmRestoredTop;
      property FormRestoredLeft: Integer read fFrmRestoredLeft write fFrmRestoredLeft;
      property FormRestoredHeight: Integer read fFrmRestoredHeight write fFrmRestoredHeight;
      property FormRestoredWidth: Integer read fFrmRestoredWidth write fFrmRestoredWidth;

      property SqlFileLocation: String read fSqlFileLocation write fSqlFileLocation;
      property DbGridRowHighlight: Boolean read fDbGridRowHighlight write fDbGridRowHighlight;
      property AskToOpenExportFile: Boolean read fAskToOpenExportFile write fAskToOpenExportFile;
      property KeepLastOrganization: Boolean read fKeepLastOrganization write fKeepLastOrganization;

      property RapportMappingError: Boolean read fRapportMappingError write fRapportMappingError;
      property RapportFatalError: Boolean read fRapportFatalError write fRapportFatalError;
      property RapportFieldIsEmpty: Boolean read fRapportFieldIsEmpty write fRapportFieldIsEmpty;
      property RapportFieldIsMissing: Boolean read fRapportFieldIsMissing write fRapportFieldIsMissing;
      property RapportOutOfRange: Boolean read fRapportOutOfRange write fRapportOutOfRange;
      property MappingFile: String read fMappingFile write fMappingFile;
    end;

  { TSingleSettingConfigTrx }

  TSingleSettingConfigTrx  = class(TTransaction, ITrxExec)
    private
      fSettingName: String;
      fSettingsFile: String;
      fSettingValue: String;
    public
      function Execute(aMgr: ITransactionManager): boolean;
      property SettingsLocationAndFileName: String read fSettingsFile write fSettingsFile;
      property SettingName: String read fSettingName write fSettingName;
      property SettingValue: String read fSettingValue write fSettingValue;
  end;

implementation
// uses StrUtils; { for: 'IndexText' etc... }

{ utility functions, taking advantage of the publicly exported functions,
  in a unit that we can't /see/ from here. compiler imports for us :o) gotta love FPC }
function Pch2Str(aPch: pchar): string; external name 'BC_PCH2STR';
function Str2Pch(aStr: string): pchar; external name 'BC_STR2PCH';
{ the above 2 imports, are here to avoid referencing 'model.decl' in this unit.
  if you have to, then only in this 'implementation' part. }

{ TSimpletestTrx }


procedure InitTrax;
begin
//  RegisterTransactionClass(prSimpleTestConfigure,TSimpleTestTrx);  // Dit is de manier om met de fabriek te werken.!!!
//  RegisterTransactionClass(prFormStateConfigure, TSettingsConfigTrx);
  ////////// YOU FORGOT THIS ////////////
end;

{ TSettingsConfigTrx }
function TSettingsConfigTrx.Execute(aMgr : ITransactionManager) : Boolean;
var
  lRec: TSettingsRec;
begin
  Result:= aMgr.OwnerConfig.Model.DoesFileExists(SettingsLocationAndFileName);

  If Result then begin
    lRec.setSettingsFile:= SettingsLocationAndFileName;  // Verry important.

    if ReadSettings then begin
      lRec.setReadSettings:= ReadSettings;
      if ReadFormState then begin
        lRec.setFrmName:= FormName;
        lRec:= aMgr.OwnerConfig.Model.ReadFormState(@lRec);
        lRec.setReadSettings:= ReadSettings;
        lRec.setReadFormState:= ReadFormState;

        aMgr.OwnerConfig.Provider.NotifySubscribers(prFormStateConfig, Self, @lRec);
      end
      else begin  // read all other settings
        // pass the settings
        // first some defaults....
        lRec.setApplicationName:= AppName;
        lRec.setApplicationVersion:= AppVersion;
        lRec.setApplicationBuildDate:= AppBuildDate;
        lRec.setWriteSettings:= WriteSettings;
        lRec:= aMgr.OwnerConfig.Model.ReadSettings(@lRec);   // <---
        lRec.setFrmName:= FormName;

        aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSettingsConfig, Self, @lRec);
      end;
    end
    else if WriteSettings then begin
      lRec.setWriteSettings:= WriteSettings;

      if StoreFormstate then begin
        lRec.setFrmName:= FormName;
        lRec.setFrmWindowState:= FormWindowstate;
        lRec.setFrmTop:= FormTop;
        lRec.setFrmLeft:= FormLeft;
        lRec.setFrmHeight:= FormHeight;
        lRec.setFrmWidth:= FormWidth;
        lRec.setFrmRestoredTop:= FormRestoredTop;
        lRec.setFrmRestoredLeft:= FormRestoredLeft;
        lRec.setFrmRestoredHeight:= FormRestoredHeight;
        lRec.setFrmRestoredWidth:= FormRestoredWidth;
        lRec:= aMgr.OwnerConfig.Model.StoreFormState(@lRec);

        aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSettingsConfig, Self, @lRec);
      end
      else begin // Write the regular settings.
        lRec.setWriteSettings:= True;
        lRec.setFrmName:= FormName;
        lRec.setActivateLogging:= ActivateLogging;
        lRec.setAppendLogging:= AppendLogging;
        lRec.setLanguage:= Language;

        lRec.setApplicationName:= ApplicationName;
        lRec.setApplicationVersion:= Application_version;
        lRec.setApplicationBuildDate:= Application_build_date;
        lRec.setSqlFileLocation:= SqlFileLocation;
        lRec.setDbGridRowHighlight:= DbGridRowHighlight;
        lRec.setAskToOpenExportFile:= AskToOpenExportFile;
        lRec.setKeepLastOrganization:= KeepLastOrganization;

        lRec.setRapportMappingError:= RapportMappingError;
        lRec.setRapportFatalError:= RapportFatalError;
        lRec.setRapportFieldIsEmpty:= RapportFieldIsEmpty;
        lRec.setRapportFieldIsMissing:= RapportFieldIsMissing;
        lRec.setRapportOutOfRange:= RapportOutOfRange;
        lRec.setMappingFile:= MappingFile;

        // new settings...

        lRec:= aMgr.OwnerConfig.Model.WriteSettings(@lRec);
        aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSettingsConfig, Self, @lRec);
      end;
    end;
  end
  else begin
    // Write defaults.
    lRec.setSettingsFile:= SettingsLocationAndFileName;
    lRec.setApplicationName:= AppName;
    lRec.setApplicationVersion:= AppVersion;
    lRec.setApplicationBuildDate:= AppBuildDate;
    aMgr.OwnerConfig.Model.WriteSettings(@lRec);  // Create the settingsfile.

    lRec.setSucces:= False;
    lRec.setMessage:= 'SettingsFileNotExists' ;
    aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSettingsConfig, Self, @lRec);
  end;
end;

{ TSingleSettingConfigTrx }

function TSingleSettingConfigTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TSingleSettingRec;
begin
  Result:= aMgr.OwnerConfig.Model.DoesFileExists(SettingsLocationAndFileName);

  if Result then begin
    lRec.ssSettingsFile:= SettingsLocationAndFileName;
    lRec.ssName:= SettingName;
    lRec.ssValue:= SettingValue;

    aMgr.OwnerMain.Model.WriteSingleSetting(@lRec);
  end
  else begin
    lRec.ssSucces:= False;
    lRec.ssMessage:= 'SettingsFileNotExists';
    aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSettingsConfig, Self, @lRec);
  end;
end;

initialization
  InitTrax; { sets up & registers transactions with TrxManager's Class-Factory }

finalization
  //FreeAndNil(__Example);

end.
