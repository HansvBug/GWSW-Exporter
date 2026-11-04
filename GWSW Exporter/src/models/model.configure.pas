{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
{ The base of this unit is made with "MVP-Setup" from Benny Christensen a.k.a. cdbc
  https://gitlab.com/cdbc-public/releases/mvp-setup
}
unit model.configure;
{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{-$define dbg}
interface
uses classes, sysutils, istrlist, model.base, model.decl, model.intf,
  Settings, applogging;
//const

type
{$Region 'TModelConfigureH'}
  { TModelConfigure }
  TModelConfigure = class(TObject,IModelConfigure)
  private
    fSettings: TSettings;

  { remember to use 'RegisterSection' in units, you want to add to 'Sects' :o) }
  const Sects: TStringArray = ('[view.configure]', '[view.configure.StatusbarTexts]', '[view.configure.hints]', '[view.configure.logging]'); { very necessary, because we need it at idx 0 }

  var
    fInSection: boolean; { used while searching static text sections }
    fSecId: integer; { tmp id while searching }
    fSectMaxIdx: integer; { used while searching static text sections, limits to 1 section }
    function Obj: TObject;
  protected
    fPresenter: IPresenterConfigure;
    fRoot: shortstring;
    fSection: string; // for use in getting static texts
    fTarget: integer; // for targetting the right result in view
    fTextCache: IStringList;
    { used while searching static text sections, new implementation ;) }
    procedure DoEach(const aValue: string; const {%H-}anIdx: ptrint; {%H-}anObj: TObject; aData: pointer);
    function LeftWord(const aStr: string): string; // used while searching static text sections
  public
    constructor Create(aPresenter: IPresenterConfigure; const aRoot: shortstring = ''); overload;
    destructor Destroy; override;
    function GetStaticTexts(const aSection: string; out aTarget: integer): IStringList;
    procedure RefreshSections;
    procedure ReloadTextCache;
    procedure SwitchLanguage;
    function DoesFileExists(const settingsFile: String): Boolean;
    function ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
    function StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
    procedure WriteSingleSetting(SettingData: PSingleSettingRec);
    procedure WriteToLog(const aLogAction, LogText: String);
  end;

{$EndRegion 'TModelConfigureH'}

{ datastore factory, its intended use is in scenarios, where the 'viewmodel' / 'presenter'
  does NOT OWN the 'model' and thus doesn't free it on end of use...,
  in other words:
    "if you create your 'model' with this factory-function then DON'T FREE IT!" just := nil }
function gModelConfigure(aPresenter: IPresenterConfigure; const aRoot: shortstring): IModelConfigure;


implementation
uses obs_prosu,strutils;

var Singleton: TModelConfigure = nil;

{ ModelConfigure factory }
function gModelConfigure(aPresenter: IPresenterConfigure; const aRoot: shortstring): IModelConfigure;
begin
  if not Assigned(Singleton) then Singleton:= TModelConfigure.Create(aPresenter,aRoot);
  Result:= Singleton;
end;

{$Region 'TModelConfigure'}
{ TModelConfigure }
function TModelConfigure.Obj: TObject;
begin
  Result:= Self;
end;

procedure TModelConfigure.DoEach(const aValue: string; const anIdx: ptrint;
                               anObj: TObject; aData: pointer);
var
  ls: string;
  lid: integer;
begin
  if fSecId = -1 then exit;
  ls:= LeftWord(aValue);
  lid:= IndexText(ls,Sects);
  if lid = fSecId then begin
    IStringList(aData).Append(aValue); //<- new feature <aData> typecast :o)
    fInSection:= true;
  end else begin
    if fInSection then begin
      if ((lid >= 0) and (lid <= fSectMaxIdx)) then fInSection:= false; /// 250824 /bc
      if fInSection then IStringList(aData).Append(aValue); //<- new feature <aData>
    end;
  end;
end;

function TModelConfigure.LeftWord(const aStr: string): string;
var
  li: integer = 1;
begin { pick the left-most word in a string }
  Result:= aStr;
  if Result = '' then exit;
  while ((li <= Length(aStr)) and (not (Result[li] in [#9,#13,#10,' ']))) do inc(li);
  SetLength(Result,li-1);
end;

constructor TModelConfigure.Create(aPresenter: IPresenterConfigure; const aRoot: shortstring);
begin
  inherited Create;
  fPresenter:= aPresenter;
  fRoot:= aRoot;
  UpdateConfigureSections(Sects); { <- fetch our registered sections, v- i18n }
  fTextCache:= CreStrListFromFile(format(confTexts,[fRoot,Lang])); ///<- i18n
  { we need the model, this is a minor flaw if it fails, because then the }
  if fTextCache.Count = 0 then { user will see a view filled with 'dummy' ;) }
    fPresenter.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(!) ERROR: Could NOT retrieve static texts!'));
end; /// the above text can't be translated in the i18n'ed mvptexts, the count is 0! ///

destructor TModelConfigure.Destroy;
begin
  if assigned(FSettings) then FreeAndNil(FSettings);

  fTextCache:= nil; // com-object
  fPresenter:= nil;
  inherited Destroy;
end;

function TModelConfigure.GetStaticTexts(const aSection: string; out aTarget: integer): IStringList;
begin { we use the [] here, because it fits in with standard ini-file format, nifty huh?!? }
  if aSection = '' then exit(nil);
  fSection:= '['+aSection+']'; fSecId:= IndexText(fSection,sects); { iterator-search }
  fSectMaxIdx:= high(Sects); { iterator-search, sets up the section 'break-off' maxidx }

  if (fTextCache = nil) or (fTextCache.Count = 0) then begin
    fPresenter.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(!) ERROR: Could NOT retrieve static texts!'));
    exit(nil); /// the above text can't be translated in the i18n'ed mvptexts, the count is 0! ///
  end;

  Result:= CreateStrList; { create our resulting stringlist }
  fTextCache.ForEach(@DoEach,Result);{ iterate over the source-list items, sending 'Result' along }
  aTarget:= fSecId; { for the presenter to differentiate between views }
end;

procedure TModelConfigure.RefreshSections;
begin
  UpdateConfigureSections(Sects);
end;

procedure TModelConfigure.ReloadTextCache;
begin
  fTextCache.Clear;
  fTextCache.LoadFromFile(format(mvpTexts,[fRoot,Lang]));
end;

procedure TModelConfigure.SwitchLanguage;
begin
  fTextCache.Clear;
  fTextCache:= CreStrListFromFile(format(confTexts,['',Lang]));
end;

function TModelConfigure.DoesFileExists(const settingsFile: String): Boolean;
begin
  Result:= FileExists(settingsFile);
end;

function TModelConfigure.ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
begin
  if not Assigned(fSettings) then
    fSettings:= TSettings.Create;

  if Settingsdata <> nil then begin
    if PSettingsRec(Settingsdata)^.setSettingsFile <> '' then
      fSettings.SettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
    if PSettingsRec(Settingsdata)^.setFrmName <> '' then
      fSettings.FormName:= PSettingsRec(Settingsdata)^.setFrmName;
  end;

  fSettings.ReadFormState;
  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

function TModelConfigure.ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
begin
  if not Assigned(fSettings) then
    fSettings:= TSettings.Create;

  if Settingsdata <> nil then begin
    if PSettingsRec(Settingsdata)^.setSettingsFile <> '' then
      fSettings.SettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
    if PSettingsRec(Settingsdata)^.setApplicationName <> '' then
      fSettings.AppName:= PSettingsRec(Settingsdata)^.setApplicationName;
    if PSettingsRec(Settingsdata)^.setApplicationVersion <> '' then
      fSettings.AppVersion:= PSettingsRec(Settingsdata)^.setApplicationVersion;
    if PSettingsRec(Settingsdata)^.setApplicationBuildDate <> '' then
      fSettings.AppBuildDate:= PSettingsRec(Settingsdata)^.setApplicationBuildDate;
  end;

  fSettings.Load;
  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

function TModelConfigure.StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
var
  lMerged: TSettingsRec;
begin
  if not Assigned(fSettings) then begin
    Result.setSucces:= False;
    Result.setMessage:= 'SettingsNotInitialized';
    Exit;
  end;

  if Settingsdata <> nil then begin
    lMerged:= fSettings.AllSettings;
    with PSettingsRec(Settingsdata)^ do begin
      if setSettingsFile <> '' then lMerged.setSettingsFile:= setSettingsFile;
      if setFrmName <> '' then lMerged.setFrmName:= setFrmName;
      lMerged.setFrmWindowState:= setFrmWindowState;
      lMerged.setFrmTop:= setFrmTop;
      lMerged.setFrmLeft:= setFrmLeft;
      lMerged.setFrmHeight:= setFrmHeight;
      lMerged.setFrmWidth:= setFrmWidth;
      lMerged.setFrmRestoredTop:= setFrmRestoredTop;
      lMerged.setFrmRestoredLeft:= setFrmRestoredLeft;
      lMerged.setFrmRestoredHeight:= setFrmRestoredHeight;
      lMerged.setFrmRestoredWidth:= setFrmRestoredWidth;
    end;
    fSettings.AllSettings:= lMerged;
    fSettings.StoreFormState;
  end;

  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

function TModelConfigure.WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
var
  lMerged: TSettingsRec;
begin
  if not Assigned(fSettings) then
    fSettings:= TSettings.Create;

  if Settingsdata <> nil then begin
    lMerged:= fSettings.AllSettings;
    with PSettingsRec(Settingsdata)^ do begin
      if setSettingsFile <> '' then lMerged.setSettingsFile:= setSettingsFile;
      if setApplicationName <> '' then lMerged.setApplicationName:= setApplicationName;
      if setApplicationVersion <> '' then lMerged.setApplicationVersion:= setApplicationVersion;
      if setApplicationBuildDate <> '' then lMerged.setApplicationBuildDate:= setApplicationBuildDate;
      if setFrmName <> '' then lMerged.setFrmName:= setFrmName;
      if setLanguage <> '' then lMerged.setLanguage:= setLanguage;
      if setWriteSettings then begin
        lMerged.setActivateLogging:= setActivateLogging;
        lMerged.setAppendLogging:= setAppendLogging;
        lMerged.setSqlFileLocation:= setSqlFileLocation;
        lMerged.setDbGridRowHighlight:= setDbGridRowHighlight;
        lMerged.setAskToOpenExportFile:= setAskToOpenExportFile;
        lMerged.setKeepLastOrganization:= setKeepLastOrganization;
        lMerged.setRapportMappingError:= setRapportMappingError;
        lMerged.setRapportFatalError:= setRapportFatalError;
        lMerged.setRapportFieldIsEmpty:= setRapportFieldIsEmpty;
        lMerged.setRapportFieldIsMissing:= setRapportFieldIsMissing;
        lMerged.setRapportOutOfRange:= setRapportOutOfRange;
        lMerged.setMappingFile:= setMappingFile;
        lMerged.setGWSWVersion:= setGWSWVersion;
      end;
    end;
    fSettings.AllSettings:= lMerged;
  end;

  fSettings.Save;
  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

procedure TModelConfigure.WriteSingleSetting(SettingData: PSingleSettingRec);
begin
  if assigned(fSettings) then begin
    with PSingleSettingRec(Settingdata)^ do begin
      fSettings.SettingsFile:= ssSettingsFile;
      fSettings.WriteSetting(ssName, ssSection, ssValue);
    end;
  end;
end;

procedure TModelConfigure.WriteToLog(const aLogAction, LogText: String);
begin
  if Logging <> Nil then begin
    case aLogAction of
      'Information': Logging.WriteToLogInfo(LogText);
      'Warning': Logging.WriteToLogWarning(LogText);
      'Error': Logging.WriteToLogError(LogText);
      'Debug': Logging.WriteToLogDebug(LogText);
    else ;
    end;
  end;
end;


{$EndRegion 'TModelconfigure'}
initialization
  RegisterConfigureSection(TModelConfigure.ClassName);
finalization
  if Assigned(Singleton) then FreeAndNil(Singleton); { checks for nil explicitly, freeandnil doesn't! }
end.
