{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
{ The base of this unit is made with "MVP-Setup" from Benny Christensen a.k.a. cdbc
  https://gitlab.com/cdbc-public/releases/mvp-setup
}
unit model.main;
{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{-$define dbg}
interface
uses classes, sysutils, istrlist, model.decl, model.intf,
  LazFileUtils,
  Settings, AppLogging, DatabaseModule, uIGWSWDataProvider,
  exportProgressReporter.intf, DbGridHelper;
//const

type
{$Region 'TModelMainH'}
  { TModelMain }
  TModelMain = class(TObject,IModelMain, IExportProgressReporter)
  private { remember to use 'RegisterSection' in units, you want to add to 'Sects' :o) }
    fSettings: TSettings;
    fDatabaseModule: TDatabaseModule;

    fDbGridHelper: TDbGridHelper;

    function CreateORADataProvider(aQuery: TObject; DisableControles: Boolean): IGWSWDataProvider;
    function CreateCSVDataProvider(const FileName: string; Delimiter: Char = ',';
                                  QuoteChar: Char = '"'; HasHeader: Boolean = True): IGWSWDataProvider;

    // (progress) reporting
    procedure ReportProgressMsg(const Message: string);
    procedure ReportError(const ErrMsg: string; const ErrorType: Integer = 0;const Guid: string = '');
    procedure ReportProgressCount(const Current, Total: Integer);

  const Sects: TStringArray = ('[view.main]', '[view.main.statusbartexts]', '[view.main.hints]', '[view.main.logging]', '[view.about]'); { very necessary, because we need it at idx 0 }
  var                                              
    fInSection: boolean; { used while searching static text sections }
    fSecId: integer; { tmp id while searching }
    fSectMaxIdx: integer; { used while searching static text sections, limits to 1 section }
    function Obj: TObject;
  protected
    fPresenter: IPresenterMain;
    fRoot: shortstring;
    fSection: string; // for use in getting static texts
    fTarget: integer; // for targetting the right result in view
    fTextCache: IStringList;
    { used while searching static text sections, new implementation ;) }
    procedure DoEach(const aValue: string; const {%H-}anIdx: ptrint; {%H-}anObj: TObject; aData: pointer);
    function LeftWord(const aStr: string): string; // used while searching static text sections    
  public
    constructor Create(aPresenter: IPresenterMain;const aRoot: shortstring = ''); overload;
    destructor Destroy; override;
    function GetStaticTexts(const aSection: string; out aTarget: integer): IStringList;
    procedure RefreshSections;
    procedure ReloadTextCache;
    //...
    procedure SwitchLanguage;
    function IsDirWritable(const aRootDir: string): Boolean;
    function CreateDirs(const aRootDir, AppDir: string; const aDirList: string = ''): TNewDirectoriesRec;
    function SetStatusbarText(const aText: string; panel: Word): TStatusbarPanelText;
    function DoesFileExists(const FullFileName: String): Boolean;
    function ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
    function StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
    procedure WriteSingleSetting(SettingData: PSingleSettingRec);
    procedure StartLogging;
    procedure StopLogging;
    procedure WriteToLog(const LogType, LogText: String);
    function SetStatusbarPanelsWidth(stbWidth, lpWidth, rpWidth: Integer): TstbPanelsSize;
    function IsFileInUse(const FileName: String): Boolean;
    // databasebeheer
    function GetDatabaseModule: TDatabaseModule;
    function IsConnected: Boolean;
    procedure MakeDbConnection(DbConnectionData: PDbConnectRec);
    procedure DbDisconnect(DbConnectionData: PDbConnectRec);

    // Execute query and get data
    function RetrieveData(data: PRetrieveDataRec): TRetrieveDataRec;
    function ExportToOroxTtlFile(Data: PExportToOroxTtlFileRec): TExportToOroxTtlFileRec;
    function GetSQLfileLocation: String;
    function GetKeepLastOrganization: Boolean;
    function GetLastUsedOrganization: String;

    function GetSetting_AskToOpenExportFile: Boolean;
    procedure DisableChildControls(aData: TExportInProgressRec);
    function GetSingleSetting(const SettingName: String): String;
    procedure SortDbGrid(Data: PSortDbGridRec);
    function LoadCSVData(aRec: PRetrieveCSVDataRec): TRetrieveCSVDataRec;
  end;

{$EndRegion 'TModelMainH'}

{ datastore factory, its intended use is in scenarios, where the 'viewmodel' / 'presenter'
  does NOT OWN the 'model' and thus doesn't free it on end of use...,
  in other words:
    "if you create your 'model' with this factory-function then DON'T FREE IT!" just := nil }
function gModelMain(aPresenter: IPresenterMain;const aRoot: shortstring): IModelMain;


implementation
uses obs_prosu, strutils, common.consts, common.utils, QueryDataProvider,
  CSVDataProvider, OroxExport;

var Singleton: TModelMain = nil;

{ ModelMain factory }
function gModelMain(aPresenter: IPresenterMain; const aRoot: shortstring): IModelMain;
begin
  if not Assigned(Singleton) then Singleton:= TModelMain.Create(aPresenter,aRoot);
  Result:= Singleton;
end;

{$Region 'TModelMain'}

function TModelMain.CreateORADataProvider(aQuery: TObject; DisableControles: Boolean): IGWSWDataProvider;
begin
  Result:= TQueryDataProvider.Create(aQuery, DisableControles);  // No free/nil required
end;

function TModelMain.CreateCSVDataProvider(const FileName: string;
  Delimiter: Char; QuoteChar: Char; HasHeader: Boolean): IGWSWDataProvider;
begin
  Result := TCSVDataProvider.Create(FileName, Delimiter, QuoteChar, HasHeader);
end;

procedure TModelMain.ReportProgressMsg(const Message: string);
begin
  fPresenter.Provider.NotifySubscribers(prReportProgress, Self, Str2Pch('ReportInfo' + '|' + Message));
end;

procedure TModelMain.ReportError(const ErrMsg: string; const ErrorType: Integer; const Guid: string);
var
  UpdateView: Boolean;
var
  lMsg: String;
  lErrorType: String;
begin
  UpdateView:= False;
  case ErrorType of
    0: begin
         lErrorType:= 'Information: --> ';  { #note : De string moet in het taal bestand komen }
         UpdateView:= True;  // Wordt nog niet gebruikt
    end;
    1: begin
         lErrorType:= 'Mapping: --> ';
         if fSettings.AllSettings.setRapportMappingError then UpdateView:= True;
    end;
    2: begin
         lErrorType:= 'Fatal: --> ';
         if fSettings.AllSettings.setRapportFatalError then UpdateView:= True;
    end;
    3: begin
         lErrorType:= 'Empty Field: --> ';
         if fSettings.AllSettings.setRapportFieldIsEmpty then UpdateView:= True;
    end;
    4: begin
         lErrorType:= 'Field is missing: --> ';
         if fSettings.AllSettings.setRapportFieldIsMissing then UpdateView:= True;
    end;
    5: begin
         lErrorType:= 'Value out of range: --> ';
         if fSettings.AllSettings.setRapportOutOfRange then UpdateView:= True;
    end;
  end;

  lMsg:= lErrorType + '|'+ ErrMsg + '|' + ' {'+ Guid + ').';

  if UpdateView then
    { todo : Let op moet aangepast worden!!!!!!!!!!!!!!!! }
    fPresenter.Provider.NotifySubscribers(prReportError, Self, Str2Pch('ReportError' + '|' + lMsg)); // "ReportError=ERROR: "     is niet meer juist
end;

procedure TModelMain.ReportProgressCount(const Current, Total: Integer);
begin
  fPresenter.Provider.NotifySubscribers(prReportProgressCount, Self, Str2Pch(Format('%d|%d', [Current, Total])));
end;

{ TModelMain }
function TModelMain.Obj: TObject;
begin
  Result:= Self;
end;

procedure TModelMain.DoEach(const aValue: string; const anIdx: ptrint; anObj: TObject; aData: pointer);
var ls: string; lid: integer;
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

function TModelMain.LeftWord(const aStr: string): string;
var li: integer = 1;
begin { pick the left-most word in a string }
  Result:= aStr;
  if Result = '' then exit;
  while ((li <= Length(aStr)) and (not (Result[li] in [#9,#13,#10,' ']))) do inc(li);
  SetLength(Result,li-1);
end; 

constructor TModelMain.Create(aPresenter: IPresenterMain; const aRoot: shortstring);
begin
  inherited Create;
  fPresenter:= aPresenter;
  fRoot:= aRoot;

  fDbGridHelper:= TDbGridHelper.Create;

  UpdateSections(Sects); { <- fetch our registered sections, v- i18n }
  fTextCache:= CreStrListFromFile(format(mvpTexts,[fRoot,Lang])); ///<- i18n
  { we need the model, this is a minor flaw if it fails, because then the }
  if fTextCache.Count = 0 then { user will see a view filled with 'dummy' ;) }
    fPresenter.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(!) ERROR: Could NOT retrieve static texts!'));
end; /// the above text can't be translated in the i18n'ed mvptexts, the count is 0! ///

destructor TModelMain.Destroy;
begin
  if Assigned(fDbGridHelper) then FreeAndNil(fDbGridHelper);
  if Assigned(fDatabaseModule) then FreeAndNil(fDatabaseModule);

  fTextCache:= nil; // com-object
  fPresenter:= nil;

  StopLogging;
  if assigned(fSettings) then FreeAndNil(fSettings);

  inherited Destroy;
end;

function TModelMain.GetStaticTexts(const aSection: string; out aTarget: integer): IStringList;
begin { we use the [] here, because it fits in with standard ini-file format, nifty huh?!? }
  if aSection = '' then exit(nil);
  fSection:= '['+aSection+']';
  fSecId:= IndexText(fSection,sects); { iterator-search }
  fSectMaxIdx:= high(Sects); { iterator-search, sets up the section 'break-off' maxidx }
  if fTextCache.Count = 0 then begin
    fPresenter.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(!) ERROR: Could NOT retrieve static texts!'));
    exit(nil); /// the above text can't be translated in the i18n'ed mvptexts, the count is 0! ///
  end;
  Result:= CreateStrList; { create our resulting stringlist }
  fTextCache.ForEach(@DoEach,Result);{ iterate over the source-list items, sending 'Result' along }
  aTarget:= fSecId; { for the presenter to differentiate between views }
end;

procedure TModelMain.RefreshSections;
begin
  UpdateSections(Sects);
end;

procedure TModelMain.ReloadTextCache;
begin
  fTextCache.Clear;
  fTextCache.LoadFromFile(format(mvpTexts,[fRoot,Lang]));
end;

procedure TModelMain.SwitchLanguage;
begin
  fTextCache.Clear;
  fTextCache:= CreStrListFromFile(format(mvpTexts,['',Lang]));
end;

function TModelMain.IsDirWritable(const aRootDir: string): Boolean;
begin
  Result:= False;
  if DirectoryIsWritable(aRootDir) then begin  // add to uses: LazFileUtils
    Result:= True;
  end;
end;

function TModelMain.CreateDirs(const aRootDir, AppDir: string; const aDirList: string): TNewDirectoriesRec;
var
  lDirList: TStringList;
  s: string;
  currentPath: string;
begin
  // Initialize result.
  Result.dirSucces:= True;
  Result.dirSuccesMsg:= amSucces;

  // Validate root directory.
  if aRootDir = '' then
    Exit;

  try
    // Check if root directory is writable.
    if not DirectoryIsWritable(aRootDir) then begin
      Result.dirSucces:= False;
      Result.dirSuccesMsg:= 'Root directory is not writable: ' + aRootDir;
      Exit;
    end;

    // Create full root path.
    currentPath:= IncludeTrailingPathDelimiter(aRootDir) + AppDir;
    currentPath:= IncludeTrailingPathDelimiter(currentPath);
    // Create root directory if needed.
    if not DirectoryExists(CurrentPath) then begin
      if not ForceDirectories(currentPath) then begin
        Result.dirSucces:= False;
        Result.dirSuccesMsg:= 'Failed to create root directory: ' + currentPath;
        Exit;
      end;
    end;
    // Process subdirectories if provided.
    if aDirList <> '' then begin
      lDirList:= TStringList.Create;
      try
        lDirList.Text:= aDirList;
        for s in lDirList do begin
          if s = '' then
            Continue;
          try
            if not ForceDirectories(currentPath + s) then begin
              Result.dirSucces:= False;
              Result.dirSuccesMsg:= 'Failed to create subdirectory: ' + s;
              // Continue trying other directories rather than exiting.
            end;
          except
            on E: Exception do begin
              Result.dirSucces:= False;
              Result.dirSuccesMsg:= 'Error creating subdirectory ' + s + ': ' + E.Message;
            end;
          end;
        end;
      finally
        lDirList.Free;
      end;
    end;
    // Update result with final path.
    Result.dirRoot:= currentPath;
  except
    on E: Exception do begin
      Result.dirSucces:= False;
      Result.dirSuccesMsg:= 'Unexpected error: ' + E.Message;
    end;
  end;
end;

function TModelMain.SetStatusbarText(const aText: string; panel: Word): TStatusbarPanelText;
begin
  with Result do begin
    stbPanelText:= ' ' + aText;  // Add one space.
    stbActivePanel:= panel;
  end;
end;

function TModelMain.DoesFileExists(const FullFileName: String): Boolean;
begin
  Result:= FileExists(FullFileName);
end;

function TModelMain.ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
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

function TModelMain.ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
begin
  if not Assigned(fSettings) then
    fSettings:= TSettings.Create;

  if Settingsdata <> nil then begin
    if PSettingsRec(Settingsdata)^.setSettingsFile <> '' then
      fSettings.SettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
    if PSettingsRec(Settingsdata)^.setFrmName <> '' then
      fSettings.FormName:= PSettingsRec(Settingsdata)^.setFrmName;
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

function TModelMain.StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
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
      lMerged.setSplitterMemos:= setSplitterMemos;
      lMerged.setSplitterDataSettings:= setSplitterDataSettings;
      lMerged.setSplitterdataGrid:= setSplitterdataGrid;
    end;
    fSettings.AllSettings:= lMerged;
    fSettings.StoreFormState;
  end;

  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

function TModelMain.WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
var
  lMerged: TSettingsRec;
begin
  if not Assigned(fSettings) then
    fSettings:= TSettings.Create;   // defaults zitten nu in de constructor

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
        // Algemene settings
        lMerged.setActivateLogging:= setActivateLogging;
        lMerged.setAppendLogging:= setAppendLogging;
        lMerged.setSqlFileLocation:= setSqlFileLocation;
        lMerged.setDbGridRowHighlight:= setDbGridRowHighlight;
        lMerged.setAskToOpenExportFile:= setAskToOpenExportFile;
        lMerged.setKeepLastOrganization:= setKeepLastOrganization;
        lMerged.setLastUsedOrganization:= setLastUsedOrganization;
        lMerged.setRapportMappingError:= setRapportMappingError;
        lMerged.setRapportFatalError:= setRapportFatalError;
        lMerged.setRapportFieldIsEmpty:= setRapportFieldIsEmpty;
        lMerged.setRapportFieldIsMissing:= setRapportFieldIsMissing;
        lMerged.setRapportOutOfRange:= setRapportOutOfRange;
        lMerged.setMappingFile:= setMappingFile;
        lMerged.setGWSWVersion:= setGWSWVersion;
        lMerged.setSplitterDataSettings:= setSplitterDataSettings;
        lMerged.setSplitterdataGrid:= setSplitterdataGrid;
        lMerged.setSplitterMemos:= setSplitterMemos;

        // Export — Put
        lMerged.setIncludePutLengte:= setIncludePutLengte;
        lMerged.setIncludePutBreedte:= setIncludePutBreedte;
        lMerged.setIncludePutHoogte:= setIncludePutHoogte;
        lMerged.setIncludePutDiameter:= setIncludePutDiameter;
        lMerged.setIncludePutVorm:= setIncludePutVorm;
        lMerged.setIncludePutMateriaal:= setIncludePutMateriaal;
        lMerged.setIncludePutFundering:= setIncludePutFundering;
        lMerged.setIncludePutBegindatum:= setIncludePutBegindatum;
        lMerged.setIncludePutEinddatum:= setIncludePutEinddatum;
        lMerged.setIncludePutMaaiveldhoogte:= setIncludePutMaaiveldhoogte;

        // Export — Leiding
        lMerged.setIncludeLeidingLengte:= setIncludeLeidingLengte;
        lMerged.setIncludeLeidingBreedte:= setIncludeLeidingBreedte;
        lMerged.setIncludeLeidingHoogte:= setIncludeLeidingHoogte;
        lMerged.setIncludeLeidingDiameter:= setIncludeLeidingDiameter;
        lMerged.setIncludeLeidingVorm:= setIncludeLeidingVorm;
        lMerged.setIncludeLeidingMateriaal:= setIncludeLeidingMateriaal;
        lMerged.setIncludeLeidingFundering:= setIncludeLeidingFundering;
        lMerged.setIncludeLeidingStatusFunctioneren:= setIncludeLeidingStatusFunctioneren;
        lMerged.setIncludeLeidingWIBONThema:= setIncludeLeidingWIBONThema;
        lMerged.setIncludeLeidingBegindatum:= setIncludeLeidingBegindatum;
        lMerged.setIncludeLeidingEinddatum:= setIncludeLeidingEinddatum;
        lMerged.setIncludeLeidingBobBegin:= setIncludeLeidingBobBegin;
        lMerged.setIncludeLeidingBobEind:= setIncludeLeidingBobEind;

        // Export — Persleiding
        lMerged.setIncludePersleidingLengte:= setIncludePersleidingLengte;
        lMerged.setIncludePersleidingHoogte:= setIncludePersleidingHoogte;
        lMerged.setIncludePersleidingDiameter:= setIncludePersleidingDiameter;
        lMerged.setIncludePersleidingVorm:= setIncludePersleidingVorm;
        lMerged.setIncludePersleidingMateriaal:= setIncludePersleidingMateriaal;
        lMerged.setIncludePersleidingStatusFunctioneren:= setIncludePersleidingStatusFunctioneren;
        lMerged.setIncludePersleidingBegindatum:= setIncludePersleidingBegindatum;
        lMerged.setIncludePersleidingEinddatum:= setIncludePersleidingEinddatum;
        lMerged.setIncludePersleidingBobBegin:= setIncludePersleidingBobBegin;
        lMerged.setIncludePersleidingBobEind:= setIncludePersleidingBobEind;

        // Export — Kolk
        lMerged.setIncludeKolkLengte:= setIncludeKolkLengte;
        lMerged.setIncludeKolkBreedte:= setIncludeKolkBreedte;
        lMerged.setIncludeKolkHoogte:= setIncludeKolkHoogte;
        lMerged.setIncludeKolkDiameter:= setIncludeKolkDiameter;
        lMerged.setIncludeKolkVorm:= setIncludeKolkVorm;
        lMerged.setIncludeKolkMateriaal:= setIncludeKolkMateriaal;
        lMerged.setIncludeKolkWanddikte:= setIncludeKolkWanddikte;
        lMerged.setIncludeKolkBegindatum:= setIncludeKolkBegindatum;
        lMerged.setIncludeKolkEinddatum:= setIncludeKolkEinddatum;
      end;

    end;
    fSettings.AllSettings:= lMerged;
  end;

  fSettings.Save;
  Result:= fSettings.AllSettings;
  Result.setSucces:= fSettings.Succes;
end;

procedure TModelMain.WriteSingleSetting(SettingData: PSingleSettingRec);
begin
  if Assigned(fSettings) then begin
    with PSingleSettingRec(SettingData)^ do begin
      fSettings.SettingsFile:= ssSettingsFile;
      fSettings.WriteSetting(ssName, ssSection, ssValue);
    end;
  end;
end;

procedure TModelMain.StartLogging;
var
  userName: String;
begin
  if assigned(fSettings) then begin
    if fSettings.AllSettings.setActivateLogging then begin
      if Logging <> Nil then begin
        { #todo : Retrieve the Location from the view. }

        userName:= StringReplace(GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';
        Logging.FileName:= userName + ApplicationName + '.log';

        {$IFDEF MSWINDOWS}
        Logging.LogFolder:= GetEnvironmentVariable('appdata') + PathDelim + ApplicationName + PathDelim + adLogging + PathDelim;
        {$ENDIF}
        {$IFDEF LINUX}
        Logging.LogFolder:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config' + PathDelim + ApplicationName + PathDelim + adLogging + PathDelim;
        {$ENDIF}

        Logging.ActivateLogging:= fSettings.AllSettings.setActivateLogging;
        Logging.AppendLogFile:= fSettings.AllSettings.setAppendLogging;
        Logging.ApplicationName:= ApplicationName;
        Logging.AppVersion:= Application_version;
        Logging.StartLogging;
      end;
    end;
  end;
end;

procedure TModelMain.StopLogging;
begin
  if assigned(Logging) then begin
    Logging.StopLogging;
  end;
end;

procedure TModelMain.WriteToLog(const LogType, LogText: String);
begin
  if Logging <> Nil then begin
    case LogType of
      'Information': Logging.WriteToLogInfo(LogText);
      'Warning': Logging.WriteToLogWarning(LogText);
      'Error': Logging.WriteToLogError(LogText);
      'Debug': Logging.WriteToLogDebug(LogText);
    else ;
    end;
  end;
end;

function TModelMain.SetStatusbarPanelsWidth(stbWidth, lpWidth, rpWidth: Integer): TstbPanelsSize;
var
  pWidth: Integer;
begin
  pWidth:= common.utils.SetStatusbarPnlWidth(stbWidth, lpWidth, rpWidth);
  with Result do begin
    mpWidth:= pWidth;
  end;
end;

function TModelMain.IsFileInUse(const FileName: String): Boolean;
begin
  Result:= common.utils.IsFileInUse(FileName);
end;

function TModelMain.GetDatabaseModule: TDatabaseModule;
begin
  Result:= fDatabaseModule;
end;

function TModelMain.IsConnected: Boolean;
begin
  Result:= False;
  if Assigned(fDatabaseModule) and (fDatabaseModule.IsConnected) then
    Result:= fDatabaseModule.IsConnected;
end;

procedure TModelMain.MakeDbConnection(DbConnectionData: PDbConnectRec);
var
  lResult: TConnectionResult;
begin
  if not Assigned(fDatabaseModule) then
    fDatabaseModule:= TDatabaseModule.Create();

  if not IsConnected then begin
    try
      lResult:= fDatabaseModule.MakeDbConnection(
            DbConnectionData^.DatabaseName,
            DbConnectionData^.UserName,
            DbConnectionData^.SchemaPassword
          );

      DbConnectionData^.HasConnection:= lResult.Success;
      DbConnectionData^.Message:= lResult.Message;

      if DbConnectionData^.HasConnection then
        fPresenter.WriteToLog('view.main', ltInformation, 'DbConnEstablished')
      else begin
        fPresenter.WriteToLog('view.main', ltError, 'DbConnFailed');
        WriteToLog(ltError, DbConnectionData^.Message);   // { #todo 1 : Dat kan niet meer. Moet via een getstatictext! }
      end;
    except
      on E: Exception do begin
        DbConnectionData^.HasConnection:= False;
        DbConnectionData^.Message:= 'Error: ' + E.Message;
        fPresenter.WriteToLog('view.main', ltInformation, 'DbConnFailed');
        WriteToLog(ltError, e.Message);
      end;
    end;
  end;
end;

procedure TModelMain.DbDisconnect(DbConnectionData: PDbConnectRec);
var
  lResult: TConnectionResult;
begin
  if Assigned(fDatabaseModule) then begin
    if IsConnected then begin
      lResult:= fDatabaseModule.DbDisconnect;
      DbConnectionData^.HasConnection:= lResult.Success;
    end;
    FreeAndNil(fDatabaseModule);
  end;
end;

function TModelMain.RetrieveData(data: PRetrieveDataRec): TRetrieveDataRec;    { #todo : Rename naar RetrieveOraData }
var
  lResult: TDataSetResult;
begin
  if Assigned(fDatabaseModule) then begin

    lResult:= fDatabaseModule.RetrieveData(data^.SqlText);

    Result.DataSource:= lResult.DataSource;
    Result.Success:= lResult.Success;
  end
  else
    Result.DataSource:= Nil;
end;

function TModelMain.ExportToOroxTtlFile(Data: PExportToOroxTtlFileRec): TExportToOroxTtlFileRec;
var
  DataProvider: IGWSWDataProvider;
  OroxExport: TOroxExport;
begin
  Result.Success:= False;
  Result.Message:= '';

  if IsFileInUse(Data^.FileNameCsvFile) then begin
    Result.Message:= 'CsvFileInUse';  // File is in use by another process.
    Exit;
  end;

  if data^.DataType = '' then Exit;

  try
    fSettings.Load;
    if data^.DataType = dtORA then begin
      DataProvider:= CreateORADataProvider(fDatabaseModule.CurrentQuery, True);  // Create a data provider. This is the query that retrieved the data.
    end
    else if data^.DataType = dtCSV then begin
      DataProvider:= CreateCSVDataProvider(Data^.FileNameCsvFile, ',', '"', True);  { #todo : Let nu nog hard in de code maar moet naar de configuratie }
    end;

    if DataProvider = nil then begin
      raise Exception.Create('Could not create data provider');
      Exit;
    end;

    OroxExport:= TOroxExport.Create(DataProvider, Data^.FileNameExportFile,
          Data^.OrganizationName, Data^.MappingFile, Self as IExportProgressReporter,
          fSettings.ExportConfig);

    try
      OroxExport.ExportToOrox(Data^.Version);
      Result.FileNameExportFile:= Data^.FileNameExportFile;
      Result.Success:= True;
      Result.Message:= 'ExportIsCompleted';
    finally
      OroxExport.Free;
    end;

  except
    on E: Exception do begin
      ReportError(E.Message);
      Result.Success:= False;
      Result.Message:= E.Message;
    end;
  end;
end;

function TModelMain.GetSQLfileLocation: String;
begin
  Result:= fSettings.AllSettings.setSqlFileLocation;
end;

function TModelMain.GetKeepLastOrganization: Boolean;
begin
  Result:=fSettings.AllSettings.setKeepLastOrganization;
end;

function TModelMain.GetLastUsedOrganization: String;
begin
  Result:=fSettings.AllSettings.setLastUsedOrganization;
end;

function TModelMain.GetSetting_AskToOpenExportFile: Boolean;
begin
  Result:= fSettings.AllSettings.setAskToOpenExportFile;
end;

procedure TModelMain.DisableChildControls(aData: TExportInProgressRec);
begin
  if aData.IsInProgress then
    common.utils.DisableChildControls(aData.aParent)
  else
    common.utils.EnableChildControls(aData.aParent)
end;

function TModelMain.GetSingleSetting(const SettingName: String): String;
begin
  case SettingName of
    'MappingFile': Result:=fSettings.AllSettings.setMappingFile;
    'GWSWVersion': Result:=fSettings.AllSettings.setGWSWVersion;
    'KeepLastOrganization': Result:=fSettings.AllSettings.setKeepLastOrganization.ToString();
  end;
end;

procedure TModelMain.SortDbGrid(Data: PSortDbGridRec);
begin
  if data^.DataType = dtORA then begin
    Data^.DataProvider:= fDatabaseModule.CurrentQuery;
    fDbGridHelper.SortOracleDbGrid(Data^.DataProvider, Data^.Column);
  end
  else if data^.DataType = dtCSV then begin
    fDbGridHelper.SortCSVDataSet(Data^.DataProvider, Data^.Column);  // Hier is de dataprovider de dbgrid.datasource.dataset. (Zou bij Ora ook zo kunnen).
  end;
end;

function TModelMain.LoadCSVData(aRec: PRetrieveCSVDataRec): TRetrieveCSVDataRec;  { #todo : Rename naar RetrieveCsvData }
var
  csvDataProvider: TCSVDataProvider;
  lDataSourceObj, lDataSetObj: TObject;
begin
  // Initialiseer result
  Result.FilePath:= aRec^.FilePath;
  Result.Delimiter:= aRec^.Delimiter;
  Result.QuoteChar:= aRec^.QuoteChar;
  Result.HasHeader:= aRec^.HasHeader;
  Result.DataSource:= nil;
  Result.DataSet:= nil;

  csvDataProvider:= TCSVDataProvider.Create(
    aRec^.FilePath,
    aRec^.Delimiter,
    aRec^.QuoteChar,
    aRec^.HasHeader
  );

  try
    // CSVDataProvider creëert objecten als TObject
    if csvDataProvider.LoadCSVData(lDataSourceObj, lDataSetObj) then begin
      if (lDataSourceObj <> Nil) and (lDataSetObj <> Nil) then begin
        Result.DataSource:= lDataSourceObj;
        Result.DataSet:= lDataSetObj;
        Result.Message:= '';
        Result.Success:= True;
      end
      else begin
        Result.Message:= 'Kon CSV niet laden (onbekende fout).';  { #todo : Taalinstelling }
        Result.Success:= False;
      end;
    end;
  finally
    csvDataProvider.Free;
  end;
end;


{$EndRegion 'TModelMain'}
initialization
  RegisterSection(TModelMain.ClassName);
finalization
  if Assigned(Singleton) then FreeAndNil(Singleton); { checks for nil explicitly, freeandnil doesn't! }
end.
