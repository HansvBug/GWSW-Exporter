{ Copyright ©2025 Hans van Buggenum }
unit model.main;
{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{-$define dbg}
interface
uses classes, sysutils, istrlist, model.decl, model.intf,
  LazFileUtils,
  Settings, AppLogging, DatabaseModule, uIGWSWDataProvider,
  exportProgressReporter.intf, DbGridHelper,
  ExportConfig;
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

  const Sects: TStringArray = ('[view.main]', '[view.main.statusbartexts]', '[view.main.hints]', '[view.main.logging]'); { very necessary, because we need it at idx 0 }
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
    function SetStatusbarText(const aText: string; panel : Word): TStatusbarPanelText;
    function DoesFileExists(const FullFileName: String): Boolean;
    function ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
    function StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
    procedure WriteSingleSetting(SettingData : PSingleSettingRec);
    procedure StartLogging;
    procedure StopLogging;
    procedure WriteToLog(const LogType, LogText: String);
    function SetStatusbarPanelsWidth(stbWidth, lpWidth, rpWidth: Integer): TstbPanelsSize;
    function IsFileInUse(const FileName: String): Boolean;
    // databasebeheer
    function GetDatabaseModule: TDatabaseModule;
    function IsConnected: Boolean;
    procedure MakeDbConnection(DbConnectionData : PDbConnectRec);
    procedure DbDisconnect(DbConnectionData : PDbConnectRec);

    // Execute query and get data
    function RetrieveData(data: PRetrieveDataRec): TRetrieveDataRec;
    function ExportToOroxTtlFile(Data: PExportToOroxTtlFileRec): TExportToOroxTtlFileRec;
    function GetSQLfileLocation: String;
    function GetKeepLastOrganization: Boolean;
    function GetLastUsedOrganization: String;

    function GetSetting_AskToOpenExportFile: Boolean;
    procedure DisableChildControls(aData : TExportInProgressRec);
    function GetSingleSetting(const SettingName: String): String;
    function SortDbGrid(Data: PSortDbGridRec): TSortDbGridRec;
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

procedure TModelMain.ReportProgressMsg(const Message : string);
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
         if fSettings.RapportMappingError then UpdateView:= True;
    end;
    2: begin
         lErrorType:= 'Fatal: --> ';
         if fSettings.RapportFatalError then UpdateView:= True;
    end;
    3: begin
         lErrorType:= 'Empty Field: --> ';
         if fSettings.RapportFieldIsEmpty then UpdateView:= True;
    end;
    4: begin
         lErrorType:= 'Field is missing: --> ';
         if fSettings.RapportFieldIsMissing then UpdateView:= True;
    end;
    5: begin
         lErrorType:= 'Value out of range: --> ';
         if fSettings.RapportOutOfRange then UpdateView:= True;
    end;
  end;

  lMsg:= lErrorType + '|'+ ErrMsg + '|' + ' {'+ Guid + ').';

  if UpdateView then
    { #note : Let op moet aangepast worden!!!!!!!!!!!!!!!! }
    fPresenter.Provider.NotifySubscribers(prReportError, Self, Str2Pch('ReportError' + '|' + lMsg)); // "ReportError=ERROR: "     is niet meer juist
end;

procedure TModelMain.ReportProgressCount(const Current, Total : Integer);
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

function TModelMain.IsDirWritable(const aRootDir : string) : Boolean;
begin
  Result:= False;
  if DirectoryIsWritable(aRootDir) then begin  // add to uses: LazFileUtils
    Result:= True;
  end;
end;

function TModelMain.CreateDirs(const aRootDir, AppDir : string; const aDirList : string) : TNewDirectoriesRec;
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
    if not DirectoryIsWritable(aRootDir) then
    begin
      Result.dirSucces:= False;
      Result.dirSuccesMsg:= 'Root directory is not writable: ' + aRootDir;
      Exit;
    end;

    // Create full root path.
    currentPath:= IncludeTrailingPathDelimiter(aRootDir) + AppDir;
    currentPath:= IncludeTrailingPathDelimiter(currentPath);
    // Create root directory if needed.
    if not DirectoryExists(CurrentPath) then
    begin
      if not ForceDirectories(currentPath) then
      begin
        Result.dirSucces:= False;
        Result.dirSuccesMsg:= 'Failed to create root directory: ' + currentPath;
        Exit;
      end;
    end;
    // Process subdirectories if provided.
    if aDirList <> '' then
    begin
      lDirList := TStringList.Create;
      try
        lDirList.Text:= aDirList;
        for s in lDirList do
        begin
          if s = '' then
            Continue;
          try
            if not ForceDirectories(currentPath + s) then
            begin
              Result.dirSucces:= False;
              Result.dirSuccesMsg:= 'Failed to create subdirectory: ' + s;
              // Continue trying other directories rather than exiting.
            end;
          except
            on E: Exception do
            begin
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
    on E: Exception do
    begin
      Result.dirSucces:= False;
      Result.dirSuccesMsg:= 'Unexpected error: ' + E.Message;
    end;
  end;
end;

function TModelMain.SetStatusbarText(const aText : string; panel : Word) : TStatusbarPanelText;
begin
  with Result do begin
    stbPanelText:= ' ' + aText;  // Add one space.
    stbActivePanel:= panel;
  end;
end;

function TModelMain.DoesFileExists(const FullFileName : String) : Boolean;
begin
  Result:= FileExists(FullFileName);
end;

function TModelMain.ReadFormState(Settingsdata : PSettingsRec) : TSettingsRec;
begin
  if assigned(fSettings) then FreeAndNil(fSettings);

  fSettings := TSettings.Create;
  try
    with PSettingsRec(Settingsdata)^ do begin
      //  Transferring the variables to settings object.
      fSettings.FormName:= setFrmName;
      fSettings.SettingsFile:= setSettingsFile;
    end;

    fsettings.ReadFormState;
  finally
  end;

  with Result do begin
    try
      setSettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
      setReadSettings:= PSettingsRec(Settingsdata)^.setReadSettings;
      setWriteSettings:= PSettingsRec(Settingsdata)^.setWriteSettings;

      setFrmName:= fSettings.FormName;
      setFrmWindowState:= fSettings.FormWindowstate;
      setFrmTop:= fSettings.FormTop;
      setFrmLeft:= fSettings.FormLeft;
      setFrmHeight:= fSettings.FormHeight;
      setFrmWidth:= fSettings.FormWidth;
      setFrmRestoredTop:= fSettings.FormRestoredTop;
      setFrmRestoredLeft:= fSettings.FormRestoredLeft;
      setFrmRestoredHeight:= fSettings.FormRestoredHeight;
      setFrmRestoredWidth:= fSettings.FormRestoredWidth;
      setSucces:= FSettings.Succes;
    except on E:Exception do
      setSucces := False;
    end;
  end;
end;

function TModelMain.ReadSettings(Settingsdata : PSettingsRec) : TSettingsRec;
begin
  if assigned(fSettings) then FreeAndNil(fSettings);
  fSettings := TSettings.Create;

  try
    with PSettingsRec(Settingsdata)^ do begin
      fSettings.FormName:= setFrmName;
      fSettings.SettingsFile:= setSettingsFile;
      fSettings.AppName:= setApplicationName;
      fSettings.AppVersion:= setApplicationVersion;
      fSettings.AppBuildDate:= setApplicationBuildDate;
    end;
    fsettings.ReadFile;
  finally
    //...
  end;

  with Result do try
    setSettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
    setReadSettings:= PSettingsRec(Settingsdata)^.setReadSettings;
    setWriteSettings:= PSettingsRec(Settingsdata)^.setWriteSettings;

    setApplicationName:= fSettings.AppName;
    setActivateLogging := fSettings.ActivateLogging;
    setAppendLogging:= fSettings.AppendLogFile;
    setLanguage:= fSettings.Language;
    setMappingFile:= fSettings.MappingFile;
    setSplitterDataSettings:= fSettings.SplitterDataSettings;
    setSplitterdataGrid:= fSettings.SplitterdataGrid;
    setSplitterMemos:= fSettings.SplitterMemos;
    setDbGridRowHighlight:= fSettings.DbGridRowHighlight;
    setAskToOpenExportFile:= fSettings.AskToOpenExportFile;
    setLastUsedOrganization:= fSettings.LastUsedOrganization;
    //Export settings
    // Put
    setIncludePutLengte:= fSettings.IncludePutLengte;
    setIncludePutBreedte:= fSettings.IncludePutBreedte;
    setIncludePutHoogte:= fSettings.IncludePutHoogte;
    setIncludePutDiameter:= fSettings.IncludePutDiameter;
    setIncludePutVorm:= fSettings.IncludePutVorm;
    setIncludePutMateriaal:= fSettings.IncludePutMateriaal;
    setIncludePutFundering:= fSettings.IncludePutFundering;
    setIncludePutBegindatum:= fSettings.IncludePutBegindatum;
    setIncludePutEinddatum:= fSettings.IncludePutEinddatum;
    setIncludePutMaaiveldhoogte:= fSettings.IncludePutMaaiveldhoogte;
    //Leiding
    setIncludeLeidingLengte:= fSettings.IncludeLeidingLengte;
    setIncludeLeidingBreedte:= fSettings.IncludeLeidingBreedte;
    setIncludeLeidingHoogte:= fSettings.IncludeLeidingHoogte;
    setIncludeLeidingDiameter:= fSettings.IncludeLeidingDiameter;
    setIncludeLeidingVorm:= fSettings.IncludeLeidingVorm;
    setIncludeLeidingMateriaal:= fSettings.IncludeLeidingMateriaal;
    setIncludeLeidingFundering:= fSettings.IncludeLeidingFundering;
    setIncludeLeidingStatusFunctioneren:= fSettings.IncludeLeidingStatusFunctioneren;
    setIncludeLeidingWIBONThema:= fSettings.IncludeLeidingWIBONThema;
    setIncludeLeidingBegindatum:= fSettings.IncludeLeidingBegindatum;
    setIncludeLeidingEinddatum:= fSettings.IncludeLeidingEinddatum;
    setIncludeLeidingBobBegin:= fSettings.IncludeLeidingBobBegin;
    setIncludeLeidingBobEind:= fSettings.IncludeLeidingBobEind;
    // Persleiding
    setIncludePersleidingLengte:= fSettings.IncludePersleidingLengte;
    setIncludePersleidingHoogte:= fSettings.IncludePersleidingHoogte;
    setIncludePersleidingDiameter:= fSettings.IncludePersleidingDiameter;
    setIncludePersleidingVorm:= fSettings.IncludePersleidingVorm;
    setIncludePersleidingMateriaal:= fSettings.IncludePersleidingMateriaal;
    setIncludePersleidingStatusFunctioneren:= fSettings.IncludePersleidingStatusFunctioneren;
    setIncludePersleidingBegindatum:= fSettings.IncludePersleidingBegindatum;
    setIncludePersleidingEinddatum:= fSettings.IncludePersleidingEinddatum;
    setIncludePersleidingBobBegin:= fSettings.IncludePersleidingBobBegin;
    setIncludePersleidingBobEind:= fSettings.IncludePersleidingBobEind;
    // Kolk
    setIncludeKolkLengte:= fSettings.IncludeKolkLengte;
    setIncludeKolkBreedte:= fSettings.IncludeKolkBreedte;
    setIncludeKolkHoogte:= fSettings.IncludeKolkHoogte;
    setIncludeKolkDiameter:= fSettings.IncludeKolkDiameter;
    setIncludeKolkVorm:= fSettings.IncludeKolkVorm;
    setIncludeKolkMateriaal:= fSettings.IncludeKolkMateriaal;
    setIncludeKolkWanddikte:= fSettings.IncludeKolkWanddikte;
    setIncludeKolkBegindatum:= fSettings.IncludeKolkBegindatum;
    setIncludeKolkEinddatum:= fSettings.IncludeKolkEinddatum;

    setSucces:= fSettings.Succes;
    setSucces:= True;

  except on E:Exception do
    setSucces:= False;
  end;
end;

function TModelMain.StoreFormState(Settingsdata : PSettingsRec) : TSettingsRec;
begin
  if assigned(fSettings) then begin
    with PSettingsRec(Settingsdata)^ do begin
      fSettings.SettingsFile:= setSettingsFile;
      fSettings.FormName:= setFrmName;
      fSettings.FormWindowstate:= setFrmWindowState;
      fSettings.FormTop:= setFrmTop;
      fSettings.FormLeft:= setFrmLeft;
      fSettings.FormHeight:= setFrmHeight;
      fSettings.FormWidth:= setFrmWidth;
      fSettings.FormRestoredTop:= setFrmRestoredTop;
      fSettings.FormRestoredLeft:= setFrmRestoredLeft;
      fSettings.FormRestoredHeight:= setFrmRestoredHeight;
      fSettings.FormRestoredWidth:= setFrmRestoredWidth;
      fSettings.SplitterMemos:= setSplitterMemos;
      fSettings.SplitterDataSettings:= setSplitterDataSettings;
      fSettings.SplitterDataGrid:= setSplitterdataGrid;
    end;

    fSettings.StoreFormState;

    With result do begin
      setSucces:= fSettings.Succes;
      setSplitterdataGrid:= fSettings.SplitterDataGrid;
      setSplitterMemos:= fSettings.SplitterMemos;
      setSplitterDataSettings:= fSettings.SplitterDataSettings;
    end;
  end;
end;

function TModelMain.WriteSettings(Settingsdata : PSettingsRec) : TSettingsRec;
begin
  if assigned(fSettings) then begin
    with PSettingsRec(Settingsdata)^ do begin
      fSettings.ActivateLogging:= setActivateLogging;
      fSettings.AppendLogFile:= setAppendLogging;
      if setLanguage <> '' then fSettings.Language:= setLanguage;

      //... add new settings

      fSettings.SettingsFile:= setSettingsFile;
    end;

    fSettings.WriteSettings;
  end
  else begin // firt run, there is no settingsfile
     if not assigned(FSettings) then begin
       fSettings := TSettings.Create;
       fSettings.SettingsFile:= PSettingsRec(Settingsdata)^.setSettingsFile;
       fSettings.AppName:= PSettingsRec(Settingsdata)^.setApplicationName;
       fSettings.AppVersion:= PSettingsRec(Settingsdata)^.setApplicationVersion;
       fSettings.AppBuildDate:= PSettingsRec(Settingsdata)^.setApplicationBuildDate;

       fSettings.ActivateLogging:= True;
       fSettings.AppendLogFile:= True;

       fSettings.WriteSettings;
     end;
  end;

  with Result do begin
    setReadSettings:= PSettingsRec(Settingsdata)^.setReadSettings;
    setWriteSettings:= PSettingsRec(Settingsdata)^.setWriteSettings;

    setActivateLogging:= PSettingsRec(Settingsdata)^.setActivateLogging;
    setAppendLogging:= PSettingsRec(Settingsdata)^.setAppendLogging;
    if PSettingsRec(Settingsdata)^.setLanguage <> '' then
      setLanguage:= PSettingsRec(Settingsdata)^.setLanguage;

    //... add new settings

    setSucces:= fSettings.Succes;
  end;
end;

procedure TModelMain.WriteSingleSetting(SettingData : PSingleSettingRec);
begin
  if assigned(fSettings) then begin
    with PSingleSettingRec(Settingdata)^ do begin
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
    if fSettings.ActivateLogging then begin
      if Logging <> Nil then begin
        { #todo : Retrieve the Location from the view. }

        userName:= StringReplace(GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';
        Logging.FileName:= userName + ApplicationName + '.log';

        {$IFDEF MSWINDOWS}
        Logging.LogFolder:= GetEnvironmentVariable('appdata') + PathDelim + ApplicationName + PathDelim + adLogging + PathDelim;
        {$ENDIF}
        {$IFDEF LINUX}
        Logging.LogFolder := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config' + PathDelim + ApplicationName + PathDelim + adLogging + PathDelim;
        {$ENDIF}

        Logging.ActivateLogging:= fSettings.ActivateLogging;  // Readsettings must be executed before startlogging.
        Logging.AppendLogFile:= fSettings.AppendLogFile;
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

procedure TModelMain.WriteToLog(const LogType, LogText : String);
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

function TModelMain.SetStatusbarPanelsWidth(stbWidth, lpWidth, rpWidth : Integer) : TstbPanelsSize;
var
  pWidth: Integer;
begin
  pWidth:= common.utils.SetStatusbarPnlWidth(stbWidth, lpWidth, rpWidth);
  with Result do begin
    mpWidth := pWidth;
  end;
end;

function TModelMain.IsFileInUse(const FileName : String) : Boolean;
begin
  Result:= common.utils.IsFileInUse(FileName);
end;

function TModelMain.GetDatabaseModule : TDatabaseModule;
begin
  Result:= fDatabaseModule;
end;

function TModelMain.IsConnected : Boolean;
begin
  Result:= False;
  if Assigned(fDatabaseModule) and (fDatabaseModule.IsConnected) then
    Result:= fDatabaseModule.IsConnected;
end;

procedure TModelMain.MakeDbConnection(DbConnectionData : PDbConnectRec);
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
      on E: Exception do
      begin
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

function TModelMain.RetrieveData(data : PRetrieveDataRec) : TRetrieveDataRec;    { #todo : Rename naar RetrieveOraData }
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

function TModelMain.ExportToOroxTtlFile(Data : PExportToOroxTtlFileRec) : TExportToOroxTtlFileRec;
var
  DataProviderOraExp: IGWSWDataProvider;
  DataProviderCSVExp: IGWSWDataProvider;
  OroxExport: TOroxExport;
  ExportConfig: TExportConfig;
begin
  Result.Success:= False;
  Result.Message:= '';

  if IsFileInUse(Data^.FileNameCsvFile) then begin
    Result.Message:= 'CsvFileInUse';  // File is in use by another process.
    Exit;
  end;

  if data^.DataType = '' then Exit;

  try
    fSettings.LoadExportConfigFromIni;
    ExportConfig:= fSettings.ExportConfig;

    if data^.DataType = dtORA then begin
      DataProviderOraExp:= CreateORADataProvider(fDatabaseModule.CurrentQuery, True);  // Create a data provider. This is the query that retrieved the data.
      OroxExport:= TOroxExport.Create(DataProviderOraExp, Data^.FileNameExportFile, Data^.OrganizationName, Data^.MappingFile, Self as IExportProgressReporter, ExportConfig  );
    end
    else if data^.DataType = dtCSV then begin
      DataProviderCSVExp:= CreateCSVDataProvider(Data^.FileNameCsvFile, ',', '"', True);  { #todo : Let nu nog hard in de code maar moet naar de configuratie }
      OroxExport:= TOroxExport.Create(DataProviderCSVExp, Data^.FileNameExportFile, Data^.OrganizationName, Data^.MappingFile, Self as IExportProgressReporter, ExportConfig  );
    end;

    try
      fSettings.ReadFile; // Kan vervelend worden. leest het hele settingsbestand opnieuw terwijl maar 1 setting nodig is.
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

function TModelMain.GetSQLfileLocation : String;
begin
  Result:= fSettings.SqlFileLocation;
end;

function TModelMain.GetKeepLastOrganization: Boolean;
begin
  Result:= fSettings.KeepLastOrganization;
end;

function TModelMain.GetLastUsedOrganization: String;
begin
  Result:= fSettings.LastUsedOrganization;
end;

function TModelMain.GetSetting_AskToOpenExportFile : Boolean;
begin
  Result:= fSettings.AskToOpenExportFile;
end;

procedure TModelMain.DisableChildControls(aData : TExportInProgressRec);
begin
  if aData.IsInProgress then
    common.utils.DisableChildControls(aData.aParent)
  else
    common.utils.EnableChildControls(aData.aParent)
end;

function TModelMain.GetSingleSetting(const SettingName: String): String;
begin { #todo : Als dit er meer gaan worden dan anders opzetten. Dan een settings object in de view maken. }
  case SettingName of
    'MappingFile' : Result:= fSettings.MappingFile;
    'GWSWVersion' : Result:= fSettings.GWSWVersion;
  end;
end;

function TModelMain.SortDbGrid(Data: PSortDbGridRec): TSortDbGridRec;
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

  csvDataProvider := TCSVDataProvider.Create(
    aRec^.FilePath,
    aRec^.Delimiter,
    aRec^.QuoteChar,
    aRec^.HasHeader
  );

  try
    // CSVDataProvider creëert objecten als TObject
    if csvDataProvider.LoadCSVData(lDataSourceObj, lDataSetObj) then
    begin
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
