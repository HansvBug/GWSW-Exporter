{ Copyright ©2025 Hans van Buggenum }
unit model.main;
{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{-$define dbg}
interface
uses classes, sysutils, istrlist, model.decl, model.intf,
  LazFileUtils,
  Settings, AppLogging, DatabaseModule;
//const

type
{$Region 'TModelMainH'}
  { TModelMain }
  TModelMain = class(TObject,IModelMain)
  private { remember to use 'RegisterSection' in units, you want to add to 'Sects' :o) }
    fSettings: TSettings;
    fDatabaseModule: TDatabaseModule;

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
    procedure DoEach(const aValue: string; const anIdx: ptrint; anObj: TObject; aData: pointer);
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

    // Execute query and get data
    function RetrieveData(data: PRetrieveDataRec): TRetrieveDataRec;
    function ExportToOroxTtlFile(Data: PExportToOroxTtlFileRec): TExportToOroxTtlFileRec;
  end;

{$EndRegion 'TModelMainH'}

{ datastore factory, its intended use is in scenarios, where the 'viewmodel' / 'presenter'
  does NOT OWN the 'model' and thus doesn't free it on end of use...,
  in other words:
    "if you create your 'model' with this factory-function then DON'T FREE IT!" just := nil }
function gModelMain(aPresenter: IPresenterMain;const aRoot: shortstring): IModelMain;


implementation
uses obs_prosu, strutils, common.consts, common.utils, uIGWSWDataProvider, QueryDataProvider,
  OroxExport;

var Singleton: TModelMain = nil;

{ ModelMain factory }
function gModelMain(aPresenter: IPresenterMain; const aRoot: shortstring): IModelMain;
begin
  if not Assigned(Singleton) then Singleton:= TModelMain.Create(aPresenter,aRoot);
  Result:= Singleton;
end;

{$Region 'TModelMain'}
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
  UpdateSections(Sects); { <- fetch our registered sections, v- i18n }
  fTextCache:= CreStrListFromFile(format(mvpTexts,[fRoot,Lang])); ///<- i18n
  { we need the model, this is a minor flaw if it fails, because then the }
  if fTextCache.Count = 0 then { user will see a view filled with 'dummy' ;) }
    fPresenter.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(!) ERROR: Could NOT retrieve static texts!'));
end; /// the above text can't be translated in the i18n'ed mvptexts, the count is 0! ///

destructor TModelMain.Destroy;
begin
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
  fSection:= '['+aSection+']'; fSecId:= IndexText(fSection,sects); { iterator-search }
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
    //...

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
    end;

    fSettings.StoreFormState;

    With result do begin
      setSucces:= fSettings.Succes;
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
      fSettings.WriteSetting(ssName, ssValue);
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
        WriteToLog(ltError, DbConnectionData^.Message);
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
  end; { #todo : Maybe something else if there is already a connection... }
end;

function TModelMain.RetrieveData(data : PRetrieveDataRec) : TRetrieveDataRec;
var
  lResult: TDataSetResult;
begin
  if Assigned(fDatabaseModule) then begin
    lResult:= fDatabaseModule.RetrieveData(data^.SqlText);

    Result.DataSource:= lResult.DataSet;
  end
  else
    Result.DataSource:= Nil;
end;

function TModelMain.ExportToOroxTtlFile(Data : PExportToOroxTtlFileRec) : TExportToOroxTtlFileRec;
var
  DataProvider: IGWSWDataProvider;
  OroxExport: TOroxExport;
begin
  // Create a data provider. This is the query that retrieved the data.
  DataProvider:= TQueryDataProvider.Create(fDatabaseModule.CurrentQuery);  // No free/nil required

  OroxExport:= TOroxExport.Create(DataProvider, Data^.FileName, Data^.OrganizationName, Data^.MappingFile);
  try
    OroxExport.ExportToOrox;
  finally
    OroxExport.Free;
  end;
end;





{$EndRegion 'TModelMain'}
initialization
  RegisterSection(TModelMain.ClassName);
finalization
  if Assigned(Singleton) then FreeAndNil(Singleton); { checks for nil explicitly, freeandnil doesn't! }
end.
