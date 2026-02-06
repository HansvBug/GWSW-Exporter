{ Copyright ©2025 Hans van Buggenum }
unit presenter.trax;
{$mode objfpc}{$H+}
{.$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, istrlist, model.intf, model.base,
  exp.dirinfo; /// <experimental> /// 

(* this unit is by design, for specialized transactions... can have siblings :o)    *)
(* the 'transaction-manager now uses a transaction-factory, which means: to use it  *)
(* correctly, you have to 'register' your transaction class in correlation with the *)
(* 'modreason' at unit startup, i.e.: 'initialization' part  --  see at the bottom. *)

type
  { TTextEdit example, ultra simple, uses the inherited 'Title' prop }
  TTextEdit = class(TTransaction,ITrxExec) { <- notice the interface => important }
  private
    fSuccess: boolean;       { this trx gets triggered by 'prDataChanged' ~ 3 }
  public
    function Execute(aMgr: ITransactionManager): boolean;
    property Success: boolean read fSuccess write fSuccess;
  end;

  { TDirInfoTrx is tied to 'prFetchDirData' ~ 113, uses 'prDirDataNeeded' ~ 112 }
  TDirInfoTrx = class(TTransaction,ITrxExecNoRes)
  private
    fDi: IDirInfo;
  public
    procedure Execute(aMgr: ITransactionManager);
  end;
  { TFileInfoTrx is tied to 'prFetchFileData' ~ 114, uses 'prDirDataNeeded' ~ 112 }
  TFileInfoTrx = class(TTransaction,ITrxExecNoRes)
  private
    fDi: IDirInfo;
  public
    procedure Execute(aMgr: ITransactionManager);
  end;  

  { Create new transactions here. ---------------------------------------------------------------------------------}

  { TCreDirTrx }
  TCreDirTrx = class(TTransaction, ITrxExec)
  private
    fAppName: String;
    fDirIsWriteable: Boolean;
    fNewDirnames: TStrings;
    fRootDir: string;
    fSucces: Boolean;
  public
    constructor {%H-}Create(aModReason: word);
    destructor Destroy; override;
    function Execute(aMgr: ITransactionManager): Boolean;
    property RootDir: string read fRootDir write fRootDir;
    property NewDirNames: TStrings read fNewDirNames write fNewDirNames;  // New Directory Names.
    property Succes: Boolean read fSucces write fSucces;
    property DirIsWriteable: Boolean read fDirIsWriteable write fDirIsWriteable;
    property AppName: String read fAppName write fAppName;
  end;

  { TSettingstrx }
  TSettingstrx = class(TTransaction, ITrxExec)
    private
      fActivateLogging: Boolean;
      fAppendLogging: Boolean;
      fLanguage: String;

      fAppName: string;
      fAppVersion: string;
      fAppBuildDate: string;
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
      fManholeShape: Boolean;
      fRead: Boolean;
      fReadFrmState: Boolean;
      fSettingsFile: String;
      fSplitterdataGrid: Integer;
      fSplitterDataSettings: Integer;
      fSplitterMemos: Integer;
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

      property AppName: string read fAppName write fAppName;
      property AppVersion: string read fAppVersion write fAppVersion;
      property AppBuildDate: string read fAppBuildDate write fAppBuildDate;

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
      property SplitterdataGrid: Integer read fSplitterdataGrid write fSplitterdataGrid;
      property SplitterDataSettings: Integer read fSplitterDataSettings write fSplitterDataSettings;
      property SplitterMemos: Integer read fSplitterMemos write fSplitterMemos;
      //Export settings
      property ManholeShape: Boolean read fManholeShape write fManholeShape;
    end;

  { TSingleSettingTrx }
  TSingleSettingTrx = class(TTransaction, ITrxExec)
  private
    fSection: String;
    fSettingName: String;
    fSettingsFile: String;
    fSettingValue: String;
  public
    function Execute(aMgr: ITransactionManager): boolean;
    property SettingsLocationAndFileName: String read fSettingsFile write fSettingsFile;
    property Section: String read fSection write fSection;
    property SettingName: String read fSettingName write fSettingName;
    property SettingValue: String read fSettingValue write fSettingValue;
  end;

  { TRetrieveDataTrx }
  TRetrieveDataTrx = class(TTransaction, ITrxExec)
    private
      fDataGrid : TObject;
      fDataSet : TObject;
      fMessage : String;
      fOrganizationName : String;
      fSqlText : String;
    public
      function Execute(aMgr: ITransactionManager): boolean;
      property SqlText: String read fSqlText write fSqlText;
      property OrganizationName: String read fOrganizationName write fOrganizationName;
      property DataGrid: TObject read fDataGrid write fDataGrid;
      property DataSource: TObject read fDataSet write fDataSet;
      Property Message: String read fMessage write fMessage;
  end;

  { TExportToOroxTtlFileTrx }
  TExportToOroxTtlFileTrx = class(TTransaction, ITrxExec)
    private
      fDataType: String;
      fDisableErrorReport : Boolean;
      fFileNameCsvFile: String;
      fFileNameExportFile : String;
      fMappingFile : String;
      fMessage : String;
      fOrganizationName : String;
      fSuccess : Boolean;
      fVersion : String;
    public
      function Execute(aMgr: ITransactionManager): boolean;

      property FileNameExportFile: String read fFileNameExportFile write fFileNameExportFile;
      property FileNameCsvFile : String read fFileNameCsvFile write fFileNameCsvFile;
      property MappingFile: String read fMappingFile write fMappingFile;
      property OrganizationName: String read fOrganizationName write fOrganizationName;
      property DisableErrorReport: Boolean read fDisableErrorReport write fDisableErrorReport;
      property Version: String read fVersion write fVersion;
      property DataType: String read fDataType write fDataType;
      property Success: Boolean read fSuccess write fSuccess;
      property Message: String read fMessage write fMessage;
  end;

  { TUniqueStringlistTrx }
  TUniqueStringlistTrx = class(TTransaction, ITrxExecNoRes)
    private
      faComponent : TObject;
      fListItems : TStrings;
      fNewString : String;

    public
      constructor {%H-}Create(aModReason: word);
      destructor Destroy; override;
      procedure Execute(aMgr: ITransactionManager);

      property ListItems: TStrings read fListItems write fListItems;
      property NewString: String read fNewString write fNewString;
      property aComponent: TObject read faComponent write faComponent;
  end;

  { TSortDbGridTrx }
  TSortDbGridTrx = class(TTransaction, ITrxExec)
  private
    fColumn: TObject;       // TColumn
    fDataProvider: TObject; // SQLQuery
    fDataType: String;
    fDbGrid: TObject;
    fFieldName: String;
    fCurrentSortOrder: String; // 'ASC' of 'DESC'
  public
    function Execute(aMgr: ITransactionManager): boolean;

    property Column: TObject read fColumn write fColumn;
    property DataProvider: TObject read fDataProvider write fDataProvider;
    property DbGrid: TObject read fDbGrid write fDbGrid;
    property FieldName: string read fFieldName write fFieldName;
    property CurrentSortOrder: string read fCurrentSortOrder write fCurrentSortOrder;
    property DataType: String read fDataType write fDataType;
  end;


  { TRetrieveCSVDataTrx }
  TRetrieveCSVDataTrx = class(TTransaction, ITrxExec)
    private
      fDataGrid: TObject;
      fDataSource: TObject;
      fDelimiter: Char;
      fFileName: String;
      fHasHeader: Boolean;
      fQuoteChar: Char;
    public
      function Execute(aMgr: ITransactionManager): boolean;

      property FileName: String read fFileName write fFileName;
      property HasHeader: Boolean read fHasHeader write fHasHeader;
      property Delimiter: Char read fDelimiter write fDelimiter;
      property QuoteChar: Char read fQuoteChar write fQuoteChar;
      property DataGrid: TObject read fDataGrid write fDataGrid;
      property DataSource: TObject read fDataSource write fDataSource;
  end;

implementation
// uses StrUtils; { for: 'IndexText' etc... }

{ utility functions, taking advantage of the publicly exported functions,
  in a unit that we can't /see/ from here. compiler imports for us :o) gotta love FPC }
function Pch2Str(aPch: pchar): string; external name 'BC_PCH2STR';
function Str2Pch(aStr: string): pchar; external name 'BC_STR2PCH'; 
{ the above 2 imports, are here to avoid referencing 'model.decl' in this unit.
  if you have to, then only in this 'implementation' part. }       


{ TTextEdit }
function TTextEdit.Execute(aMgr: ITransactionManager): boolean;
var
  lsl: IStringList;
begin { very lazy example :-/ i don't even touch the model => DON'T DO THIS }
  Result:= FileExists(fTitle);
  if Result then begin
    lsl:= CreateStrList;
    lsl.LoadFromFile(fTitle);
    fSuccess:= (lsl.Count > 0);
    aMgr.OwnerMain.Provider.NotifySubscribers(prDataChanged,Self,lsl);
  end;
end; { end of example }

{ TDirInfoTrx }
procedure TDirInfoTrx.Execute(aMgr: ITransactionManager);
var
  lsl: IStringList;
begin
  if fTitle = '' then aMgr.OwnerMain.Provider.NotifySubscribers(prDirDataNeeded,self,nil);
  if fTitle = '' then exit;
  fDi:= GetDirInfo; { nifty little <experimental> manual interface }
  lsl:= fDi.GetDirs(fTitle,true);
  aMgr.OwnerMain.Provider.NotifySubscribers(prFetchDirData,self,lsl);
  aMgr.OwnerMain.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(i) Dir-Info on "'+Title+'" fetched successfully'));
end;

{ TFileInfoTrx }
procedure TFileInfoTrx.Execute(aMgr: ITransactionManager);
var
  lsl: IStringList;
begin
  if fTitle = '' then aMgr.OwnerMain.Provider.NotifySubscribers(prDirDataNeeded,self,nil);
  if fTitle = '' then exit;
  fDi:= GetDirInfo; { nifty little <experimental> manual interface :o) }
  lsl:= fDi.GetFiles(fTitle,true);
  aMgr.OwnerMain.Provider.NotifySubscribers(prFetchFileData,self,lsl);
  aMgr.OwnerMain.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(i) File-Info on "'+Title+'" fetched successfully'));
end; { end of examples }

{ TCreDirTrx }
constructor TCreDirTrx.Create(aModReason : word);
begin
  fModReason:= aModReason;
  fNewDirNames:= TStringList.Create;
  fNewDirNames.SkipLastLineBreak:= true;
end;

destructor TCreDirTrx.Destroy;
begin
  fNewDirNames.Free;  // Free the directory names list.
  inherited Destroy;
end;

function TCreDirTrx.Execute(aMgr : ITransactionManager) : Boolean;
var
  lRec: TNewDirectoriesRec;
begin
  // First check if we have writing rights at the location. If not then we do not have to start de model functions.
  Result:= aMgr.OwnerMain.Model.IsDirWritable(RootDir);

  if Result then begin
    lRec:= aMgr.OwnerMain.Model.CreateDirs(RootDir, appname, NewDirNames.Text);
    lRec.dirSucces:= Result;
    lRec.dirIsWriteable:= Result;
    aMgr.OwnerMain.Provider.NotifySubscribers(prCreateDir, Self, @lRec); // Go back to the view. If succes is false there are no subdirs and the program must stop.
  end
  else begin
    // Return that the location has no write permissions.
    lRec.dirSucces:= Result;
    lRec.dirIsWriteable:= False;
    aMgr.OwnerMain.Provider.NotifySubscribers(prCreateDir, Self, @lRec);
  end;
end;

{ TSettingstrx }
function TSettingstrx.Execute(aMgr : ITransactionManager) : Boolean;
var
  lRec: TSettingsRec;
begin
  Result:= aMgr.OwnerMain.Model.DoesFileExists(SettingsLocationAndFileName);

  If Result then begin
    lRec.setSettingsFile:= SettingsLocationAndFileName;  // Verry important.

    if ReadSettings then begin
      lRec.setReadSettings:= ReadSettings;
      if ReadFormState then begin
        lRec.setFrmName:= FormName;
        lRec:= aMgr.OwnerMain.Model.ReadFormState(@lRec);
        lRec.setReadSettings:= ReadSettings;
        lRec.setReadFormState:= ReadFormState;

        aMgr.OwnerMain.Provider.NotifySubscribers(prFormState, Self, @lRec);
      end
      else begin  // read all other settings
        // pass the settings
        // first some defaults....
        lRec.setApplicationName:= AppName;
        lRec.setApplicationVersion:= AppVersion;
        lRec.setApplicationBuildDate:= AppBuildDate;
        lRec.setWriteSettings:= WriteSettings;

        lRec:= aMgr.OwnerMain.Model.ReadSettings(@lRec);

        aMgr.OwnerMain.Provider.NotifySubscribers(prAppSettings, Self, @lRec);
      end;
    end
    else if WriteSettings then begin
      lRec.setWriteSettings:= WriteSettings;

      if StoreFormstate then begin
        lRec.setFrmName := FormName;
        lRec.setFrmWindowState := FormWindowstate;
        lRec.setFrmTop:= FormTop;
        lRec.setFrmLeft := FormLeft;
        lRec.setFrmHeight := FormHeight;
        lRec.setFrmWidth := FormWidth;
        lRec.setFrmRestoredTop := FormRestoredTop;
        lRec.setFrmRestoredLeft := FormRestoredLeft;
        lRec.setFrmRestoredHeight := FormRestoredHeight;
        lRec.setFrmRestoredWidth := FormRestoredWidth;
        lRec.setSplitterdataGrid:= SplitterdataGrid;  // Noodzakelijk, andsers kan er een div by zero ontstaa in de doappsettings omdat deze settings dan een default waarde die onbekend is.
        lRec.setSplitterDataSettings:= SplitterDataSettings;
        lRec.setSplitterMemos:= SplitterMemos;
        lRec:= aMgr.OwnerMain.Model.StoreFormState(@lRec);

        aMgr.OwnerMain.Provider.NotifySubscribers(prAppSettings, Self, @lRec);
      end
      else begin // Write the regular settings.
        lRec.setApplicationName:= Application_build_date;
        lRec.setApplicationVersion:= Application_version;
        lRec.setApplicationBuildDate:= Application_build_date;
        lRec.setActivateLogging:= ActivateLogging;
        lRec.setAppendLogging:= AppendLogging;
        lRec.setLanguage:= Language;
        lRec.setApplicationName:= ApplicationName;
        lRec.setApplicationVersion:= Application_version;
        lRec.setApplicationBuildDate:= Application_build_date;

        lRec:= aMgr.OwnerMain.Model.WriteSettings(@lRec);
        aMgr.OwnerMain.Provider.NotifySubscribers(prAppSettings, Self, @lRec);
      end;
    end;
  end
  else begin
    // Write defaults.
    lRec.setSettingsFile:= SettingsLocationAndFileName;
    lRec.setApplicationName:= AppName;
    lRec.setApplicationVersion:= AppVersion;
    lRec.setApplicationBuildDate:= AppBuildDate;
    aMgr.OwnerMain.Model.WriteSettings(@lRec);  // Create the settingsfile.

    lRec.setSucces:= False;
    lRec.setMessage:= 'SettingsFileNotExists' ;
    aMgr.OwnerMain.Provider.NotifySubscribers(prAppSettings, Self, @lRec);
  end;
end;

{ TSingleSettingTrx }
function TSingleSettingTrx.Execute(aMgr : ITransactionManager) : boolean;
var
  lRec: TSingleSettingRec;
begin
  Result:= aMgr.OwnerMain.Model.DoesFileExists(SettingsLocationAndFileName);

  if Result then begin
    lRec.ssSettingsFile:= SettingsLocationAndFileName;
    lRec.ssName:= SettingName;
    lRec.ssSection:= Section;
    lRec.ssValue:= SettingValue;

    aMgr.OwnerMain.Model.WriteSingleSetting(@lRec);
  end
  else begin
    lRec.ssSucces:= False;
    lRec.ssMessage:= 'SettingsFileNotExists' ;
    aMgr.OwnerMain.Provider.NotifySubscribers(prAppSettings, Self, @lRec);
  end;
end;

{ TRetrieveDataTrx }
function TRetrieveDataTrx.Execute(aMgr : ITransactionManager) : boolean;
var
  lRec: TRetrieveDataRec;
begin
  Result:= aMgr.OwnerMain.Model.IsConnected;

  if aMgr.OwnerMain.Model.GetSQLfileLocation <> '' then
    Result:= True
  else begin
    Result:= False;
    lRec.DataSource:= Nil;
    lRec.Message:= 'QueryFileNotLoaded';
  end;

  if Result then begin
    lRec.DataSource:= Nil;
    lRec.SqlText:= SqlText;
    lRec.DataGrid:= DataGrid;
    lRec:= aMgr.OwnerMain.Model.RetrieveData(@lRec);
  end;

  aMgr.OwnerMain.Provider.NotifySubscribers(prRetrieveData, DataGrid, @lRec);
end;

{ TExportToOroxTtlFileTrx }
function TExportToOroxTtlFileTrx.Execute(aMgr : ITransactionManager) : boolean;
var
  lRec : TExportToOroxTtlFileRec;
begin
  lRec.OrganizationName:= OrganizationName;
  lRec.FileNameExportFile:= FileNameExportFile;
  lRec.FileNameCsvFile:= FileNameCsvFile;
  lRec.MappingFile:= MappingFile;
  lRec.Version:= Version;
  lRec.DataType:= DataType;

  lRec:= aMgr.OwnerMain.Model.ExportToOroxTtlFile(@lRec);

  aMgr.OwnerMain.Provider.NotifySubscribers(prExportToOroxTtlFile, Nil, @lRec);
end;

{ TUniqueStringlistTrx }
constructor TUniqueStringlistTrx.Create(aModReason : word);
begin
  fModReason:= aModReason;
  fListItems:= Nil; //TStringList.Create;
 // fListItems.SkipLastLineBreak:= True;
end;

destructor TUniqueStringlistTrx.Destroy;
begin
  //fListItems.Free;
  inherited Destroy;
end;

procedure TUniqueStringlistTrx.Execute(aMgr : ITransactionManager);
var
  lRec: TUniqueStringlistRec;
  tempList: TStringList;
begin
  tempList:= TStringList.Create;
  try
    tempList.Text:= fListItems.Text;

    if (NewString <> '') and (tempList.IndexOf(NewString) = -1) then
      tempList.Add(NewString);

    lRec.ListItems:= tempList;
    //lRec:= aMgr.OwnerMain.Model.AddToStringListUnique(NewString, fListItems.Text);
    aMgr.OwnerMain.Provider.NotifySubscribers(prUniqueStringlist, aComponent, @lRec);
  finally
    tempList.Free;
  end;
end;

{ TSortDbGridTrx }
function TSortDbGridTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TSortDbGridRec;
begin
  // Bereid de record voor om naar Model te sturen
  lRec.Column:= fColumn;
  lRec.FieldName:= fFieldName;
  lRec.CurrentSortOrder:= fCurrentSortOrder;
  lRec.DataType:= DataType;
  lRec.DataProvider:= DataProvider;  // wordt voor de csv dataaset gebruikt.
  lRec:= aMgr.OwnerMain.Model.SortDbGrid(@lRec);

  // Stuur het resultaat terug naar de View
  aMgr.OwnerMain.Provider.NotifySubscribers(prSortDbGrid, DbGrid, @lRec);
end;

{ TRetrieveCSVDataTrx }
function TRetrieveCSVDataTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TRetrieveCSVDataRec;
begin
  Result:= aMgr.OwnerMain.Model.DoesFileExists(FileName);

  if Result then begin
    lRec.FilePath:= FileName;
    lRec.Delimiter:= Delimiter;
    lRec.QuoteChar:= QuoteChar;
    lRec.HasHeader:= HasHeader;
    lRec.DataSource:= Nil;
    lRec:= aMgr.OwnerMain.Model.LoadCSVData(@lRec);
  end
  else begin
      lRec.Success:= False;
      LRec.Message:= 'File does not exists';  { #todo : Taalinstelling }
      lRec.DataSource:= Nil;
      lRec.DataSet:= Nil;
  end;

  aMgr.OwnerMain.Provider.NotifySubscribers(prRetrieveCSVData, DataGrid, @lRec);
end;






procedure InitTrax;
begin
  RegisterTransactionClass(prDataChanged,TTextEdit);
  RegisterTransactionClass(prFetchDirData,TDirInfoTrx);
  RegisterTransactionClass(prFetchFileData,TFileInfoTrx);
end;

initialization
  InitTrax; { sets up & registers transactions with TrxManager's Class-Factory }

finalization 
//  FreeAndNil(__Example);
  
end.

