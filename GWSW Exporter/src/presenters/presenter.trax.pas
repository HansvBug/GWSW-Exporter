{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
{ The base of this unit is made with "MVP-Setup" from Benny Christensen a.k.a. cdbc
  https://gitlab.com/cdbc-public/releases/mvp-setup
}
unit presenter.trax;
{$mode objfpc}{$H+}
{.$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, istrlist, model.intf, model.base;

(* this unit is by design, for specialized transactions... can have siblings :o)    *)
(* the 'transaction-manager now uses a transaction-factory, which means: to use it  *)
(* correctly, you have to 'register' your transaction class in correlation with the *)
(* 'modreason' at unit startup, i.e.: 'initialization' part  --  see at the bottom. *)

type
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
      fDataGrid: TObject;
      fDataSet: TObject;
      fMessage: String;
      fOrganizationName: String;
      fSqlText: String;
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
      fDisableErrorReport: Boolean;
      fFileNameCsvFile: String;
      fFileNameExportFile: String;
      fMappingFile: String;
      fMessage: String;
      fOrganizationName: String;
      fSuccess: Boolean;
      fVersion: String;
    public
      function Execute(aMgr: ITransactionManager): boolean;

      property FileNameExportFile: String read fFileNameExportFile write fFileNameExportFile;
      property FileNameCsvFile: String read fFileNameCsvFile write fFileNameCsvFile;
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
      faComponent: TObject;
      fListItems: TStrings;
      fNewString: String;

    public
      constructor {%H-}Create(aModReason: word);
      destructor Destroy; override;
      procedure Execute(aMgr: ITransactionManager);

      property ListItems: TStrings read fListItems write fListItems;
      property NewString: String read fNewString write fNewString;
      property aComponent: TObject read faComponent write faComponent;
  end;

  { TSortDbGridTrx }
  TSortDbGridTrx = class(TTransaction, ITrxExecNoRes)
  private
    fColumn: TObject;       // TColumn
    fDataProvider: TObject; // SQLQuery
    fDataType: String;
    fDbGrid: TObject;
    fFieldName: String;
    fCurrentSortOrder: String; // 'ASC' of 'DESC'
  public
    procedure Execute(aMgr: ITransactionManager);

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

  { TLoadFormStateTrx }
  TLoadFormStateTrx = class(TTransaction, ITrxExec)
    private
      fSettingsFile: string;
      fFormName: string;
    public
      function Execute(aMgr: ITransactionManager): Boolean;
      property SettingsFile: string read fSettingsFile write fSettingsFile;
      property FormName: string read fFormName write fFormName;
    end;

  { TStoreFormStateTrx }
  TStoreFormStateTrx = class(TTransaction, ITrxExec)
    private
      fSettingsFile: string;
      fSettings: TSettingsRec;
    public
      function Execute(aMgr: ITransactionManager): Boolean;
      property SettingsFile: string read fSettingsFile write fSettingsFile;
      property Settings: TSettingsRec read fSettings write fSettings;
    end;


{------------------------------------------------------------------------------}

implementation
// uses StrUtils; { for: 'IndexText' etc... }

{ utility functions, taking advantage of the publicly exported functions,
  in a unit that we can't /see/ from here. compiler imports for us :o) gotta love FPC }
function Pch2Str(aPch: pchar): string; external name 'BC_PCH2STR';
function Str2Pch(aStr: string): pchar; external name 'BC_STR2PCH'; 
{ the above 2 imports, are here to avoid referencing 'model.decl' in this unit.
  if you have to, then only in this 'implementation' part. }       


{ TCreDirTrx }
constructor TCreDirTrx.Create(aModReason: word);
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

function TCreDirTrx.Execute(aMgr: ITransactionManager): Boolean;
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


{ TSingleSettingTrx }
function TSingleSettingTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TSingleSettingRec;
begin
  Result:=aMgr.OwnerMain.Model.DoesFileExists(SettingsLocationAndFileName);

  if Result then begin
    lRec.ssSettingsFile:=SettingsLocationAndFileName;
    lRec.ssName:=SettingName;
    lRec.ssSection:=Section;
    lRec.ssValue:=SettingValue;
    aMgr.OwnerMain.Model.WriteSingleSetting(@lRec);
    lRec.ssSucces:=True;
  end
  else begin
    lRec.ssSucces:=False;
    lRec.ssMessage:='SettingsFileNotExists';
  end;

  aMgr.OwnerMain.Provider.NotifySubscribers(prAppSingleSetting, Self, @lRec);
end;

{ TRetrieveDataTrx }
function TRetrieveDataTrx.Execute(aMgr: ITransactionManager): boolean;
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
function TExportToOroxTtlFileTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TExportToOroxTtlFileRec;
begin
  lRec.OrganizationName:= OrganizationName;
  lRec.FileNameExportFile:= FileNameExportFile;
  lRec.FileNameCsvFile:= FileNameCsvFile;
  lRec.MappingFile:= MappingFile;
  lRec.Version:= Version;
  lRec.DataType:= DataType;

  lRec:= aMgr.OwnerMain.Model.ExportToOroxTtlFile(@lRec);

  Result:= lRec.Success;

  aMgr.OwnerMain.Provider.NotifySubscribers(prExportToOroxTtlFile, Nil, @lRec);
end;

{ TUniqueStringlistTrx }
constructor TUniqueStringlistTrx.Create(aModReason: word);
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

procedure TUniqueStringlistTrx.Execute(aMgr: ITransactionManager);
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
procedure TSortDbGridTrx.Execute(aMgr: ITransactionManager);
var
  lRec: TSortDbGridRec;
begin
  // Prepare the record to send to Model
  lRec.Column:= fColumn;
  lRec.FieldName:= fFieldName;
  lRec.CurrentSortOrder:= fCurrentSortOrder;
  lRec.DataType:= DataType;
  lRec.DataProvider:= DataProvider;  // is used by the csv dataset.
  aMgr.OwnerMain.Model.SortDbGrid(@lRec);

  // Send the result back to the View
  aMgr.OwnerMain.Provider.NotifySubscribers(prSortDbGrid, DbGrid, Nil);
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

{ TLoadFormStateTrx }
function TLoadFormStateTrx.Execute(aMgr: ITransactionManager): Boolean;
var
  lRec: TSettingsRec;
begin
  lRec.setSettingsFile:= fSettingsFile;
  lRec.setFrmName:= fFormName;
  lRec:= aMgr.OwnerMain.Model.ReadFormState(@lRec);
  lRec.setReadFormState:= True;
  aMgr.OwnerMain.Provider.NotifySubscribers(prLoadFormState, Self, @lRec);
  Result:= lRec.setSucces;
end;

{ TStoreFormStateTrx }
function TStoreFormStateTrx.Execute(aMgr: ITransactionManager): Boolean;
var
  lRec: TSettingsRec;
begin
  lRec:= fSettings;
  lRec.setSettingsFile:= fSettingsFile;
  lRec.setStoreFormState:= True;
  lRec:= aMgr.OwnerMain.Model.StoreFormState(@lRec);
  lRec.setStoreFormState:= True;
  aMgr.OwnerMain.Provider.NotifySubscribers(prStoreFormState, Self, @lRec);
  Result:= lRec.setSucces;
end;



procedure InitTrax;
begin
  // All examples removed, so the transaction factory is empty when the tool starts...
end;

initialization
  InitTrax; { sets up & registers transactions with TrxManager's Class-Factory }

finalization 
//  FreeAndNil(__Example);
  
end.

