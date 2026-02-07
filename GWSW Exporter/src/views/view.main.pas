{ Copyright ©2025-2026 Hans van Buggenum }
unit view.main;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, DBGrids, Buttons, DBCtrls, fpspreadsheetctrls,
  SynEdit, LCLIntf,
  SynHighlighterSQL, istrlist, model.intf, model.decl, presenter.main,
  presenter.trax, DB, fpcsvexport, oracleconnection, SQLDB, csvdataset;

type
  { TfrmMain }

  TfrmMain = class(TForm, IViewMain)
    btnConnect: TButton;
    btnExportDbgridToCsv : TButton;
    btnExportToFileOra: TButton;
    btnExportToFileCSV: TButton;
    btnGetData: TButton;
    btnSaveQuery : TButton;
    btnOpenQuery : TButton;
    btnClose : TButton;
    btnSaveErrorLog: TButton;
    btnSelectCsvFile: TButton;
    btnDisconnect: TButton;
    cbDatabaseName: TComboBox;
    cbOrganizationName: TComboBox;
    cbOrganizationNameCSV: TComboBox;
    cbUserName: TComboBox;
    chkIncludePersleidingBobBegin: TCheckBox;
    chkIncludePersleidingBobEind: TCheckBox;
    chkIncludePutMaaiveldhoogte: TCheckBox;
    chkIncludeKolkLengte: TCheckBox;
    chkIncludeKolkBreedte: TCheckBox;
    chkIncludeKolkHoogte: TCheckBox;
    chkIncludeKolkDiameter: TCheckBox;
    chkIncludeKolkVorm: TCheckBox;
    chkIncludeKolkMateriaal: TCheckBox;
    chkIncludeKolkWanddikte: TCheckBox;
    chkIncludeKolkBegindatum: TCheckBox;
    chkIncludeKolkEinddatum: TCheckBox;
    chkIncludePersleidingLengte: TCheckBox;
    chkIncludePersleidingHoogte: TCheckBox;
    chkIncludePersleidingDiameter: TCheckBox;
    chkIncludePersleidingVorm: TCheckBox;
    chkIncludePersleidingMateriaal: TCheckBox;
    chkIncludePersleidingStatusFunctioneren: TCheckBox;
    chkIncludePersleidingBegindatum: TCheckBox;
    chkIncludePersleidingEinddatum: TCheckBox;
    chkIncludeLeidingLengte: TCheckBox;
    chkIncludeLeidingBegindatum: TCheckBox;
    chkIncludeLeidingEinddatum: TCheckBox;
    chkIncludeLeidingBobBegin: TCheckBox;
    chkIncludeLeidingBobEind: TCheckBox;
    chkIncludeLeidingBreedte: TCheckBox;
    chkIncludeLeidingHoogte: TCheckBox;
    chkIncludeLeidingDiameter: TCheckBox;
    chkIncludeLeidingVorm: TCheckBox;
    chkIncludeLeidingMateriaal: TCheckBox;
    chkIncludeLeidingFundering: TCheckBox;
    chkIncludeLeidingStatusFunctioneren: TCheckBox;
    chkIncludeLeidingWIBONThema: TCheckBox;
    chkIncludePutLengte: TCheckBox;
    chkIncludePutBreedte: TCheckBox;
    chkIncludePutHoogte: TCheckBox;
    chkIncludePutDiameter: TCheckBox;
    chkIncludePutMateriaal: TCheckBox;
    chkIncludePutFundering: TCheckBox;
    chkIncludePutBegindatum: TCheckBox;
    chkIncludePutEinddatum: TCheckBox;
    chkIncludePutVorm: TCheckBox;
    dbgSewerData : TDBGrid;
    DBNavigator : TDBNavigator;
    edtCsvFile: TEdit;
    edtPassword: TEdit;
    gbConnection: TGroupBox;
    gbExport: TGroupBox;
    gbExport1: TGroupBox;
    gbGetData: TGroupBox;
    gbQuery : TGroupBox;
    gbManholes : TGroupBox;
    gbPipelines: TGroupBox;
    gbMechanicalPipeline: TGroupBox;
    gbGully: TGroupBox;
    ImageListDbGridSort: TImageList;
    lblDatabaseName: TLabel;
    lblOrganizationName: TLabel;
    lblOrganizationNameCsv: TLabel;
    lblPassword: TLabel;
    lblProgress: TLabel;
    lblSQLFileLocation : TLabel;
    lblError : TLabel;
    lblProgressTitle : TLabel;
    lblUserName: TLabel;
    MainMenu1 : TMainMenu;
    miOptionsLanguageNL : TMenuItem;
    miOptionsLanguageEN : TMenuItem;
    mioptionsAbout : TMenuItem;
    miOptionsLanguage : TMenuItem;
    miOptionsOptions : TMenuItem;
    miOptions : TMenuItem;
    miProgramClose : TMenuItem;
    miProgram : TMenuItem;
    memReportError : TMemo;
    memReportProgress : TMemo;
    PageControl1 : TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlDataSettings : TPanel;
    pnlProgress : TPanel;
    pnlError : TPanel;
    pnlProgressExport : TPanel;
    pnlDataGrid : TPanel;
    pnlSettings : TPanel;
    pnlQuery : TPanel;
    pnlPrepareData : TPanel;
    pnlPrepareMain : TPanel;
    pnlMainTop : TPanel;
    pnlBottom : TPanel;
    pnlMainAllClient : TPanel;
    ProgressBar: TProgressBar;
    SplitterDataSettings : TSplitter;
    SplitterdataGrid : TSplitter;
    SplitterMemos : TSplitter;
    stbInfo : TStatusBar;
    SynEditSqlQuery : TSynEdit;
    SynSQLSyn1 : TSynSQLSyn;
    tsOracle: TTabSheet;
    tsCsv: TTabSheet;
    tsQuery : TTabSheet;
    tsPrepare : TTabSheet;
    tsExportSettings : TTabSheet;
    procedure btnConnectClick(Sender : TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnExportDbgridToCsvClick(Sender : TObject);
    procedure btnExportToFileCSVClick(Sender: TObject);
    procedure btnExportToFileOraClick(Sender : TObject);
    procedure btnGetDataClick(Sender : TObject);
    procedure btnOpenQueryClick(Sender : TObject);
    procedure btnSaveErrorLogClick(Sender: TObject);
    procedure btnSaveQueryClick(Sender : TObject);
    procedure btnSelectCsvFileClick(Sender: TObject);
    procedure cbDatabaseNameExit(Sender : TObject);
    procedure cbOrganizationNameExit(Sender : TObject);
    procedure cbUserNameExit(Sender : TObject);
    procedure chkIncludeKolkBegindatumChange(Sender: TObject);
    procedure chkIncludeKolkBreedteChange(Sender: TObject);
    procedure chkIncludeKolkDiameterChange(Sender: TObject);
    procedure chkIncludeKolkEinddatumChange(Sender: TObject);
    procedure chkIncludeKolkHoogteChange(Sender: TObject);
    procedure chkIncludeKolkLengteChange(Sender: TObject);
    procedure chkIncludeKolkMateriaalChange(Sender: TObject);
    procedure chkIncludeKolkVormChange(Sender: TObject);
    procedure chkIncludeKolkWanddikteChange(Sender: TObject);
    procedure chkIncludeLeidingBegindatumChange(Sender: TObject);
    procedure chkIncludeLeidingBobBeginChange(Sender: TObject);
    procedure chkIncludeLeidingBobEindChange(Sender: TObject);
    procedure chkIncludeLeidingBreedteChange(Sender: TObject);
    procedure chkIncludeLeidingDiameterChange(Sender: TObject);
    procedure chkIncludeLeidingEinddatumChange(Sender: TObject);
    procedure chkIncludeLeidingFunderingChange(Sender: TObject);
    procedure chkIncludeLeidingHoogteChange(Sender: TObject);
    procedure chkIncludeLeidingLengteChange(Sender: TObject);
    procedure chkIncludeLeidingMateriaalChange(Sender: TObject);
    procedure chkIncludeLeidingStatusFunctionerenChange(Sender: TObject);
    procedure chkIncludeLeidingVormChange(Sender: TObject);
    procedure chkIncludeLeidingWIBONThemaChange(Sender: TObject);
    procedure chkIncludePersleidingBegindatumChange(Sender: TObject);
    procedure chkIncludePersleidingBobBeginChange(Sender: TObject);
    procedure chkIncludePersleidingBobEindChange(Sender: TObject);
    procedure chkIncludePersleidingDiameterChange(Sender: TObject);
    procedure chkIncludePersleidingEinddatumChange(Sender: TObject);
    procedure chkIncludePersleidingHoogteChange(Sender: TObject);
    procedure chkIncludePersleidingLengteChange(Sender: TObject);
    procedure chkIncludePersleidingMateriaalChange(Sender: TObject);
    procedure chkIncludePersleidingStatusFunctionerenChange(Sender: TObject);
    procedure chkIncludePersleidingVormChange(Sender: TObject);
    procedure chkIncludePutBegindatumChange(Sender: TObject);
    procedure chkIncludePutEinddatumChange(Sender: TObject);
    procedure chkIncludePutBreedteChange(Sender: TObject);
    procedure chkIncludePutDiameterChange(Sender: TObject);
    procedure chkIncludePutFunderingChange(Sender: TObject);
    procedure chkIncludePutHoogteChange(Sender: TObject);
    procedure chkIncludePutLengteChange(Sender: TObject);
    procedure chkIncludePutMaaiveldhoogteChange(Sender: TObject);
    procedure chkIncludePutMateriaalChange(Sender: TObject);
    procedure chkIncludePutVormChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlDataSettingsResize(Sender : TObject);
    procedure pnlPrepareDataResize(Sender : TObject);
    procedure pnlProgressExportResize(Sender : TObject);
    procedure SplitterdataGridMoved(Sender : TObject);
    procedure SplitterDataSettingsMoved(Sender : TObject);
    procedure SplitterMemosMoved(Sender : TObject);
  private
    fPresenter: IPresenterMain;
    fSubscriber: IobsSubscriber;

    fCanContinue: Boolean;

    fCurrentCSVDataSource: TDataSource;
    fCurrentCSVDataSet: TCSVDataset;

    function get_Observer: IobsSubscriber;
    function get_Presenter: IPresenterMain;
    function Obj: TObject;
    procedure set_Observer(aValue: IobsSubscriber);
    procedure set_Presenter(aValue: IPresenterMain);
    //
    function CheckLanguageFiles: Boolean;  { Check if there is at least 1 language file present. }
    procedure SetAppLanguage;  { Set the application language. }
    function GetSettingsFile: String;
    procedure CreateDirectories;
    procedure ReadSettings;
    procedure ReadFormState;
    procedure StoreFormstate;
    procedure StartLogging;
    procedure WriteSingleSetting(Setting, Section, aValue: String);
    procedure AddCbListItem(sender: TObject);
    procedure LoadComboBoxItems;
    procedure ExportInProgress(IsInProgress: Boolean);

    { Event handlers }
    procedure miProgramCloseOnClick(Sender : TObject);
    procedure OnFormShow(Sender: TObject);
    procedure OnFormResize(Sender : TObject);
    procedure miOptionsOptionsOnClick(Sender : TObject);
    procedure miOptionsLanguageENOnClick(Sender : TObject);
    procedure miOptionsLanguageNLOnClick(Sender : TObject);
    procedure dbgSewerDataTitleClick(Column: TColumn);
    procedure LoadCsvData(const FileName: String);

    // Testen, Zou moeten voorkomen dat op Linux de componenten verschuiven -- > Werkt niet
    procedure setFormFonts_Default(F: TForm);  { #todo : Doet niets in Linux, gui gaat nog steeds kapot. later naar kijken }

    procedure CleanupCurrentCSVData;
  protected
    procedure DoStaticTexts(Texts: IStrings);           
    procedure DoStatus(anObj: TObject; aData: pointer);
    procedure DoCreateDir({%H-}anObj: TObject; aData: PNewDirectoriesRec);
    procedure DoAppSettings({%H-}anObj: TObject; aData: PSettingsRec);
    procedure DoFormState({%H-}anObj: TObject; Settings: PSettingsRec);
    procedure DoStbPanelWidth(anObj: TObject; aData: pointer);
    procedure DoDbConnection(anObj: TObject; aData: PDbConnectRec);
    procedure DoRetrieveData(anObj: TObject; aData: PRetrieveDataRec);
    procedure DoExportToOroxTtlFile(anObj: TObject; aData: PExportToOroxTtlFileRec);
    procedure DoExportProgress(anObj: TObject; aData: PExportToOroxTtlFileRec);
    procedure DoReportProgressCount(anObj: TObject; aData: pointer);
    procedure DoExportError(anObj: TObject; aData: PExportToOroxTtlFileRec);
    procedure DoUniqueStringlist(anObj: TObject; aData: PUniqueStringlistRec);
    procedure DoSortDbGrid(anObj: TObject; aData: PSortDbGridRec);
    procedure DoRetrieveCSVData(anObj: TObject; aData: PRetrieveCSVDataRec);
    procedure HandleObsNotify(aReason: ptrint; aNotifyObj: TObject; aData: pointer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Observer: IobsSubscriber read get_Observer write set_Observer;
    property Presenter: IPresenterMain read get_Presenter write set_Presenter;
  end;

var
  frmMain: TfrmMain;

implementation
uses obs_prosu, common.consts, common.utils, view.configure;


{$R *.lfm}

{ TfrmMain }
{$Region 'getter/setter'}

procedure TfrmMain.btnConnectClick(Sender : TObject);
var
  lRec: TDbConnectRec;
begin
  { #todo : Eventhandler nog los koppelen }
  Screen.Cursor:= crHourGlass;
  lRec.DatabaseName:= cbDatabaseName.Text;
  lRec.UserName:= cbUserName.Text;
  lRec.SchemaPassword:= edtPassword.Text;
  lRec.HasConnection:= btnConnect.Enabled;  // // If there is already a connection, the button must remain active. So pass True

  fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.StatusbarTexts', 'Connecting'), 0);
  fPresenter.MakeDbConnection(@lRec);
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
var
  lRec: TDbConnectRec;
begin
  lRec.HasConnection:= btnConnect.Enabled;
  fPresenter.DbDisconnect(@lRec);
end;

procedure TfrmMain.btnExportDbgridToCsvClick(Sender : TObject);
var
  csvExporter: TCSVExporter;
  saveDialog: TSaveDialog;
  Success: Boolean;
begin
  csvExporter:= nil;
  saveDialog:= nil;
  Success:= False;
  try
    try
      csvExporter:= TCSVExporter.Create(Nil);
      saveDialog:= TSaveDialog.Create(Nil);

      csvExporter.Dataset:= dbgSewerData.DataSource.DataSet;  // Configureer exporter

      saveDialog.Title:= fPresenter.GetstaticText(UnitName, 'SaveCSVFile');
      saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgCSVFilesFilter');
      saveDialog.DefaultExt:= 'csv';
      {$IFDEF MSWINDOWS}
      saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName;
      {$ENDIF}
      {$IFDEF LINUX}
      saveDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
                               PathDelim + '.config' + PathDelim + ApplicationName;
      {$ENDIF}
      saveDialog.Options:= saveDialog.Options + [ofOverwritePrompt, ofPathMustExist];

      if not saveDialog.Execute then
        Exit; // User has canceled

      // Check if file is available for writing BEFORE we try to export
      if FileExists(saveDialog.FileName) and IsFileInUse(saveDialog.FileName) then
      begin
        MessageDlg(fPresenter.GetstaticText('view.main.statusbartexts', 'CsvFileInUse') + sLineBreak +
                   saveDialog.FileName, mtError, [mbOK], 0); { #todo : Mischien een view.main.messages maken }
        Exit;
      end;

      // Start the export
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'SavingFile'), 0);
      Screen.Cursor:= crHourGlass;

      csvExporter.FileName:= saveDialog.FileName;

      csvExporter.Execute;  // Run the export
      Success:= True;

      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'FileSaved'), 0);
    except
      on E: EFOpenError do
      begin
        // Komt hier nooit want dit wordt al afgevangen met IsFileInUse(saveDialog.FileName).
        MessageDlg(Format(fPresenter.GetstaticText('view.main.statusbartexts', 'FileLockedError'),
                  [saveDialog.FileName]), mtError, [mbOK], 0);
        fPresenter.WriteToLog(UnitName, ltWarning, fPresenter.GetstaticText('view.main.statusbartexts', 'FileLockedError') + ': '+ saveDialog.FileName + ' - ' + E.Message);

      end;
      on E: EStreamError do
      begin
        MessageDlg(Format(fPresenter.GetstaticText('view.main.statusbartexts', 'StreamError'),
                  [E.Message]), mtError, [mbOK], 0);
        fPresenter.WriteToLog(UnitName, ltWarning, fPresenter.GetstaticText('view.main.statusbartexts', 'StreamError') + ': ' + E.Message);
      end;
      on E: Exception do
      begin
        MessageDlg(fPresenter.GetstaticText('view.main.statusbartexts', 'UnexpectedExportError') +
                  sLineBreak + E.ClassName + ': ' + E.Message,
                  mtError, [mbOK], 0);
        fPresenter.WriteToLog(UnitName, ltWarning, fPresenter.GetstaticText('view.main.statusbartexts', 'UnexpectedExportError') + ': ' + E.ClassName + ' - ' + E.Message);
      end;
    end;

  finally
    if Assigned(csvExporter) then csvExporter.Free;
    if Assigned(saveDialog) then saveDialog.Free;
    if Success then fPresenter.SetStatusbarText('', 0);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TfrmMain.btnExportToFileCSVClick(Sender: TObject);
var
  saveDialog: TSaveDialog;
  lTrx: TExportToOroxTtlFileTrx;
  lFileName: String;
  lCanContinue: Boolean;
begin
  lCanContinue:= False;

  if fPresenter.GetSingleSetting('MappingFile') = '' then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, 'MissingMappingFile'), mtWarning, [mbOK],0);
    Exit;
  end;

  if cbOrganizationNameCSV.Text = '' then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'),
               fPresenter.GetstaticText(UnitName, 'OrganizationNameIsBlank') + sLineBreak +
               sLineBreak +
               fPresenter.GetstaticText(UnitName, 'EnterDasetName')
               , mtWarning, [mbOK],0);
    cbOrganizationNameCSV.SetFocus;
    Exit;
  end;

  saveDialog:= TSaveDialog.Create(Nil);
  saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSaveTtlFile');
  saveDialog.DefaultExt:= DefaultOroXFileExt;  // Is prescribed in this way.
  {$IFDEF MSWINDOWS}
  saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adExport;
  {$ENDIF}
  {$IFDEF LINUX}
  saveDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adExport;
  {$ENDIF}

  try
    if saveDialog.Execute then begin
      lFileName:= saveDialog.FileName;
      lCanContinue:= True;
    end;
  finally
    saveDialog.Free;
  end;

  if lCanContinue then begin
    ExportInProgress(lCanContinue);
    try
      Screen.Cursor:= crHourGlass;
      lblProgress.Caption:= fPresenter.GetstaticText(UnitName, 'ExportStarted');
      memReportProgress.Clear;
      memReportError.Clear;

      lTrx:= fPresenter.TrxMan.StartTransaction(prExportToOroxTtlFile) as TExportToOroxTtlFileTrx;

      lTrx.OrganizationName:= cbOrganizationNameCSV.Text;
      lTrx.FileNameExportFile:= lFileName;
      lTrx.FileNameCsvFile:= edtCsvFile.Text;
      lTrx.MappingFile:= fPresenter.GetSingleSetting('MappingFile');
      lTrx.DataType:= dtCSV;
      lTrx.Success:= btnExportToFileOra.Enabled;
      lTrx.Version:= fPresenter.GetSingleSetting('GWSWVersion');

      ProgressBar.Visible:= True;
      fPresenter.TrxMan.CommitTransaction;
      ProgressBar.Visible:= False;
      ExportInProgress(False);
      Screen.Cursor:= crDefault;
    except
      fPresenter.TrxMan.RollbackTransaction;
      ExportInProgress(False);
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorExportToOroxTtlFile'), 0);
    end;
  end;
end;

procedure TfrmMain.btnExportToFileOraClick(Sender : TObject);
var
  saveDialog: TSaveDialog;
  lTrx: TExportToOroxTtlFileTrx;
  lFileName: String;
  lCanContinue: Boolean;
begin
  lCanContinue:= False;

  if fPresenter.GetSingleSetting('MappingFile') = '' then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, 'MissingMappingFile'), mtWarning, [mbOK],0);
    Exit;
  end;

  if dbgSewerData.DataSource = Nil then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, 'QueryNotActive'), mtWarning, [mbOK],0);
    Exit;
  end;
  if SynEditSqlQuery.Lines.Count = 0 then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, 'QueryFileNotLoaded'), mtWarning, [mbOK],0);
    Exit;
  end;

  if cbOrganizationName.Text = '' then begin
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'),
               fPresenter.GetstaticText(UnitName, 'OrganizationNameIsBlank') + sLineBreak +
               sLineBreak +
               fPresenter.GetstaticText(UnitName, 'EnterDasetName')
               , mtWarning, [mbOK],0);
    cbOrganizationName.SetFocus;
    Exit;
  end;

  saveDialog:= TSaveDialog.Create(Nil);
  saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSaveTtlFile');
  saveDialog.DefaultExt:= DefaultOroXFileExt;  // Is prescribed in this way.
  {$IFDEF MSWINDOWS}
  saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adExport;
  {$ENDIF}
  {$IFDEF LINUX}
  saveDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adExport;
  {$ENDIF}

  try
    if saveDialog.Execute then begin
      lFileName:= saveDialog.FileName;
      lCanContinue:= True;
    end;
  finally
    saveDialog.Free;
  end;

  if lCanContinue then begin
    ExportInProgress(lCanContinue);
    try
      Screen.Cursor:= crHourGlass;
      lblProgress.Caption:= fPresenter.GetstaticText(UnitName, 'ExportStarted');
      memReportProgress.Clear;
      memReportError.Clear;

      lTrx:= fPresenter.TrxMan.StartTransaction(prExportToOroxTtlFile) as TExportToOroxTtlFileTrx;

      lTrx.OrganizationName:= cbOrganizationName.Text;
      lTrx.FileNameExportFile:= lFileName;
      lTrx.MappingFile:= fPresenter.GetSingleSetting('MappingFile');
      lTrx.Success:= btnExportToFileOra.Enabled;
      lTrx.Version:= fPresenter.GetSingleSetting('GWSWVersion');
      lTrx.DataType:= dtORA;

      ProgressBar.Visible:= True;
      fPresenter.TrxMan.CommitTransaction;
      ProgressBar.Visible:= False;
      ExportInProgress(False);
      Screen.Cursor:= crDefault;
    except
      fPresenter.TrxMan.RollbackTransaction;
      ExportInProgress(False);
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorExportToOroxTtlFile'), 0);
    end;
  end;
end;

procedure TfrmMain.btnGetDataClick(Sender : TObject);
var
  lTrx: TRetrieveDataTrx;
begin
  Screen.Cursor:= crHourGlass;
  fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'RetrievingData'), 0);

  lTrx:= fPresenter.TrxMan.StartTransaction(prRetrieveData) as TRetrieveDataTrx;
  try
    lTrx.DataGrid:= dbgSewerData;
    lTrx.DataSource:= Nil;
    lTrx.SqlText:= SynEditSqlQuery.Text;
    lTrx.OrganizationName:= cbOrganizationName.Text;

    fPresenter.TrxMan.CommitTransaction;
    fPresenter.SetStatusbarText('', 0);
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(FPresenter.GetstaticText('view.main.statusbartexts', 'ErrorRetrieveData'), 0);
  end;
end;

procedure TfrmMain.btnOpenQueryClick(Sender : TObject);
var
  openDialog: TOpenDialog;
begin
  { #todo : Make this MVP proof }
  openDialog:= TOpenDialog.Create(Nil);

  openDialog.Title:= fPresenter.GetstaticText(UnitName, 'OpenSqlFile');
  openDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSqlFilesFilter');
  openDialog.DefaultExt:= 'sql';
  {$IFDEF MSWINDOWS}
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adQueries;
  {$ENDIF}
  {$IFDEF LINUX}
  openDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adQueries;
  {$ENDIF}

  openDialog.Options:= openDialog.Options + [ofFileMustExist];

  try
    if openDialog.Execute then begin
      SynEditSqlQuery.Lines.LoadFromFile(openDialog.FileName);
      SynEditSqlQuery.Modified:= False;
      WriteSingleSetting('SQLfileLocation', 'Configure', openDialog.FileName);  // Immediately put the open file in the ini.
      ReadSettings;  /// and update model.main fSettings
      lblSQLFileLocation.Caption:= openDialog.FileName;
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TfrmMain.btnSaveErrorLogClick(Sender: TObject);
var
  saveDialog: TSaveDialog;
begin
  { #todo : Make this MVP proof }
  if memReportError.Lines.Count > 0 then begin
    saveDialog:= TSaveDialog.Create(Nil);
    try
      saveDialog.Title:= fPresenter.GetstaticText(UnitName, 'SaveErrorLogFile');
      saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSaveErrorLogFilesFilter');
      saveDialog.DefaultExt:= 'txt';
      {$IFDEF MSWINDOWS}
      saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adQueries;
      {$ENDIF}
      {$IFDEF LINUX}
      saveDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adQueries;
      {$ENDIF}
      saveDialog.Options:= saveDialog.Options + [ofOverwritePrompt];

      if saveDialog.Execute then begin
        memReportError.Lines.SaveToFile(saveDialog.FileName);
      end;
    finally
      saveDialog.Free;
    end;
  end
  else
    messageDlg(fPresenter.GetstaticText(UnitName, 'Information'), fPresenter.GetstaticText(UnitName, 'NoErrorReportPresent'), mtInformation, [mbOK],0);
end;

procedure TfrmMain.btnSaveQueryClick(Sender : TObject);
var
  saveDialog: TSaveDialog;
begin
  { #todo : Make this MVP proof }
  saveDialog:= TSaveDialog.Create(Nil);
  try
    saveDialog.Title:= fPresenter.GetstaticText(UnitName, 'SaveSqlFile');
    saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSqlFilesFilter');
    saveDialog.DefaultExt:= 'sql';
    {$IFDEF MSWINDOWS}
    saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adQueries;
    {$ENDIF}
    {$IFDEF LINUX}
    saveDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adQueries;
    {$ENDIF}


    saveDialog.Options:= saveDialog.Options + [ofOverwritePrompt];

    if saveDialog.Execute then begin
      SynEditSqlQuery.Lines.SaveToFile(saveDialog.FileName);
      WriteSingleSetting('SQLfileLocation','Configure',  saveDialog.FileName);  // Save the location in the settings file as well.
      ReadSettings;  // and update model.main fSettings
      lblSQLFileLocation.Caption:= saveDialog.FileName;
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TfrmMain.btnSelectCsvFileClick(Sender: TObject);
var
  openDialog: TOpenDialog;
begin
  { #todo : Make this MVP proof }
  openDialog:= TOpenDialog.Create(Nil);

  openDialog.Title:= fPresenter.GetstaticText(UnitName, 'SelectCsvFile');
  openDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgCSVFilesFilter');
  openDialog.DefaultExt:= 'csv';
  {$IFDEF MSWINDOWS}
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName;
  {$ENDIF}
  {$IFDEF LINUX}
  openDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName;
  {$ENDIF}
  openDialog.Options:= openDialog.Options + [ofFileMustExist];

  try
    if openDialog.Execute then begin
      edtCsvFile.Text:= openDialog.FileName;
      btnExportToFileOra.Enabled:= True;

      // Data naar de dbgrid halen
      LoadCsvData(openDialog.FileName);
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TfrmMain.cbDatabaseNameExit(Sender : TObject);
begin
  AddCbListItem(Sender);
end;

procedure TfrmMain.cbOrganizationNameExit(Sender : TObject);
begin
  AddCbListItem(Sender);
  if cbOrganizationName.Text <> '' then
    WriteSingleSetting('LastUsedOrganization','Configure',  cbOrganizationName.Text);
end;

procedure TfrmMain.cbUserNameExit(Sender : TObject);
begin
  AddCbListItem(Sender);
end;

procedure TfrmMain.chkIncludeKolkBegindatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkBegindatum', 'Export',  BoolToStr(chkIncludeKolkBegindatum.Checked));
end;

procedure TfrmMain.chkIncludeKolkBreedteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkBreedte', 'Export',  BoolToStr(chkIncludeKolkBreedte.Checked));
end;

procedure TfrmMain.chkIncludeKolkDiameterChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkDiameter', 'Export',  BoolToStr(chkIncludeKolkDiameter.Checked));
end;

procedure TfrmMain.chkIncludeKolkEinddatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkEinddatum', 'Export',  BoolToStr(chkIncludeKolkEinddatum.Checked));
end;

procedure TfrmMain.chkIncludeKolkHoogteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkHoogte', 'Export',  BoolToStr(chkIncludeKolkHoogte.Checked));
end;

procedure TfrmMain.chkIncludeKolkLengteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkLengte', 'Export',  BoolToStr(chkIncludeKolkLengte.Checked));
end;

procedure TfrmMain.chkIncludeKolkMateriaalChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkMateriaal', 'Export',  BoolToStr(chkIncludeKolkMateriaal.Checked));
end;

procedure TfrmMain.chkIncludeKolkVormChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkVorm', 'Export',  BoolToStr(chkIncludeKolkVorm.Checked));
end;

procedure TfrmMain.chkIncludeKolkWanddikteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeKolkWanddikte', 'Export',  BoolToStr(chkIncludeKolkWanddikte.Checked));
end;

procedure TfrmMain.chkIncludeLeidingBegindatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingBegindatum', 'Export',  BoolToStr(chkIncludeLeidingBegindatum.Checked));
end;

procedure TfrmMain.chkIncludeLeidingBobBeginChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingBobBegin', 'Export',  BoolToStr(chkIncludeLeidingBobBegin.Checked));
end;

procedure TfrmMain.chkIncludeLeidingBobEindChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingBobEind', 'Export',  BoolToStr(chkIncludeLeidingBobEind.Checked));
end;

procedure TfrmMain.chkIncludeLeidingBreedteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingBreedte', 'Export',  BoolToStr(chkIncludeLeidingBreedte.Checked));
end;

procedure TfrmMain.chkIncludeLeidingDiameterChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingDiameter', 'Export',  BoolToStr(chkIncludeLeidingDiameter.Checked));
end;

procedure TfrmMain.chkIncludeLeidingEinddatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingEinddatum', 'Export',  BoolToStr(chkIncludeLeidingEinddatum.Checked));
end;

procedure TfrmMain.chkIncludeLeidingFunderingChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingFundering', 'Export',  BoolToStr(chkIncludeLeidingFundering.Checked));
end;

procedure TfrmMain.chkIncludeLeidingHoogteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingHoogte', 'Export',  BoolToStr(chkIncludeLeidingHoogte.Checked));
end;

procedure TfrmMain.chkIncludeLeidingLengteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingLengte', 'Export',  BoolToStr(chkIncludeLeidingLengte.Checked));
end;

procedure TfrmMain.chkIncludeLeidingMateriaalChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingMateriaal', 'Export',  BoolToStr(chkIncludeLeidingMateriaal.Checked));
end;

procedure TfrmMain.chkIncludeLeidingStatusFunctionerenChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingStatusFunctioneren', 'Export',  BoolToStr(chkIncludeLeidingStatusFunctioneren.Checked));
end;

procedure TfrmMain.chkIncludeLeidingVormChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingVorm', 'Export',  BoolToStr(chkIncludeLeidingVorm.Checked));
end;

procedure TfrmMain.chkIncludeLeidingWIBONThemaChange(Sender: TObject);
begin
  WriteSingleSetting('IncludeLeidingWIBONThema', 'Export',  BoolToStr(chkIncludeLeidingWIBONThema.Checked));
end;

procedure TfrmMain.chkIncludePersleidingBegindatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingBegindatum', 'Export',  BoolToStr(chkIncludePersleidingBegindatum.Checked));
end;

procedure TfrmMain.chkIncludePersleidingBobBeginChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingBobBegin', 'Export',  BoolToStr(chkIncludePersleidingBobBegin.Checked));
end;

procedure TfrmMain.chkIncludePersleidingBobEindChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingBobEind', 'Export',  BoolToStr(chkIncludePersleidingBobEind.Checked));
end;

procedure TfrmMain.chkIncludePersleidingDiameterChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingDiameter', 'Export',  BoolToStr(chkIncludePersleidingDiameter.Checked));
end;

procedure TfrmMain.chkIncludePersleidingEinddatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingEinddatum', 'Export',  BoolToStr(chkIncludePersleidingEinddatum.Checked));
end;

procedure TfrmMain.chkIncludePersleidingHoogteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingHoogte', 'Export',  BoolToStr(chkIncludePersleidingHoogte.Checked));
end;

procedure TfrmMain.chkIncludePersleidingLengteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingLengte', 'Export',  BoolToStr(chkIncludePersleidingLengte.Checked));
end;

procedure TfrmMain.chkIncludePersleidingMateriaalChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingMateriaal', 'Export',  BoolToStr(chkIncludePersleidingMateriaal.Checked));
end;

procedure TfrmMain.chkIncludePersleidingStatusFunctionerenChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingStatusFunctioneren', 'Export',  BoolToStr(chkIncludePersleidingStatusFunctioneren.Checked));
end;

procedure TfrmMain.chkIncludePersleidingVormChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePersleidingVorm', 'Export',  BoolToStr(chkIncludePersleidingVorm.Checked));
end;

procedure TfrmMain.chkIncludePutBegindatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutBegindatum', 'Export', BoolToStr(chkIncludePutBegindatum.Checked));
end;

procedure TfrmMain.chkIncludePutEinddatumChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutEinddatum', 'Export', BoolToStr(chkIncludePutEinddatum.Checked));
end;

procedure TfrmMain.chkIncludePutBreedteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutBreedte', 'Export', BoolToStr(chkIncludePutBreedte.Checked));
end;

procedure TfrmMain.chkIncludePutDiameterChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutDiameter', 'Export',  BoolToStr(chkIncludePutDiameter.Checked));
end;

procedure TfrmMain.chkIncludePutFunderingChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutFundering', 'Export',  BoolToStr(chkIncludePutFundering.Checked));
end;

procedure TfrmMain.chkIncludePutHoogteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutHoogte', 'Export',  BoolToStr(chkIncludePutHoogte.Checked));
end;

procedure TfrmMain.chkIncludePutLengteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutLengte', 'Export',  BoolToStr(chkIncludePutLengte.Checked));
end;

procedure TfrmMain.chkIncludePutMaaiveldhoogteChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutMaaiveldhoogte',  'Export', BoolToStr(chkIncludePutMaaiveldhoogte.Checked));
end;

procedure TfrmMain.chkIncludePutMateriaalChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutMateriaal',  'Export', BoolToStr(chkIncludePutMateriaal.Checked));
end;

procedure TfrmMain.chkIncludePutVormChange(Sender: TObject);
begin
  WriteSingleSetting('IncludePutVorm',  'Export', BoolToStr(chkIncludePutVorm.Checked));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  setFormFonts_Default(frmMain);
end;

procedure TfrmMain.pnlDataSettingsResize(Sender : TObject);
begin
  (sender as TPanel).Repaint;
end;

procedure TfrmMain.pnlPrepareDataResize(Sender : TObject);
begin
  dbgSewerData.BeginUpdate;  // kijken of dat helpt voor de vertraging bij splitter wijziging
  (sender as TPanel).Repaint;
  dbgSewerData.EndUpdate();
end;

procedure TfrmMain.pnlProgressExportResize(Sender : TObject);
begin
  dbgSewerData.BeginUpdate;
  (sender as TPanel).Repaint;
  dbgSewerData.EndUpdate();
end;

procedure TfrmMain.SplitterdataGridMoved(Sender : TObject);
begin
  WriteSingleSetting('SplitterdataGrid','Configure',  pnlDataGrid.Height.ToString);
end;

procedure TfrmMain.SplitterDataSettingsMoved(Sender : TObject);
begin
  WriteSingleSetting('SplitterDataSettings','Configure',  pnlDataSettings.Width.ToString);
end;

procedure TfrmMain.SplitterMemosMoved(Sender : TObject);
begin
  WriteSingleSetting('SplitterMemos', 'Configure', pnlProgress.Width.ToString);
end;

function TfrmMain.get_Observer: IobsSubscriber;
begin
  Result:= fSubscriber;
end;

function TfrmMain.get_Presenter: IPresenterMain;
begin
  Result:= fPresenter;
end;

function TfrmMain.Obj: TObject;
begin
  Result:= Self;
end;

procedure TfrmMain.set_Observer(aValue: IobsSubscriber);
begin
  if aValue <> nil then begin
    if fSubscriber <> nil then begin // trying to avoid dangling refs
      if Assigned(fPresenter) then fPresenter.Provider.UnSubscribe(fSubscriber);
      fSubscriber.Obj.Free;
    end;
    fSubscriber:= aValue;
    if Assigned(fSubscriber) then begin
      fSubscriber.SetUpdateSubscriberMethod(@HandleObsNotify);
      if Assigned(fPresenter) then fPresenter.Provider.Subscribe(fSubscriber);
    end;
  end; /// we don't care for nil 'cause we need the observer/subscriber to function at all!
end;

procedure TfrmMain.set_Presenter(aValue: IPresenterMain);
begin
  if aValue <> nil then begin
    if Assigned(fPresenter) then begin
      fPresenter.Provider.UnSubscribe(fSubscriber); { no dangling subscriptions }
      fPresenter.Obj.Free;
    end;
    fPresenter:= aValue;
    fPresenter.Provider.Subscribe(fSubscriber);
    fPresenter.GetStaticTexts(UnitName); { important startup-code }
    // etc...
  end else begin //aValue = nil
    if Assigned(fPresenter) then fPresenter.Obj.Free;
    fPresenter:= aValue; // ~ nil
  end;
end;
{$EndRegion 'getter/setter'}

{$Region 'subscriber-events'}
procedure TfrmMain.DoStaticTexts(Texts: IStrings);
var
  i: integer;
  lc: TComponent;
begin
  Caption:= Texts.Values[Name];

  for i:= 0 to ComponentCount-1 do begin
    lc:= Components[i];
    if (lc is TMenuItem) then
      TMenuItem(lc).Caption:= Texts.Values[TMenuItem(lc).Name]
    else if (lc is TButton) then
      TButton(lc).Caption:= Texts.Values[TButton(lc).Name]
    else if (lc is TPanel) then  // Remove the TPanel captions. In the development environment you see captions, but in the binary they are gone. Unless they are in a "mvp" text file.
      TPanel(lc).Caption:= Texts.Values[TPanel(lc).Name]
    else if (lc is TLabel) then
      TLabel(lc).Caption:= Texts.Values[TLabel(lc).Name]
    else if (lc is TTabSheet) then
      TTabSheet(lc).Caption:= Texts.Values[TTabSheet(lc).Name]
    else if (lc is TCheckBox) then
      TCheckBox(lc).Caption:= Texts.Values[TCheckBox(lc).Name]
    else if (lc is TMemo) then
      TMemo(lc).Caption:= Texts.Values[TMemo(lc).Name]
    else if (lc is TBitBtn) then
      TBitBtn(lc).Caption:= Texts.Values[TBitBtn(lc).Name]
    else if (lc is TGroupBox) then
      TGroupBox(lc).Caption:= Texts.Values[TGroupBox(lc).Name]
    else if (lc is TRadioButton) then
      TRadioButton(lc).Caption:= Texts.Values[TRadioButton(lc).Name]
    {No Edit or combo boxes here. The text box will be emptied. This is inconvenient when changing languages.}
  end;
end;

procedure TfrmMain.DoStatus(anObj: TObject; aData: pointer);
begin { in case of extended status object present }
  if anObj <> nil then begin
    with PStatusbarPanelText(aData)^ do begin
      if stbActivePanel <> 2 then begin
        stbInfo.Panels[stbActivePanel].Text:= stbPanelText;
      end
      else begin // last panel, holds the db file name.
        { #todo : Het geopende mappingsbestand hier benoemen. }
        //stbInfo.Panels[stbActivePanel].Text:= fPresenter.GetstaticText('view.main.statusbartexts', 'File') + stbPanelText + '      ';  // Add spaces. Because the last panel is aligned to the right, part of the text will otherwise be lost. Then falls behind the grib.
      end;
      Application.ProcessMessages;
    end;
  end
end;

procedure TfrmMain.DoCreateDir(anObj : TObject; aData : PNewDirectoriesRec);
begin
  With PNewDirectoriesRec(aData)^ do begin
    fCanContinue:= dirSucces;
    if not DirIsWriteable then begin
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.StatusbarTexts', 'DirIsNotWriteable'), 0);
    end;
  end;
end;

procedure TfrmMain.DoAppSettings(anObj : TObject; aData : PSettingsRec);
begin
  With PSettingsRec(aData)^ do begin
    if not setSucces then begin
      if setMessage <> '' then begin
        fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.StatusbarTexts', setMessage), 0);
      end;
      // No FCanContinue is needed here. If there is no settingsfile then default settings are used.
    end
    else begin
      if setReadSettings then begin
        if (setLanguage = 'en') or (setLanguage = '') then begin
          miOptionsLanguageEN.Checked:= True;
          miOptionsLanguageNL.Checked:= False;
        end
        else if setLanguage = 'nl' then begin
          miOptionsLanguageEN.Checked:= False;
          miOptionsLanguageNL.Checked:= True;
        end;
        pnlDataSettings.Width:= setSplitterDataSettings;
        pnlDataGrid.Height:= setSplitterdataGrid;
        pnlProgress.Width:= setSplitterMemos;

        if setDbGridRowHighlight then
          dbgSewerData.Options:= dbgSewerData.Options + [dgRowHighlight]
        else
          dbgSewerData.Options:= dbgSewerData.Options - [dgRowHighlight];

        // Put
        chkIncludePutLengte.OnChange:= Nil;
        chkIncludePutBreedte.OnChange:= Nil;
        chkIncludePutHoogte.OnChange:= Nil;
        chkIncludePutDiameter.OnChange:= Nil;
        chkIncludePutVorm.OnChange:= Nil;
        chkIncludePutMateriaal.OnChange:= Nil;
        chkIncludePutFundering.OnChange:= Nil;
        chkIncludePutBegindatum.OnChange:= Nil;
        chkIncludePutEinddatum.OnChange:= Nil;
        chkIncludePutMaaiveldhoogte.OnChange:= Nil;

        chkIncludePutLengte.Checked:= setIncludePutLengte;
        chkIncludePutBreedte.Checked:= setIncludePutBreedte;
        chkIncludePutHoogte.Checked:= setIncludePutHoogte;
        chkIncludePutDiameter.Checked:= setIncludePutDiameter;
        chkIncludePutVorm.Checked:= setIncludePutVorm;
        chkIncludePutMateriaal.Checked:= setIncludePutMateriaal;
        chkIncludePutFundering.Checked:= setIncludePutFundering;
        chkIncludePutBegindatum.Checked:= setIncludePutBegindatum;
        chkIncludePutEinddatum.Checked:= setIncludePutEinddatum;
        chkIncludePutMaaiveldhoogte.Checked:= setIncludePutMaaiveldhoogte;

        chkIncludePutLengte.OnChange:= @chkIncludePutLengteChange;
        chkIncludePutBreedte.OnChange:= @chkIncludePutBreedteChange;
        chkIncludePutHoogte.OnChange:= @chkIncludePutHoogteChange;
        chkIncludePutVorm.OnChange:= @chkIncludePutVormChange;
        chkIncludePutDiameter.OnChange:= @chkIncludePutDiameterChange;
        chkIncludePutMateriaal.OnChange:= @chkIncludePutMateriaalChange;
        chkIncludePutFundering.OnChange:= @chkIncludePutFunderingChange;
        chkIncludePutBegindatum.OnChange:= @chkIncludePutBegindatumChange;
        chkIncludePutEinddatum.OnChange:= @chkIncludePutEinddatumChange;
        chkIncludePutMaaiveldhoogte.OnChange:= @chkIncludePutMaaiveldhoogteChange;

        // Leiding
        chkIncludeLeidingLengte.OnChange:= Nil;
        chkIncludeLeidingBreedte.OnChange:= Nil;
        chkIncludeLeidingHoogte.OnChange:= Nil;
        chkIncludeLeidingDiameter.OnChange:= Nil;
        chkIncludeLeidingVorm.OnChange:= Nil;
        chkIncludeLeidingMateriaal.OnChange:= Nil;
        chkIncludeLeidingFundering.OnChange:= Nil;
        chkIncludeLeidingStatusFunctioneren.OnChange:= Nil;
        chkIncludeLeidingWIBONThema.OnChange:= Nil;
        chkIncludeLeidingBegindatum.OnChange:= Nil;
        chkIncludeLeidingEinddatum.OnChange:= Nil;
        chkIncludeLeidingBobBegin.OnChange:= Nil;
        chkIncludeLeidingBobEind.OnChange:= Nil;

        chkIncludeLeidingLengte.Checked:= setIncludeLeidingLengte;
        chkIncludeLeidingBreedte.Checked:= setIncludeLeidingBreedte;
        chkIncludeLeidingHoogte.Checked:= setIncludeLeidingHoogte;
        chkIncludeLeidingDiameter.Checked:= setIncludeLeidingDiameter;
        chkIncludeLeidingVorm.Checked:= setIncludeLeidingVorm;
        chkIncludeLeidingMateriaal.Checked:= setIncludeLeidingMateriaal;
        chkIncludeLeidingFundering.Checked:= setIncludeLeidingFundering;
        chkIncludeLeidingStatusFunctioneren.Checked:= setIncludeLeidingStatusFunctioneren;
        chkIncludeLeidingWIBONThema.Checked:= setIncludeLeidingWIBONThema;
        chkIncludeLeidingBegindatum.Checked:= setIncludeLeidingBegindatum;
        chkIncludeLeidingEinddatum.Checked:= setIncludeLeidingEinddatum;
        chkIncludeLeidingBobBegin.Checked:= setIncludeLeidingBobBegin;
        chkIncludeLeidingBobEind.Checked:= setIncludeLeidingBobEind;

        chkIncludeLeidingLengte.OnChange:= @chkIncludeLeidingLengteChange;
        chkIncludeLeidingBreedte.OnChange:= @chkIncludeLeidingBreedteChange;
        chkIncludeLeidingHoogte.OnChange:= @chkIncludeLeidingHoogteChange;
        chkIncludeLeidingDiameter.OnChange:= @chkIncludeLeidingDiameterChange;
        chkIncludeLeidingVorm.OnChange:= @chkIncludeLeidingVormChange;
        chkIncludeLeidingMateriaal.OnChange:= @chkIncludeLeidingMateriaalChange;
        chkIncludeLeidingFundering.OnChange:= @chkIncludeLeidingFunderingChange;
        chkIncludeLeidingStatusFunctioneren.OnChange:= @chkIncludeLeidingStatusFunctionerenChange;
        chkIncludeLeidingWIBONThema.OnChange:= @chkIncludeLeidingWIBONThemaChange;
        chkIncludeLeidingBegindatum.OnChange:= @chkIncludeLeidingBegindatumChange;
        chkIncludeLeidingEinddatum.OnChange:= @chkIncludeLeidingEinddatumChange;
        chkIncludeLeidingBobBegin.OnChange:= @chkIncludeLeidingBobBeginChange;
        chkIncludeLeidingBobEind.OnChange:= @chkIncludeLeidingBobEindChange;

        // export Persleiding
        chkIncludePersleidingLengte.OnChange:= Nil;
        chkIncludePersleidingHoogte.OnChange:= Nil;
        chkIncludePersleidingDiameter.OnChange:= Nil;
        chkIncludePersleidingVorm.OnChange:= Nil;
        chkIncludePersleidingMateriaal.OnChange:= Nil;
        chkIncludePersleidingStatusFunctioneren.OnChange:= Nil;
        chkIncludePersleidingBegindatum.OnChange:= Nil;
        chkIncludePersleidingEinddatum.OnChange:= Nil;
        chkIncludePersleidingBobBegin.OnChange:= Nil;
        chkIncludePersleidingBobEind.OnChange:= Nil;

        chkIncludePersleidingLengte.Checked:= setIncludePersleidingLengte;
        chkIncludePersleidingHoogte.Checked:= setIncludePersleidingHoogte;
        chkIncludePersleidingDiameter.Checked:= setIncludePersleidingDiameter;
        chkIncludePersleidingVorm.Checked:= setIncludePersleidingVorm;
        chkIncludePersleidingMateriaal.Checked:= setIncludePersleidingMateriaal;
        chkIncludePersleidingStatusFunctioneren.Checked:= setIncludePersleidingStatusFunctioneren;
        chkIncludePersleidingBegindatum.Checked:= setIncludePersleidingBegindatum;
        chkIncludePersleidingEinddatum.Checked:= setIncludePersleidingEinddatum;
        chkIncludePersleidingBobBegin.Checked:= setIncludePersleidingBobBegin;
        chkIncludePersleidingBobEind.Checked:= setIncludePersleidingBobEind;

        chkIncludePersleidingLengte.OnChange:= @chkIncludePersleidingLengteChange;
        chkIncludePersleidingHoogte.OnChange:= @chkIncludePersleidingHoogteChange;
        chkIncludePersleidingDiameter.OnChange:= @chkIncludePersleidingDiameterChange;
        chkIncludePersleidingVorm.OnChange:= @chkIncludePersleidingVormChange;
        chkIncludePersleidingMateriaal.OnChange:= @chkIncludePersleidingMateriaalChange;
        chkIncludePersleidingStatusFunctioneren.OnChange:= @chkIncludePersleidingStatusFunctionerenChange;
        chkIncludePersleidingBegindatum.OnChange:= @chkIncludePersleidingBegindatumChange;
        chkIncludePersleidingEinddatum.OnChange:= @chkIncludePersleidingEinddatumChange;
        chkIncludePersleidingBobBegin.OnChange:= @chkIncludePersleidingBobBeginChange;
        chkIncludePersleidingBobEind.OnChange:= @chkIncludePersleidingBobEindChange;

        // Export Kolk
        chkIncludeKolkLengte.OnChange:= Nil;
        chkIncludeKolkBreedte.OnChange:= Nil;
        chkIncludeKolkHoogte.OnChange:= Nil;
        chkIncludeKolkDiameter.OnChange:= Nil;
        chkIncludeKolkVorm.OnChange:= Nil;
        chkIncludeKolkMateriaal.OnChange:= Nil;
        chkIncludeKolkWanddikte.OnChange:= Nil;
        chkIncludeKolkBegindatum.OnChange:= Nil;
        chkIncludeKolkEinddatum	.OnChange:= Nil;

        chkIncludeKolkLengte.Checked:= setIncludeKolkLengte;
        chkIncludeKolkBreedte.Checked:= setIncludeKolkBreedte;
        chkIncludeKolkHoogte.Checked:= setIncludeKolkHoogte;
        chkIncludeKolkDiameter.Checked:= setIncludeKolkDiameter;
        chkIncludeKolkVorm.Checked:= setIncludeKolkVorm;
        chkIncludeKolkMateriaal.Checked:= setIncludeKolkMateriaal;
        chkIncludeKolkWanddikte.Checked:= setIncludeKolkWanddikte;
        chkIncludeKolkBegindatum.Checked:= setIncludeKolkBegindatum;
        chkIncludeKolkEinddatum	.Checked:= setIncludeKolkEinddatum;

        chkIncludeKolkLengte.OnChange:= @chkIncludeKolkLengteChange;
        chkIncludeKolkBreedte.OnChange:= @chkIncludeKolkBreedteChange;
        chkIncludeKolkHoogte.OnChange:= @chkIncludeKolkHoogteChange;
        chkIncludeKolkDiameter.OnChange:= @chkIncludeKolkDiameterChange;
        chkIncludeKolkVorm.OnChange:= @chkIncludeKolkVormChange;
        chkIncludeKolkMateriaal.OnChange:= @chkIncludeKolkMateriaalChange;
        chkIncludeKolkWanddikte.OnChange:= @chkIncludeKolkWanddikteChange;
        chkIncludeKolkBegindatum.OnChange:= @chkIncludeKolkBegindatumChange;
        chkIncludeKolkEinddatum	.OnChange:= @chkIncludeKolkEinddatumChange;
      end;
    end;
  end;
end;

procedure TfrmMain.DoFormState(anObj : TObject; Settings : PSettingsRec);
var
  lastWindowState: TWindowstate;
begin
  with Settings^ do begin
    if (setReadFormState) and (setFrmName = UnitName) then begin
      lastWindowState:= TWindowState(Settings^.setFrmWindowState);

      if lastWindowState = wsMaximized then begin
        BoundsRect:= Bounds(
          Settings^.setFrmRestoredLeft,
          Settings^.setFrmRestoredTop,
          Settings^.setFrmRestoredWidth,
          Settings^.setFrmRestoredHeight);

        WindowState:= wsMaximized;
      end
      else begin
        WindowState:= wsNormal;
        BoundsRect:= Bounds(
          Settings^.setFrmLeft,
          Settings^.setFrmTop,
          Settings^.setFrmWidth,
          Settings^.setFrmHeight);

        BoundsRect:= CheckFormIsEntireVisible(BoundsRect);
      end;
    end;
  end;
end;

procedure TfrmMain.DoStbPanelWidth(anObj : TObject; aData : pointer);
begin
  with PStbPanelsSize(aData)^ do begin
    TStatusbar(anObj).Panels[1].Width:= mpWidth;
  end;
end;

procedure TfrmMain.DoDbConnection(anObj: TObject; aData : PDbConnectRec);
var
  lMsg: string;
  Parts: TStringArray;
  i: Integer;
begin
  With aData^ do begin
    btnGetData.Enabled:= HasConnection;
    if HasConnection then
      btnDisconnect.Enabled:= True
    else begin
      btnDisconnect.Enabled:= False;
      fPresenter.SetStatusbarText('', 0);
      memReportProgress.Clear;
      memReportError.Clear;
    end;

    if Message = 'Success' then begin
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'DbConnEstablished'), 0);
    end
    else begin
      Parts:= aData^.Message.Split('|');
      lMsg:='';

      if Length(Parts) > 1 then begin  // Errors only (for now)
        for i:= Low(Parts) to High(Parts) do begin
          lMsg:= lMsg + fPresenter.GetstaticText('view.main.statusbartexts', Parts[i]);  // Build the string
        end;
        fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', lMsg), 0);
      end;

    end;
  end;
  Screen.Cursor:= crDefault;
end;

procedure TfrmMain.DoRetrieveData(anObj: TObject; aData : PRetrieveDataRec);
var
  lDbGrid: TDBGrid;
  i: Integer;
begin
  if (aData = Nil) or (anObj = Nil) then Exit;

  With aData^ do begin
    if anObj <> Nil then begin
      lDbGrid:= TDBGrid(anObj);
      lDbGrid.BeginUpdate;

      lDbGrid.DataSource:= TDataSource(aData^.DataSource);
      DBNavigator.DataSource:= TDataSource(aData^.DataSource);

      for i:= 0 to lDbGrid.Columns.Count -1 do begin
        lDbGrid.Columns[i].Width:= lDbGrid.Columns[i].Width + ExtraColumnWidthForImage;
      end;

      lDbGrid.EndUpdate(True);
    end;
    if Message <> '' then
    messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, Message), mtWarning, [mbOK],0);

    btnExportToFileOra.Enabled:= Success;
    btnExportDbgridToCsv.Enabled:= Success;
  end;
  Screen.Cursor:= crDefault;
end;

procedure TfrmMain.DoExportToOroxTtlFile(anObj : TObject; aData : PExportToOroxTtlFileRec);
var
  OpenFile: Boolean;
begin
  if aData = Nil then Exit;

  With aData^ do begin
    fPresenter.SetStatusbarText( fPresenter.GetstaticText('view.main', aData^.Message), 0);
    memReportError.Lines.Add('');
    if Success then begin
      OpenFile:= FPresenter.Model.GetSetting_AskToOpenExportFile;  // Export is ready then aks to open file
      if OpenFile then begin
        if MessageDlg(fPresenter.GetstaticText(UnitName, 'OpenFile'), fPresenter.GetstaticText(UnitName, 'ExportIsCompleteOpenFile'), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          //ShellExecute(Handle, 'open', PChar(FileNameExportFile), nil, nil, SW_SHOWNORMAL);   // uses: ShellAPI
          OpenDocument(aData^.FileNameExportFile);// OpenDocument is een cross-platform functie uit de LCLIntf unit. gebruikt op Windows ShellExecute en op Linux xdg-open
        end;
      end
      else
        messageDlg(fPresenter.GetstaticText(UnitName, 'Information'),
                   fPresenter.GetstaticText(UnitName, 'ExportIsCompleted') +
                   sLineBreak +
                   '(' + FileNameExportFile + ').'
                   , mtInformation, [mbOK],0);
    end;
  end;
end;

procedure TfrmMain.DoExportProgress(anObj : TObject; aData : PExportToOroxTtlFileRec);
var
  Msg: string;
  Parts: TStringArray;
  i: Integer;
  lMsg: string;
begin
  if adata = Nil then Exit;

  Msg:= PChar(aData);
  Parts:= Msg.Split('|');
  lMsg:= '';

  if Assigned(memReportProgress) then begin
    for i:= Low(Parts) to High(Parts) do begin
      lMsg:= lMsg + fPresenter.GetstaticText('view.main', Parts[i]); // Build the string
    end;

    memReportProgress.Lines.Add(FormatDateTime('dd-mm-yyyy hh:nn:ss', Now) + ' ' + lMsg);
    // "Auto-scroll"
    memReportProgress.SelStart:= Length(memReportProgress.Text);
    memReportProgress.SelLength:= 0;
  end;

  fPresenter.SetStatusbarText(lMsg, 0);

  // Forceer UI update
//  Application.ProcessMessages;
end;

procedure TfrmMain.DoReportProgressCount(anObj : TObject; aData : pointer);
var
  lMsg: string;
  Parts: TStringArray;
  Current, Total: Integer;
begin
  lMsg:= PChar(aData);

  // Parse the message which should be "Current,Total"
  Parts:= lMsg.Split('|');
  if Length(Parts) = 2 then
  begin
    Current:= StrToIntDef(Parts[0], 0);
    Total:= StrToIntDef(Parts[1], 0);

    // Update progress bar
    if Total > 0 then
    begin
      ProgressBar.Max:= Total;
      ProgressBar.Position:= Current;
      if Current= Total then
        lblProgress.Caption:= Format(fPresenter.GetstaticText(UnitName, 'ExportIsCompleted'), [Total])
      else
        lblProgress.Caption:= Format(fPresenter.GetstaticText(UnitName, 'ExportingNumberFromTotalCount'), [Current, Total]);
    end;
  end;

  Application.ProcessMessages;  // Force UI update
end;

procedure TfrmMain.DoExportError(anObj : TObject; aData : PExportToOroxTtlFileRec);
var
  Msg: string;
  Parts: TStringArray;
  i: Integer;
  lMsg: string;
begin
  if adata = Nil then Exit;

  Msg:= PChar(aData);
  Parts:= Msg.Split('|');
  lMsg:= '';

  // Add to the memo (line)
  if Assigned(memReportError) then begin
    for i:= Low(Parts) to High(Parts) do begin
      lMsg:= lMsg + fPresenter.GetstaticText('view.main', Parts[i]) + ' ';  // Build the string
    end;

    memReportError.Lines.Add(FormatDateTime('dd-mm-yyyy hh:nn:ss', Now) + ' ' + lMsg);
    // "Auto-scroll"
    memReportError.SelStart:= Length(memReportError.Text);
    memReportError.SelLength:= 0;
  end;

//  Application.ProcessMessages;  // Force UI update
end;

procedure TfrmMain.DoUniqueStringlist(anObj : TObject; aData : PUniqueStringlistRec);
var
  combo: TComboBox;
begin
  if (aData = Nil) and (anObj = Nil) then
    Exit;

  combo:= TComboBox(anObj);

  if TStringList(aData^.ListItems).Count > 0 then
  begin
    combo.Items.Clear;
    combo.Items.Assign(TStringList(aData^.ListItems));
  end;
end;

procedure TfrmMain.DoSortDbGrid(anObj: TObject; aData: PSortDbGridRec);
begin
  Screen.Cursor:= crDefault;
  if anObj = Nil then Exit;
  TDBGrid(anObj).EndUpdate(True);
end;

procedure TfrmMain.DoRetrieveCSVData(anObj: TObject; aData: PRetrieveCSVDataRec);
var
  lDataSource: TDataSource;
  lDataSet: TCSVDataset;
  lDbGrid: TDBGrid;
  i: Integer;
begin
  if not Assigned(aData) then
    Exit;

  // Cleanup previous data
  CleanupCurrentCSVData;

  lDbGrid:= TDBGrid(anObj);
  lDbGrid.BeginUpdate;

  // Cast TObject to the right types
  if (aData^.DataSource <> Nil) and (aData^.DataSet <> Nil) then
  begin
    lDataSource:= TDataSource(aData^.DataSource);
    lDataSet:= TCSVDataset(aData^.DataSet);

    for i:= 0 to lDbGrid.Columns.Count -1 do begin
      lDbGrid.Columns[i].Width:= lDbGrid.Columns[i].Width + ExtraColumnWidthForImage;
    end;

    lDbGrid.EndUpdate(True);

    if aData^.Message <> '' then
      messageDlg(fPresenter.GetstaticText(UnitName, 'Warning'), fPresenter.GetstaticText(UnitName, aData^.Message), mtWarning, [mbOK],0);

    btnExportToFileOra.Enabled:= aData^.Success;
    btnExportDbgridToCsv.Enabled:= aData^.Success;

    // Save references
    fCurrentCSVDataSource:= lDataSource;
    fCurrentCSVDataSet:= lDataSet;

    // Connect to DBGrid
    dbgSewerData.DataSource := lDataSource;

    // Markeer dat we ownership hebben overgenomen
    aData^.DataSource:= nil;
    aData^.DataSet:= nil;

    fPresenter.SetStatusbarText('', 0);
  end
  else
  begin
    // Unexpected types - cleanup
    if Assigned(aData^.DataSource) then
      aData^.DataSource.Free;
    if Assigned(aData^.DataSet) then
      aData^.DataSet.Free;

    aData^.DataSource:= nil;
    aData^.DataSet:= nil;
  end;

  Screen.Cursor:= crDefault;
end;


procedure TfrmMain.HandleObsNotify(aReason: ptrint; aNotifyObj: TObject; aData: pointer);
begin
  case aReason of
    prMainStaticTexts  : DoStaticTexts(IStringList(aData)); { important startup-code }
    prStatus: if aNotifyObj = nil then
                fPresenter.SetStatusbarText(Pch2Str(aData), 0)
              else
                DoStatus(aNotifyObj, aData);
    prCreateDir               : DoCreateDir(aNotifyObj, aData);
    prStatusBarPanelText      : DoStatus(aNotifyObj, aData);
    prAppSettings             : DoAppSettings(aNotifyObj, aData);
    prFormState               : DoFormState(aNotifyObj,aData);
    prStatusBarPanelWidth     : DoStbPanelWidth(aNotifyObj,aData);
    prDbConnection            : DoDbConnection(aNotifyObj,aData);
    prRetrieveData            : DoRetrieveData(aNotifyObj,aData);
    prExportToOroxTtlFile     : DoExportToOroxTtlFile(aNotifyObj,aData);
    prReportProgress          : DoExportProgress(aNotifyObj, aData);
    prReportProgressCount     : DoReportProgressCount(aNotifyObj, aData);
    prReportError             : DoExportError(aNotifyObj, aData);
    prUniqueStringlist        : DoUniqueStringlist(aNotifyObj, aData);
    prSortDbGrid              : DoSortDbGrid(aNotifyObj, aData);
    prRetrieveCSVData         : DoRetrieveCSVData(aNotifyObj, aData);
  end;
end;
{$EndRegion 'subscriber-events'}

procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction; { now, everything is said & done, we're ready to show }

  self.Color:= clWindow;
  fCanContinue:= CheckLanguageFiles;  // In the View because at least 1 language file is required when creating the presenter(s) and the model(s). So check immediately.
  if fCanContinue then begin
    SetAppLanguage;
    FSubscriber:= CreateObsSubscriber(@HandleObsNotify); //bm: FPresenter.Provider
    Presenter:= CreatePresenterMain(''); // Should take care of subscribing etc.
  end;

  if fCanContinue then CreateDirectories;
  if fCanContinue then ReadSettings;
  if fCanContinue then StartLogging;

  if fCanContinue then fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'Welcome'), 0);  // Just testing.
  if fCanContinue then begin
    // Voorlopig in het Nederlands opstarten. Dat moet eerst op orde zijn.
    // Daarna twee talig maken
    Lang:= 'nl';
    fPresenter.SwitchLanguage(UnitName);
    fPresenter.GetStaticTexts('view.main.StatusbarTexts');
    WriteSingleSetting('Language', 'Configure', Lang);
    //

    if fPresenter.GetSQLfileLocation <> '' then begin
      SynEditSqlQuery.Lines.LoadFromFile(fPresenter.GetSQLfileLocation);  // A bit of an abbreviation
      lblSQLFileLocation.Caption:= fPresenter.GetSQLfileLocation;
    end;

    if fPresenter.GetKeepLastOrganization then begin
      cbOrganizationName.Text:= fPresenter.GetLastUsedOrganization;
      cbOrganizationNameCSV.Text:= fPresenter.GetLastUsedOrganization;
    end
    else begin
      cbOrganizationName.Text:= '';
      cbOrganizationNameCSV.Text:= '';
    end;

    LoadComboBoxItems;  // Load the combobox items from a text file.
  end;

  fPresenter.SetStatusbarPanelsWidth(stbInfo, stbInfo.Width, stbInfo.Panels[0].Width, stbInfo.Panels[2].Width);

  // Connect eventhandlers.
  miProgramClose.OnClick:= @miProgramCloseOnClick;
  btnClose.OnClick:= @miProgramCloseOnClick;
  miOptionsOptions.OnClick:= @miOptionsOptionsOnClick;
  miOptionsLanguageEN.OnClick:= @miOptionsLanguageENOnClick;
  miOptionsLanguageNL.OnClick:= @miOptionsLanguageNLOnClick;
  Self.OnShow:= @OnFormShow;
  Self.OnResize:= @OnFormResize;
  dbgSewerData.OnTitleClick:= @dbgSewerDataTitleClick;

  btnGetData.Enabled:= False;
  btnExportToFileOra.Enabled:= False;
  btnExportDbgridToCsv.Enabled:= False;
  ProgressBar.Visible:= False;
  PageControl1.ActivePageIndex:= 0;
  btnDisconnect.Enabled:= False;
  PageControl2.ActivePageIndex:= 0;

end;

procedure TfrmMain.BeforeDestruction;
var
  UserName, Location: String;
begin
  if fCanContinue then begin
    StoreFormstate;  // Store form position and size.

    CleanupCurrentCSVData;  // tonen csvdata rommel nog opruimen.

    UserName:= StringReplace(SysUtils.GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';
    {$IFDEF MSWINDOWS}
    Location:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName+PathDelim + adSettings;
    {$ENDIF}
    {$IFDEF LINUX}
    Location:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adSettings;
    {$ENDIF}

    cbOrganizationName.Items.SaveToFile(Location + PathDelim + UserName+'_Organisation.txt');
    cbDatabaseName.Items.SaveToFile(Location + PathDelim + UserName+'_DatabaseName.txt');
    cbUserName.Items.SaveToFile(Location + PathDelim + UserName+'_UserName.txt');
  end;

  if fSubscriber <> nil then begin
    if fPresenter <> nil then
      fPresenter.Provider.UnSubscribe(fSubscriber);
    fSubscriber.Obj.Free;
    fSubscriber:= nil;
  end;

  if fPresenter <> nil then fPresenter.Obj.Free;
  fPresenter:= nil;
  inherited BeforeDestruction;
end;

function TfrmMain.CheckLanguageFiles : Boolean;
var
  aRoot: String;
begin
  // Please note, language files must be present. If they are not found, the application crashes with an access violation and memory leak.
  aRoot:= ExtractFilePath(ParamStr(0));  // Get he location where the executable is located. This is where the language files should be.

  if FileExists(format(mvpTexts,[aRoot,Lang])) then begin  // Default Lang = 'en'. // mvpTexts = '%sILmain_text.%s'
    Result:= True;
  end
  else begin
    Result:= False;
    MessageDlg(ApplicationName + ': Error', 'No language file was found.', mtError, [mbOK],0);
  end;
end;

procedure TfrmMain.SetAppLanguage;
var
  SetFile: String;
begin
  SetFile:= GetSettingsFile;
  if FileExists(SetFile) then
    Lang:= CreStrListFromFile(SetFile).Values['Language'].Trim; ///<- i18n.

  if (Lang = '') or (SetFile = '') then
    Lang:= 'en';  // The default comes to the rescue when the settingsfile is (not) found but language is empty.
end;

function TfrmMain.GetSettingsFile : String;
var
  UserName: String;
begin
  UserName:= StringReplace(SysUtils.GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';
  {$IFDEF MSWINDOWS}
  Result:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config' + PathDelim +  ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';
  {$ENDIF}
end;

procedure TfrmMain.CreateDirectories;
var
  lTrx: TCreDirTrx;
  dirList: TStringList;
begin
  dirList:= TStringList.Create;  // Create stringlist that contains the directory names.
  lTrx:= FPresenter.TrxMan.StartTransaction(prCreateDir) as TCreDirTrx;
  try
    dirList.Add(adSettings);    // Settings directory.
    dirList.Add(adLogging);     // Logging directory.
    dirList.Add(adDomainlist);  // Domain list (mapping list) directory
    dirList.Add(adExport);      // Export directory
    dirList.Add(adQueries);     // Query directory
    lTrx.NewDirNames.AddStrings(dirList);

    lTrx.AppName:= ApplicationName;
    //  lTrx.RootDir should never be empty, because then no directory will be created.
    {$IFDEF MSWINDOWS}
    lTrx.RootDir:= SysUtils.GetEnvironmentVariable('APPDATA'); // Win 11: lTrx.RootDir = 'C:\Users\<username>\AppData\Roaming'     // + ApplicationName;  // C:\Users\<username>\AppData\Roaming\<Application Name>
    {$ENDIF}
    {$IFDEF LINUX}
    lTrx.RootDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config';  // Linux: lTrx.RootDir = '/home/<username>/.config'
    {$ENDIF}

    lTrx.AppName:= ApplicationName;
    fPresenter.TrxMan.CommitTransaction;
    dirList.Free;
  except
    fPresenter.TrxMan.RollbackTransaction;  // does NOTHING and _frees_ transaction
    fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorCreateDir'), 0);
  end;
end;

procedure TfrmMain.ReadSettings;
var
  lTrx: TSettingsTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prAppSettings) as TSettingsTrx;
  try
    lTrx.ReadSettings:= True;  // <<---
    lTrx.AppName:= ApplicationName;
    lTrx.AppVersion:= Application_version;
    lTrx.AppBuildDate:= Application_build_date;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorReadSettings'), 0);
  end;
end;

procedure TfrmMain.ReadFormState;
var
  lTrx : TSettingsTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prFormState) as TSettingsTrx;
  try
    lTrx.ReadSettings:= True;
    lTrx.ReadFormState:= True;
    lTrx.FormName:= UnitName;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;

    FPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorReadFormState'), 0);
  end;
end;

procedure TfrmMain.StoreFormstate;
var
  lTrx : TSettingsTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prAppSettings) as TSettingsTrx;
  try
    lTrx.WriteSettings:= True;
    lTrx.ReadSettings:= False;
    lTrx.StoreFormState:= True; // <<---
    lTrx.FormName:= UnitName;
    lTrx.FormWindowstate:= integer(Windowstate);
    lTrx.FormTop:= Top;
    lTrx.FormLeft:= Left;
    lTrx.FormHeight:= Height;
    lTrx.FormWidth:= Width;
    lTrx.FormRestoredTop:= RestoredTop;
    lTrx.FormRestoredLeft:= RestoredLeft;
    lTrx.FormRestoredHeight:= RestoredHeight;
    lTrx.FormRestoredWidth:= RestoredWidth;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;
    lTrx.SplitterdataGrid:= pnlDataGrid.Height;
    lTrx.SplitterDataSettings:= pnlDataSettings.Width;
    lTrx.SplitterMemos:= pnlProgress.Width;

    FPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorStoreFormState'), 0);
  end;
end;

procedure TfrmMain.StartLogging;
begin
  fPresenter.StartLogging;
end;

procedure TfrmMain.WriteSingleSetting(Setting, Section, aValue : String);
var
  lTrx: TSingleSettingTrx;
begin
  lTrx:= FPresenter.TrxMan.StartTransaction(prAppSingleSetting) as TSingleSettingTrx;
  try
    lTrx.SettingName:= Setting;
    lTrx.Section:= Section;
    lTrx.SettingValue:= aValue;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(FPresenter.GetstaticText('view.main.statusbartexts', 'ErrorSaveSingleSetting'), 0);
  end;
end;

procedure TfrmMain.AddCbListItem(sender : TObject);
var
  lTrx: TUniqueStringlistTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prUniqueStringlist) as TUniqueStringlistTrx;
  try
    lTrx.ListItems:= TComboBox(sender).Items;
    lTrx.NewString:= TComboBox(sender).Text;
    lTrx.aComponent:= Sender;
    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(FPresenter.GetstaticText('view.main.statusbartexts', 'ErrorUniqueStringlist'), 0);
  end;
end;

procedure TfrmMain.LoadComboBoxItems;
var
  UserName, Location: String;
begin
  UserName:= StringReplace(SysUtils.GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';
  {$IFDEF MSWINDOWS}
  Location:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName+PathDelim + adSettings;
  {$ENDIF}
  {$IFDEF LINUX}
  Location:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adSettings;
  {$ENDIF}

  if FileExists(Location + PathDelim + UserName+'_DatabaseName.txt') then
    cbDatabaseName.Items.LoadFromFile(Location + PathDelim + UserName+'_DatabaseName.txt');

  if FileExists(Location + PathDelim + UserName+'_UserName.txt') then
    cbUserName.Items.LoadFromFile(Location + PathDelim + UserName+'_UserName.txt');

  if FileExists(Location + PathDelim + UserName+'_Organisation.txt') then
    cbOrganizationName.Items.LoadFromFile(Location + PathDelim + UserName+'_Organisation.txt');
  cbOrganizationName.Sorted:= True;  { #todo : Instelbaar maken }
end;

procedure TfrmMain.ExportInProgress(IsInProgress : Boolean);
var
  lRec: TExportInProgressRec;
begin
  lRec.IsInProgress:= IsInProgress;
  lRec.aParent:= Self;

  fPresenter.ExportInProgress(lRec);
end;

procedure TfrmMain.miProgramCloseOnClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmMain.OnFormShow(Sender : TObject);
begin
  if fCanContinue then ReadFormState;
  if fCanContinue then ReadSettings;
  if PageControl2.ActivePageIndex = 0 then
    cbDatabaseName.SetFocus;

  // Prevent the text from being selected and put cursor behind any text
  cbDatabaseName.SelStart := Length(cbDatabaseName.Text);
  cbDatabaseName.SelLength:= 0;
end;

procedure TfrmMain.OnFormResize(Sender : TObject);
begin
  fPresenter.SetStatusbarPanelsWidth(stbInfo, stbInfo.Width, stbInfo.Panels[0].Width, stbInfo.Panels[2].Width);
end;

procedure TfrmMain.miOptionsOptionsOnClick(Sender : TObject);
begin
  SetUpConfigureView(FPresenter);
  ReadSettings;  // Read the settings, they can be changed in the configure view.
end;

procedure TfrmMain.miOptionsLanguageENOnClick(Sender : TObject);
begin
  Lang:= 'en';
  fPresenter.SwitchLanguage(UnitName);

  if not miOptionsLanguageEN.Checked then begin
    miOptionsLanguageEN.Checked:= True;
    miOptionsLanguageNL.Checked:= False;
    fPresenter.GetStaticTexts('view.main.StatusbarTexts'); // get the English texts.
    fPresenter.WriteToLog(UnitName, ltInformation, 'SwitchlanguageEN'); //'English language option Enabled'.
    WriteSingleSetting('Language','Configure',  Lang);
    SetAppLanguage;
  end;
end;

procedure TfrmMain.miOptionsLanguageNLOnClick(Sender : TObject);
begin
  Lang:= 'nl';
  fPresenter.SwitchLanguage(UnitName);

  if not miOptionsLanguageNL.Checked then begin
    miOptionsLanguageNL.Checked:= True;
    miOptionsLanguageEN.Checked:= False;
    fPresenter.GetStaticTexts('view.main.StatusbarTexts');
    fPresenter.WriteToLog(UnitName, ltInformation, 'SwitchlanguageNL' ); //'Dutch language option Enabled'.
    WriteSingleSetting('Language','Configure',  Lang);
    SetAppLanguage;
  end;
end;

procedure TfrmMain.dbgSewerDataTitleClick(Column: TColumn);
var
  lTrx: TSortDbGridTrx;
//  DataSet: TDataSet = Nil;
begin
  if not Assigned(dbgSewerData.DataSource) or
     not Assigned(dbgSewerData.DataSource.DataSet) then
    Exit;

  Screen.Cursor:= crHourGlass;
  dbgSewerData.BeginUpdate;

//  DataSet:= dbgSewerData.DataSource.DataSet;

  lTrx:= fPresenter.TrxMan.StartTransaction(prSortDbGrid) as TSortDbGridTrx;
  try
    lTrx.Column:= Column;
    lTrx.FieldName:= Column.FieldName;
    lTrx.DbGrid:= dbgSewerData;

    // Bepaal huidige sorteer volgorde
    if Column.Title.ImageIndex = 0 then
      lTrx.CurrentSortOrder:= 'ASC'
    else if Column.Title.ImageIndex = 1 then
      lTrx.CurrentSortOrder:= 'DESC'
    else
      lTrx.CurrentSortOrder:= ''; // Nog niet gesorteerd

    if dbgSewerData.DataSource.DataSet is TSQLQuery then begin
      lTrx.DataType:= dtORA;
    end
    else if dbgSewerData.DataSource.DataSet is TCSVDataset then begin
      lTrx.DataType:= dtCSV;
      lTrx.DataProvider:= dbgSewerData.DataSource.DataSet;  // zou voor Ora ook zo kunnen.
    end;


    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorSortingGrid'), 0);
    dbgSewerData.EndUpdate(True);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TfrmMain.LoadCsvData(const FileName: String);
var
  lTrx: TRetrieveCSVDataTrx;
begin
  if FileName = '' then Exit;

  Screen.Cursor:= crHourGlass;
  fPresenter.SetStatusbarText('Loading CSV data...', 0);  { #todo : Taalinstelling }


  lTrx:= fPresenter.TrxMan.StartTransaction(prRetrieveCSVData) as TRetrieveCSVDataTrx;
  try
    lTrx.FileName:= FileName;
    lTrx.HasHeader:= True;  { #todo : Moet instelbaar worden. }
    lTrx.Delimiter:= ',';   { #todo : Moet instelbaar worden. }
    lTrx.QuoteChar:= '"';   { #todo : Moet instelbaar worden. }
    lTrx.DataGrid:= dbgSewerData;
    lTrx.DataSource:= nil;

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    Screen.Cursor:= crDefault;
    fPresenter.SetStatusbarText('Error loading CSV data', 0);  { #todo : Taalinstelling }
  end;
end;

procedure TfrmMain.setFormFonts_Default(F: TForm);
begin
  {$IFDEF LINUX}
     F.Font.Name:='Noto Sans';
     F.Font.Height:=12;
  {$ENDIF}
end;

procedure TfrmMain.CleanupCurrentCSVData;
var
  tmpDataSource: TDataSource;
  tmpDataSet: TDataSet;
begin
{  The box of tricks opens. This is ugly but works and
   prevents a memory leak that occurred when reading a csv dataset into the dbgrid}

  { #todo : See if this can be improved }

  // Save to local variables and reset fields
  tmpDataSource:= FCurrentCSVDataSource;
  tmpDataSet:= TDataSet(FCurrentCSVDataSet);

  FCurrentCSVDataSource:= nil;
  FCurrentCSVDataSet:= nil;

  // Decoupling DBGrid
  dbgSewerData.DataSource:= nil;

  // Remove link between DataSource and DataSet
  if Assigned(tmpDataSource) then
    tmpDataSource.DataSet:= nil;

  // Release DataSet
  if Assigned(tmpDataSet) then
  begin
    try
      if tmpDataSet.Active then begin
        //tmpDataSet.Close;  // Gebruik Close in plaats van Active := False
      tmpDataSet.Free;
      tmpDataSet:= Nil;
      end;
    except
      // Ignoring closing errors
    end;

    //tmpDataSet.Free;
  end;

  // Release DataSource
  if Assigned(tmpDataSource) then
    tmpDataSource.Free;
end;


end.

