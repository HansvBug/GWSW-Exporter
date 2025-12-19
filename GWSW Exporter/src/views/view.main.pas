{ Copyright ©2025 Hans van Buggenum }
unit view.main;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, DBGrids, Buttons, fpspreadsheetctrls, SynEdit,
  SynHighlighterSQL, istrlist, model.intf, model.decl, presenter.main,
  presenter.trax, DB, fpcsvexport;

type
  { TfrmMain }

  TfrmMain = class(TForm, IViewMain)
    BitBtnSelectMappingsFile : TBitBtn;
    btnConnect : TButton;
    btnGetData : TButton;
    btnExportToFile : TButton;
    btnExportDbgridToCsv : TButton;
    btnSaveQuery : TButton;
    btnOpenQuery : TButton;
    cbGWSWVersion : TComboBox;
    dbgSewerData : TDBGrid;
    edtMappingsFile : TEdit;
    edtOrganizationName : TEdit;
    edtDatabaseName : TEdit;
    edtUserName : TEdit;
    edtPassword : TEdit;
    gbConnection : TGroupBox;
    gbExport : TGroupBox;
    gbGetData : TGroupBox;
    gbQuery : TGroupBox;
    gbSettings : TGroupBox;
    lblGWSWVersion : TLabel;
    lblMappingsFile : TLabel;
    lblOrganizationName : TLabel;
    lblDatabaseName : TLabel;
    lblUserName : TLabel;
    lblPassword : TLabel;
    MainMenu1 : TMainMenu;
    miOptionsLanguageNL : TMenuItem;
    miOptionsLanguageEN : TMenuItem;
    mioptionsAbout : TMenuItem;
    miOptionsLanguage : TMenuItem;
    miOptionsOptions : TMenuItem;
    miOptions : TMenuItem;
    miProgramClose : TMenuItem;
    miProgram : TMenuItem;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    pnlSettings : TPanel;
    pnlQuery : TPanel;
    pnlPrepareData : TPanel;
    pnlPrepareMain : TPanel;
    pnlMainTop : TPanel;
    pnlBottom : TPanel;
    pnlMainAllClient : TPanel;
    Splitter1 : TSplitter;
    stbInfo : TStatusBar;
    SynEditSqlQuery : TSynEdit;
    SynSQLSyn1 : TSynSQLSyn;
    tsQuery : TTabSheet;
    tsPrepare : TTabSheet;
    tsSettings : TTabSheet;
    procedure BitBtnSelectMappingsFileClick(Sender : TObject);
    procedure btnConnectClick(Sender : TObject);
    procedure btnExportDbgridToCsvClick(Sender : TObject);
    procedure btnExportToFileClick(Sender : TObject);
    procedure btnGetDataClick(Sender : TObject);
    procedure btnOpenQueryClick(Sender : TObject);
    procedure btnSaveQueryClick(Sender : TObject);
    procedure PageControl1Change(Sender : TObject);
  private


    fPresenter: IPresenterMain;
    fSubscriber: IobsSubscriber;

    fCanContinue: Boolean;

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
    procedure WriteSingleSetting(Setting, aValue: String);

    { Event handlers }
    procedure miProgramCloseOnClick(Sender : TObject);
    procedure OnFormShow(Sender: TObject);
    procedure OnFormResize(Sender : TObject);
    procedure miOptionsOptionsOnClick(Sender : TObject);
    procedure miOptionsLanguageENOnClick(Sender : TObject);
    procedure miOptionsLanguageNLOnClick(Sender : TObject);
    procedure edtMappingsFileOnChange(Sender : TObject);

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
  lRec.DatabaseName:= edtDatabaseName.Text;
  lRec.UserName:= edtUserName.Text;
  lRec.SchemaPassword:= edtPassword.Text;
  lRec.HasConnection:= False;

  fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.StatusbarTexts','Connecting'), 0);
  fPresenter.MakeDbConnection(@lRec);
end;

procedure TfrmMain.btnExportDbgridToCsvClick(Sender : TObject);
var
  csvExporter: TCSVExporter;
  saveDialog: TSaveDialog;
begin
  { #todo : Make this MVP proof }
  csvExporter:= TCSVExporter.Create(Nil);
  saveDialog:= TSaveDialog.Create(Nil);
  try
    csvExporter.Dataset:= dbgSewerData.DataSource.DataSet;

    saveDialog.Title:= fPresenter.GetstaticText(UnitName, 'SaveCSVFile');
    saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgCSVFilesFilter');
    saveDialog.DefaultExt:= 'csv';
    saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName; { #todo : Make Linux proof };;
    saveDialog.Options:= saveDialog.Options + [ofOverwritePrompt];

    if saveDialog.Execute then begin
      Screen.Cursor:= crHourGlass;
      csvExporter.FileName:= saveDialog.FileName;
      csvExporter.Execute;
      Screen.Cursor:= crDefault;
    end;

  finally
    csvExporter.Free;
    saveDialog.Free;
  end;
end;

procedure TfrmMain.btnExportToFileClick(Sender : TObject);
var
  saveDialog: TSaveDialog;
  lTrx: TExportToOroxTtlFileTrx;
  lFileName: String;
  lCanContinue: Boolean;
begin
  lCanContinue:= False;
  if dbgSewerData.DataSource = Nil then begin
    ShowMessage(fPresenter.GetstaticText(UnitName, 'QueryNotActive'));  { #todo : create a msgdialog }
    Exit;
  end;
  if edtMappingsFile.Text = '' then begin
    ShowMessage('Mappingsbestand ontbreekt');
    Exit
  end;

  saveDialog:= TSaveDialog.Create(Nil);
  saveDialog.Filter := 'GWSW-OroX Bestanden (*.orox.ttl)|*.orox.ttl|Turtle Bestanden (*.ttl)|*.ttl|Alle bestanden (*.*)|*.*';
  saveDialog.DefaultExt := 'orox.ttl';  // Is prescribed in this way.
  saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName + PathDelim + adExport; { #todo : Make Linux proof };
  try
    if saveDialog.Execute then begin
      lFileName:= saveDialog.FileName;
      lCanContinue:= True;
    end;
  finally
    saveDialog.Free;
  end;

  if lCanContinue then begin
    try
      lTrx:= fPresenter.TrxMan.StartTransaction(prExportToOroxTtlFile) as TExportToOroxTtlFileTrx;

      lTrx.OrganizationName:= edtOrganizationName.Text;
      lTrx.FileName:= lFileName;
      lTrx.MappingFile:= edtMappingsFile.Text;
      fPresenter.TrxMan.CommitTransaction;

    except
      fPresenter.TrxMan.RollbackTransaction;
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'ErrorExportToOroxTtlFile'), 0);
    end;
  end;
end;

procedure TfrmMain.BitBtnSelectMappingsFileClick(Sender : TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog:= TOpenDialog.Create(self);
  openDialog.Title:= fPresenter.GetstaticText('view.main.', 'SelectMapFile');
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName + PathDelim + adDomainlist; { #todo : Make Linux proof }
                          // ExtractFilePath(ParamStr(0)) + PathDelim + adDomainlist;
  openDialog.Options:= [ofFileMustExist];
  openDialog.Filter:= 'BOR-GWSW mapping files|*.ods';
  try
    if openDialog.Execute then begin
      edtMappingsFile.OnChange:= Nil;  // temporarily disable otherwise WriteSingleSetting triggers 2x
      edtMappingsFile.Text:= openDialog.FileName;
      edtMappingsFile.OnChange:= @edtMappingsFileOnChange;
      WriteSingleSetting('MappingFile', openDialog.FileName);
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TfrmMain.btnGetDataClick(Sender : TObject);
var
  lTrx: TRetrieveDataTrx;
begin
  { #todo : Eventhandler nog los koppelen }
  Screen.Cursor:= crHourGlass;
  fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'RetrievingData'), 0);

  lTrx:= fPresenter.TrxMan.StartTransaction(prRetrieveData) as TRetrieveDataTrx;
  try
    lTrx.DataGrid:= dbgSewerData;
    lTrx.DataSource:= Nil;
    lTrx.SqlText:= SynEditSqlQuery.Text;
    lTrx.OrganizationName:= edtOrganizationName.Text;

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

  openDialog.Title:= fPresenter.GetstaticText(UnitName, 'OpenTextFile');
  openDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgTextFilesFilter');
  openDialog.DefaultExt:= 'txt';
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName; { #todo : Make Linux proof };;
  openDialog.Options:= openDialog.Options + [ofFileMustExist];

  try
    if openDialog.Execute then begin
      SynEditSqlQuery.Lines.LoadFromFile(openDialog.FileName);
      SynEditSqlQuery.Modified:= False;
    end;
  finally
    openDialog.Free;
  end;


end;

procedure TfrmMain.btnSaveQueryClick(Sender : TObject);
var
  saveDialog: TSaveDialog;
begin
  { #todo : Make this MVP proof }
  saveDialog:= TSaveDialog.Create(Nil);
  try
    saveDialog.Title:= fPresenter.GetstaticText(UnitName, 'SaveTextFile');
    saveDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgTextFilesFilter');
    saveDialog.DefaultExt:= 'txt';
    saveDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName; { #todo : Make Linux proof };;
    saveDialog.Options:= saveDialog.Options + [ofOverwritePrompt];

    if saveDialog.Execute then begin
      SynEditSqlQuery.Lines.SaveToFile(saveDialog.FileName);
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TfrmMain.PageControl1Change(Sender : TObject);
begin
  if PageControl1.ActivePageIndex = 1 then begin
    edtMappingsFile.SelStart:= 0;
  end;
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
    else if (lc is TComboBox) then
      TComboBox(lc).Caption:= Texts.Values[TComboBox(lc).Name]
    else if (lc is TBitBtn) then
      TBitBtn(lc).Caption:= Texts.Values[TBitBtn(lc).Name]
    // Clear the TEdit texts
    else if (lc is TEdit) then
      TEdit(lc).Text:= Texts.Values[TEdit(lc).Text]
    else if (lc is TGroupBox) then
      TGroupBox(lc).Caption:= Texts.Values[TGroupBox(lc).Name]
    else if (lc is TRadioButton) then
      TRadioButton(lc).Caption:= Texts.Values[TRadioButton(lc).Name]
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
        edtMappingsFile.Text:= setMappingFile;
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

procedure TfrmMain.DoDbConnection(anObj : TObject; aData : PDbConnectRec);
begin
  With aData^ do begin
    btnGetData.Enabled:= HasConnection;

    if Message = 'Success' then begin
      fPresenter.SetStatusbarText(fPresenter.GetstaticText('view.main.statusbartexts', 'DbConnEstablished'), 0);
    end
    else begin
      fPresenter.SetStatusbarText(Message, 0);

    end;
  end;
  Screen.Cursor:= crDefault;
end;

procedure TfrmMain.DoRetrieveData(anObj : TObject; aData : PRetrieveDataRec);
begin
  With aData^do begin
    if anObj <> Nil then begin
      TDBGrid(anObj).DataSource := TDataSource(aData^.DataSource);
    end;
  end;

  Screen.Cursor:= crDefault;
end;

procedure TfrmMain.DoExportToOroxTtlFile(anObj : TObject; aData : PExportToOroxTtlFileRec);
var
  tmp: String;
begin
  With aData^ do begin
    tmp:= '';
  end;

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

  fPresenter.SetStatusbarPanelsWidth(stbInfo, stbInfo.Width, stbInfo.Panels[0].Width, stbInfo.Panels[2].Width);


  // Connect eventhandlers.
  miProgramClose.OnClick:= @miProgramCloseOnClick;
  miOptionsOptions.OnClick:= @miOptionsOptionsOnClick;
  miOptionsLanguageEN.OnClick:= @miOptionsLanguageENOnClick;
  miOptionsLanguageNL.OnClick:= @miOptionsLanguageNLOnClick;
  Self.OnShow:= @OnFormShow;
  Self.OnResize:= @OnFormResize;
  edtMappingsFile.OnChange:= @edtMappingsFileOnChange;

  btnGetData.Enabled:= False;
  PageControl1.ActivePageIndex:= 0;
  cbGWSWVersion.ItemIndex:= 0; { #note : There are no versions to choose from yet. Version 1.6 is still hard in the code }
end;

procedure TfrmMain.BeforeDestruction;
begin
  if fCanContinue then StoreFormstate;  // Store form position and size.

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
  Result:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';
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
    lTrx.NewDirNames.AddStrings(dirList);

    lTrx.AppName:= ApplicationName;
    //  lTrx.RootDir should never be empty, because then no directory will be created.
    {$IFDEF MSWINDOWS}
    lTrx.RootDir:= GetEnvironmentVariable('appdata'); // Win 11: lTrx.RootDir = 'C:\Users\Hans\AppData\Roaming'     // + ApplicationName;  // C:\Users\<username>\AppData\Roaming\<Application Name>
    {$ENDIF}
    {$IFDEF LINUX}
    lTrx.RootDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config';  // Linux: lTrx.RootDir = '/home/hvb/.config'
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
  lTrx := fPresenter.TrxMan.StartTransaction(prFormState) as TSettingsTrx;
  try
    lTrx.ReadSettings:= True;
    lTrx.ReadFormState:= True;
    lTrx.FormName := UnitName;
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
  lTrx := fPresenter.TrxMan.StartTransaction(prAppSettings) as TSettingsTrx;
  try
    lTrx.WriteSettings:= True;
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

procedure TfrmMain.WriteSingleSetting(Setting, aValue : String);
var
  lTrx: TSingleSettingTrx;
begin
  lTrx:= FPresenter.TrxMan.StartTransaction(prAppSingleSetting) as TSingleSettingTrx;
  try
    lTrx.SettingName:= Setting;
    lTrx.SettingValue:= aValue;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
    fPresenter.SetStatusbarText(FPresenter.GetstaticText('view.main.statusbartexts', 'ErrorSaveSingleSetting'), 0);
  end;
end;

procedure TfrmMain.miProgramCloseOnClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmMain.OnFormShow(Sender : TObject);
begin
  if fCanContinue then ReadFormState;
  if fCanContinue then ReadSettings;
  edtDatabaseName.SetFocus;

  // Prevent the text from being selected and put cursor behind any text
  edtDatabaseName.SelStart  := Length(edtDatabaseName.Text);
  edtDatabaseName.SelLength := 0;
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
    WriteSingleSetting('Language', Lang);
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
    WriteSingleSetting('Language', Lang);
    SetAppLanguage;
  end;
end;

procedure TfrmMain.edtMappingsFileOnChange(Sender : TObject);
begin
  WriteSingleSetting('MappingFile', edtMappingsFile.Text);
end;

end.

