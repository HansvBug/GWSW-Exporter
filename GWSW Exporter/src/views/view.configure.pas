{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
unit view.configure;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls, istrlist, model.intf, model.decl, presenter.configure,
  presenter.configure.trax, common.utils;

type

  { TfrmConfigure }

  TfrmConfigure = class(TForm, IViewConfigure)
    BitBtnQueryFileLocation: TBitBtn;
    BitBtnSelectMappingsFile: TBitBtn;
    btnClose: TButton;
    cbGWSWVersion: TComboBox;
    chkAskToOpenExportFile: TCheckBox;
    chkBdGridRowhighligth: TCheckBox;
    chkKeepLastOrganization: TCheckBox;
    chkValueOutOfRange: TCheckBox;
    chkFieldIsMissing: TCheckBox;
    chkFieldIsEmpty: TCheckBox;
    chkFatal: TCheckBox;
    chkMapping: TCheckBox;
    chkActiveLogging: TCheckBox;
    chkAppendLogging: TCheckBox;
    edtMappingsFile: TEdit;
    edtSqlFileLocation: TEdit;
    gbLogging: TGroupBox;
    gbErrorReporting: TGroupBox;
    gbMiscelleneous: TGroupBox;
    gbGWSW: TGroupBox;
    lblGWSWVersion: TLabel;
    lblMappingsFile: TLabel;
    lblQueryFileLocation: TLabel;
    pgcConfigure: TPageControl;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    stbInfo: TStatusBar;
    tbsGWSW: TTabSheet;
    tbsMiscellaneous: TTabSheet;
    procedure BitBtnQueryFileLocationClick(Sender: TObject);
    procedure BitBtnSelectMappingsFileClick(Sender: TObject);
    procedure chkActiveLoggingChange(Sender: TObject);
    procedure pgcConfigureChange(Sender: TObject);
  private
    fPresenter: IPresenterConfigure;
    fSubscriber: IobsSubscriber;
    function get_Observer: IobsSubscriber;
    function get_Presenter: IPresenterConfigure;
    function Obj: TObject;
    procedure set_Observer(aValue: IobsSubscriber);
    procedure set_Presenter(aValue: IPresenterConfigure);
    //...
    procedure SetAppLanguage;  { Set the application language. }
    function GetSettingsFile: String;
    procedure ReadFormState;
    procedure StoreFormstate;
    procedure WriteSettings;
    procedure ReadSettings;

    { Event handlers }
    procedure OnFormCreate(Sender: TObject);
    procedure OnFormShow(Sender: TObject);
    procedure BtnCloseOnClick(Sender: TObject);
  protected
    procedure DoStaticTexts(Texts: IStrings);
    procedure DoAppSettings({%H-}anObj: TObject; aData: PSettingsRec);
    procedure DoFormState({%H-}anObj: TObject; aData: PSettingsRec);
    procedure SetStaticHints(Texts: IStrings);
    procedure HandleObsNotify(aReason: ptrint; aNotifyClass: TObject; aData: pointer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Observer: IobsSubscriber read get_Observer write set_Observer;
    property Presenter: IPresenterConfigure read get_Presenter write set_Presenter;
  end;

  function SetUpConfigureView(aMainPresenter: IPresenterMain): Boolean;
var
  frmConfigure: TfrmConfigure;

implementation
uses obs_prosu, common.consts;

function SetUpConfigureView(aMainPresenter: IPresenterMain): Boolean;
begin (*  S T A R T U P  F U N C T I O N  *)
  with TfrmConfigure.Create(nil) do try
    { this takes care of setting up the trx-stuff, with model & owner }
    Presenter:= aMainPresenter.CreatePresenterConfig();
    FPresenter.SwitchLanguage(UnitName);
    ReadSettings;  // <-- !

    Result:= (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmConfigure }

procedure TfrmConfigure.chkActiveLoggingChange(Sender: TObject);
begin
  if chkActiveLogging.Checked then begin
    chkAppendLogging.Enabled:= True;
  end
  else begin
    chkAppendLogging.Enabled:= False;
    chkAppendLogging.Checked:= False;
  end;
end;

procedure TfrmConfigure.pgcConfigureChange(Sender: TObject);
begin
  if pgcConfigure.ActivePageIndex = 1 then begin
    edtMappingsFile.SelStart:= 0;
  end;
end;

procedure TfrmConfigure.BitBtnQueryFileLocationClick(Sender: TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog:= TOpenDialog.Create(self);
  openDialog.Title:= fPresenter.GetstaticText(UnitName, 'SelectQueryFile');
  {$IFDEF MSWINDOWS}
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adQueries;
  {$ENDIF}
  {$IFDEF LINUX}
  openDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adQueries;
  {$ENDIF}
  openDialog.Options:= [ofFileMustExist];
  openDialog.Filter:= fPresenter.GetstaticText(UnitName, 'DlgSqlFilesFilter');
  try
    if openDialog.Execute then begin
      edtSqlFileLocation.Text:= openDialog.FileName;
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TfrmConfigure.BitBtnSelectMappingsFileClick(Sender: TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog:= TOpenDialog.Create(self);
  openDialog.Title:= fPresenter.GetstaticText('view.configure', 'SelectMapFile');


  {$IFDEF MSWINDOWS}
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName + PathDelim + adDomainlist;
  {$ENDIF}
  {$IFDEF LINUX}
  openDialog.InitialDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + PathDelim + '.config' + PathDelim + ApplicationName + PathDelim + adDomainlist;
  {$ENDIF}
                          // ExtractFilePath(ParamStr(0)) + PathDelim + adDomainlist;
  openDialog.Options:= [ofFileMustExist];
  openDialog.Filter:= fPresenter.GetstaticText('view.configure', 'BORGWSWMappingFile');
  try
    if openDialog.Execute then begin
      edtMappingsFile.Text:= openDialog.FileName;
    end;
  finally
    openDialog.Free;
  end;
end;

function TfrmConfigure.get_Observer: IobsSubscriber;
begin
  Result:= fSubscriber;
end;

function TfrmConfigure.get_Presenter: IPresenterConfigure;
begin
  Result:= fPresenter;
end;

function TfrmConfigure.Obj: TObject;
begin
  Result:= Self;
end;

procedure TfrmConfigure.set_Observer(aValue: IobsSubscriber);
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

procedure TfrmConfigure.set_Presenter(aValue: IPresenterConfigure);
begin
  if aValue <> nil then begin
    if Assigned(fPresenter) then begin
      fPresenter.Provider.UnSubscribe(fSubscriber); { no dangling subscriptions }
      fPresenter.Obj.Free;
    end;

    fPresenter:= aValue;
    fPresenter.Provider.Subscribe(fSubscriber);

    fPresenter.GetStaticTexts(UnitName); { important startup-code }  // Runs once.
    FPresenter.GetStaticTexts('view.configure.hints');   // Get the hint texts.
    // etc...
  end else begin //aValue = nil
    if Assigned(fPresenter) then fPresenter.Obj.Free;
    fPresenter:= aValue; // ~ nil
  end;
end;

procedure TfrmConfigure.SetAppLanguage;
var
  SetFile: String;
begin
  SetFile:= GetSettingsFile;
  if FileExists(SetFile) then
    Lang:= CreStrListFromFile(SetFile).Values['Language'].Trim; ///<- i18n. // This just reads te line from the ini file.

  if (Lang = '') or (SetFile = '') then
    Lang:= 'en';  // The default comes to the rescue when the settingsfile is (not) found but language is empty.
end;

function TfrmConfigure.GetSettingsFile: String;
var
  UserName: String;
begin
  UserName:= StringReplace(SysUtils.GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';

  {$IFDEF MSWINDOWS}
  Result:= SysUtils.GetEnvironmentVariable('APPDATA') + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config' + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';  // lTrx.RootDir = '/home/<username>/.config'
  {$ENDIF}
end;

procedure TfrmConfigure.ReadFormState;
var
  lTrx: TLoadConfigFormStateTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prLoadConfigFormState) as TLoadConfigFormStateTrx;
  try
    lTrx.SettingsFile:= GetSettingsFile;
    lTrx.FormName:= UnitName;
    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.StoreFormstate;
var
  lRec: TSettingsRec;
  lTrx: TStoreConfigFormStateTrx;
begin
  lRec.setSettingsFile:= GetSettingsFile;
  lRec.setFrmName:= UnitName;
  lRec.setFrmWindowState:= Integer(WindowState);
  lRec.setFrmTop:= Top;
  lRec.setFrmLeft:= Left;
  lRec.setFrmHeight:= Height;
  lRec.setFrmWidth:= Width;
  lRec.setFrmRestoredTop:= RestoredTop;
  lRec.setFrmRestoredLeft:= RestoredLeft;
  lRec.setFrmRestoredHeight:= RestoredHeight;
  lRec.setFrmRestoredWidth:= RestoredWidth;

  lTrx:= fPresenter.TrxMan.StartTransaction(prStoreConfigFormState) as TStoreConfigFormStateTrx;
  try
    lTrx.SettingsFile:= GetSettingsFile;
    lTrx.Settings:= lRec;
    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.WriteSettings;
var
  lRec: TSettingsRec;
begin
  lRec:= fPresenter.GetSettings(GetSettingsFile);
  lRec.setFrmName:= UnitName;
  lRec.setActivateLogging:= chkActiveLogging.Checked;
  lRec.setAppendLogging:= chkAppendLogging.Checked;
  lRec.setLanguage:= Lang;
  lRec.setDbGridRowHighlight:= chkBdGridRowhighligth.Checked;
  lRec.setSqlFileLocation:= edtSqlFileLocation.Text;
  lRec.setAskToOpenExportFile:= chkAskToOpenExportFile.Checked;
  lRec.setKeepLastOrganization:= chkKeepLastOrganization.Checked;
  lRec.setRapportMappingError:= chkMapping.Checked;
  lRec.setRapportFatalError:= chkFatal.Checked;
  lRec.setRapportFieldIsEmpty:= chkFieldIsEmpty.Checked;
  lRec.setRapportFieldIsMissing:= chkFieldIsMissing.Checked;
  lRec.setRapportOutOfRange:= chkValueOutOfRange.Checked;
  lRec.setMappingFile:= edtMappingsFile.Text;
  lRec.setGWSWVersion:= cbGWSWVersion.Text;

  fPresenter.SaveSettings(lRec);
end;

procedure TfrmConfigure.ReadSettings;
var
  lRec: TSettingsRec;
begin
  lRec:= fPresenter.GetSettings(GetSettingsFile);
  lRec.setFrmName:= UnitName;
  DoAppSettings(Self, @lRec);
end;

procedure TfrmConfigure.OnFormCreate(Sender: TObject);
begin
  pgcConfigure.ActivePage:= tbsMiscellaneous; // Set the active page.
end;

procedure TfrmConfigure.OnFormShow(Sender: TObject);
begin
  ReadFormState;
end;

procedure TfrmConfigure.BtnCloseOnClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmConfigure.DoStaticTexts(Texts: IStrings);
var
  i: integer;
  lc: TComponent;
begin
  Caption:= Texts.Values[Name];

  for i:= 0 to ComponentCount-1 do begin
    lc:= Components[i];
    if (lc is TButton) then
      TButton(lc).Caption:= Texts.Values[TButton(lc).Name]
    else if (lc is TCheckBox) then
      TCheckBox(lc).Caption:= Texts.Values[TCheckBox(lc).Name]
    else if (lc is TTabSheet) then
      TTabSheet(lc).Caption:= Texts.Values[TTabSheet(lc).Name]
    else if (lc is TGroupBox) then
      TGroupBox(lc).Caption:= Texts.Values[TGroupBox(lc).Name]
    else if (lc is TPanel) then
      TPanel(lc).Caption:= Texts.Values[TPanel(lc).Name]
    else if (lc is TLabel) then
      TLabel(lc).Caption:= Texts.Values[TLabel(lc).Name]
    else if (lc is TBitBtn) then
      TBitBtn(lc).Caption:= Texts.Values[TBitBtn(lc).Name];
  end;
end;

procedure TfrmConfigure.DoAppSettings(anObj: TObject; aData: PSettingsRec);
begin
  With PSettingsRec(aData)^ do begin
    if setSucces then begin
      // Set the form position.
      if (setReadSettings) and (setFrmName = UnitName) then begin
        chkActiveLogging.Checked:= setActivateLogging;
        chkAppendLogging.Checked:= setAppendLogging;
        chkBdGridRowhighligth.Checked:= setDbGridRowHighlight;
        chkAskToOpenExportFile.Checked:= setAskToOpenExportFile;
        chkKeepLastOrganization.Checked:= setKeepLastOrganization;
        edtSqlFileLocation.Text:= setSqlFileLocation;
        chkMapping.Checked:= setRapportMappingError;
        chkFatal.Checked:= setRapportFatalError;
        chkFieldIsEmpty.Checked:= setRapportFieldIsEmpty;
        chkFieldIsMissing.Checked:= setRapportFieldIsMissing;
        chkValueOutOfRange.Checked:= setRapportOutOfRange;
        edtMappingsFile.Text:= setMappingFile;
        cbGWSWVersion.Text:= setGWSWVersion; // Get the gwsw version
      end
      else if (setWriteSettings) and (setFrmName = UnitName) then begin
        // ...
      end
    end;
  end;
end;

procedure TfrmConfigure.DoFormState(anObj: TObject; aData: PSettingsRec);
var
  LastWindowState: TWindowstate;
begin
  with aData^ do begin
    if (setReadFormState) and (setFrmName = UnitName) then begin
      LastWindowState:= TWindowState(aData^.setFrmWindowState);

      if LastWindowState = wsMaximized then begin
        BoundsRect:= Bounds(
          aData^.setFrmRestoredLeft,
          aData^.setFrmRestoredTop,
          aData^.setFrmRestoredWidth,
          aData^.setFrmRestoredHeight);

        WindowState:= wsMaximized;
      end
      else begin
        WindowState:= wsNormal;
        BoundsRect:= Bounds(
          aData^.setFrmLeft,
          aData^.setFrmTop,
          aData^.setFrmWidth,
          aData^.setFrmHeight);

        BoundsRect:= CheckFormIsEntireVisible(BoundsRect);
      end;
    end;
  end;
end;
procedure TfrmConfigure.SetStaticHints(Texts: IStrings);
var
  i: integer;
  lc: TComponent;
begin
  for i:= 0 to ComponentCount-1 do begin
    lc:= Components[i];
    if (lc is TCheckBox) then
      TCheckBox(lc).Hint:= Texts.Values[TCheckBox(lc).Name];
    if (lc is TGroupBox) then
      TGroupBox(lc).Hint:= Texts.Values[TGroupBox(lc).Name];
  end;
end;

procedure TfrmConfigure.HandleObsNotify(aReason: ptrint; aNotifyClass: TObject; aData: pointer);
begin
  case aReason of
    prConfigStaticTexts: DoStaticTexts(IStringList(aData)); { important startup-code }
    prStaticHintsConfig   : SetStaticHints(IStringList(aData));

    prLoadConfigFormState : DoFormState(aNotifyClass, aData);
    prSaveConfigSettings  : DoAppSettings(aNotifyClass, aData);
  end;
end;

procedure TfrmConfigure.AfterConstruction;
begin
  inherited AfterConstruction; { now, everything is said & done, we're ready to show }
  SetAppLanguage;

  fSubscriber:= CreateObsSubscriber(@HandleObsNotify); //bm: fPresenter.Provider
  // Presenter gets set from the outside...! in the startup-function above  !!!!
  // So here no fuctions which are in need of the presenter.  <<--------------!!!!

  self.Color:= clWindow;
  edtSqlFileLocation.Text:= '';
  // Connect eventhandlers.
  Self.OnCreate:= @OnFormCreate;
  Self.OnShow:= @OnFormShow;
  btnClose.OnClick:= @BtnCloseOnClick;

  // gwsw version
  cbGWSWVersion.Items.Add(GWSW_versie_16);
  cbGWSWVersion.Items.Add(GWSW_versie_170);

  pgcConfigure.ActivePageIndex:= 0;
end;

procedure TfrmConfigure.BeforeDestruction;
begin
  WriteSettings;
  StoreFormstate;

  if fSubscriber <> nil then begin
    if fPresenter <> nil then fPresenter.Provider.UnSubscribe(fSubscriber);
    fSubscriber.Obj.Free; fSubscriber:= nil;
  end;
  if fPresenter <> nil then fPresenter.Obj.Free; fPresenter:= nil;
  inherited BeforeDestruction;
end;

end.

