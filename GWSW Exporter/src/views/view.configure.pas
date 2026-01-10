{ Copyright ©2025-2026 Hans van Buggenum }
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
    btnClose: TButton;
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
    edtSqlFileLocation: TEdit;
    gbLogging: TGroupBox;
    gbErrorReporting: TGroupBox;
    gbMiscelleneous: TGroupBox;
    lblQueryFileLocation: TLabel;
    pgcConfigure: TPageControl;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    stbInfo : TStatusBar;
    tbsMiscellaneous: TTabSheet;
    procedure BitBtnQueryFileLocationClick(Sender : TObject);
    procedure chkActiveLoggingChange(Sender : TObject);
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
    procedure DoStatus({%H-}anObj: TObject; {%H-}aData: pointer);
    procedure DoAppSettings({%H-}anObj: TObject; aData: PSettingsRec);
    procedure DoFormState({%H-}anObj: TObject; aData: PSettingsRec);
    procedure SetStaticHints(Texts: IStrings);
    procedure HandleObsNotify(aReason : ptrint; aNotifyClass : TObject; aData : pointer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Observer: IobsSubscriber read get_Observer write set_Observer;
    property Presenter: IPresenterConfigure read get_Presenter write set_Presenter;
  end;

  function SetUpConfigureView(aMainPresenter: IPresenterMain): Boolean;

var
  frmConfigure : TfrmConfigure;

implementation
uses obs_prosu, common.consts;

function SetUpConfigureView(aMainPresenter : IPresenterMain) : Boolean;
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

procedure TfrmConfigure.chkActiveLoggingChange(Sender : TObject);
begin
  if chkActiveLogging.Checked then begin
    chkAppendLogging.Enabled:= True;
  end
  else begin
    chkAppendLogging.Enabled:= False;
    chkAppendLogging.Checked:= False;
  end;
end;

procedure TfrmConfigure.BitBtnQueryFileLocationClick(Sender : TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog:= TOpenDialog.Create(self);
  openDialog.Title:= fPresenter.GetstaticText(UnitName, 'SelectQueryFile');
  openDialog.InitialDir:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName + PathDelim + adQueries; { #todo : Make Linux proof }
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

function TfrmConfigure.get_Observer : IobsSubscriber;
begin
  Result:= fSubscriber;
end;

function TfrmConfigure.get_Presenter : IPresenterConfigure;
begin
  Result:= fPresenter;
end;

function TfrmConfigure.Obj : TObject;
begin
  Result:= Self;
end;

procedure TfrmConfigure.set_Observer(aValue : IobsSubscriber);
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

procedure TfrmConfigure.set_Presenter(aValue : IPresenterConfigure);
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
    Lang := 'en';  // The default comes to the rescue when the settingsfile is (not) found but language is empty.
end;

function TfrmConfigure.GetSettingsFile : String;
var
  UserName: String;
begin
  UserName:= StringReplace(SysUtils.GetEnvironmentVariable('USERNAME') , ' ', '_', [rfIgnoreCase, rfReplaceAll]) + '_';

  {$IFDEF MSWINDOWS}
  Result:= SysUtils.GetEnvironmentVariable('appdata') + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config' + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ UserName + ApplicationName+'.cfg';  // lTrx.RootDir = '/home/<username>/.config'
  {$ENDIF}
end;

procedure TfrmConfigure.ReadFormState;
var
  lTrx: TSettingsConfigTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prFormStateConfig) as TSettingsConfigTrx;
  try
    lTrx.ReadSettings:= True;
    lTrx.ReadFormState:= True;
    lTrx.FormName:= UnitName;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.StoreFormstate;
var
  lTrx : TSettingsConfigTrx;
begin
  lTrx := fPresenter.TrxMan.StartTransaction(prAppSettingsConfig) as TSettingsConfigTrx;
  try
    lTrx.WriteSettings:= True;
    lTrx.StoreFormState:= True;
    lTrx.FormName := UnitName;
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

    fPresenter.TrxMan.CommitTransaction;
  except
    fPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.WriteSettings;
var
  lTrx: TSettingsConfigTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prAppSettingsConfig) as TSettingsConfigTrx;
  try
    lTrx.WriteSettings:= True;
    lTrx.ReadSettings:= False;
    lTrx.FormName := UnitName;
    lTrx.ActivateLogging:= chkActiveLogging.Checked;
    lTrx.AppendLogging:= chkAppendLogging.Checked;
    lTrx.Language:= Lang;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;
    lTrx.DbGridRowHighlight:= chkBdGridRowhighligth.Checked;
    lTrx.SqlFileLocation:= edtSqlFileLocation.Text;
    lTrx.AskToOpenExportFile:= chkAskToOpenExportFile.Checked;
    lTrx.KeepLastOrganization:= chkKeepLastOrganization.Checked;
    lTrx.RapportMappingError:= chkMapping.Checked;
    lTrx.RapportFatalError:= chkFatal.Checked;
    lTrx.RapportFieldIsEmpty:= chkFieldIsEmpty.Checked;
    lTrx.RapportFieldIsMissing:= chkFieldIsMissing.Checked;
    lTrx.RapportOutOfRange:= chkValueOutOfRange.Checked;

    fPresenter.TrxMan.CommitTransaction;
  except
    FPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.ReadSettings;
var
  lTrx: TSettingsConfigTrx;
begin
  lTrx:= fPresenter.TrxMan.StartTransaction(prAppSettingsConfig) as TSettingsConfigTrx;
  try
    lTrx.ReadSettings:= True;
    lTrx.WriteSettings:= False;
    lTrx.SettingsLocationAndFileName:= GetSettingsFile;
    lTrx.FormName:= UnitName;

    FPresenter.TrxMan.CommitTransaction;
  except
    FPresenter.TrxMan.RollbackTransaction;
  end;
end;

procedure TfrmConfigure.OnFormCreate(Sender : TObject);
begin
  pgcConfigure.ActivePage:= tbsMiscellaneous; // Set the active page.
end;

procedure TfrmConfigure.OnFormShow(Sender : TObject);
begin
  ReadFormState;
end;

procedure TfrmConfigure.BtnCloseOnClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmConfigure.DoStaticTexts(Texts : IStrings);
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

procedure TfrmConfigure.DoStatus(anObj : TObject; aData : pointer);
begin
  //...
end;

procedure TfrmConfigure.DoAppSettings(anObj : TObject; aData : PSettingsRec);
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
      end
      else if (setWriteSettings) and (setFrmName = UnitName) then begin
        // ...
      end
    end;
  end;
end;

procedure TfrmConfigure.DoFormState(anObj : TObject; aData : PSettingsRec);
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
        BoundsRect := Bounds(
          aData^.setFrmLeft,
          aData^.setFrmTop,
          aData^.setFrmWidth,
          aData^.setFrmHeight);

        BoundsRect:= CheckFormIsEntireVisible(BoundsRect);
      end;
    end;
  end;
end;

procedure TfrmConfigure.SetStaticHints(Texts : IStrings);
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

procedure TfrmConfigure.HandleObsNotify(aReason : ptrint; aNotifyClass : TObject; aData : pointer);
begin
  case aReason of
    prConfigStaticTexts: DoStaticTexts(IStringList(aData)); { important startup-code }
    prStatus: if aNotifyClass = nil then stbInfo.SimpleText:= Pch2Str(aData)
              else DoStatus(aNotifyClass,aData);
    prFormStateConfig    : DoFormState(aNotifyClass, aData);
    prAppSettingsConfig  : DoAppSettings(aNotifyClass, aData);
    prStaticHintsConfig  : SetStaticHints(IStringList(aData));
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
  // Connect eventhandlers.
  Self.OnCreate:= @OnFormCreate;
  Self.OnShow:= @OnFormShow;
  btnClose.OnClick:= @BtnCloseOnClick;
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

