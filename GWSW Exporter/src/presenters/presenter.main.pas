{ Copyright ©2025 Hans van Buggenum }
unit presenter.main;
{$mode ObjFPC}{$H+}
{$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, istrlist, model.base, 
  model.intf, model.decl, presenter.trax, model.main;

type

  { TPresenterMain }
  TPresenterMain = class(TObject,IPresenterMain)
  private
    fInternalMsg: IStringList; ///<- i18n
    fModel: IModelMain;
    fProvider: IobsProvider;
    fTrxMgr: TTransactionManager;
    fRootPath: string;

    function get_Model: IModelMain;
    function get_Provider: IobsProvider;
    function get_TrxMan: ITransactionManager;
    function Obj: TObject;
    procedure set_Provider(aValue: IobsProvider);
  public
    constructor Create(const aRootPath: shortstring = '');
    destructor Destroy; override;
    procedure GetStaticTexts(const aSection: string);
    procedure RefreshTextCache(const aSection,aLangStr: string);
    //...
    function CreatePresenterConfig(): IPresenterConfigure;
    procedure SwitchLanguage(const aSection: String);
    procedure SetStatusbarText(const aText: string; panel: word);
    function GetstaticText(const aView, aText: string): string;
    procedure StartLogging;
    procedure WriteToLog(const aSection, LogType, LogText: String);
    procedure SetStatusbarPanelsWidth(Sender: TObject; stbWithd, lpWidth, rpWidth: Integer);

    procedure MakeDbConnection(DbConnectionData : PDbConnectRec);
    function GetSQLfileLocation: String;

    property Model: IModelMain read get_Model; // TODO: write set_Model; if needed...
    property Provider: IobsProvider read get_Provider write set_Provider;
    property TrxMan: ITransactionManager read get_TrxMan;
  end; { TPresenterMain }

function CreatePresenterMain(const aRootPath: shortstring = ''): IPresenterMain;

implementation
uses presenter.configure;

function CreatePresenterMain(const aRootPath: shortstring): IPresenterMain;
begin { the constructor knows what to do if/with an empty string :o) }
  Result:= TPresenterMain.Create(aRootPath);
end;

{ TPresenterMain }
function TPresenterMain.get_Model: IModelMain;
begin
  Result:= fModel;
end;

function TPresenterMain.get_Provider: IobsProvider;
begin
  Result:= fProvider;
end;

function TPresenterMain.get_TrxMan: ITransactionManager;
begin
  Result:= fTrxMgr;
end;

function TPresenterMain.Obj: TObject;
begin
  Result:= Self;
end;

procedure TPresenterMain.set_Provider(aValue: IobsProvider);
begin
  if Assigned(fProvider) then begin
    fProvider.Obj.Free;
    fProvider:= nil;
  end;
  fProvider:= aValue;
end;

constructor TPresenterMain.Create(const aRootPath: shortstring);
var
  ldummy: integer;
begin
  inherited Create; { below we leave an option open for user to provide root-path in app-param }
  fProvider:= obs_prosu.CreateObsProvider; { as early as posssible, we need comms }
  if aRootPath <> '' then fModel:= TModelMain.Create(Self,aRootPath)
  else fModel:= TModelMain.Create(Self,ExtractFilePath(ParamStr(0))); // should include \/ trailing
  fInternalMsg:= fModel.GetStaticTexts(ClassName,ldummy); ///<- i18n
  fTrxMgr:= TTransactionManager.Create(Self,fModel);
end;

destructor TPresenterMain.Destroy;
begin
  fTrxMgr.Free; fTrxMgr:= nil;         { it's a class :o) }
  fInternalMsg:= nil;                  { it's com, so not strictly necessary :o) }
  fModel.Obj.Free; fModel:= nil;       { it's corba :o) }
  fProvider.Obj.Free; fProvider:= nil; { it's corba :o) }
  inherited Destroy;
end;

procedure TPresenterMain.GetStaticTexts(const aSection: string); //=^
var
  lsl: IStringList;
  lt: integer;
  lreason: TProviderReason;
begin { due to usage of 'out'-param, 'lt' can't be anything else than integer }
  lsl:= fModel.GetStaticTexts(aSection,lt);
  case lt of
    0: lreason:= prMainStaticTexts;    // remember to correlate with model.sects
    1: lreason:= prStatusBarPanelText; // model.main: Const Sects: [view.main.StatusbarTexts]
    3: lreason:= prLoggingText;        //model.main: Const Sects: [view.main.logging]

  end;
  fProvider.NotifySubscribers(lreason,nil,lsl);
  { below we make use of the fInternalMsg stringlist to support i18n }
  //fProvider.NotifySubscribers(prStatus,nil,Str2Pch(fInternalMsg.Values['msgUpnRun'])); ///<-i18n
  FProvider.NotifySubscribers(prStatus, nil, Str2Pch(FInternalMsg.Values['Welcome'])); ///<-i18n  // Presenter only
end;

procedure TPresenterMain.RefreshTextCache(const aSection,aLangStr: string); //=^
var
  ldummy: integer;
begin
  Lang:= trim(aLangStr);
  fModel.ReloadTextCache;
  fInternalMsg.AssignEx(fModel.GetStaticTexts(ClassName,ldummy)); /// clears first
  GetStaticTexts(aSection);
end;

function TPresenterMain.CreatePresenterConfig() : IPresenterConfigure;
begin
  Result:= TPresenterConfigure.Create(fRootPath,fTrxMgr);
end;

procedure TPresenterMain.SwitchLanguage(const aSection : String);
var
  ldummy: integer;
begin
  fModel.SwitchLanguage;
  fInternalMsg:= fModel.GetStaticTexts(ClassName, ldummy);
  GetStaticTexts(aSection);
  //fProvider.NotifySubscribers(prStatus,nil,Str2Pch('(!) Attention: TEST ')); // Voorbeeld. Meer gebruiken op deze manier.
end;

procedure TPresenterMain.SetStatusbarText(const aText : string; panel : word);
var
  lstbRec: TStatusbarPanelText;
  ldummy: integer;
  panelText: String;
begin
  fInternalMsg:= fModel.GetStaticTexts('view.main.statusbartexts', ldummy);

  if aText <> '' then
    panelText:= fInternalMsg.Values[aText];
  if panelText <> '' then
    lstbRec := fModel.SetStatusbartext(panelText, panel)
  else
    lstbRec := fModel.SetStatusbartext(aText, panel);

  fProvider.NotifySubscribers(prStatusBarPanelText, Self, @lstbRec);
end;

function TPresenterMain.GetstaticText(const aView, aText : string) : string;
var
  ldummy: integer;
  statText: string;
begin
  fInternalMsg:= fModel.GetStaticTexts(aView, ldummy);
  statText:= fInternalMsg.Values[aText];
  Result:= statText;
end;

procedure TPresenterMain.StartLogging;
begin
  fModel.StartLogging;
end;

procedure TPresenterMain.WriteToLog(const aSection, LogType, LogText : String);
var
  logString: String;
  ldummy: integer;
  logSection: string;
begin
  case aSection of
    'view.main': begin
       logSection := 'view.main.logging';
    end;
  end;

  fInternalMsg:= fModel.GetStaticTexts(logSection, ldummy);
  logString:= FInternalMsg.Values[LogText];
  fModel.WriteToLog(LogType, logString);
end;

procedure TPresenterMain.SetStatusbarPanelsWidth(Sender : TObject; stbWithd, lpWidth, rpWidth : Integer);
var
  lRec: TStbPanelsSize;
begin
  lRec:= fModel.SetStatusbarPanelsWidth(stbWithd, lpWidth,rpWidth);
  fProvider.NotifySubscribers(prStatusBarPanelWidth, Sender, @lRec);
end;

procedure TPresenterMain.MakeDbConnection(DbConnectionData : PDbConnectRec);
begin
  fModel.MakeDbConnection(DbConnectionData);

  fProvider.NotifySubscribers(prDbConnection, Nil, DbConnectionData);
end;

function TPresenterMain.GetSQLfileLocation : String;
begin
  Result:= fModel.GetSQLfileLocation;
end;

initialization                         { we're taking advantage of the fact, that }
  RegisterSection(TPresenterMain.ClassName); { 'Classname' is a class function }

end.
