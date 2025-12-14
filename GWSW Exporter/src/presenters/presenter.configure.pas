{ Copyright ©2025 Hans van Buggenum }
unit presenter.configure;
{$mode ObjFPC}{$H+}
{$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, istrlist, model.base,
  model.intf, model.decl, presenter.trax, model.configure;

type

  { TPresenterConfigure }
  TPresenterConfigure = class(TObject,IPresenterConfigure)
  private
    fInternalMsg: IStringList; ///<- i18n
    fModel: IModelConfigure;
    fProvider: IobsProvider;
    fTrxMgr: TTransactionManager;
    function get_Model: IModelConfigure;
    function get_Provider: IobsProvider;
    function get_TrxMan: ITransactionManager;
    function Obj: TObject;
    procedure set_Provider(aValue: IobsProvider);
  public
    constructor Create(const aRootPath: shortstring;aTrxMgr: TTransactionManager);
    destructor Destroy; override;
    procedure GetStaticTexts(const aSection: string);
    procedure RefreshTextCache(const aSection,aLangStr: string);
    //...
    procedure SwitchLanguage(const aSection : String);
    procedure WriteToLog(const aSection, aLogAction, LogText: String);

    property Model: IModelConfigure read get_Model;
    property Provider: IobsProvider read get_Provider write set_Provider;
    property TrxMan: ITransactionManager read get_TrxMan;
  end; { TPresenterConfigure }


implementation


{ TPresenterConfigure }
function TPresenterConfigure.get_Model: IModelConfigure;
begin
  Result:= fModel;
end;

function TPresenterConfigure.get_Provider: IobsProvider;
begin
  Result:= fProvider;
end;

function TPresenterConfigure.get_TrxMan: ITransactionManager;
begin
  Result:= fTrxMgr;
end;

function TPresenterConfigure.Obj: TObject;
begin
  Result:= Self;
end;

procedure TPresenterConfigure.set_Provider(aValue: IobsProvider);
begin
  if Assigned(fProvider) then begin
    fProvider.Obj.Free;
    fProvider:= nil;
  end;
  fProvider:= aValue;
end;

constructor TPresenterConfigure.Create(const aRootPath: shortstring; aTrxMgr: TTransactionManager);
var
  ldummy: integer;
begin
  inherited Create; { below we leave an option open for user to provide root-path in app-param }
  fProvider:= obs_prosu.CreateObsProvider; { as early as posssible, we need comms }
  fTrxMgr:= aTrxMgr;

  if aRootPath <> '' then
    fModel:= TModelConfigure.Create(Self,aRootPath)
  else
    fModel:= TModelConfigure.Create(Self,ExtractFilePath(ParamStr(0))); // should include \/ trailing

  fTrxMgr.ModelConfig:= fModel;
  fTrxMgr.OwnerConfig:= Self;

  fInternalMsg:= fModel.GetStaticTexts(ClassName,ldummy); ///<- i18n
end;

destructor TPresenterConfigure.Destroy;
begin
  fTrxMgr:= nil;                       { it's a class, we just borrowed :o) }
  fInternalMsg:= nil;                  { it's com, so not strictly necessary :o) }
  fModel.Obj.Free; fModel:= nil;       { it's corba :o) }
  fProvider.Obj.Free; fProvider:= nil; { it's corba :o) }
  inherited Destroy;
end;

procedure TPresenterConfigure.GetStaticTexts(const aSection: string); //=^
var
  lsl: IStringList;
  lt: integer;
  lreason: TProviderReason;
begin { due to usage of 'out'-param, 'lt' can't be anything else than integer }
  lsl:= fModel.GetStaticTexts(aSection,lt);
  case lt of
    0: lreason:= prConfigStaticTexts; // remember to correlate with model.sects  !!!!!
    2: lreason:= prStaticHintsConfig;
//    1: lreason:= prStatusBarPanelText; // model.configure: Const Sects: [view.configure.StatusbarTexts]
        //2: lreason:=                       // model.configure: Const Sects: [view.configure.hints]
  end;

  fProvider.NotifySubscribers(lreason,nil,lsl);
  { below we make use of the fInternalMsg stringlist to support i18n }
  fProvider.NotifySubscribers(prStatus,nil,Str2Pch(fInternalMsg.Values['msgUpnRun'])); ///<-i18n
end;

procedure TPresenterConfigure.RefreshTextCache(const aSection,aLangStr: string); //=^
var
  ldummy: integer;
begin
  Lang:= trim(aLangStr);
  fModel.ReloadTextCache;
  fInternalMsg.AssignEx(fModel.GetStaticTexts(ClassName,ldummy)); /// clears first
  GetStaticTexts(aSection);
end;

procedure TPresenterConfigure.SwitchLanguage(const aSection : String);
var
  ldummy: integer;
begin
  fModel.SwitchLanguage;
  fInternalMsg:= fModel.GetStaticTexts(ClassName, ldummy);
  GetStaticTexts(aSection);
  //fProvider.NotifySubscribers(prStatus,nil,Str2Pch('(!) Attention: TEST ')); // Voorbeeld. Meer gebruiken op deze manier.
end;

procedure TPresenterConfigure.WriteToLog(const aSection, aLogAction, LogText : String);
var
  LogString: String;
  ldummy: integer;
  logSection: string;
begin
  case aSection of
    'view.configure': begin
       logSection := 'view.configure.logging';
    end;
  end;

  fInternalMsg:= FModel.GetStaticTexts(logSection, ldummy);
  LogString:= fInternalMsg.Values[LogText];
  fModel.WriteToLog(aLogAction, LogString);
end;

initialization                         { we're taking advantage of the fact, that }
  RegisterConfigureSection(TPresenterConfigure.ClassName); { 'Classname' is a class function }

end.
