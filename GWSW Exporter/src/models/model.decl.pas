{ Copyright ©2025 Hans van Buggenum }
unit model.decl;
{$mode ObjFPC}{$H+}
interface 
uses classes,sysutils, {contnrs,} common.consts, {istrlist,} obs_prosu, model.base, model.intf,
  presenter.trax, presenter.configure.trax;
const
  Version = '0.21.04.2025'; // 1.st commit,  (mvp made easy set up by cdbc)
  bcMaxWord = obs_prosu.prMaxWord;
  mvpTexts = '%smain_text.%s';     // <-- remember to change to your choice
  confTexts = '%sconfig_text.%s';  // <-- remember to change to your choice
  Lang: string = 'en'; //<- - - -^ i18n support ~ writable const / static var

type
  TTransaction = model.base.TTransaction;  { ancester alias }
  { TTransactionmanager governs alterations to datastore / model }
  TTransactionManager = class(TObject,ITransactionManager)
  private
    fTrx: TTransaction;
    fOwner: IPresenterMain;
    fModel: IModelMain;
    fTraxFactory: ITraxClassList;

    fOwnerConfig: IPresenterConfigure;
    fModelConfig: IModelConfigure;

    function get_ActiveTrx: ITransaction;
    function get_Model: IModelMain;
    function get_ModelConfig : IModelConfigure;
    function get_Owner: IPresenterMain;
    function get_OwnerConfig : IPresenterConfigure;
    function Obj: TObject;
    procedure set_ModelConfig(AValue : IModelConfigure);
    procedure set_OwnerConfig(AValue : IPresenterConfigure);
  public
    constructor Create(anOwner: IPresenterMain;aModel: IModelMain);
    destructor Destroy; override;
    procedure CommitTransaction;
    function InTransaction: boolean; { below is register for runtime jic, NOT startup!!! }
    procedure RegisterTransactionClass(aModReason: ptrint; aTransactionClass: TTransactionClass);
    procedure RollbackTransaction;
    function StartTransaction(aModReason: ptrint): TTransaction;

    property ActiveTrx: ITransaction read get_ActiveTrx;
    property Model: IModelMain read get_Model;
    property Owner: IPresenterMain read get_Owner;
    property ModelConfig: IModelConfigure read get_ModelConfig write set_ModelConfig;
    property OwnerConfig: IPresenterConfigure read get_OwnerConfig write set_OwnerConfig;
  end;


  { alias }
  TNewDirectoriesRec        = model.base.TNewDirectoriesRec;
  PNewDirectoriesRec        = model.base.PNewDirectoriesRec;
  TStatusbarPanelText       = model.base.TStatusbarPanelText;
  PStatusbarPanelText       = model.base.PStatusbarPanelText;
  TSettingsRec              = model.base.TSettingsRec;
  PSettingsRec              = model.base.PSettingsRec;
  TSingleSettingRec         = model.base.TSingleSettingRec;
  PSingleSettingRec         = model.base.PSingleSettingRec;
  TstbPanelsSize            = model.base.TstbPanelsSize;
  PStbPanelsSize            = model.base.PStbPanelsSize;
  TDbConnectRec             = model.base.TDbConnectRec;
  PDbConnectRec             = model.base.PDbConnectRec;
  TRetrieveDataRec          = model.base.TRetrieveDataRec;
  PRetrieveDataRec          = model.base.PRetrieveDataRec;
  TExportToOroxTtlFileRec   = model.base.TExportToOroxTtlFileRec;
  PExportToOroxTtlFileRec   = model.base.PExportToOroxTtlFileRec;
  TUniqueStringlistRec      = model.base.TUniqueStringlistRec;
  PUniqueStringlistRec      = model.base.PUniqueStringlistRec;
  TExportInProgressRec      = model.base.TExportInProgressRec;
  PExportInProgressRec      = model.base.PExportInProgressRec;

{ utility functions, here we publicly export these functions, to be made available
  in other units, that can't /see/ us, for import :o) gotta love FPC \o/\ö/\o/ }
function Pch2Str(aPch: pchar): string; public name 'BC_PCH2STR';
function Str2Pch(aStr: string): pchar; public name 'BC_STR2PCH'; 

implementation

{$Region 'utility'}
function Pch2Str(aPch: pchar): string;
begin // compiler conversion, implicit new memory allocation :o)
  if aPch = nil then exit('');
  Result:= string(aPch);
end;

function Str2Pch(aStr: string): pchar;
begin // compiler conversion, pointer :o)
  if aStr = '' then exit(nil);
  Result:= pchar(aStr);
end;
{$EndRegion 'utility'}

{$Region 'TTransactionManager'}
{ TTransactionManager }
function TTransactionManager.Obj: TObject;
begin
  Result:= Self;
end;

procedure TTransactionManager.set_ModelConfig(AValue : IModelConfigure);
begin
  fModelConfig:= aValue;
end;

procedure TTransactionManager.set_OwnerConfig(AValue : IPresenterConfigure);
begin
  fOwnerConfig:= aValue;
end;

function TTransactionManager.get_ActiveTrx: ITransaction;
begin
  Result:= fTrx;
end;

function TTransactionManager.get_Model: IModelMain; //+
begin
  Result:= fModel;
end;

function TTransactionManager.get_ModelConfig : IModelConfigure;
begin
  Result:= fModelConfig;
end;

function TTransactionManager.get_Owner: IPresenterMain; //+
begin
  Result:= fOwner;
end;

function TTransactionManager.get_OwnerConfig : IPresenterConfigure;
begin
  Result:= fOwnerConfig;
end;
  
constructor TTransactionManager.Create(anOwner: IPresenterMain;aModel: IModelMain);
begin
  inherited Create;
  fOwner:= anOwner;
  fModel:= aModel;  
  fTrx:= nil;
  fTraxFactory:= TraxClassMapFactory(); 
  UpdateTransactionManager(fTraxFactory);
end;

destructor TTransactionManager.Destroy;
begin       
  if InTransaction then RollbackTransaction; // forget changes
  fOwner:= nil;
  fModel:= nil;
  fTraxFactory:= nil;
  inherited Destroy;
end;

procedure TTransactionManager.CommitTransaction;
var 
  lte: ITrxExec = nil; { (l)ocal (t)rx (e)xec, "self-committing" transactions }
  enr: ITrxExecNoRes = nil; { (e)xec (n)o (r)esult, this returns no result }
begin
  if InTransaction then begin { first we check the 2 autonomous transactions }
    if fTrx.GetInterface(SGUIDITrxExec,lte) then begin
      if not lte.Execute(Self) then { this sibling returns a result of operation }
        fOwner.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(i) Transaction "'+lte.Obj.ClassName+'" failed to execute'));
    end else if fTrx.GetInterface(SGUIDITrxExecNoRes,enr) then begin
      enr.Execute(Self); { this sibling doesn't return a result,
      thus as a consequence, user feedback has to come from the transaction itself
      fOwner.Provider.NotifySubscribers(prStatus,nil,Str2Pch('(i) Transaction "'+enr.Obj.ClassName+'" executed successfully'));
      --- you can enable it if you need it :) --- }
    end else case fTrx.ModReason of { ok, so it wasn't autonomous, then maybe we've made a case for it?!? }
      prNone: raise Exception.Create('Error! TTransactionManager.CommitTransaction: "ModReason = prNone"'); // failsafe
      prDataAdded: ;      // 2  
      { example of using the trxExec interface, "TTextEdit" implements it (if you write the code) }
      prDataChanged: if fTrx.GetInterface(SGUIDITrxExec,lte) then begin
                       if lte.Execute(Self) then
                         fOwner.Provider.NotifySubscribers(prStatus,nil,Str2Pch(' (i) Textfile "'+lte.Title+'" changed successfully'));
                     end;      // 3 
      prDataDeleted: ;    // 4
    end;
    FreeAndNil(fTrx); // done, free and make sure it's nil again!
  end;
end;

function TTransactionManager.InTransaction: boolean;
begin { selbstgänger }
  Result:= Assigned(fTrx);
end;

procedure TTransactionManager.RegisterTransactionClass(aModReason: ptrint; aTransactionClass: TTransactionClass);
begin { this is register for runtime jic, NOT startup!!! }
  fTraxFactory.AddTraxClassSorted(aModReason,aTransactionClass);
end;
  
procedure TTransactionManager.RollbackTransaction;
begin { brutal but effective }
  if InTransaction then FreeAndNil(fTrx);
end;

function TTransactionManager.StartTransaction(aModReason: ptrint): TTransaction;
begin { first we check if the reason is in our TransactionFactory, if so then get it }
  if not InTransaction then begin
    if fTraxFactory.Contains(aModReason) then
      fTrx:= fTraxFactory.GetTransactionByReason(aModReason)
    else case aModReason of { ok, so it wasn't in the factory, then maybe we've made a case for it?!? }
      prCustom: fTrx:= TTransaction.Create(aModReason); // 1 ~ doesn't cause an exception, 0 does!
      prDataChanged: fTrx:= TTextEdit.Create(aModReason); // 1
      //...
      prCreateDir           : fTrx:= TCreDirTrx.Create(aModReason);
      prAppSettings         : fTrx:= TSettingstrx.Create(aModReason);
      prAppSingleSetting    : fTrx:= TSingleSettingTrx.Create(aModReason);
      prFormState           : fTrx:= TSettingsTrx.Create(aModReason);
      prFormStateConfig     : fTrx:= TSettingsConfigTrx.Create(aModReason);
      prAppSettingsConfig   : fTrx:= TSettingsConfigTrx.Create(aModReason);
      prRetrieveData        : fTrx:= TRetrieveDataTrx.Create(aModReason);
      prExportToOroxTtlFile : fTrx:= TExportToOroxTtlFileTrx.Create(aModReason);
      prUniqueStringlist    : fTrx:= TUniqueStringlistTrx.Create(aModReason);
      /// etc...

      else fTrx:= TTransaction.Create(aModReason); // 0 or anything undefined by us
    end;
  end;   
  Result:= fTrx;
end;
{$EndRegion 'TTransactionManager'}

end.
