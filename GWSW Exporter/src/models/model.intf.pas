{ Copyright ©2025 Hans van Buggenum }
unit model.intf;
{$mode ObjFPC}{$H+}
{-$define dbg}
interface
uses classes, sysutils, istrlist, model.base, obs_prosu;
type
{$interfaces corba}
  IPresenterMain = interface;       // forward
  IPresenterConfigure = interface;  // forward

  { aliases for provider & subscriber }
  IobsProvider = obs_prosu.IobsProvider;
  IobsSubscriber = obs_prosu.IobsSubscriber;
  { alias for transaction }
  ITransaction = model.base.ITransaction;

  { Model Maiin-----------------------------------------------------------------}
  { the actual model/datastore api, the "King" }
  IModelMain = interface(ICorba)['{0349B40E-FBB4-4C6E-9CB0-A63690CF0898}']
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

    procedure MakeDbConnection(DbConnectionData : PDbConnectRec);
    function IsConnected: Boolean;
    function RetrieveData(Data: PRetrieveDataRec): TRetrieveDataRec;
    function ExportToOroxTtlFile(Data: PExportToOroxTtlFileRec): TExportToOroxTtlFileRec;
    function GetSQLfileLocation: String;
    function GetKeepLastOrganization: Boolean;
    function GetLastUsedOrganization: String;
    // get a setting
    function GetSetting_AskToOpenExportFile: Boolean;
    procedure DisableChildControls(aData : TExportInProgressRec);
  end; { IModelMain }

  {--------------------------------------------------------------------------------------------------------}
  { the child model/datastore api, the "BigBrother" }
  IModelConfigure = interface(ICorba)['{F5E9E12D-F9B5-412F-9708-C751F50A14D2}']
    function GetStaticTexts(const aSection: string; out aTarget: integer): IStringList;
    procedure RefreshSections;
    procedure ReloadTextCache;
    procedure SwitchLanguage;
    function DoesFileExists(const settingsFile: String): Boolean;
    function ReadFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function ReadSettings(Settingsdata: PSettingsRec): TSettingsRec;
    function StoreFormState(Settingsdata: PSettingsRec): TSettingsRec;
    function WriteSettings(Settingsdata: PSettingsRec): TSettingsRec;
    procedure WriteSingleSetting(SettingData : PSingleSettingRec);
    procedure WriteToLog(const aLogAction, LogText: String);
  end; { IModelConfigure }


  { ITransactionManager reacts to user actions }
  ITransactionManager = interface(ICorba)['{C75AE70E-43DA-4A76-A52D-061AFC6562D0}']
    function get_ActiveTrx: ITransaction;
    function get_Model: IModelMain;
    function get_ModelConfig : IModelConfigure;
    function get_Owner: IPresenterMain;
    procedure CommitTransaction;
    function get_OwnerConfig : IPresenterConfigure;
    function InTransaction: boolean; { below is register for runtime jic, NOT startup!!! }
    procedure RegisterTransactionClass(aModReason: ptrint; aTransactionClass: TTransactionClass);
    procedure RollbackTransaction;
    procedure set_ModelConfig(AValue : IModelConfigure);
    procedure set_OwnerConfig(AValue : IPresenterConfigure);
    function StartTransaction(aModReason: ptrint): TTransaction;
    property ActiveTrx: ITransaction read get_ActiveTrx;

    // Main
    property ModelMain: IModelMain read get_Model;
    property OwnerMain: IPresenterMain read get_Owner;
    // Configure
    property ModelConfig: IModelConfigure read get_ModelConfig write set_ModelConfig;
    property OwnerConfig: IPresenterConfigure read get_OwnerConfig write set_OwnerConfig;
  end; { ITransactionManager }

  { specialized transaction, sports an 'execute' method, i.e.: it knows how to commit itself ;-) }
  ITrxExec = interface(ITransaction)['{2E618F58-DE15-4A79-ADEF-C4E0A7CBECA4}']
    function Execute(aMgr: ITransactionManager): boolean;
  end; { ITrxExec } 
  { sibling trx, but without returning a result }
  ITrxExecNoRes = interface(ITransaction)['{5E4DCDBA-8CB0-45E4-8DC3-6CD859DB4F1B}']
    procedure Execute(aMgr: ITransactionManager);
  end; { ITrxExecNoRes }

  { Presenter Main--------------------------------------------------------------}
  { the "Queen" }
  IPresenterMain = interface(ICorba)['{736B14F8-8BB7-4F5D-ADAA-B90A1735765C}']
    function get_Model: IModelMain;
    function get_Provider: IobsProvider;
    function get_TrxMan: ITransactionManager;
    procedure set_Provider(aValue: IobsProvider);
    procedure GetStaticTexts(const aSection: string); 
    procedure RefreshTextCache(const aSection,aLangStr: string);
    function CreatePresenterConfig(): IPresenterConfigure;
    //...
    procedure SwitchLanguage(const aSection: String);
    procedure SetStatusbarText(const aText: string; panel: word);
    function GetstaticText(const aView, aText: string): string;
    procedure StartLogging;
    procedure WriteToLog(const aSection, aLogType, LogText: String);
    procedure SetStatusbarPanelsWidth(Sender: TObject; stbWithd, lpWidth, rpWidth: Integer);
    procedure MakeDbConnection(DbConnectionData: PDbConnectRec);
    function GetSQLfileLocation: String;
    function GetKeepLastOrganization: Boolean;
    function GetLastUsedOrganization: String;
    procedure ExportInProgress(aData: TExportInProgressRec);

    property Model: IModelMain read get_Model; // TODO: write set_Model;
    property Provider: IobsProvider read get_Provider write set_Provider;
    property TrxMan: ITransactionManager read get_TrxMan;
  end; { IPresenterMain }


  {--------------------------------------------------------------------------------------------------------}
  { the "BigSister" }
  IPresenterConfigure = interface(ICorba)['{C948C2C9-2590-4030-B8B3-54B44AEB06F9}']
    function get_Model: IModelConfigure;
    function get_Provider: IobsProvider;
    function get_TrxMan: ITransactionManager;
    procedure set_Provider(aValue: IobsProvider);
    procedure GetStaticTexts(const aSection: string);
    function GetstaticText(const aView, aText: string): string;
    procedure RefreshTextCache(const aSection,aLangStr: string);
    //...
    procedure SwitchLanguage(const aSection: String);
    procedure WriteToLog(const aSection, aLogAction, LogText: String);

    property Model: IModelConfigure read get_Model;
    property Provider: IobsProvider read get_Provider write set_Provider;
    property TrxMan: ITransactionManager read get_TrxMan;
  end; { IPresenterConfigure }


  { View Main ------------------------------------------------------------------}
  { this is the 'contract' the view has to fulfill, for us to work with it }
  IViewMain = interface(ICorba)['{5AC3C283-C7EB-437D-8A72-3A0BD7A47B1C}']
    function get_Observer: IobsSubscriber; 
    function get_Presenter: IPresenterMain;
    procedure set_Observer(aValue: IobsSubscriber);
    procedure set_Presenter(aValue: IPresenterMain);
    procedure HandleObsNotify(aReason: ptrint; aNotifyObj: TObject; aData: pointer);
    { mainly here for when implementing console-views for the same back-end / engine }
    procedure Show; // TForm implements it too

    property Observer: IobsSubscriber read get_Observer write set_Observer;
    property Presenter: IPresenterMain read get_Presenter write set_Presenter;
  end; { IViewMain }

  { this is the 'contract' the view has to fulfill, for us to work with it }
  IViewConfigure = interface(ICorba)['{07D9D3BE-8426-4E7A-BC7F-5AC0BBD70803}']
    function get_Observer: IobsSubscriber;
    function get_Presenter: IPresenterConfigure;
    procedure set_Observer(aValue: IobsSubscriber);
    procedure set_Presenter(aValue: IPresenterConfigure);
    procedure HandleObsNotify(aReason: ptrint; aNotifyObj: TObject; aData: pointer);
    { mainly here for when implementing console-views for the same back-end / engine }
    procedure Show; // TForm implements it too

    property Observer: IobsSubscriber read get_Observer write set_Observer;
    property Presenter: IPresenterConfigure read get_Presenter write set_Presenter;
  end; { IViewConfigure }
{$interfaces com}

{ convenience function for cleaning up CORBA references }
procedure FreeCorbaThenNil(var anIntf); 
{ registers sections for use in 'GetStaticTexts()', put it in 'initialization'
  at the bottom of the unit, for the objects/units you'll be registering }
procedure RegisterSection(const aSect: string);
procedure RegisterConfigureSection(const aSect: string);
{ gets called from model.main.constructor to update the 'Sects' array }
procedure UpdateSections(var anArray: TStringArray); // <- - needs 'sysutils' added in top-uses
procedure UpdateConfigureSections(var anArray: TStringArray);

implementation
uses contnrs;

var QSections, ConfSections: TQueue; { storage for our registrations till the model comes online }

procedure FreeCorbaThenNil(var anIntf);
begin
  if pointer(anIntf) <> nil then begin
    ICorba(anIntf).Obj.Free;
    pointer(anIntf):= nil;
  end;
end;
   
procedure RegisterSection(const aSect: string);
begin
  if aSect = '' then exit; { no empty allocations }
  QSections.Push(strnew(pchar('['+aSect+']'))); { enqueue the [section] for later update }
end;

procedure RegisterConfigureSection(const aSect : string);
begin
  if aSect = '' then exit; { no empty allocations }
  ConfSections.Push(strnew(pchar('['+aSect+']'))); { enqueue the [section] for later update }
end;

procedure UpdateSections(var anArray: TStringArray);
var
  lai: integer;
  lp: pchar = nil;
begin
  if anArray = nil then exit; { we want at least '[view.main]' @ idx 0 present before we start }
  lai:= length(anArray); // start index for additions ~ 1
  SetLength(anArray,lai+QSections.Count); { make room for all the registered sections at once }
  while QSections.AtLeast(1) do begin { now as long as there are items in the queue }
    lp:= QSections.Pop;                  { dequeue/pop the sections from queue }
    anArray[lai]:= string(lp);                  { and add to array as a string }
    inc(lai);                       { step our (l)ocal(a)rray(i)ndex 1 forward }
    StrDispose(lp);      { now release the memory the string was being held in }
  end;
end;

procedure UpdateConfigureSections(var anArray : TStringArray);
var
  lai: integer;
  lp: pchar = nil;
begin
  if anArray = nil then exit; { we want at least '[view.configure]' @ idx 0 present before we start }
  lai:= length(anArray); // start index for additions ~ 1
  SetLength(anArray,lai+ConfSections.Count); { make room for all the registered sections at once }
  while ConfSections.AtLeast(1) do begin { now as long as there are items in the queue }
    lp:= ConfSections.Pop;                  { dequeue/pop the sections from queue }
    anArray[lai]:= string(lp);                  { and add to array as a string }
    inc(lai);                       { step our (l)ocal(a)rray(i)ndex 1 forward }
    StrDispose(lp);      { now release the memory the string was being held in }
  end;
end;

procedure FinalizeQueue;
begin
  while QSections.AtLeast(1) do StrDispose(QSections.Pop); { clear the queue, jic }
  QSections.Free; { free the queue }

  while ConfSections.AtLeast(1) do StrDispose(ConfSections.Pop);
  ConfSections.Free; { free the config queue }
end;

initialization
  QSections:= TQueue.Create;
  ConfSections:= TQueue.Create;

finalization
  FinalizeQueue;

end.
