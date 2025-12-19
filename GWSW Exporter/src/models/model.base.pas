{ Copyright ©2025 Hans van Buggenum }
unit model.base;
{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
interface
uses classes,sysutils,obs_prosu,common.consts;

type
{$interfaces corba}
  { this is the ancestor }
  ICorba = interface['{3563A63E-8C1A-40E1-9574-3E4171BB3ACF}']
    function Obj: TObject;
  end;
  TTransaction = class; // forward
  { ITransaction is our container vessel for changes }
  ITransaction = interface['{AFE2A986-7D3C-46AB-A4BF-2A0BDA1DECDF}']
    function get_DataPtr: pointer;
    function get_Id: ptrint;
    function get_ModReason: ptrint;
    function get_Sender: TObject;
    function get_StrProp(anIndex: integer): shortstring;
    function Obj: TTransaction;
    procedure set_DataPtr(aValue: pointer);
    procedure set_Id(aValue: ptrint);
    procedure set_ModReason(aValue: ptrint);  
    procedure set_Sender(aValue: TObject);
    procedure set_StrProp(anIndex: integer;aValue: shortstring);
    
    property DataPtr: pointer read get_DataPtr write set_DataPtr; 
    property ID: ptrint read get_Id write set_Id;
    Property ModReason: ptrint read get_ModReason write set_ModReason;
    Property Sender: TObject read get_Sender write set_Sender; 
    property Title: shortstring index 0 read get_StrProp write set_StrProp;
  end;
{$interfaces com}

{ ---------------------------------------------------------------------------- }
// New records go here...
    PNewDirectoriesRec = ^TNewDirectoriesRec;  // Used with CreateDirectories, TNewCreDirTransaction
    TNewDirectoriesRec = record
      dirNewDirNames, { we'll translate like this: TStringList.Text:= dirNewDirNames; }
      dirRoot,
      dirAppName,
      dirSuccesMsg: String;
      dirIsWriteable: Boolean;
      dirSucces: boolean;
    end;


    PStatusbarPanelText = ^TStatusbarPanelText;
    TStatusbarPanelText = record
      stbPanelText: String;
      stbActivePanel : Word;
    end;

    PSettingsRec = ^TSettingsRec;  // used with ReadSettings, TSettingstransaction , ReadFormState; and   StoreFormstate;
    TSettingsRec = record
      setActivateLogging,
      setAppendLogging: Boolean;
      setLanguage : String;
      setSettingsFile,
      setDbFileLocation,
      setApplicationName,
      setApplicationVersion,
      setApplicationBuildDate : String;
      setFrmName : String;
      setFrmWindowState,
      setFrmTop,
      setFrmLeft,
      setFrmHeight,
      setFrmWidth,
      setFrmRestoredTop,
      setFrmRestoredLeft,
      setFrmRestoredHeight,
      setFrmRestoredWidth : Integer;
      setSucces,
      setReadSettings,
      setWriteSettings,
      setReadFormState: Boolean;
      setMessage: String;
      setMappingFile: String;
    end;

    PSingleSettingRec = ^TSingleSettingRec;
    TSingleSettingRec = record
      ssSettingsFile,
      ssName,
      ssValue: String;
      ssSucces: Boolean;
      ssMessage: String;
    end;

    PStbPanelsSize = ^TStbPanelsSize;
    TStbPanelsSize = record
      lpWidth,
      mpWidth,
      rpWidth: Integer;
    end;

    { Database connection data }
    PDbConnectRec = ^TDbConnectRec;
    TDbConnectRec = record
      DatabaseName,
      UserName,
      SchemaPassword,
      Message: String;
      HasConnection: Boolean;
    end;

    PRetrieveDataRec = ^TRetrieveDataRec;
    TRetrieveDataRec = record
      SqlText,
      OrganizationName: String;
      DataSource: TObject;
    end;

    PExportToOroxTtlFileRec = ^TExportToOroxTtlFileRec;
    TExportToOroxTtlFileRec = record
      //Dataset: TObject;
      //DataProvider: TObject;
      FileName,
      MappingFile,
      OrganizationName: String;
    end;



{ ---------------------------------------------------------------------------- }
  { TTransaction is our container vessel for changes }
  TTransaction = class(TObject,ITransaction)
  protected
    fDataPtr: pointer;
    fID: ptrint;
    fModReason: ptrint; 
    fSender: TObject;
    fTitle: shortstring;
    function get_DataPtr: pointer; virtual;
    function get_Id: ptrint; virtual;
    function get_ModReason: ptrint; virtual;
    function get_Sender: TObject; virtual;
    function get_StrProp(anIndex: integer): shortstring; virtual;
    function Obj: TTransaction; virtual;
    procedure set_DataPtr(aValue: pointer); virtual;
    procedure set_Id(aValue: ptrint); virtual;
    procedure set_ModReason(aValue: ptrint); virtual;
    procedure set_Sender(aValue: TObject); virtual;
    procedure set_StrProp(anIndex: integer;aValue: shortstring); virtual;
  public
    constructor Create(aModReason: ptrint); virtual;
    destructor Destroy; override;
    property DataPtr: pointer read get_DataPtr write set_DataPtr;
    property ID: ptrint read get_Id write set_Id;
    Property ModReason: ptrint read get_ModReason write set_ModReason;
    Property Sender: TObject read get_Sender write set_Sender;
    property Title: shortstring index 0 read get_StrProp write set_StrProp;
  end; { TTransaction }
  { TTransaction class reference type }          
  TTransactionClass = class of TTransaction;    
  { ITraxClassList is a com-object which holds our registered transaction-classes }
  ITraxClassList = interface(IInterface)['{49E01D1B-B733-4299-A144-172CCC8153D7}']
    function get_Item(aReason: ptrint): TTransactionClass;
    procedure set_Item(aReason: ptrint; aValue: TTransactionClass);  
    { adds the transaction-class to the list no fuzz & quicker, returns current index }
    function AddTraxClass(aModReason: ptrint;aTrax: TTransactionClass): ptrint;
    { adds the transaction-class to the list in a sorted manner, returns current index }
    function AddTraxClassSorted(aModReason: ptrint;aTrax: TTransactionClass): ptrint; 
    { copies everything and the kitchen-sink }
    procedure AssignTo(aDest: ITraxClassList);
    procedure Clear;
    function Count: ptrint;
    { returns the ModReason if the Transaction-class is registered otherwise -1 }
    function Contains(aClass: TTransactionClass): ptrint;
    { returns true if 'aReason' has a Transaction-class registered otherwise false }
    function Contains(aReason: ptrint): boolean;
    { returns (if registered) the transaction ascociated with 'aModReason' otherwise NIL }
    function GetTransactionByReason(aModReason: ptrint): TTransaction;
    { Careful! returns classes! NOT objects ...and returns NIL on non-existend reason!!! }
    property Transactions[aReason: ptrint]: TTransactionClass read get_Item write set_Item; default;
  end; { ITraxClassList } 
  { TTraxClassList is an internal list of registered transaction-classes }
  TTraxClassList = class(TInterfacedObject,ITraxClassList)
  private
  type
    PMapping = ^TMapping;
    TMapping = record ModReason: ptrint; TraxClass: TTransactionClass; end;
  var
    fList: TFPList;
    function get_Item(aReason: ptrint): TTransactionClass;
    procedure set_Item(aReason: ptrint;aValue: TTransactionClass);
  protected
    function CreMapping(aReason: ptrint;aTraxClass: TTransactionClass): PMapping;
    procedure FreeMapping(aMapping: PMapping);
    function IndexOfReason(aReason: ptrint): ptrint;
  public
    constructor Create;
    destructor Destroy; override;  
    { adds the transaction-class to the list no fuzz & quicker, returns current index }
    function AddTraxClass(aModReason: ptrint;aTrax: TTransactionClass): ptrint;
    { adds the transaction-class to the list in a sorted manner, returns current index }
    function AddTraxClassSorted(aModReason: ptrint;aTrax: TTransactionClass): ptrint;
    { copies everything and the kitchen-sink }
    procedure AssignTo(aDest: ITraxClassList);
    procedure Clear; 
    function Count: ptrint;
    { returns the ModReason if the Transaction-class is registered otherwise -1 }
    function Contains(aClass: TTransactionClass): ptrint;
    { returns true if 'aReason' has a Transaction-class registered otherwise false }
    function Contains(aReason: ptrint): boolean;
    { returns (if registered) the transaction ascociated with 'aModReason' otherwise NIL }
    function GetTransactionByReason(aModReason: ptrint): TTransaction;
    { Careful! returns classes! NOT objects ...and returns NIL on non-existend reason!!! }
    property Transactions[aReason: ptrint]: TTransactionClass read get_Item write set_Item; default;
  end; { TTraxClassList }

{ transaction-class registering mechanism }
procedure RegisterTransactionClass(aModReason: ptrint; aTransactionClass: TTransactionClass);
procedure UpdateTransactionManager(aTraxMap: ITraxClassList);
{ factory function for ITraxClassList }
function TraxClassMapFactory(): ITraxClassList;
                                       
implementation

var TCL: ITraxClassList = nil;

{$Region 'Transaction Registry'} 
function TraxClassMapFactory(): ITraxClassList;
begin
  Result:= TTraxClassList.Create;
end;

procedure RegisterTransactionClass(aModReason: ptrint; aTransactionClass: TTransactionClass);
begin
  if not Assigned(TCL) then TCL:= TraxClassMapFactory(); { lazy creation }
  TCL.AddTraxClassSorted(aModReason,aTransactionClass); { cache class for later creation }
end;

procedure UpdateTransactionManager(aTraxMap: ITraxClassList);
begin
  TCL.AssignTo(aTraxMap); { everything and the kitchen-sink }
  TCL:= nil; { release this reference, it's not needed after the manager is up }
end;
{$EndRegion 'Transaction Registry'}
{$Region 'TTraxClassList'}
{ TTraxClassList }
function TTraxClassList.Count: ptrint;
begin
  Result:= fList.Count;
end;

function TTraxClassList.get_Item(aReason: ptrint): TTransactionClass;
var
  lidx: ptrint;
begin
  lidx:= IndexOfReason(aReason);
  if lidx > -1 then Result:= PMapping(fList[lidx])^.TraxClass else Result:= nil;
end;

procedure TTraxClassList.set_Item(aReason:ptrint; aValue:TTransactionClass);
var
  lidx: ptrint;
begin
  lidx:= IndexOfReason(aReason);               { is it registered? }
  if lidx > -1 then begin                      { if yes, then }
    FreeMapping(fList[lidx]);                  { free the old one and }
    fList[lidx]:= CreMapping(aReason,aValue); { substitute with a new one}
  end; { else non-existing -> add?!? }
end;

function TTraxClassList.CreMapping(aReason: ptrint; aTraxClass: TTransactionClass): PMapping;
begin
  assert(((aTraxClass <> nil) and (aReason > -1)),'aTraxClass is NIL or aReason < 0!');
  new(Result);
  Result^.ModReason:= aReason;
  Result^.TraxClass:= aTraxClass;
end;

procedure TTraxClassList.FreeMapping(aMapping: PMapping);
begin
  if aMapping <> nil then dispose(aMapping);
end;

function TTraxClassList.IndexOfReason(aReason: ptrint): ptrint;
var
  li: ptrint;
begin
  Result:= -1;
  for li:= 0 to fList.Count-1 do
    if PMapping(fList[li])^.ModReason = aReason then exit(li);
end;

constructor TTraxClassList.Create;
begin
  inherited Create;
  fList:= TFPList.Create;
end;

destructor TTraxClassList.Destroy;
begin
  Clear; { frees mappings }
  fList.Free;
  inherited Destroy;
end;

function TTraxClassList.AddTraxClass(aModReason: ptrint; aTrax: TTransactionClass): ptrint;
begin
  Result:= fList.Add(CreMapping(aModReason,aTrax));
end;

{ A < B: result <= -1,      A = B: result = 0,       A > B: result >= 1 }
function ReasonCompare(A,B: ptrint): integer; begin Result:= (A - B); end;

function TTraxClassList.AddTraxClassSorted(aModReason: ptrint; aTrax: TTransactionClass): ptrint;
var
  li: ptrint = 0;
begin { add sorted }
  Result:= IndexOfReason(aModReason); { should be -1 ~ not found }
  if Result > -1 then exit(Result); { no doublets, return existing }
  if fList.Count = 0 then exit(fList.Add(CreMapping(aModReason,aTrax))); { easy one first }
  while li < fList.Count do begin
    if ReasonCompare(PMapping(fList[li])^.ModReason,aModReason) > 0 then begin { found bigger }
      fList.Insert(li,CreMapping(aModReason,aTrax)); { now insert in its place }
      exit(li);                              { ...and we're done, return index }
    end;
    inc(li);
  end;
  { if we get here, then a bigger Reason was not in list & li = flist.count }
  if li = fList.Count then Result:= fList.Add(CreMapping(aModReason,aTrax));
end;

procedure TTraxClassList.AssignTo(aDest: ITraxClassList);
var
  pm: PMapping;
begin { this is a copy operation, hence no sorting, 'cause it's already sorted on addition }
  if aDest = nil then exit;
  for pm in fList do aDest.AddTraxClass(pm^.ModReason,pm^.TraxClass);
end;

procedure TTraxClassList.Clear;
var
  li: ptrint = 0;
begin { here we free mappings, before clearing the list }
  for li:= fList.Count-1 downto 0 do FreeMapping(fList[li]);
  fList.Clear;
end;

function TTraxClassList.Contains(aClass: TTransactionClass): ptrint;
var
  li: ptrint;
begin { returns the ModReason if the Transaction-class is registered otherwise -1 }
  Result:= -1;
  for li:= 0 to fList.Count-1 do
    if PMapping(fList[li])^.TraxClass = aClass.ClassType then exit(PMapping(fList[li])^.ModReason);
end;

function TTraxClassList.Contains(aReason: ptrint): boolean;
begin
  Result:= (IndexOfReason(aReason) > -1);
end;

function TTraxClassList.GetTransactionByReason(aModReason: ptrint): TTransaction;
var
  li: ptrint;
begin { this list is sorted without doublets }
  Result:= nil;
  li:= IndexOfReason(aModReason);
  if li > -1 then Result:= PMapping(fList[li])^.TraxClass.Create(aModReason);
end;
{$EndRegion 'TTraxClassList'}
{$Region 'TTransaction'}
{ TTransaction }
function TTransaction.get_DataPtr: pointer;
begin
  Result:= fDataPtr;
end;

function TTransaction.get_Id: ptrint;
begin
  Result:= fID;
end;

function TTransaction.get_ModReason: ptrint;
begin
  Result:= fModReason;
end;

function TTransaction.get_Sender: TObject;
begin
  Result:= fSender;
end;

function TTransaction.get_StrProp(anIndex: integer): shortstring;
begin
  case anIndex of
    0: Result:= fTitle;
    1: ;
    2: ;
    else Result:= '';
  end;
end;

function TTransaction.Obj: TTransaction;
begin
  Result:= Self;
end;

procedure TTransaction.set_DataPtr(aValue: pointer);
begin
  fDataPtr:= aValue;
end;

procedure TTransaction.set_Id(aValue: ptrint);
begin
  fID:= aValue;
end;

procedure TTransaction.set_ModReason(aValue: ptrint);
begin
  fModReason:= aValue;
end;

procedure TTransaction.set_Sender(aValue: TObject);
begin
  fSender:= aValue;
end;

procedure TTransaction.set_StrProp(anIndex: integer;aValue: shortstring);
begin
  case anIndex of
    0: fTitle:= aValue;
    1: ;
    2: ;
  end;
end;

constructor TTransaction.Create(aModReason: ptrint);
begin
  inherited Create;
  fModReason:= aModReason;
end;

destructor TTransaction.Destroy;
begin
  inherited Destroy;
end;
{$EndRegion 'TTransaction'}


end.

