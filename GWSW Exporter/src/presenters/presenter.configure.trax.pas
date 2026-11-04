{ Copyright ©2025-2026 Hans van Buggenum
  MIT License
}
{ The base of this unit is made with "MVP-Setup" from Benny Christensen a.k.a. cdbc
  https://gitlab.com/cdbc-public/releases/mvp-setup
}
unit presenter.configure.trax;
{$mode objfpc}{$H+}
{.$define dbg}
interface
uses classes,sysutils,obs_prosu, common.consts, {istrlist,} model.intf, model.base{,
  exp.dirinfo}; /// <experimental> ///

(* this unit is by design, for specialized transactions... can have siblings :o)    *)
(* the 'transaction-manager now uses a transaction-factory, which means: to use it  *)
(* correctly, you have to 'register' your transaction class in correlation with the *)
(* 'modreason' at unit startup, i.e.: 'initialization' part  --  see at the bottom. *)

{ -----------------------------------------------------------------------------}

type
  { TSingleSettingConfigTrx }
  TSingleSettingConfigTrx  = class(TTransaction, ITrxExec)
    private
      fSettingName: String;
      fSettingsFile: String;
      fSettingValue: String;
    public
      function Execute(aMgr: ITransactionManager): boolean;
      property SettingsLocationAndFileName: String read fSettingsFile write fSettingsFile;
      property SettingName: String read fSettingName write fSettingName;
      property SettingValue: String read fSettingValue write fSettingValue;
  end;

  { TLoadConfigFormStateTrx }

  TLoadConfigFormStateTrx = class(TTransaction, ITrxExec)
   private
     fSettingsFile: string;
     fFormName: string;
   public
     function Execute(aMgr: ITransactionManager): Boolean;
     property SettingsFile: string read fSettingsFile write fSettingsFile;
     property FormName: string read fFormName write fFormName;
   end;

  { TStoreConfigFormStateTrx }

  TStoreConfigFormStateTrx = class(TTransaction, ITrxExec)
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

procedure InitTrax;
begin
//  RegisterTransactionClass(prSimpleTestConfigure,TSimpleTestTrx);  // Dit is de manier om met de fabriek te werken.!!!
end;


{ TSingleSettingConfigTrx }
function TSingleSettingConfigTrx.Execute(aMgr: ITransactionManager): boolean;
var
  lRec: TSingleSettingRec;
begin
  Result:= aMgr.OwnerConfig.Model.DoesFileExists(SettingsLocationAndFileName);

  if Result then begin
    lRec.ssSettingsFile:= SettingsLocationAndFileName;
    lRec.ssName:= SettingName;
    lRec.ssValue:= SettingValue;

    aMgr.OwnerMain.Model.WriteSingleSetting(@lRec);
  end
  else begin
    lRec.ssSucces:= False;
    lRec.ssMessage:= 'SettingsFileNotExists';
    aMgr.OwnerConfig.Provider.NotifySubscribers(prAppSingleSetting, Self, @lRec);
  end;
end;

{ TLoadConfigFormStateTrx }
function TLoadConfigFormStateTrx.Execute(aMgr: ITransactionManager): Boolean;
var
  lRec: TSettingsRec;
begin
  lRec.setSettingsFile:= fSettingsFile;
  lRec.setFrmName:= fFormName;
  lRec:= aMgr.OwnerConfig.Model.ReadFormState(@lRec);
  lRec.setReadFormState:= True;   // ← nieuw
  aMgr.OwnerConfig.Provider.NotifySubscribers(prLoadConfigFormState, Self, @lRec);
  Result:= lRec.setSucces;
end;

{ TStoreConfigFormStateTrx }
function TStoreConfigFormStateTrx.Execute(aMgr: ITransactionManager): Boolean;
var
  lRec: TSettingsRec;
begin
  lRec:= fSettings;
  lRec.setSettingsFile:= fSettingsFile;
  lRec.setStoreFormState:= True;
  lRec:= aMgr.OwnerConfig.Model.StoreFormState(@lRec);
  lRec.setStoreFormState:= True;
  aMgr.OwnerConfig.Provider.NotifySubscribers(prStoreConfigFormState, Self, @lRec);
  Result:= lRec.setSucces;
end;

initialization
  InitTrax; { sets up & registers transactions with TrxManager's Class-Factory }

finalization
  //FreeAndNil(__Example);

end.
