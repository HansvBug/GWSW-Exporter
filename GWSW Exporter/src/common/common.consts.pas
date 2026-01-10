{ Copyright ©2025-2026 Hans van Buggenum }
unit common.consts;
{$mode objfpc}{$H+}
interface (* this unit collects nearly all constants & is referenced in most units *)
uses classes, sysutils, obs_prosu;
const
  { Application constants }
  Application_version            = '0.4.0.0';
  Application_initial_start_date = '12-12-2025';
  Application_build_date         = '10-01-2026';

  DefaultOroXFileExt = 'orox.ttl';// Is prescribed in this way.

  { (a)pplication (m)essages }
  amSucces = 'Success';
  amFailed = 'Failed';

  { (l)og (T)ype }
  ltInformation = 'Information';
  ltWarning     = 'Warning';
  ltError       = 'Error';
  ltDebug       = 'Debug';

  (* from model.base *)
  SGUIDITransaction = '{AFE2A986-7D3C-46AB-A4BF-2A0BDA1DECDF}';
  { (p)rovider (r)easons for observer action/reaction, they're here because then we can use them in
    'presenter.trax' without /cyclic dependencies/ with 'model.decl' :o) }
  prStatus = obs_prosu.prStatus; { carries an optional object in aNotifyClass and usually a pchar in UserData }
  prMainStaticTexts         = prUser + 2; { carries an 'IStrings' object in UserData }
  prCreateDir               = prUser + 3;
  prStatusBarPanelText      = prUser + 4;
  prLoggingText             = prUser + 5;
  prAppSettings             = prUser + 6;
  prAppSingleSetting        = prUser + 7;
  prFormState               = prUser + 8;
  prStatusBarPanelWidth     = prUser + 9;
  prDbConnection            = prUser + 10;
  prRetrieveData            = prUser + 11;
  prExportToOroxTtlFile     = prUser + 12;
  prReportProgress          = prUser + 13;
  prReportError             = prUser + 14;
  prReportProgressCount     = prUser + 15;
  prUniqueStringlist        = prUser + 16;

  prDirDataNeeded = prUser + 100; { carries a TDirInfoTrx in aNotifyClass }
  prFetchDirData = prUser + 101; { carries a TDirInfoTrx in aNotifyClass & an 'IStrings' in UserData }
  prFetchFileData = prUser + 102; { carries a TDirInfoTrx in aNotifyClass & an 'IStrings' in UserData }
  //etc...

  // View.configure
  prConfigStaticTexts    = prUser + 1000;
  prFormStateConfig      = prUser + 1001;
  prAppSettingsConfig    = prUser + 1002;
  prStaticHintsConfig    = prUser + 1003;
  prAppSingleSettingConfig = prUser + 1004;

  (* from model.decl *)
  { string guids for the interfaces in this app, used when we can't "see" the type }
  SGUIDIModelMain = '{0349B40E-FBB4-4C6E-9CB0-A63690CF0898}';          //=^
  SGUIDIPresenterMain = '{736B14F8-8BB7-4F5D-ADAA-B90A1735765C}';      //=^
  SGUIDITransactionManager = '{C75AE70E-43DA-4A76-A52D-061AFC6562D0}'; //=^ 
  SGUIDITraxClassList = '{49E01D1B-B733-4299-A144-172CCC8153D7}';      //=^
  SGUIDITrxExec = '{2E618F58-DE15-4A79-ADEF-C4E0A7CBECA4}';            //=^
  SGUIDITrxExecNoRes = '{5E4DCDBA-8CB0-45E4-8DC3-6CD859DB4F1B}';       //=^
  SGUIDIViewMain = '{5AC3C283-C7EB-437D-8A72-3A0BD7A47B1C}';           //=^
  SGUIDIModelConfigure = '{F5E9E12D-F9B5-412F-9708-C751F50A14D2}';
  SGUIDIPresenterConfigure = '{C948C2C9-2590-4030-B8B3-54B44AEB06F9}';
  SGUIDIViewConfigure = '{07D9D3BE-8426-4E7A-BC7F-5AC0BBD70803}';

  { targettexts below lets you use 1 handler in view, 1 result record and differentiate on target-id }
  TargetTexts: TStringArray = ('Directories', 'Resources', 'Models', 'Presenters', 'Views'); // example

  { (a)pplication (d)irectories }
  adSettings   = 'Settings';
  adLogging    = 'Logging';
  adDomainlist = 'Domainlist';
  adExport     = 'Export';
  adQueries    = 'Queries';

  // GWSW versies
  GWSW_versie_16 = '1.6';

  { (e)xport (e)rror (t)ypes }
  eetInformation     = 0;  // Default
  eetMapping         = 1;  // Mapping fout - bij het vertalen van velden
  eetFatal           = 2;  // Voorlopig onbekende fouten hier onder zetten. Moet dan onderzocht worden
  eetFieldIsEmpty    = 3;  // Veld is leeg.
  eetFieldIsMissing  = 4;  // Veld ontbreekt
  eetValueOutOfRange = 5;  // De aangetroffen waarde valt buiten het bereik dat gwsw opgeeft.

implementation

end.
