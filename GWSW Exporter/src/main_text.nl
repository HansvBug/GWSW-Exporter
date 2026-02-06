[view.main]
frmMain=GWSW-Exporter
miProgram=&Programma
miProgramClose=Afsluiten

miOptions=&Opties
miOptionsOptions=&Opties...
miOptionsLanguage=&Taal
miOptionsLanguageEN=&Engels
miOptionsLanguageNL=&Nederlands

tsPrepare=Voorbereiden
tsExportSettings=Export instellingen
tsQuery=Query
gbConnection=Connectie
gbGetData=Haal de data op
gbExport=Exporteer
btnSaveQuery=Opslaan
btnOpenQuery=Open
btnSaveErrorLog=Opslaan
btnExportDbgridToCsv=Opslaan
lblDatabaseName=Database
lblUserName=User
lblPassword=Wachtwoord
lblOrganizationName=Organisatie
lblOrganizationNameCsv=Organisatie
lblProgressTitle=Voortgang log
lblProgress=-
lblError=Fouten log
btnConnect=Connect
btnDisconnect=Disconnect
btnGetData=Haal de data op
btnExportToFileOra=Export
btnExportToFileCSV=Export
btnClose=Sluiten
MappingsFileIsMissing=Mappingsbestand ontbreekt. Ga naar tabblad instellingen.
SaveSqlFile=Opslaan sql bestand.
SaveErrorLogFile=Error log opslaan
SaveCSVFile=Opslaan csv bestand.
DlgSqlFilesFilter=SQL bestanden|*.sql|Alle bestanden|*.*
DlgCSVFilesFilter=CSV Bestanden|*.csv|Alle bestanden|*.*
DlgSaveTtlFile=GWSW-OroX Bestanden (*.orox.ttl)|*.orox.ttl|Turtle Bestanden (*.ttl)|*.ttl|Alle bestanden (*.*)|*.*
DlgSaveErrorLogFilesFilter=Tekstbestanden (*.txt)|*.txt|Alle bestanden (*.*)|*.*					  
OpenSqlFile=Open sql bestand.
chkPutGrondwaterstand=Grondwaterstand
ExportFailed=Export gefaald: 
MappingFileNotFound=Exporteren is mislukt, mapping bestand niet gevonden.
TypeIsMissing=Type ontbreekt in de bron data. Is een verplicht onderdeel van GWSW. Bron nakijken.
ReportError=Fout: 
ReportInfo=Info: 
Warning=Waarschuwing
Information=Informatie
QueryNotActive=Query is niet actief. Activeer eerst de query.
MissingMappingFile=Mappingsbestand ontbreekt.
QueryFileNotLoaded=Sql bestand is niet ingeladen.
OrganizationNameIsBlank=Organisatienaam mag niet leeg zijn.
EnterDasetName=Vul hier de datasetnaam in zoals deze op de GWSW-server bekend is.

StelseltypeIsMissing=Geen "Stelseltype" gevonden. Brondata aanpassen. "Type" is verplicht, de validatie op de gwsw server verwacht altijd een type.
PuttypeIsMissing=Geen "Puttype" gevonden. Brondata aanpassen. "Type" is verplicht, de validatie op de gwsw server verwacht altijd een type.
LeidingtypeIsMissing=Geen "Leidingtype" gevonden. Brondata aanpassen. "Type" is verplicht, de validatie op de gwsw server verwacht altijd een type.
PersleidingtypeIsMissing=Geen "Persleidingtype" gevonden. Brondata aanpassen. "Type" is verplicht, de validatie op de gwsw server verwacht altijd een type.
KolktypeIsMissing=Geen "Kolktype" gevonden. Brondata aanpassen. "Type" is verplicht, de validatie op de gwsw server verwacht altijd een type.

MappingManhole=Mapping Put...
MappingSewerSystem= Mapping Stelsel...
MappingPipeline=Mapping Leiding...
MappingMechanicalPipeline=Mapping Mechanische leiding...
MappingGully=Mapping Kolk...

ManholeWidth=Put: Breedte moet tussen 
ManholeLength=Put: Lengte moet tussen 
ManholeHeight=Put: Hoogte moet tussen 
ManholeDiameter=Put: Diameter moet tussen 
PipelineWidth=Leiding: Breedte moet tussen 
PipelineLength=Leiding: Lengte moet tussen 
PipelineHeight=Leiding: Hoogte moet tussen 
PipelineDiameter=Leiding: Diameter moet tussen 
MechanicPipelineWidth=Mechanische leiding: Breedte moet tussen 
MechanicPipelineLength=Mechanische leiding: Lengte moet tussen 
MechanicPipelineHeight=Mechanische leiding: Hoogte moet tussen 
MechanicPipelineDiameter=Mechanische leiding: Diameter moet tussen 
GullyWidth=Kolk: Breedte moet tussen 
GullyLength=Kolk: Lengte moet tussen 
GullyHeight=Kolk: Hoogte moet tussen 
GullyDiameter=Kolk: Diameter moet tussen 
And= en 
Millimeters= mm liggen. 
Found=Aangetroffen: 
Meters= m liggen. 

ExportIsCompleted=De export is voltooid.
OpenFile=Bestand openen?
ExportIsCompleteOpenFile=Export voltooid. Bestand openen?
TotalCountRecordsExported=Export voltooid, %d records gëexporteerd.
ExportingNumberFromTotalCount=Bezig met exporteren: %d van %d
ExportStarted=Export gestart...
NoErrorReportPresent=Er is geen fouten logging aanwezig

gbManholes=Rioolput
chkIncludePutLengte=Lengte
chkIncludePutBreedte=Breedte
chkIncludePutHoogte=Hoogte
chkIncludePutDiameter=Diameter
chkIncludePutVorm=Vorm
chkIncludePutMateriaal=Materiaal
chkIncludePutFundering=Fundering
chkIncludePutBegindatum=Begindatum
chkIncludePutEinddatum=Einddatum
chkIncludePutMaaiveldhoogte=Maaiveldhoogte

gbPipelines=Vrijvervalleidingen
chkIncludeLeidingLengte=Lengte
chkIncludeLeidingBreedte=Breedte
chkIncludeLeidingHoogte=Hoogte
chkIncludeLeidingDiameter=Diameter
chkIncludeLeidingVorm=Vorm
chkIncludeLeidingMateriaal=Materiaal
chkIncludeLeidingFundering=Fundering
chkIncludeLeidingStatusFunctioneren=Status functioneren
chkIncludeLeidingWIBONThema=Wibon thema
chkIncludeLeidingBegindatum=Begindatum
chkIncludeLeidingEinddatum=Einddatum
chkIncludeLeidingBobBegin=B.o.b. begin
chkIncludeLeidingBobEind=B.o.b. eind

gbMechanicalPipeline=Persleiding
chkIncludePersleidingLengte=Lengte
chkIncludePersleidingHoogte=Hoogte
chkIncludePersleidingDiameter=Diameter
chkIncludePersleidingVorm=Vorm
chkIncludePersleidingMateriaal=Materiaal
chkIncludePersleidingStatusFunctioneren=Status functioneren
chkIncludePersleidingBegindatum=Begindatum
chkIncludePersleidingEinddatum=Einddatum
chkIncludePersleidingBobBegin=Diepte begin
chkIncludePersleidingBobEind=Diepte eind

gbGully=Kolk
chkIncludeKolkLengte=Lengte
chkIncludeKolkBreedte=Breedte
chkIncludeKolkHoogte=Hoogte
chkIncludeKolkDiameter=Diameter
chkIncludeKolkVorm=Vorm
chkIncludeKolkMateriaal=Materiaal
chkIncludeKolkWanddikte=Wanddikte
chkIncludeKolkBegindatum=Begindatum
chkIncludeKolkEinddatum=Einddatum

tsOracle=Oracle
tsCsv=Csv
btnSelectCsvFile=Selecteer een CSV bestand
SelectCsvFile=Selecteer een CSV bestand
CsvFileInUse=Het csv bestand is in gebruik door een ander proces.

[view.main.statusbartexts]
Welcome=Welkom.
DirIsNotWriteable=
ErrorCreateDir=(!) Onverwachte fout bij het aanmaken van de directory's.
ErrorReadSettings=(!) Onverwachte fout bij het ophalen van de settings.
ErrorReadFormState=(!) Onverwachte fout bij het ophalen van de window positie.
ErrorStoreFormState=(!) Onverwachte fout bij het opslaan van de window positie.
ErrorSaveSingleSetting=(!) Onverwachte fout bij het opslaan van een setting.
Connecting=Connectie maken...
DbConnEstablished=Databaseverbinding tot stand gebracht
ErrorRetrieveData=(!) Onverwachte fout bij het ophalen van de data.
RetrievingData=Ophalen data...
ErrorExportToOroxTtlFile=(!) Onverwachte fout bij het aanmaken van het exportbestand.
ErrorUniqueStringlist=(!) Onverwachte fout bij het aanmaken van de unieke stringlist.
DbConnNotInitialized=Databaseverbinding niet geïnitialiseerd

DbError=(!)Database fout. 
CannotFindConnIdentifier=Kan de verbindingsidentificatie niet vinden. Controleer TNSNAMES.ORA.
InvalidUserOrPwd=Ongeldige gebruikersnaam/wachtwoord.
NoListener=Geen listener op de server.
ListenerDoesNotKnowServer=Listener kent de services niet.
SIDNotFound=SID niet gevonden.
NUllPwd=Geen wachtwoord gegeven; Login geweigerd.
ConnectTimeout=Verbinding time-out is opgetreden
OperationTimeout=Geen reactie
ProtocolAdapterError=Protocol adapter fout
NoDataFound=Geen data gevonden
TooManyRows=Te veel rijen

ServerNotFound=Netwerkfout: Kan server niet vinden.
ServerNotResponding=Timeout: Server reageert niet.
CheckConnection=Netwerkfout: Controleer uw verbinding.
InternalError=Interne fout
SavingFile=Opslaan bestand...
CsvFileInUse=Het csv bestand is in gebruik door een ander proces.
FileSaved=Het bestand is opgeslagen.
FileLockedError=Bestand kon niet geopend worden (locked door andere applicatie).
StreamError=Schrijffout (disk vol, permissies, etc.) 
UnexpectedExportError=Onverwachte fout bij het opslaan.
CsvFileInUse=Het csv bestand is in gebruik door een ander proces.
LoadingCSVData=Ophalen data uit csv bestand...


[view.main.hints]


[view.main.logging]
SwitchlanguageNL=Taal aangepast naar Nederlands.
SwitchlanguageEN=Taal aangepast naar Engels.
DbConnEstablished=Databaseverbinding tot stand gebracht
DbConnFailed=Databaseverbinding mislukt
