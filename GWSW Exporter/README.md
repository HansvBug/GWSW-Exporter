# GWSW Exporter

This is an export tool that converts data to the GWSW-OroX Turtle (TTL) format, in accordance with the specifications of Stichting RIONED.

This is a work in progress that has only just begun. You can view the progress in the change log. 
The MVP framework is created with the help of "The MVP-Setup" tool, created by Benny Christensen. The tool and the source can be found at: https://gitlab.com/cdbc-public/releases/mvp-setup
The tool uses an unit from FPSpreadsheet. FPSpreadsheet can be downloaded via the online package manager.

Place gwsw_exporter.exe in its own directory and put the .en and .nl files in the same directory. Only then start the application.
Before you can retrieve data, a Query must be loaded. (Query tab). And before you can export, a mapping file must be present. (Menu, Options).
For the time being, the application is only suitable for Windows.

The fields returned by the query must be in accordance with the field names below. If you want to convert a csv file to a ttl file then the csv file must have headers and these must conform to the field names below.
The application works and provides a kind of basic ttl export for the GWSW. Not all attributes that could possibly be exported are built in. But the set-up is easy to expand with new attributes if necessary.

Mandatory naming of fields/headers:
OBJECT_TYPE
ID
GUID
NAAM
STELSEL_TYPE
STELSEL_NAAM
LEIDING_TYPE
BEGINPUT_ID
EINDPUT_ID
LENGTE
BREEDTE
HOOGTE
DIAMETER
MATERIAAL
VORM
STATUS_FUNCTIONEREN
PUT_TYPE
KOLK_TYPE
MAAIVELD
BEGINDATUM
EINDDATUM
BOB_BEGIN
BOB_EIND
WANDDIKTE
TYPE_REINIGING
VERBINDINGSTYPE
FUNDERING
WIBON_THEMA
LATERAAL_AFVOEREND_OPPERVLAK
WKT_GEOMETRY
STELSEL_ID