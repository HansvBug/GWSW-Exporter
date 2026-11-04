# GWSW Exporter

**Turn your sewer (BOR) data into a GWSW-compliant OroX Turtle file.**

GWSW Exporter is an application that reads data from an Oracle database or a CSV file and converts it into a `.orox.ttl` file that complies with the GWSW-OroX specification of Stichting RIONED. You can then upload or validate that file with the regular GWSW tooling.

Whether you are preparing a data delivery, validating a dataset, or simply need to convert a CSV export, GWSW Exporter handles the heavy lifting — you just point it at your data.

---

## Table of Contents

1. [What you can do with it](#what-you-can-do-with-it)
2. [What you need](#what-you-need)
3. [Installation](#installation)
4. [First-time setup](#first-time-setup)
5. [Step-by-step: exporting your data](#step-by-step-exporting-your-data)
6. [Preparing your data](#preparing-your-data)
7. [The mapping file](#the-mapping-file)
8. [Options and settings](#options-and-settings)
9. [Troubleshooting](#troubleshooting)
10. [Known limitations](#known-limitations)
11. [Getting help](#getting-help)
12. [License and credits](#license-and-credits)

---

## What you can do with it

- **Connect to an Oracle database** and run SQL queries directly from the application.
- **Load a CSV file** if you do not have direct database access.
- **Preview the data** in a sortable grid before exporting.
- **Export to GWSW-OroX Turtle (`.ttl`)** — the export includes stelsels, putten, leidingen, persleidingen and kolken, with their (not all) attributes.
- **Control what is exported** — toggle individual attributes on or off (e.g., only export diameters, skip materials).
- **Validate automatically** — dimensions outside the GWSW-allowed range are flagged in the error overview.


> ℹ️ *GWSW Exporter is under development. The core export works, but not every GWSW attribute is supported yet. See [Known limitations](#known-limitations).*

---

## What you need

To run GWSW Exporter you need:

| Item | Notes |
|---|---|
| **Windows 10 or 11** | The primary supported platform. Linux is not yet released. |
| **The application** (`gwsw_exporter.exe`) | Placed in its own folder. |
| **Four language files** | `main_text.en`, `main_text.nl`, `config_text.en`, `config_text.nl` — **must sit next to the executable**, otherwise the app will not start. |
| **A mapping file (ODS)** | Required before you can export. See [The mapping file](#the-mapping-file). |
| **Oracle client** | Only needed if you connect to an Oracle database. A working `TNSNAMES.ora` is required. |
| **Your data** | Either a live Oracle database or a CSV file with the correct column headers. See [Preparing your data](#preparing-your-data). |

---

## Installation

Installing GWSW Exporter is simple:

1. **Create a folder** for the application, for example `C:\Program Files\GWSW-Exporter\` or a folder in your own user directory.
2. **Copy** `gwsw_exporter.exe` into that folder.
3. **Copy** the four language files (`main_text.en`, `main_text.nl`, `config_text.en`, `config_text.nl`) into the **same folder**.
4. **Double-click** `gwsw_exporter.exe` to start.

On first run, the application automatically creates its working folders under your user profile:

```
%APPDATA%\GWSW-Exporter\
├── Settings\      (configuration and validation files)
├── Logging\       (log files, if enabled)
├── Domainlist\    (mapping files)
├── Export\        (default location for exports)
└── Queries\       (saved SQL queries)
```

> 💡 **Tip:** You do not need administrator rights. Installing under `%APPDATA%\GWSW-Exporter\` or any folder in your user profile works fine.

---

## First-time setup

Before you can export anything, take a few minutes to configure the application.

### 1. Set up the mapping file

Open **Menu → Options → Options** and locate the **Mapping file** field. Point it to your ODS mapping file (see [The mapping file](#the-mapping-file)). This file tells the application how your source values (e.g., "BETON") translate to GWSW concepts (e.g., `gwsw:Beton`).

### 2. Choose a GWSW version

In the same options screen, select the GWSW version you are targeting — currently **1.6** or **1.7.0**. This affects the URIs generated in the export.

### 3. (Optional) Enable logging

If you want a log file for troubleshooting, enable logging in the same screen. Log files are stored under `%APPDATA%\GWSW-Exporter\Logging\`.

Click **Close** — your settings are saved automatically.

### 4. Review the export options

In the main screen, decide **which attributes to include** in the export. You can toggle attributes separately for putten, leidingen, persleidingen and kolken. All common attributes are enabled by default; you can disable any you do not need.

---

## Step-by-step: exporting your data

### Step 1 — Connect to your data source

**If you use Oracle:**
1. Go to the **Oracle** tab.
2. Enter the database name (TNS alias), your username and password.
3. Click **Connect**.
4. When the connection succeeds, the status bar confirms it.

**If you use a CSV file:**
1. Go to the **CSV** tab.
2. Click **Select CSV file…** and open your file.
3. The data is loaded into the grid.

### Step 2 — Load a SQL query (Oracle only)

1. Go to the **Query** tab.
2. Click **Open query…** and pick a `.sql` file.
3. The query appears in the editor. You can edit it directly if needed.
4. Click **Save query…** if you want to keep changes.

> 💡 CSV users can skip this step — the CSV column headers serve as the field names.

### Step 3 — Retrieve the data

Click **Get data**. The result appears in the data grid below. You can:

- **Sort** by clicking any column header.
- **Scroll** through the rows to inspect the data.
- **Export the grid to CSV** with the *Export to CSV* button (if you want a copy of what you see).

### Step 4 — Export to GWSW-OroX Turtle

1. Click **Export to TTL** (or **Export to TTL from CSV** on the CSV tab).
2. Choose a location and filename. The extension `.orox.ttl` is applied automatically.
3. Confirm. The export begins.

While exporting you will see:

- **A progress list** showing which object types are being processed.
- **An error list** if any records contain missing or invalid values.

When finished, a message confirms that the file has been saved. If *Ask to open export file* is enabled in the options, you will be prompted to open it directly.

> 📁 By default, exports are saved under `%APPDATA%\GWSW-Exporter\Export\`. You can save anywhere you like.

---

## Preparing your data

The application expects **specific column names**. Whether the data comes from an Oracle query or a CSV file, the headers must match exactly (case-sensitive).

### Required for every row

| Column | What it should contain |
|---|---|
| `OBJECT_TYPE` | The type of record: `STELSEL`, `PUT`, `LEIDING`, `PERSLEIDING` or `KOLK` |
| `GUID` | A unique identifier for the object |
| `NAAM` | A human-readable name or label |
| `WKT_GEOMETRY` | The geometry, in WKT (Well-Known Text) format |
| `STELSEL_ID` | The ID of the sewer system this object belongs to |
| `STELSEL_TYPE` | The type of the sewer system |
| `STELSEL_NAAM` | The name of the sewer system |

### Additional columns per object type

| Object type | Required/optional columns |
|---|---|
| **PUT** (manhole) | `PUT_TYPE`, `BREEDTE`, `LENGTE`, `HOOGTE`, `DIAMETER`, `MATERIAAL`, `VORM`, `FUNDERING`, `MAAIVELD`, `BEGINDATUM`, `EINDDATUM` |
| **KOLK** (gully) | `KOLK_TYPE`, `BREEDTE`, `LENGTE`, `HOOGTE`, `DIAMETER`, `MATERIAAL`, `VORM`, `WANDDIKTE`, `BEGINDATUM`, `EINDDATUM` |
| **LEIDING** (pipeline) | `LEIDING_TYPE`, `BEGINPUT_ID`, `EINDPUT_ID`, `LENGTE`, `BREEDTE`, `HOOGTE`, `DIAMETER`, `MATERIAAL`, `VORM`, `FUNDERING`, `STATUS_FUNCTIONEREN`, `BOB_BEGIN`, `BOB_EIND`, `WIBON_THEMA`, `BEGINDATUM`, `EINDDATUM` |
| **PERSLEIDING** (mechanical pipeline) | Same as `LEIDING` |
| **STELSEL** (sewer system) | `NAAM`, `STELSEL_NAAM`, `STELSEL_TYPE` |

> 💡 Missing fields do **not** abort the export. They are reported in the error overview so you can correct them later.

### Example CSV header row

```csv
OBJECT_TYPE,GUID,NAAM,STELSEL_ID,STELSEL_TYPE,STELSEL_NAAM,WKT_GEOMETRY,PUT_TYPE,BREEDTE,LENGTE,HOOGTE,DIAMETER,MATERIAAL,VORM,FUNDERING,MAAIVELD,BEGINDATUM,EINDDATUM
```

---

## The mapping file

The **mapping file** is an ODS spreadsheet (open with LibreOffice Calc) that tells GWSW Exporter how your source values translate into GWSW concepts. It is required for export.

Each worksheet in the file corresponds to a category:

| Worksheet | Used for |
|---|---|
| `Materiaal_Put` | Material of manholes and gullies |
| `Materiaal_leiding` | Material of pipelines |
| `Vorm_Put` | Shape of manholes and gullies |
| `Vorm_Leiding` | Shape of pipelines |
| `StatusFunctioneren` | Status of pipeline operation |
| `Stelseltype` | Sewer system types |
| `PutType` | Manhole types |
| `LeidingType` | Pipeline types |
| `KolkType` | Gully types |
| `Fundering` | Foundations |
| `WibonThema` | WIBON themes |

Each row has:

- **Column A** — your source value (e.g., `BETON`).
- **Column B** — the GWSW URI or term (e.g., `gwsw:Beton`).

When a value in column B does **not** contain a colon, the application automatically prepends `gwsw:`. Full URIs (with `:`) are taken over unchanged.

> 💡 You can maintain your own mapping file — just open the ODS in LibreOffice and add or adjust rows. Then point GWSW Exporter at the updated file via **Options**.

---

## Options and settings

The **Options** screen (**Menu → Options → Options**) contains everything that customizes the application:

| Tab | What you configure |
|---|---|
| **GWSW** | Mapping file and GWSW version (1.6 or 1.7.0) |
| **Miscellaneous** | Language, logging, grid highlighting, whether to ask to open the export file, and whether to remember the last-used organization |

The **error reporting** section (also on the Miscellaneous tab) determines which issues are shown in the error overview during export:

- Mapping errors (values not in the mapping file)
- Fatal errors (missing geometry, etc.)
- Empty fields
- Missing fields
- Values out of range

By default, most are enabled. If you only want a clean export and do not want to be notified of every minor issue, you can turn individual categories off.

### Where your settings are stored

All settings are stored in:

```
%APPDATA%\GWSW-Exporter\Settings\<username>_GWSW-Exporter.cfg
```

You do not normally need to edit this file directly, but it is plain text (INI format) if you need to inspect or share it.

### GWSW validation rules

This contains the **minimum and maximum allowed values** for dimensions (width, length, height, diameter, Z-coordinate). You can adjust these values to match your local conventions. On first run, the defaults are written automatically.

---

## Troubleshooting

### The application does not start

- Check that **all four language files** (`main_text.en`, `main_text.nl`, `config_text.en`, `config_text.nl`) are in the same folder as `gwsw_exporter.exe`. The app will not start without them.
- Check that you have permission to write under `%APPDATA%\GWSW-Exporter\`.

### "Could not connect to the database"

- Verify that your `TNSNAMES.ora` contains the alias you entered.
- Check that the Oracle client is installed and reachable (try `tnsping <your_alias>` in a command prompt).
- Verify your username and password.

### "Mapping file not found" or export refuses to start

- Make sure the mapping file exists at the path set in **Options**.
- Check that the file is a valid ODS spreadsheet with the expected worksheets (see [The mapping file](#the-mapping-file)).

### The export finishes but the error list has many items

- Open the error list (a memo below the grid). Each line indicates which field and which object is affected.
- Common causes: values not present in the mapping file, empty required fields, or values outside the GWSW min/max ranges.
- Fix the data at the source and export again, or adjust the mapping file.

### The TTL file fails GWSW validation

- Some errors are reported by the GWSW validation service but not by GWSW Exporter — for example, disconnected networks or orphan manholes without pipelines.
- Use the error list as a starting point and cross-check with the official GWSW validator.

### "File is in use by another process"

- Close any program that has the CSV or TTL file open (Excel, a text editor, etc.) and try again.

---

## Known limitations

GWSW Exporter is under development. Things to be aware of:

- **Windows only.** The application is not yet released for Linux.
- **Not all GWSW attributes** are supported. Coverage is growing, and the set of attributes can be extended in future releases.
- **Some options require editing the settings file manually**, such as the CSV delimiter and quote character.
- **Translation coverage** is not yet 100% — a few messages may still appear in English even when the interface is set to Dutch.

---

## Getting help

- **Bugs and feature requests:** please open an issue on [Codeberg](https://codeberg.org/HansvBug/GWSW-Exporter/issues) or [GitHub](https://github.com/HansvBug/GWSW-Exporter/issues).
- **General questions:** use the issue tracker with the label "question".
- **Logs:** if the app is misbehaving, enable logging in **Options** and include the log file (`%APPDATA%\GWSW-Exporter\Logging\`) with your report.
- **Mapping problems:** include the relevant worksheet name and one or two example rows from your mapping file.

When reporting a problem, please include:

1. What you were doing.
2. What you expected to happen.
3. What actually happened.
4. The exact error message (if any).
5. Your Windows version and, if applicable, Oracle client version.

---

## License and credits

GWSW Exporter is released under the **MIT License**. See the `LICENSE` file for the full text.

**Created by Hans van Buggenum**, with thanks to:

- **Benny Christensen a.k.a. cdbc** — for the MVP framework and the observer/string utilities used throughout the application.
- **The Lazarus/FPC team** — for the Free Pascal compiler, Lazarus IDE and FPSpreadsheet library.
- **Stichting RIONED** — for the GWSW specification and data model.

**Useful links:**

- GWSW portal — https://apps.gwsw.nl/
- Source code (Codeberg) — https://codeberg.org/HansvBug/GWSW-Exporter
- Source code (GitHub) — https://github.com/HansvBug/GWSW-Exporter

---

## For developers

If you want to build GWSW Exporter from source or contribute code, these notes are for you.

**Requirements:**

- [Lazarus](https://www.lazarus-ide.org/) ≥ 4.8 with Free Pascal ≥ 3.2
- **FPSpreadsheet** via *Package → Online Package Manager*
- Oracle client libraries (only for Oracle functionality)

**Build:**

1. Clone the repository (see the links above).
2. Open `gwsw_exporter.lpi` in Lazarus.
3. *Run → Build*.
4. Place the resulting executable next to the four language files.