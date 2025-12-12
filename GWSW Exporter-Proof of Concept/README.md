# GWSW Exporter - Proof of Concept

This is a proof of concept (PoC) for an export tool that converts data from relational databases to the GWSW-OroX Turtle (TTL) format, in accordance with the specifications of Stichting RIONED.

This proof of concept demonstrates the ability to:
- Export data from Oracle databases
- Map to the standardized GWSW data model
- Generate valid Turtle (.ttl) files conforming to OroX specifications
- Support for manholes, pipes, systems, gully pots, and pressure pipelines

## Objective
To demonstrate an automated, valid data export to the national GWSW exchange format.

## Technical Features
- Export to GWSW 1.6 / OroX RDF
- Supports WKT geometries (Point, LineString)
- Configuration via mapping files (.ods)
- Fully open-source setup

## PoC Limitations
- Not all GWSW modules are implemented
- Limited validation against GWSW ontology
- No support for all hydraulic structures
- Performance not optimized for large datasets
- No version control of export configurations

## Status
**Proof of Concept** - Functionality under development

## Disclaimer
This tool is developed as a proof of concept and is not intended for production use without additional development and extensive testing.

## License 
This project is released under the MIT license.




# GWSW Exporter - Proof of Concept

Dit is een proof of concept (PoC) voor een exporttool die data uit relationele databases omzet naar het GWSW-OroX Turtle (TTL) formaat, conform de specificaties van Stichting RIONED.

Deze proof of concept demonstreert de mogelijkheid om:
- Data uit Oracle-databases te exporteren
- Mapping naar het gestandaardiseerde GWSW-datamodel
- Generatie van valide Turtle (.ttl) bestanden conform OroX-specificaties
- Ondersteuning voor putten, leidingen, stelsels, kolken en persleidingen

## Doelstelling
Aantonen van een geautomatiseerde, valide data-export naar het landelijke GWSW-uitwisselformaat.


## Technische kenmerken
- Export naar GWSW 1.6 / OroX RDF
- Ondersteunt WKT geometrieën (Point, LineString)
- Configuratie via mapping-bestanden (.ods)
- Volledige opensource-opzet

## Beperkingen van de PoC
- Niet alle GWSW modules zijn geïmplementeerd
- Beperkte validatie tegen GWSW ontologie
- Geen ondersteuning voor alle hydraulische constructies
- Performance niet geoptimaliseerd voor grote datasets
- Geen versiebeheer van exportconfiguraties

## Status
**Proof of Concept** - Functionaliteit in ontwikkeling

## Disclaimer
Deze tool is ontwikkeld als proof of concept en is niet bedoeld voor productiegebruik zonder aanvullende ontwikkeling en uitgebreide testing.

## Licentie
Dit project wordt vrijgegeven onder de MIT licentie.
