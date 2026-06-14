# A fine-grained multitrophic biodiversity and environmental dataset for understanding global change impacts in drylands

## Overview

This repository contains the complete data infrastructure associated with the Kalahari multitrophic biodiversity and environmental database, a coordinated monitoring programme established at the Kalahari Research Centre (Northern Cape, South Africa) to quantify biodiversity and environmental dynamics across multiple trophic levels in a dryland ecosystem.

The repository provides raw data, curated datasets, metadata, and reproducible R workflows used to generate the data products described in the accompanying Data Descriptor:

**Morales-González, A., Jubber, W.R., Fuller, A., Ozgul, A., Graf, K., Yáñez da Silva, C., Gómez Peña, G., Huber, S., Manser, M.B. & Paniw, M.**
*A fine-grained multitrophic biodiversity and environmental dataset for understanding global change impacts in drylands.*

The database integrates information on:

* Above-ground invertebrate communities
* Below-ground invertebrate communities
* Invertebrate biomass
* Vegetation composition and structure
* Vertebrate communities
* Soil temperature
* Soil moisture
* Macrohabitat classification
* Spatial metadata

All datasets were collected within a common sampling framework and can be linked through shared identifiers, enabling integrated analyses across trophic levels, environmental conditions, and spatial scales.

---

## Study system

Data were collected at the Kalahari Research Centre (KRC), Northern Cape, South Africa (26.9786° S, 21.8321° E).

The monitoring programme spans a heterogeneous dryland landscape composed of:

* Natural (rewilded) areas
* Rotational grazing areas
* Mixed-species grazing areas

Sampling began in June–August 2023 and continues as part of an ongoing long-term monitoring programme.

---

## Database summary

The database currently comprises eleven complementary datasets.

| Dataset | Description                               |  Observations | Taxa | Sampling units |
| ------- | ----------------------------------------- | ------------: | ---: | -------------: |
| i       | Invertebrate species composition          |         9,258 |  372 |      120 traps |
| ii      | Invertebrate biomass (pitfall traps)      |        15,708 |    – |      100 traps |
| iii     | Invertebrate biomass (subterranean traps) |           605 |    – |       20 traps |
| iv      | Vegetation species composition            |           992 |   34 |      100 plots |
| v       | Vertebrate species composition            |       705,168 |   36 |    146 cameras |
| vi      | Logger deployment metadata                |            21 |    – |     21 loggers |
| vii     | Soil temperature and moisture             |     5,286,746 |    – |     21 loggers |
| viii    | Macrohabitats                             | Spatial layer |    – |     Study area |
| ix      | Pitfall trap locations                    |           100 |    – |  100 locations |
| x       | Subterranean trap locations               |            20 |    – |   20 locations |
| xi      | Camera trap locations                     |            71 |    – |   71 locations |

Together, these datasets provide one of the most detailed integrated records currently available for dryland biodiversity, spanning soil fauna, vegetation, vertebrates, environmental conditions, and spatial context.

---

## Dataset integration

The database was designed as a relational data resource.

Datasets can be linked using shared identifiers, including:

| Variable      | Datasets              |
| ------------- | --------------------- |
| timestamp     | i, ii, iii, vi, vii   |
| trap_id       | i, ii, iii, iv, ix, x |
| serial_number | vi, vii               |
| camera_id     | v, xi                 |

These identifiers allow users to integrate biological observations with environmental measurements and spatial metadata.

Examples include:

* Linking invertebrate communities to soil temperature and moisture.
* Relating vegetation structure to invertebrate abundance and biomass.
* Evaluating vertebrate responses to environmental conditions.
* Comparing biodiversity across land-use types and macrohabitats.

---

## Repository structure

```text
BUGS_project
│
├── Rcode/
│   Reproducible data-cleaning and processing workflows.
│
├── raw_datasets/
│   Original source files collected in the field.
│
├── clean_datasets/
│   Curated datasets distributed with the publication.
│
├── metadata/
│   Supporting metadata and data dictionaries.
│
├── README.md
│
└── .gitignore
```

Additional working directories may be present during project development but are not required to reproduce the published datasets.

---

## Reproducible workflows

All datasets were processed using scripted workflows implemented in R.

For each dataset, the repository contains:

1. Raw source data.
2. Cleaning and validation scripts.
3. Curated datasets.
4. Summary scripts used to generate descriptive statistics and figures.

The processing workflow follows a transparent and reproducible structure from raw observations to final data products.

Scripts are extensively annotated to document:

* Data validation procedures
* Quality-control decisions
* Taxonomic standardisation
* Formatting corrections
* Dataset-specific processing steps

---

## Data quality assurance

Quality-control procedures were implemented throughout data collection and processing.

These procedures included:

* Validation of sampling identifiers.
* Verification of timestamps and sampling periods.
* Taxonomic standardisation.
* Detection of duplicated records.
* Consistency checks among linked datasets.
* Inspection of anomalous environmental measurements.
* Manual review of uncertain records.

Detailed validation procedures are described in the accompanying Data Descriptor.

---

## Data access

The curated datasets distributed through this repository represent the version associated with the published Data Descriptor.

Future updates may extend the temporal coverage of the monitoring programme while preserving versioned releases of previously published datasets.

---

## Citation

If you use these data, please cite both:

1. The Data Descriptor publication.
2. The archived dataset release.

Dataset DOI:

[INSERT DOI]

Data Descriptor:

[INSERT FINAL CITATION AFTER ACCEPTANCE]

---

## Contact

**Ana Morales-González**

Institute of Nature Conservation,
Polish Academy of Sciences (IOP PAN)

Kraków, Poland

**Maria Paniw**

Estación Biológica de Doñana (EBD-CSIC)

Seville, Spain
