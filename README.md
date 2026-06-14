# Data from: A fine-grained multitrophic biodiversity and environmental dataset for understanding global change impacts in drylands

## Overview

This repository contains the data and reproducible workflows associated with the Kalahari multitrophic biodiversity and environmental database, a coordinated monitoring programme established at the Kalahari Research Centre (Northern Cape, South Africa) to quantify biodiversity and environmental dynamics across multiple trophic levels in a dryland ecosystem.

The repository provides raw data, curated datasets, and reproducible R workflows used to generate the data products described in the accompanying Data Descriptor:

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

Sampling began in June 2023 and data included in the current release span 2023–2026.

---

## Database summary

The database comprises eleven complementary datasets.

| Dataset | Description | Observations | Taxa | Sampling units |
|----------|----------|----------:|----------:|----------:|
| i | Invertebrate species composition | 9,258 | 372 | 120 traps |
| ii | Invertebrate biomass for pitfall traps | 15,708 | – | 100 pitfall traps |
| iii | Invertebrate biomass for subterranean traps | 605 | – | 20 subterranean traps |
| iv | Vegetation species composition | 992 | 34 | 100 pitfall traps |
| v | Vertebrate species composition | 705,168 | 36 | 146 camera traps |
| vi | Deployment dates for loggers | 21 | – | 21 loggers |
| vii | Soil temperature and moisture | 5,286,746 | – | 21 loggers |
| viii | Macrohabitats | Spatial layer | – | Study area |
| ix | Pitfall trap locations | 100 | – | 100 locations |
| x | Subterranean trap locations | 20 | – | 20 locations |
| xi | Camera trap locations | 71 | – | 71 locations |

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

These identifiers allow users to integrate biodiversity, environmental, and spatial information within a common analytical framework.

Examples of potential applications include:

* Linking invertebrate diversity and biomass to soil temperature and moisture.
* Relating vegetation composition and structure to above-ground and below-ground communities.
* Quantifying biodiversity responses across environmental gradients and macrohabitats.
* Investigating ecological linkages among soil fauna, vegetation, vertebrates, and microclimate.
* Assessing multi-trophic responses to environmental variability and global change.

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
├── README.md
│
└── .gitignore
```

---

## Reproducible workflows

All data processing was conducted using reproducible workflows implemented in R.

Depending on the dataset, the repository may include:

- Raw source data.
- Data-cleaning and validation scripts.
- Curated datasets.
- Summary scripts used to generate descriptive statistics and figures.

Some datasets did not require data processing and are therefore provided directly as final curated datasets.

The processing workflow follows a transparent and reproducible structure from raw observations to final data products whenever data processing was required.

Scripts are extensively annotated to document:

* Data validation procedures
* Quality-control decisions
* Taxonomic standardisation
* Formatting corrections
* Dataset-specific processing steps

---

## Data access

The datasets provided in this repository correspond to those described in the accompanying manuscript.

Future updates may extend the temporal coverage of the monitoring programme while preserving versioned releases of previously published datasets.

---

## Citation

Users should cite both the archived dataset and the associated Data Descriptor when using these data.

### Dataset

Morales-González, A., Jubber, W. R., Fuller, A., Ozgul, A., Graf, K., da Silva, C. Y., Gómez-Peña, G., Huber, S., Manser, M. B., & Paniw, M. (2026). *A fine-grained multitrophic biodiversity and environmental dataset for understanding global change impacts in drylands* (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.18376603

### Data Descriptor

The citation will be updated once the manuscript has been published.

---

## Contact

**Ana Morales-González**

Institute of Nature Conservation,
Polish Academy of Sciences (IOP PAN)
Kraków, Poland

morales@iop.krakov.pl

**Maria Paniw**

Estación Biológica de Doñana (EBD-CSIC)
Seville, Spain

maria.paniw@ebd.csic.es
