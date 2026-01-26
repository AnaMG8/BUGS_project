
## A comprehensive multi-trophic biodiversity and environmental dataset for understanding global change impacts in dryland ecosystems

This repository contains the data and code associated with the manuscript in preparation “A comprehensive multi-trophic biodiversity and environmental dataset for understanding global change impacts in dryland ecosystems”

Authors: Ana Morales-González, Walter R Jubber, Andrea Fuller, Arpat Ozgul, Ken Graf, Candela Yáñez da Silva, Guillermo Gómez Peña, Sonja Huber, Marta B. Manser, Maria Paniw

The dataset compiles coordinated, long-term field observations of biodiversity and environmental conditions collected in the southern Kalahari (South Africa) from August 2023.

It integrates information on:
- Invertebrate communities
- Vegetation composition
- Vertebrate communities
- Soil temperature and moisture
- Habitat structure and spatial variables

The dataset is designed to support research on biodiversity patterns, ecosystem functioning, and ecological responses to global change in dryland systems.


## Repository structure

---

# Study area

Data were collected at the **Kalahari Research Centre (KRC)**, Northern Cape, South Africa  
(–26.9786°, 21.8321°).

The study area is characterized by:
- Semi-arid climate with strong seasonal variation
- Mean annual rainfall ~266 mm
- Sandy, nutrient-poor soils
- A mosaic of land-use types (rewilded, holistic grazing, high-impact grazing)

---

# Datasets

| Dataset | Description |
|---------|-------------|
| **Dataset i** | Invertebrate species composition |
| **Dataset ii** | Invertebrate biomass – standard pitfall traps |
| **Dataset iii** | Invertebrate biomass – subterranean pitfall traps |
| **Dataset iv** | Vegetation composition |
| **Dataset v** | Vertebrate detections (camera traps) |
| **Dataset vi** | Logger deployment metadata |
| **Dataset vii** | Soil temperature and moisture time series |
| **Dataset viii** | Habitat classification |
| **Dataset ix** | Standard pitfall locations |
| **Dataset x** | Subterranean pitfall locations |
| **Dataset xi** | Camera trap locations |

---

# Invertebrate sampling

- **Standard pitfall traps** for surface-active invertebrates  
- **Subterranean pitfall traps** to assess vertical stratification  
- Sampling period:
  - Standard pitfalls: from August 2023
  - Subterranean pitfalls: from February 2024 
- Individuals identified to the lowest possible taxonomic level
- Dry trapping method used to minimize mortality and preserve DNA integrity

---

# Vegetation sampling

Vegetation was surveyed in 4 × 4 m plots centered on pitfall traps, recording:

- Percent cover of bare soil, grasses, shrubs, and trees
- Vegetation height
- Species presence

Sampling overlapped temporally with invertebrate monitoring.

---

# Soil and microclimate sampling

- Soil temperature and moisture measured using TMS loggers
- Measurements every 15 minutes
- Multiple depths and heights per site
- Data available from February 2024

---

# Vertebrate sampling

- Vertebrates were monitored using camera traps
- 146 Browning BTC-7E cameras deployed in June 2023
- Cameras arranged in 73 locations spaced approximately 1 km apart
- Two cameras per station, facing north and south
- Cameras mounted at ~40 cm above ground
- No bait or lure used
- Cameras recorded five images per trigger (0.3 s interval)
- Stations checked monthly for maintenance
- Data include species identity, date, time, and location

---

# Data quality and validation

All datasets are currently undergoing extensive quality control, including:

- Taxonomic validation by specialists
- Standardization of variable names and formats
- Consistent timestamps (ISO 8601)
- Verification of pitfall IDs and sampling periods
- Outlier checks and correction of inconsistencies

Details of the validation workflow are provided in the manuscript.

---

# Data structure and integration

Datasets share common identifiers such as: `timestamp`, `pitfall_id`,...

These allow datasets to be linked across biological, environmental, and spatial dimensions.

Detailed variable descriptions are provided in the manuscript.

---

# Code availability

This repository (https://github.com/AnaMG8/SoilProject) contains the manuscript (`manuscript_v080126.docx`) and the following folders:

| Folder | Description |
|--------|-------------|
| **Rcode** | R scripts used to validate, clean, and prepare the raw datasets. Each script corresponds to               a specific dataset |
| **checks** | Files and figures generated during data validation and quality control. These outputs are                used for visual inspection of the data (e.g. taxonomic consistency, outliers, formatting                 issues) that cannot be fully addressed through automated procedures |
| **clean_datasets** | Final datasets ready for analysis, produced after validation and cleaning of the                         raw data |
| **figures** | Figures generated during data exploration and preparation, some may be included in                       the manuscript |
| **metadata** | Metadata files describing soil loggers, pitfall traps, and associated sampling                           information |
| **raw_datasets** | Raw data files including camera trap records, soil logger data, invertebrate                             diversity and biomass, vegetation surveys, habitat layers, and pitfall locations |


---

# Citation

If you use this dataset, please cite:

Morales-González, A., Jubber W.R., Fuller, A., Ozgul, A., Graf, K., Da Silva, C.Y., Gómez-Peña, G., Huber, S., Manser M.B., Paniw. M. A comprehensive multi-trophic biodiversity and environmental dataset for understanding global change impacts in dryland ecosystems. in prep. 

(The final citation will be updated upon publication)

---

# Contact

For questions or data requests, please contact:

Ana Morales-González  
Institute of Nature Conservation of the Polish Academy of Sciences (IOP PAN)
morales@iop.krakov.pl

Maria Paniw
Estación Biológica de Doñana-CSIC
maria.paniw@ebd.csic.es

