# geometa

[![Build Status](https://github.com/eblondel/geometa/actions/workflows/r-cmd-check.yml/badge.svg?branch=master)](https://github.com/eblondel/geometa/actions/workflows/r-cmd-check.yml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/geometa)](https://cran.r-project.org/package=geometa)
[![cran checks](https://badges.cranchecks.info/worst/geometa.svg)](https://cran.r-project.org/web/checks/check_results_geometa.html)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.9-blue.svg)](https://github.com/eblondel/geometa)
[![R-Universe](https://eblondel.r-universe.dev/badges/geometa)](http://eblondel.r-universe.dev/#package:geometa)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

**Tools for Reading and Writing ISO/OGC Geographic Metadata**

``geometa`` provides facilities to manipulate geographic metadata defined with OGC/ISO 19115 and 19139 (XML) standards, including an API to create in R representations of geographic metadata, as defined in the above standards, and write it to XML, or even to read an XML metadata into R.

Please check the [online documentation](https://github.com/eblondel/geometa/wiki) for more details!

## Sponsors

Many thanks to the following organizations that have provided fundings for strenghtening the ``geometa`` package:

<a href="https://r-consortium.org/"><img src="https://r-consortium.org/images/RConsortium_Horizontal_Pantone.webp" height="100"/></a>

For geometa sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)

## Citation

We thank in advance people that use ``geometa`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

## Metadata standards coverage

|Specification                        |Schema                               |Title                                                                               |Namespace |Coverage                                                                                                                           | Supported| Missing| Refactored| Torefactor|
|:------------------------------------|:------------------------------------|:-----------------------------------------------------------------------------------|:---------|:----------------------------------------------------------------------------------------------------------------------------------|---------:|-------:|----------:|----------:|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Citation and responsible party information (CIT) Version: 2.0                       |CIT       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  CIT](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        16|       0|         16|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Geospatial COmmon Objects (GCO) Version: 1.0                                        |GCO       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  GCO](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        24|       0|         24|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Geospatial Common eXtension (GCX) Version: 1.0                                      |GCX       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  GCX](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         3|       0|          3|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Geospatial EXtent (GEX) Version: 1.0                                                |GEX       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  GEX](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         8|       0|          8|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |metadata for LANguage and localization (LAN) Version: 1.0                           |LAN       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  LAN](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         7|       0|          7|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for ACquisition (MAC) Version: 2.0                                         |MAC       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MAC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        25|       0|         25|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Application Schemas (MAS) Version: 2.0                                 |MAS       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MAS](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         1|       0|          1|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata Common Classes (MCC) Version: 1.0                                          |MCC       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MCC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        31|       0|         31|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for COnstraints (MCO) Version: 1.0                                         |MCO       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MCO](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         6|       0|          6|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |MetaData Base (MDB) Version: 2.0                                                    |MDB       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MDB](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         2|       0|          2|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Maintenance Information (MMI) Version: 1.0                             |MMI       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MMI](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         2|       0|          2|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Portrayal Catalogues (MPC) Version: 1.0                                |MPC       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MPC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         1|       0|          1|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Resource Content (MRC) Version: 1.0                                    |MRC       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MRC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        19|       0|         19|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Resource Distribution (MRD) Version: 1.0                               |MRD       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MRD](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         7|       0|          7|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Resource Identification (MRI) Version: 1.0                             |MRI       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MRI](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        14|       0|         14|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Resource Lineage (MRL) Version: 2.0                                    |MRL       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MRL](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        11|       0|         11|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Reference Systems (MRS) Version: 1.0                                   |MRS       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MRS](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         2|       0|          2|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for Spatial Representation (MSR) Version: 2.0                              |MSR       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  MSR](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        17|       0|         17|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19115-1:2014                     |Metadata for SeRVices (SRV) Version: 2.0                                            |SRV       |[![ISO/TS 19115-3:2023  -  ISO 19115-1:2014  -  SRV](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         8|       0|          3|          5|
|ISO/TS 19115-3:2023 🆕               |ISO 19157                            |Data Quality abstract Classes (DQC) Version 1.0                                     |DQC       |[![ISO/TS 19115-3:2023  -  ISO 19157  -  DQC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         2|       0|          2|          0|
|ISO/TS 19115-3:2023 🆕               |ISO 19157                            |Metadata for Data Quality (MDQ) Version: 1.0                                        |MDQ       |[![ISO/TS 19115-3:2023  -  ISO 19157  -  MDQ](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        43|       0|         43|          0|
|ISO/TS 19115-3:2023 🆕               |ISO/TS 19139:2007                    |CATalogue Objects (CAT) Version: 1.0                                                |CAT       |[![ISO/TS 19115-3:2023  -  ISO/TS 19139:2007  -  CAT](https://img.shields.io/badge/-18%25-ad0f0f.svg)](https://github.com/eblondel/geometa)|         3|      14|          3|         14|
|ISO/TS 19115-3:2023 🆕               |ISO/TS 19139:2007                    |Feature Catalog Common (FCC) Version: 1.0                                           |FCC       |[![ISO/TS 19115-3:2023  -  ISO/TS 19139:2007  -  FCC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         2|       0|          2|          0|
|ISO/TS 19139:2007                    |ISO 19110:2005                       |Geographic Information - Methodology for feature cataloguing                        |GFC       |[![ISO/TS 19139:2007  -  ISO 19110:2005  -  GFC](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        17|       0|          0|         17|
|ISO/TS 19139:2007                    |ISO 19115-1:2003                     |Geographic Information - Metadata                                                   |GMD       |[![ISO/TS 19139:2007  -  ISO 19115-1:2003  -  GMD](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|       134|       0|        113|         21|
|ISO/TS 19139:2007                    |ISO 19115-2:2009                     |Geographic Information - Metadata - Part 2: Extensions for imagery and gridded data |GMI       |[![ISO/TS 19139:2007  -  ISO 19115-2:2009  -  GMI](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        40|       0|         39|          1|
|ISO/TS 19139:2007                    |ISO 19119:2005                       |Geographic Information - Service Metadata                                           |SRV       |[![ISO/TS 19139:2007  -  ISO 19119:2005  -  SRV](https://img.shields.io/badge/-42%25-f9ae2c.svg)](https://github.com/eblondel/geometa)|         8|      11|          3|         16|
|ISO/TS 19139:2007                    |ISO/TS 19103:2005                    |Geographic Common extensible markup language                                        |GCO       |[![ISO/TS 19139:2007  -  ISO/TS 19103:2005  -  GCO](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|        22|       0|         22|          0|
|ISO/TS 19139:2007                    |ISO/TS 19139:2007                    |Geographic Metadata XML Schema                                                      |GMX       |[![ISO/TS 19139:2007  -  ISO/TS 19139:2007  -  GMX](https://img.shields.io/badge/-15%25-ad0f0f.svg)](https://github.com/eblondel/geometa)|        10|      56|          9|         57|
|ISO/TS 19139:2007                    |ISO/TS 19139:2007                    |Geographic Metadata XML Schema - Geographic Temporal Schema (GTS)                   |GTS       |[![ISO/TS 19139:2007  -  ISO/TS 19139:2007  -  GTS](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         1|       0|          1|          0|
|GML 3.2.1 (ISO 19136)                |GML 3.2.1 (ISO 19136)                |Geographic Markup Language                                                          |GML       |[![GML 3.2.1 (ISO 19136)  -  GML 3.2.1 (ISO 19136)  -  GML](https://img.shields.io/badge/-37%25-ff0c0c.svg)](https://github.com/eblondel/geometa)|        62|     107|          0|        169|
|GML 3.2.1 Coverage (OGC GMLCOV)      |GML 3.2.1 Coverage (OGC GMLCOV)      |OGC GML Coverage Implementation Schema                                              |GMLCOV    |[![GML 3.2.1 Coverage (OGC GMLCOV)  -  GML 3.2.1 Coverage (OGC GMLCOV)  -  GMLCOV](https://img.shields.io/badge/-20%25-ff0c0c.svg)](https://github.com/eblondel/geometa)|         1|       4|          0|          5|
|GML 3.3 Referenceable Grid (OGC GML) |GML 3.3 Referenceable Grid (OGC GML) |OGC GML Referenceable Grid                                                          |GMLRGRID  |[![GML 3.3 Referenceable Grid (OGC GML)  -  GML 3.3 Referenceable Grid (OGC GML)  -  GMLRGRID](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)|         5|       0|          0|          5|
|SWE 2.0                              |SWE 2.0                              |Sensor Web Enablement (SWE) Common Data Model                                       |SWE       |[![SWE 2.0  -  SWE 2.0  -  SWE](https://img.shields.io/badge/-60%25-f2eb24.svg)](https://github.com/eblondel/geometa)              |        18|      12|          0|         30|
