# geometa

[![Build Status](https://travis-ci.org/eblondel/geometa.svg?branch=master)](https://travis-ci.org/eblondel/geometa)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geometa)](https://cran.r-project.org/package=geometa)
[![cran checks](https://cranchecks.info/badges/worst/geometa)](https://cran.r-project.org/web/checks/check_results_geometa.html)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.6--4-blue.svg)](https://github.com/eblondel/geometa)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

**Tools for Reading and Writing ISO/OGC Geographic Metadata**

``geometa`` provides facilities to manipulate geographic metadata defined with OGC/ISO 19115 and 19139 (XML) standards, including an API to create in R representations of geographic metadata, as defined in the above standards, and write it to XML, or even to read an XML metadata into R.

Please check the [online documentation](https://github.com/eblondel/geometa/wiki) for more details!

## Sponsors

Many thanks to the following organizations that have provided fundings for strenghtening the ``geometa`` package:

<a href="https://www.r-consortium.org"><img src="https://www.r-consortium.org/wp-content/uploads/sites/13/2016/09/RConsortium_Horizontal_Pantone.png" height="60"/></a>

For geometa sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)

## Citation

We thank in advance people that use ``geometa`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

## Metadata standards coverage

|Standard                             |Title                                                                               |Namespace |Coverage                                                                                                                        | Supported| Missing|
|:------------------------------------|:-----------------------------------------------------------------------------------|:---------|:-------------------------------------------------------------------------------------------------------------------------------|---------:|-------:|
|ISO/TC211 19110:2005                 |Geographic Information - Methodology for feature cataloguing                        |GFC       |[![ISO/TC211 19110:2005](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)                 |        17|       0|
|ISO/TC211 19115-1:2003               |Geographic Information - Metadata                                                   |GMD       |[![ISO/TC211 19115-1:2003](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)               |       132|       0|
|ISO/TC211 19115-2:2009               |Geographic Information - Metadata - Part 2: Extensions for imagery and gridded data |GMI       |[![ISO/TC211 19115-2:2009](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)               |        40|       0|
|ISO/TC211 19119:2005                 |Geographic Information - Service Metadata                                           |SRV       |[![ISO/TC211 19119:2005](https://img.shields.io/badge/-37%25-ff0c0c.svg)](https://github.com/eblondel/geometa)                  |         7|      12|
|ISO/TC211 19139:2007                 |Geographic Metadata XML Schema                                                      |GMX       |[![ISO/TC211 19139:2007](https://img.shields.io/badge/-8%25-ad0f0f.svg)](https://github.com/eblondel/geometa)                   |         5|      61|
|ISO/TS 19103:2005                    |Geographic Common extensible markup language                                        |GCO       |[![ISO/TS 19103:2005](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)                    |        22|       0|
|GML 3.2.1 (ISO 19136)                |Geographic Markup Language                                                          |GML       |[![GML 3.2.1 (ISO 19136)](https://img.shields.io/badge/-37%25-ff0c0c.svg)](https://github.com/eblondel/geometa)                 |        62|     107|
|GML 3.2.1 Coverage (OGC GMLCOV)      |OGC GML Coverage Implementation Schema                                              |GMLCOV    |[![GML 3.2.1 Coverage (OGC GMLCOV)](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa)      |         1|       0|
|GML 3.3 Referenceable Grid (OGC GML) |OGC GML Referenceable Grid                                                          |GMLRGRID  |[![GML 3.3 Referenceable Grid (OGC GML)](https://img.shields.io/badge/-100%25-4a4ea8.svg)](https://github.com/eblondel/geometa) |         5|       0|

