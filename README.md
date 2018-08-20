# geometa

[![Build Status](https://travis-ci.org/eblondel/geometa.svg?branch=master)](https://travis-ci.org/eblondel/geometa)
[![codecov.io](http://codecov.io/github/eblondel/geometa/coverage.svg?branch=master)](http://codecov.io/github/eblondel/geometa?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geometa)](https://cran.r-project.org/package=geometa)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.3--0-blue.svg)](https://github.com/eblondel/geometa)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

**Tools for Reading and Writing ISO/OGC Geographic Metadata**

``geometa`` provides facilities to manipulate geographic metadata defined with OGC/ISO 19115 and 19139 (XML) standards, including an API to create in R representations of geographic metadata, as defined in the above standards, and write it to XML, or even to read an XML metadata into R.

Please check the [online documentation](https://github.com/eblondel/geometa/wiki) for more details!

## Citation

We thank in advance people that use ``geometa`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1184892.svg)](https://doi.org/10.5281/zenodo.1184892)

## Metadata standards coverage status

Some specific ISO classes may be still missing, and will be added iteratively, according to needs, hence the "Partial" status highlighted for the different standards. This applies particularly to GML classes (ISO 19136) that may be needed in some ISO 19115 and 19110 classes. In case of a missing class, [create a ticket](https://github.com/eblondel/geometa/issues/new).

Standard identifier:year|Standard description|Status
------------------------|-------------|------
ISO 19115-1:2003|Geographic information -- Metadata -- Part 1: Fundamentals|Partial
ISO 19115-1:2014|Geographic information -- Metadata -- Part 1: Fundamentals|Not supported
ISO 19115-2:2009|Geographic information -- Metadata -- Part 2: Extensions for imagery and gridded data|Not supported
ISO/TS 19115-3:2016|Geographic information -- Metadata -- Part 3: XML schema implementation for fundamental concepts|Not supported
ISO 19119:2005|Geographic information -- Service Metadata|Partial
ISO 19110:2005|Geographic Information -- Methodology for Feature Cataloguing|Partial
ISO 19136:2007|Geographic information -- Geography Markup Language (GML 3.2.1)|Partial
ISO 19139:2007|XML Implemntation with **Encoding**, **Decoding** and **Validation**|Full

## Metadata validation

Default validation is perfomed using XML schema validation based on [ISO/OGC schemas](http://schemas.opengis.net/iso/19139/20070417/). geometa allows specifying a custom schema (e.g. ISO 19139 profile) in case default validation is not enough.

## Development perspectives

* Support for missing classes
* Support for not yet supported standards and new revisions
* Support for adapters/converters from/to ISO metadata to/from other metadata standard formats

For geometa sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)
