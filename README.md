# geometa

[![Build Status](https://travis-ci.org/eblondel/geometa.svg?branch=master)](https://travis-ci.org/eblondel/geometa)
[![codecov.io](http://codecov.io/github/eblondel/geometa/coverage.svg?branch=master)](http://codecov.io/github/eblondel/geometa?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geometa)](https://cran.r-project.org/package=geometa)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.2--0-blue.svg)](https://github.com/eblondel/geometa)

**Tools for Reading and Writing ISO/OGC Geographic Metadata**

``geometa`` provides facilities to manipulate geographic metadata defined with OGC/ISO 19115 and 19139 (XML) standards, including an API to create in R representations of geographic metadata, as defined in the above standards, and write it to XML, or even to read an XML metadata into R.

Please check the [online documentation](https://github.com/eblondel/geometa/wiki) for more details!

Current Focus:
* ISO 19115:2003 (Geographic metadata) classes
* ISO 19110:2005 (Feature Catalogue) main classes
* ISO 19136:2007 (Geographic Markup Language – GML) partial support (classes needed for ISO 19115 and 19110)
* ISO 19139:2007 XML with **Encoding**, **Decoding** and **Validation** support

Notes:
* on encoding/decoding: Some specific ISO classes may be still missing, and will be added iteratively, according to needs. This applies particularly to GML classes (ISO 19136) that may be needed in some ISO 19115 and 19110 classes
* on validation: validation is perfomed using XML schema validation based on [ISO/OGC schemas](http://schemas.opengis.net/iso/19139/20070417/)

Not yet supported:
* ISO 19115-2 extension for raster description
* ISO 19119:2005 (Service metadata)

For geometa sponsoring/funding, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)
