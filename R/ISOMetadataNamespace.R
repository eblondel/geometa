#' ISOMetadataNamespace
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata namespace
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Namespace
#' @format \code{\link{R6Class}} object.
#' 
#' @note ISO class used internally by geometa for specifying XML namespaces
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataNamespace <- R6Class("ISOMetadataNamespace",
  public = list(
    #'@field id id
    id = NA,
    #'@field uri uri
    uri = NA,
    #'@field standard standard
    standard = data.frame(specification = NA, schema = NA, title = NA),
    
    #'@description Initializes namespace object
    #'@param id id
    #'@param uri uri
    #'@param standard standard
    initialize = function(id, uri, standard){
      self$id = id
      self$uri = uri
      self$standard = standard
    },
    
    #'@description Get definition
    #'@return an object of class \link{list}
    getDefinition = function(){
      ns <- list(self$uri)
      names(ns) <- self$id
      return(ns)
    },
    
    #'@description Get standard
    #'@return object of class \link{data.frame}
    getStandard = function(){
      return(self$standard)
    }
    
  )
)

#' setMetadataNamespaces
#' @export
setISOMetadataNamespaces <- function(version = "19139"){
  
  #XML 1.0
  #----------------------------------------------------
  xml_namespaces <- list(
    #XLINK
    ISOMetadataNamespace$new(
      id = "xlink", uri = "http://www.w3.org/1999/xlink",
      standard = data.frame(specification = "XML 1.0", schema = "XML 1.0", title = "Extensible Markup Language (XML) 1.0 (Fifth Edition)", stringsAsFactors = FALSE)
    ),
    #XSI
    ISOMetadataNamespace$new(
      id = "xsi", uri = "http://www.w3.org/2001/XMLSchema-instance",
      standard = data.frame(specification = "XML 1.0", schema = "XML 1.0", title = "Extensible Markup Language (XML) 1.0 (Fifth Edition)", stringsAsFactors = FALSE)
    )
  )
  #GML and related
  #----------------------------------------------------
  gml_namespaces <- list(
    #GML
    ISOMetadataNamespace$new(
      id = "gml", uri = "http://www.opengis.net/gml/3.2",
      standard = data.frame(specification = "GML 3.2.1 (ISO 19136)", schema = "GML 3.2.1 (ISO 19136)", title = "Geographic Markup Language", stringsAsFactors = FALSE)
    ),
    #GMLCOV
    ISOMetadataNamespace$new(
      id = "gmlcov", uri = "http://www.opengis.net/gmlcov/1.0",
      standard = data.frame(specification = "GML 3.2.1 Coverage (OGC GMLCOV)", schema = "GML 3.2.1 Coverage (OGC GMLCOV)", title = "OGC GML Coverage Implementation Schema", stringsAsFactors = FALSE)
    ),
    #GMLRGRID
    ISOMetadataNamespace$new(
      id = "gmlrgrid", uri = "http://www.opengis.net/gml/3.3/rgrid",
      standard = data.frame(specification = "GML 3.3 Referenceable Grid (OGC GML)", schema = "GML 3.3 Referenceable Grid (OGC GML)", title = "OGC GML Referenceable Grid", stringsAsFactors = FALSE)
    )
  )
  
  #SWE
  #----------------------------------------------------
  swe_namespaces <- list(
    #SWE
    ISOMetadataNamespace$new(
      id = "swe", uri = "http://www.opengis.net/swe/2.0",
      standard = data.frame(specification = "SWE 2.0", schema = "SWE 2.0", title = "Sensor Web Enablement (SWE) Common Data Model", stringsAsFactors = FALSE)
    )
  )
  
  #ISO 19115 and related
  #----------------------------------------------------
  iso_namespaces <- switch(version,
    "19139" = list(
      #ISO 19110
      #----------------------------------------------------
      #GFC
      ISOMetadataNamespace$new(
        id = "gfc", uri = "http://www.isotc211.org/2005/gfc",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO 19110:2005", title = "Geographic Information - Methodology for feature cataloguing", stringsAsFactors = FALSE)
      ),
      #ISO 19115-1 / 19115-2 / 19139
      #----------------------------------------------------
      #GCO
      ISOMetadataNamespace$new(
        id = "gco", uri = "http://www.isotc211.org/2005/gco",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO/TS 19103:2005", title = "Geographic Common extensible markup language", stringsAsFactors = FALSE)
      ),
      #GMD
      ISOMetadataNamespace$new(
        id = "gmd", uri = "http://www.isotc211.org/2005/gmd",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO 19115-1:2003", title = "Geographic Information - Metadata", stringsAsFactors = FALSE)
      ),
      #GMI
      ISOMetadataNamespace$new(
        id = "gmi", uri = "http://www.isotc211.org/2005/gmi",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO 19115-2:2009", title = "Geographic Information - Metadata - Part 2: Extensions for imagery and gridded data", stringsAsFactors = FALSE)
      ),
      #GMX
      ISOMetadataNamespace$new(
        id = "gmx", uri = "http://www.isotc211.org/2005/gmx",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO/TS 19139:2007", title = "Geographic Metadata XML Schema", stringsAsFactors = FALSE)
      ),
      #GTS
      ISOMetadataNamespace$new(
        id = "gts", uri = "http://www.isotc211.org/2005/gts",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO/TS 19139:2007", title = "Geographic Metadata XML Schema - Geographic Temporal Schema (GTS)", stringsAsFactors = FALSE)
      ),
      #ISO 19119
      #----------------------------------------------------
      #SRV
      ISOMetadataNamespace$new(
        id = "srv", uri = "http://www.isotc211.org/2005/srv",
        standard = data.frame(specification = "ISO/TS 19139:2007", schema = "ISO 19119:2005", title = "Geographic Information - Service Metadata", stringsAsFactors = FALSE)
      )
    ),
    "19115-3" = list(
      #ISO 19110
      #----------------------------------------------------
      #FCC
      ISOMetadataNamespace$new(
        id = "fcc", uri = "https://standards.iso.org/iso/19110/fcc/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO/TS 19139:2007", title = "Feature Catalog Common (FCC) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #GFC
      ISOMetadataNamespace$new(
        id = "gfc", uri = "https://standards.iso.org/iso/19110/gfc/1.1",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO/TS 19139:2007", title = "General Feature Catalog (GFC) Version: 1.1", stringsAsFactors = FALSE)
      ),
      #ISO 19115-3
      #----------------------------------------------------
      #CAT
      ISOMetadataNamespace$new(
        id = "cat", uri = "http://standards.iso.org/iso/19115/-3/cat/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO/TS 19139:2007", title = "CATalogue Objects (CAT) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #CIT
      ISOMetadataNamespace$new(
        id = "cit", uri = "http://standards.iso.org/iso/19115/-3/cit/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Citation and responsible party information (CIT) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #GCO
      ISOMetadataNamespace$new(
        id = "gco", uri = "http://standards.iso.org/iso/19115/-3/gco/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Geospatial COmmon Objects (GCO) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #GCX
      ISOMetadataNamespace$new(
        id = "gcx", uri = "http://standards.iso.org/iso/19115/-3/gcx/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Geospatial Common eXtension (GCX) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #GEX
      ISOMetadataNamespace$new(
        id = "gex", uri = "http://standards.iso.org/iso/19115/-3/gex/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Geospatial EXtent (GEX) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #GMW
      ISOMetadataNamespace$new(
        id = "gmw", uri = "http://standards.iso.org/iso/19115/-3/gmw/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Geographic Markup Wrappers (GMW) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #LAN
      ISOMetadataNamespace$new(
        id = "lan", uri = "http://standards.iso.org/iso/19115/-3/lan/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "metadata for LANguage and localization (LAN) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MAC
      ISOMetadataNamespace$new(
        id = "mac", uri = "http://standards.iso.org/iso/19115/-3/mac/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for ACquisition (MAC) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MAS
      ISOMetadataNamespace$new(
        id = "mas", uri = "http://standards.iso.org/iso/19115/-3/mas/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Application Schemas (MAS) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MCC
      ISOMetadataNamespace$new(
        id = "mcc", uri = "http://standards.iso.org/iso/19115/-3/mcc/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata Common Classes (MCC) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MCO
      ISOMetadataNamespace$new(
        id = "mco", uri = "http://standards.iso.org/iso/19115/-3/mco/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for COnstraints (MCO) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MDA
      ISOMetadataNamespace$new(
        id = "mda", uri = "http://standards.iso.org/iso/19115/-3/mda/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "MetaData for Applications (MDA) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MDB
      ISOMetadataNamespace$new(
        id = "mdb", uri = "http://standards.iso.org/iso/19115/-3/mdb/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "MetaData Base (MDB) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MDS
      ISOMetadataNamespace$new(
        id = "mds", uri = "http://standards.iso.org/iso/19115/-3/mds/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "MetaData for Service identification (MDS) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MDT
      ISOMetadataNamespace$new(
        id = "mdt", uri = "http://standards.iso.org/iso/19115/-3/mdt/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Data Transfer (MDT) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MEX
      ISOMetadataNamespace$new(
        id = "mex", uri = "http://standards.iso.org/iso/19115/-3/mex/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata EXtensions (MEX) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MMI
      ISOMetadataNamespace$new(
        id = "mmi", uri = "http://standards.iso.org/iso/19115/-3/mmi/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Maintenance Information (MMI) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MPC
      ISOMetadataNamespace$new(
        id = "mpc", uri = "http://standards.iso.org/iso/19115/-3/mpc/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Portrayal Catalogues (MPC) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MRC
      ISOMetadataNamespace$new(
        id = "mrc", uri = "http://standards.iso.org/iso/19115/-3/mrc/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Resource Content (MRC) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MRD
      ISOMetadataNamespace$new(
        id = "mrd", uri = "http://standards.iso.org/iso/19115/-3/mrd/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Resource Distribution (MRD) Version: 1.0", stringsAsFactors = FALSE)
      ), 
      #MRI
      ISOMetadataNamespace$new(
        id = "mri", uri = "http://standards.iso.org/iso/19115/-3/mri/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Resource Identification (MRI) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MRL
      ISOMetadataNamespace$new(
        id = "mrl", uri = "http://standards.iso.org/iso/19115/-3/mrl/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Resource Lineage (MRL) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #MRS
      ISOMetadataNamespace$new(
        id = "mrs", uri = "http://standards.iso.org/iso/19115/-3/mrs/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Reference Systems (MRS) Version: 1.0", stringsAsFactors = FALSE)
      ),
      #MSR
      ISOMetadataNamespace$new(
        id = "msr", uri = "http://standards.iso.org/iso/19115/-3/msr/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for Spatial Representation (MSR) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #SRV
      ISOMetadataNamespace$new(
        id = "srv", uri = "http://standards.iso.org/iso/19115/-3/srv/2.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19115-1:2014", title = "Metadata for SeRVices (SRV) Version: 2.0", stringsAsFactors = FALSE)
      ),
      #DQC
      ISOMetadataNamespace$new(
        id = "dqc", uri = "http://standards.iso.org/iso/19157/-2/dqc/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19157", title = "Data Quality abstract Classes (DQC) Version 1.0", stringsAsFactors = FALSE)
      ),
      #MDQ
      ISOMetadataNamespace$new(
        id = "mdq", uri = "http://standards.iso.org/iso/19157/-2/mdq/1.0",
        standard = data.frame(specification = "ISO/TS 19115-3:2023", schema = "ISO 19157", title = "Metadata for Data Quality (MDQ) Version: 1.0", stringsAsFactors = FALSE)
      )
    )
  )
  all_namespaces <- c(
    #XML 1.0
    xml_namespaces,
    #GML
    gml_namespaces,
    #SWE
    swe_namespaces,
    #iso19115
    iso_namespaces
  )
  for(ns in all_namespaces){
    ISOMetadataNamespace[[toupper(ns$id)]] <- ns
  }
  .geometa.iso$namespaces <- all_namespaces
}

#' @name getISOMetadataNamespaces
#' @aliases getISOMetadataNamespaces
#' @title getISOMetadataNamespaces
#' @export
#' @description \code{getISOMetadataNamespaces} gets the list of namespaces registered
#' 
#' @usage getISOMetadataNamespaces()
#' 
#' @examples             
#'   getISOMetadataNamespaces()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOMetadataNamespaces = function(){
  return(.geometa.iso$namespaces)
}

#' @name getISOMetadataNamespace
#' @aliases getISOMetadataNamespace
#' @title getISOMetadataNamespace
#' @export
#' @description \code{getISOMetadataNamespace} gets a namespace given its id
#' 
#' @usage getISOMetadataNamespace(id)
#' 
#' @param id namespace prefix
#' 
#' @examples             
#'   getISOMetadataNamespace("GMD")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOMetadataNamespace = function(id){
  if(is.list(id)) if(getMetadataStandard() %in% names(id)) id = id[[getMetadataStandard()]]
  return(ISOMetadataNamespace[[id]])
}

#' @name registerISOMetadataNamespace
#' @aliases registerISOMetadataNamespace
#' @title registerISOMetadataNamespace
#' @export
#' @description \code{registerISOMetadataNamespace} allows to register a new namespace
#' in \pkg{geometa}
#' 
#' @usage registerISOMetadataNamespace(id, uri, force)
#' 
#' @param id prefix of the namespace
#' @param uri URI of the namespace
#' @param force logical parameter indicating if registration has be to be forced
#' in case the identified namespace is already registered
#' 
#' @examples             
#'   registerISOMetadataNamespace(id = "myprefix", uri = "http://someuri")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerISOMetadataNamespace <- function(id, uri, force = FALSE){
  if(is.list(id)) if(getMetadataStandard() %in% names(id)) id = id[[getMetadataStandard()]]
  ns <- getISOMetadataNamespace(toupper(id))
  if(!is.null(ns)){
    if(!force) stop(sprintf("ISOMetadataNamespace with id '%s' already exists. Use force = TRUE to force registration", id))
    ns <- ISOMetadataNamespace$new(id, uri)
    ISOMetadataNamespace[[toupper(id)]] <- ns
    .geometa.iso$namespaces[sapply(.geometa.iso$namespaces, function(x){x$id == id})][[1]] <- ns
  }else{
    ns <- ISOMetadataNamespace$new(id, uri)
    ISOMetadataNamespace[[toupper(id)]] <- ns
    .geometa.iso$namespaces <- c(.geometa.iso$namespaces, ns)
  }
}