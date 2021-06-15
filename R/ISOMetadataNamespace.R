#' ISOMetadataNamespace
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata namespace
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Namespace
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(id, uri)}}{
#'    This method is used to instantiate an ISOMetadata
#'  }
#' }
#' 
#' @note ISO class used internally by geometa for specifying XML namespaces
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataNamespace <- R6Class("ISOMetadataNamespace",
  public = list(
    id = NA,
    uri = NA,
    initialize = function(id, uri){
      self$id = id
      self$uri = uri
    },
    getDefinition = function(){
      ns <- list(self$uri)
      names(ns) <- self$id
      return(ns)
    }
  )
)
ISOMetadataNamespace$GCO = ISOMetadataNamespace$new("gco", "http://www.isotc211.org/2005/gco")
ISOMetadataNamespace$GFC = ISOMetadataNamespace$new("gfc", "http://www.isotc211.org/2005/gfc")
ISOMetadataNamespace$GMD = ISOMetadataNamespace$new("gmd", "http://www.isotc211.org/2005/gmd")
ISOMetadataNamespace$GMI = ISOMetadataNamespace$new("gmi", "http://www.isotc211.org/2005/gmi") #http://standards.iso.org/iso/19115/-2/gmi/1.0
ISOMetadataNamespace$GMX = ISOMetadataNamespace$new("gmx", "http://www.isotc211.org/2005/gmx")
ISOMetadataNamespace$GTS = ISOMetadataNamespace$new("gts", "http://www.isotc211.org/2005/gts")
ISOMetadataNamespace$SRV = ISOMetadataNamespace$new("srv", "http://www.isotc211.org/2005/srv")
ISOMetadataNamespace$GML = ISOMetadataNamespace$new("gml", "http://www.opengis.net/gml/3.2")
ISOMetadataNamespace$GMLCOV = ISOMetadataNamespace$new("gmlcov", "http://www.opengis.net/gmlcov/1.0")
ISOMetadataNamespace$GMLRGRID = ISOMetadataNamespace$new("gmlrgrid", "http://www.opengis.net/gml/3.3/rgrid")
ISOMetadataNamespace$XLINK = ISOMetadataNamespace$new("xlink", "http://www.w3.org/1999/xlink")
ISOMetadataNamespace$XSI = ISOMetadataNamespace$new("xsi", "http://www.w3.org/2001/XMLSchema-instance")

#' setMetadataNamespaces
#' @export
setISOMetadataNamespaces <- function(){
  .geometa.iso$namespaces <- list(
    ISOMetadataNamespace$GCO,
    ISOMetadataNamespace$GFC,
    ISOMetadataNamespace$GMD,
    ISOMetadataNamespace$GMI,
    ISOMetadataNamespace$GMX,
    ISOMetadataNamespace$GTS,
    ISOMetadataNamespace$SRV,
    ISOMetadataNamespace$GML,
    ISOMetadataNamespace$GMLCOV,
    ISOMetadataNamespace$GMLRGRID,
    ISOMetadataNamespace$XLINK,
	ISOMetadataNamespace$XSI
  )
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