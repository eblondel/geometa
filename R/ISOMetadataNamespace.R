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
ISOMetadataNamespace$GMD = ISOMetadataNamespace$new("gmd", "http://www.isotc211.org/2005/gmd")
ISOMetadataNamespace$GMI = ISOMetadataNamespace$new("gmi", "http://www.isotc211.org/2005/gmi")
ISOMetadataNamespace$GTS = ISOMetadataNamespace$new("gts", "http://www.isotc211.org/2005/gts")
ISOMetadataNamespace$SRV = ISOMetadataNamespace$new("srv", "http://www.isotc211.org/2005/srv")
ISOMetadataNamespace$all = function(){
  return(list(
    ISOMetadataNamespace$GCO,
    ISOMetadataNamespace$GMD,
    ISOMetadataNamespace$GMI,
    ISOMetadataNamespace$GTS,
    ISOMetadataNamespace$SRV
  ))
}

getISOMetadataNamespace = function(id){
  return(ISOMetadataNamespace[[id]])
}