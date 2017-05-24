#' ISOBaseDateTime
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetime
#' @return Object of \code{\link{R6Class}} for modelling an ISO DateTime
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseDateTime
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDateTime <- R6Class("ISOBaseDateTime",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "DateTime",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      if(is.null(xml)){
        if(all(class(value)==c("POSIXct","POSIXt"))){
          value <- format(value,"%Y-%m-%dT%H:%M:%S")
        }
        self$value = value
      }
    }
  )                        
)