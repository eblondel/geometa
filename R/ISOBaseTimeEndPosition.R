#' ISOBaseTimeEndPosition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO end position
#' @return Object of \code{\link{R6Class}} for modelling an ISO Time end position
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseTimeEndPosition
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseTimeEndPosition <- R6Class("ISOBaseTimeEndPosition",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "endPosition",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    wrap = FALSE,
    value = NA,
    attrs = list(),
    initialize = function(xml = NULL, value){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      if(is.null(xml)){
        if(all(class(value)==c("POSIXct","POSIXt"))){
          value <- format(value,"%Y-%m-%dT%H:%M:%S")
        }else if(class(value) == "Date"){
          value <- format(value,"%Y-%m-%d")
        }
        self$value = value
      }
    }
  )                        
)