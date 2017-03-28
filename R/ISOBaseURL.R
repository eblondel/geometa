#' ISOBaseURL
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO URL
#' @return Object of \code{\link{R6Class}} for modelling an ISO BaseURL
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseURL
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseURL <- R6Class("ISOBaseURL",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "URL",
    xmlNamespacePrefix = "GMD"
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
        self$value = value
      }
    }
  )                        
)
