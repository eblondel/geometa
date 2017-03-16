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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseURL
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseURL <- R6Class("ISOBaseURL",
  inherit = ISOMetadataElement,
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        element = "URL",
        namespace = ISOMetadataNamespace$GCO
      )
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        self$value = value
      }
    }
  )                        
)