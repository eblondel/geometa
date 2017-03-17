#' ISOBaseInteger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO integer
#' @return Object of \code{\link{R6Class}} for modelling an ISO Integer
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseInteger
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseInteger <- R6Class("ISOBaseInteger",
  inherit = ISOMetadataElement,
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        element = "Integer",
        namespace = ISOMetadataNamespace$GCO
      )
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        if(!is(value, "integer")){
          value <- as.integer(value)
        }
        self$value = value
      }
    }
  )                        
)