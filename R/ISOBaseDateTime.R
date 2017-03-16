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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseDateTime
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDateTime <- R6Class("ISOBaseDateTime",
  inherit = ISOMetadataElement,
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        element = "DateTime",
        namespace = ISOMetadataNamespace$GCO
      )
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        if(all(class(value)==c("POSIXct","POSIXt"))){
          value <- format(value,"%Y-%m-%dT%H:%M:%S")
        }
        self$value = value
      }
    }
  )                        
)