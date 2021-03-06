#' ISOAttributes
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO attributes
#' @return Spatial object of \code{\link{R6Class}} for modelling a list of ISO xml attributes
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(...)}}{
#'    This method is used to instantiate a vector of attributes to be used
#'    for empty element properties.
#'  }
#' }
#' 
#' @examples
#'   attrs <- ISOAttributes$new(href = "http://somelink", title = "sometitle")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAttributes <- R6Class("ISOAttributes",
   public = list(
     attrs = list(),
     initialize = function(...){
       simpleAttrs <- c(...)
       class(simpleAttrs) <- "list"
       self$attrs <- simpleAttrs
     }
   )                        
)