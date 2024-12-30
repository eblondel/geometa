#' ISOAttributes
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO attributes
#' @return Spatial object of \code{\link[R6]{R6Class}} for modelling a list of ISO xml attributes
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   attrs <- ISOAttributes$new(href = "http://somelink", title = "sometitle")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAttributes <- R6Class("ISOAttributes",
   public = list(
     #'@field attrs attrs
     attrs = list(),
     
     #'@description method is used to instantiate a vector of attributes to be used
     #'    for empty element properties.
     #'@param ... list of attributes
     initialize = function(...){
       simpleAttrs <- c(...)
       class(simpleAttrs) <- "list"
       self$attrs <- simpleAttrs
     }
   )                        
)
