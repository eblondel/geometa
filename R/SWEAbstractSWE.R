#' SWEAbstractSWE
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML
#' @return Object of \code{\link{R6Class}} for modelling an SWE abstract SWE object
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractSWE <- R6Class("SWEAbstractSWE",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractSWE",
     xmlNamespacePrefix = "SWE"
   ),
   public = list(
     
     #'@description Initializes an object of class \link{SWEAbstractSWE}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param element element
     #'@param attrs attrs
     #'@param defaults defaults
     #'@param wrap wrap
     initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = FALSE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element, namespace = private$xmlNamespacePrefix, 
                        attrs = attrs, defaults = defaults,
                        wrap = wrap, value_as_field = TRUE)
     }
   )                        
)