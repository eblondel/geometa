#' SWEAbstractSWE
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE abstract SWE object
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractSWE <- R6Class("SWEAbstractSWE",
   inherit = SWEAbstractObject,
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
     #'@param value_as_field whether value should be set as field
     initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = TRUE,
                           value_as_field = FALSE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element, 
                        attrs = attrs, defaults = defaults,
                        wrap = wrap, value_as_field = value_as_field)
     }
   )                        
)
