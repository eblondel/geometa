#' SWENilValues
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE nil values object
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWENilValues <- R6Class("SWENilValues",
   inherit = SWEAbstractSWE,
   private = list(
     xmlElement = "NilValues",
     xmlNamespacePrefix = "SWE"
   ),
   public = list(
     
     #'@field nilValue nil value
     nilValue = list(),
     
     #'@description Initializes a SWE Nil Values object
     #'@param xml object of class \link[XML]{XMLInternalNode-class} from \pkg{XML}
     initialize = function(xml = NULL){
       super$initialize(xml, element = private$xmlElement, 
                        attrs = list(), defaults = list(),
                        wrap = TRUE)
     },
     
     #'@description Adds a nil value with a reason
     #'@param value value
     #'@param reason reason
     addNilValue = function(value, reason){
        nilValueElem <- SWEElement$create(element = "nilValue", value = value)
        nilValueElem$setAttr("reason", reason)
        self$nilValue <- c(self$nilValue, nilValueElem)
     }
   )                        
)
