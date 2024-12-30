#' ISOElementSequence
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO record
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOElementSequence
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note This class is used internally by geometa to deal with simple type not
#' handled by proper class element. e.g. \code{name} property of \code{ISOParameter}
#' class from ISO 19119:2005
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOElementSequence <- R6Class("ISOElementSequence",
   inherit = ISOAbstractObject,
   lock_objects = FALSE,
   private = list(
     xmlElement = "ElementSequence",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
      
     #'@description Initializes sequence object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param ... other args
     initialize = function(xml = NULL, ...){
       super$initialize(xml = xml)
       if(is.null(xml)){
         fields <- list(...)
         if(!is.null(names(fields))){
           #named sequence
           for(fieldName in names(fields)){
             self[[fieldName]] <- fields[[fieldName]]
           }
         }else{
           self[["_internal_"]] <- fields
         }
       }
     }
   )                        
)
