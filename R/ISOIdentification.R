#' ISOIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Identification
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentification <- R6Class("ISOIdentification",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractMD_Identification",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
     )
   ),
   public = list(

     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults defaults list
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml = xml, defaults = defaults)
     }
   )                        
)

ISOIdentification$new = function(xml = NULL, defaults = list()){
  self <- switch(getMetadataStandard(),
     "19139" = ISOIdentification19139$new(xml = xml, defaults = defaults),
     "19115-3" = ISOIdentification19115_3$new(xml = xml, defaults = defaults)
  )
  return(self)
}
