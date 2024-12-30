#' ISOFeatureTypeInfo
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature type info
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO feature type info
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_FeatureTypeInfo}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureTypeInfo <- R6Class("ISOFeatureTypeInfo",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_FeatureTypeInfo",
     xmlNamespacePrefix = list(
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field featureTypeName featureTypeName [1..1] : ISOFeatureTypeInfo
     featureTypeName = NULL,
     #'@field featureInstanceCount featureInstanceCount [0..1]: Integer
     featureInstanceCount = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set feature type Name
     #'@param name object of class \link{ISOGenericName} or \link{character}
     setFeatureTypeName = function(name){
       if(!is(name, "ISOAbstractGenericName")){
         name = ISOAbstractGenericName$new(value = name)
       }
       self$featureTypeName = name
     },
     
     #'@description Set feature instance count
     #'@param count object of class \link{integer}
     setFeatureInstanceCount = function(count){
       self$featureInstanceCount = as.integer(count)
     }
   )                        
)
