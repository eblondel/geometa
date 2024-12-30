#' ISOInheritanceRelation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO InheritanceRelation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOInheritanceRelation
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInheritanceRelation <- R6Class("ISOInheritanceRelation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "FC_InheritanceRelation",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field name name [0..1]: character
     name = NULL,
     #'@field description description [0..1]: character
     description = NULL,
     #'@field uniqueInstance uniqueInstance: logical
     uniqueInstance = NULL,
     #'@field subtype subtype [1..1]: ISOFeatureType
     subtype = NULL,
     #'@field supertype supertype [1..1]: ISOFeatureType
     supertype = NULL,
     
     #'@description Set name
     #'@param name name
     #'@param locales list of localized texts. Default is \code{NULL}
     setName = function(name, locales = NULL){
       self$name <- as.character(name)
       if(!is.null(locales)){
         self$name <- self$createLocalisedProperty(name, locales)
       }
     },
     
     #'@description Set description
     #'@param description description
     #'@param locales list of localized texts. Default is \code{NULL}
     setDescription = function(description, locales = NULL){
       self$description <- as.character(description)
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     },
     
     #'@description Set unique instance
     #'@param uniqueInstance object of class \link{logical}
     setUniqueInstance = function(uniqueInstance){
       if(!is.logical(uniqueInstance)){
         uniqueInstance < as.logical(uniqueInstance)
         if(is.na(uniqueInstance)){
           stop("The argument value should be 'logical' or coercable as 'logical'")
         }
       }
       self$uniqueInstance <- uniqueInstance
     },
     
     #'@description Set sub feature type
     #'@param featureType object of class \link{ISOFeatureType}
     setSubtype = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument value should be an object of class 'ISOFeatureType'")
       }
       self$subtype <- featureType
     },
     
     #'@description Set super feature type
     #'@param featureType object of class \link{ISOFeatureType}
     setSupertype = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument value should be an object of class 'ISOFeatureType'")
       }
       self$supertype <- featureType
     }
     
   )         
)
