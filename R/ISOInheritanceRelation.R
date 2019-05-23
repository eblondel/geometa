#' ISOInheritanceRelation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO InheritanceRelation
#' @return Object of \code{\link{R6Class}} for modelling an ISOInheritanceRelation
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{character}}] name
#' @field description [\code{\link{character}}] description
#' @field uniqueInstance [\code{\link{logical}}]
#' @field subtype [\code{\link{ISOFeatureType}}] subtype
#' @field supertype [\code{\link{ISOFeatureType}}] supertype
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an \code{\link{ISOInheritanceRelation}}
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Set name of inheritance relation. Locale names can be specified
#'     as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Set description of inheritance relation. Locale names can be specified
#'     as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setUniqueInstance(uniqueInstance)}}{
#'    Set \code{TRUE} if it's a unique instance, \code{FALSE} otherwise
#'  }
#'  \item{\code{setSubtype(featureType)}}{
#'    Set subtype, object of class \code{\link{ISOFeatureType}}
#'  }
#'  \item{\code{setSupertype(featureType)}}{
#'    Set supertype, object of class \code{\link{ISOFeatureType}}
#'  }
#' }
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
     
     #+ name [0..1]: character
     name = NULL,
     #+ description [0..1]: character
     description = NULL,
     #+ uniqueInstance: logical
     uniqueInstance = NULL,
     #+ subtype [1..1]: ISOFeatureType
     subtype = NULL,
     #+ supertype [1..1]: ISOFeatureType
     supertype = NULL,
     
     #setName
     setName = function(name, locales = NULL){
       self$name <- as.character(name)
       if(!is.null(locales)){
         self$name <- self$createLocalisedProperty(name, locales)
       }
     },
     
     #setDescription
     setDescription = function(description, locales = NULL){
       self$description <- as.character(description)
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     },
     
     #setUniqueInstance
     setUniqueInstance = function(uniqueInstance){
       if(!is.logical(uniqueInstance)){
         uniqueInstance < as.logical(uniqueInstance)
         if(is.na(uniqueInstance)){
           stop("The argument value should be 'logical' or coercable as 'logical'")
         }
       }
       self$uniqueInstance <- uniqueInstance
     },
     
     #setSubtype
     setSubtype = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument value should be an object of class 'ISOFeatureType'")
       }
       self$subtype <- featureType
     },
     
     #setSupertype
     setSupertype = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument value should be an object of class 'ISOFeatureType'")
       }
       self$supertype <- featureType
     }
     
   )         
)