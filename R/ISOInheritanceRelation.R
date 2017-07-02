#' ISOInheritanceRelation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO InheritanceRelation
#' @return Object of \code{\link{R6Class}} for modelling an ISOInheritanceRelation
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field description
#' @field uniqueInstance
#' @field subtype
#' @field supertype
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an ISOInheritanceRelation
#'  }
#'  \item{\code{setName(name)}}{
#'    Set name of inheritance relation
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Set description of inheritance relation
#'  }
#'  \item{\code{setUniqueInstance(uniqueInstance)}}{
#'    Set \code{TRUE} if it's a unique instance, \code{FALSE} otherwise
#'  }
#'  \item{\code{setSubtype(featureType)}}{
#'    Set subtype, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{setSupertype(featureType)}}{
#'    Set supertype, object of class \code{ISOFeatureType}
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
     setName = function(name){
       self$name <- as.character(name)
     },
     
     #setDescription
     setDescription = function(description){
       self$description <- as.character(description)
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