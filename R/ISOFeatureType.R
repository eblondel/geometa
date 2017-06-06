#' ISOFeatureType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature type
#' @return Object of \code{\link{R6Class}} for modelling an ISO FeatureType
#' @format \code{\link{R6Class}} object.
#'
#' @field typeName
#' @field definition
#' @field code
#' @field isAbstract
#' @field aliases
#' @field inheritsFrom
#' @field inheritsTo
#' @field featureCatalogue
#' @field carrierOfCharacteristics
#' @field constrainedBy
#' @field definitionResource
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureType
#'  }
#'  \item{\code{setTypeName(typeName)}}{
#'    Sets the type name. Object of class \code{ISOLocalName} or \code{"character"}
#'  }
#'  \item{\code{setDefinition(definition)}}{
#'    Sets the definition
#'  }
#'  \item{\code{setCode(code)}}{
#'    Sets the code
#'  }
#'  \item{\code{setIsAbstract(isAbstract)}}{
#'    Sets TRUE/FALSE if the feature type is abstract or not
#'  }
#'  \item{\code{addAlias(alias)}}{
#'    Set alias name. Object of class \code{ISOLocalName} or \code{"character"}
#'  }
#'  \item{\code{delAlias(alias)}}{
#'    Deletes alias name
#'  }
#'  \item{\code{setFeatureCatalogue(fc)}}{
#'    Sets a feature catalogue, object of class \code{ISOFeatureCatalogue}
#'  }
#'  \item{\code{addCharacteristic(characteristic)}}{
#'    Adds a characteristic as object of class \code{ISOPropertyType} or subclass
#'  }
#'  \item{\code{delCharacteristic(characteristic)}}{
#'    Deletes a characteristic as object of class \code{ISOPropertyType} or subclass
#'  }
#'  \item{\code{addConstraint(constraint)}}{
#'    Adds a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{delConstraint(constraint)}}{
#'    Deletes a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{setDefinitionReference(definitionReference)}}{
#'    Sets the definition Reference, object of class \code{ISODefinitionReference}
#'  }
#' }
#'
#' @examples 
#'  ft <- ISOFeatureType$new()
#'  ft$setTypeName("typeName")
#'  ft$setDefinition("definition")
#'  ft$setCode("code")
#'  ft$setIsAbstract(FALSE)
#'  ft$addAlias("alias1")
#'  ft$addAlias("alias2")
#'  xml <- ft$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureType <- R6Class("ISOFeatureType",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "FC_FeatureType",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #+ typeName [1..1]: ISOLocalName
     typeName = NULL,
     #+ definition [0..1]: character
     definition = NULL,
     #+ code [0..1]: character
     code = NULL,
     #+ isAbstract [1..1]: logical
     isAbstract = FALSE,
     #+ aliases [0..*]: ISOLocalName
     aliases = list(),
     #+ inheritsFrom [0..*]: ?
     inheritsFrom = list(),
     #+ inheritsTo [0..*]: ?
     inheritsTo = list(),
     #+ featureCatalogue: ISOFeatureCatalogue
     featureCatalogue = NULL,
     #+ carrierOfCharacteristics [0..*]: ISOPropertyType / ISOBinding
     carrierOfCharacteristics = list(),
     #+ constrainedBy [0..*]: ISOConstraint
     constrainedBy = list(),
     #+ definitionReference [0..*]: ISODefinitionReference
     definitionReference = list(),
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setTypeName
     setTypeName = function(typeName){
       if(!is(typeName,"ISOLocalName")) typeName <- ISOLocalName$new(value = typeName)
       self$typeName <- typeName
     },
     
     #setDefinition
     setDefinition = function(definition){
       self$definition <- definition
     },
     
     #setCode
     setCode = function(code){
       self$code = code
     },
     
     #setIsAbstract
     setIsAbstract = function(isAbstract){
       if(!is.logical(isAbstract)) isAbstract <- as.logical(isAbstract)
       if(is.na(isAbstract)){
         stop("Value cannot be coerced to 'logical'")
       }
       self$isAbstract <- isAbstract
     },
     
     #addAlias
     addAlias = function(alias){
       if(!is(alias, "ISOLocalName")){
         alias <- ISOLocalName$new(value = alias)
       }
       return(self$addListElement("aliases", alias))
     },
     
     #delAlias
     delAlias = function(alias){
       if(!is(alias, "ISOLocalName")){
         alias <- ISOLocalName$new(value = alias)
       }
       return(self$delListElement("aliases", alias))
     },
     
     addInheritsFrom = function(){
       stop("Method not yet supported in geometa!")
     },
     
     delInheritsFrom = function(){
       stop("Method not yet supported in geometa!")
     },
     
     addInheritsTo = function(){
       stop("Method not yet supported in geometa!")
     },
     
     delInheritsTo = function(){
       stop("Method not yet supported in geometa!")
     },
     
     #setFeatureCatalogue
     setFeatureCatalogue = function(fc){
       if(!is(fc, "ISOFeatureCatalogue")){
         stop("Argument value should be an object of class 'ISOFeatureCatalogue'")
       }
       self$featureCatalogue = fc
     },
     
     #addCharacteristic
     addCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOPropertyType")){
         stop("The argument should be an object of class 'ISOPropertyType' or subclass")
       }
       return(self$addListElement("carrierOfCharacteristics", characteristic))
     },
     
     #delCharacteristic
     delCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOPropertyType")){
         stop("The argument should be an object of class 'ISOPropertyType' or subclass")
       }
       return(self$delListElement("carrierOfCharacteristics", characteristic))
     },
     
     #addConstraint
     addConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$addListElement("constrainedBy", constraint))
     },
     
     #delConstraint
     delConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$delListElement("constrainedBy", constraint))
     },
     
     #setDefinitionReference
     setDefinitionReference = function(definitionReference){
       if(!is(definition, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference = definitionReference
     }
   )         
)