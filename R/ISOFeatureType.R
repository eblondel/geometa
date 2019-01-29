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
#' @field constrainedBy
#' @field definitionReference
#' @field carrierOfCharacteristics
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureType
#'  }
#'  \item{\code{setTypeName(typeName)}}{
#'    Sets the type name. Object of class \code{ISOLocalName} or \code{"character"}
#'  }
#'  \item{\code{setDefinition(definition, locales)}}{
#'    Sets the definition. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCode(code, locales)}}{
#'    Sets the code. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
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
#'  \item{\code{addConstraint(constraint)}}{
#'    Adds a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{delConstraint(constraint)}}{
#'    Deletes a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{setDefinitionReference(definitionReference)}}{
#'    Sets the definition Reference, object of class \code{ISODefinitionReference}
#'  }
#'  \item{\code{addCharacteristic(characteristic)}}{
#'    Adds a characteristic as object of class \code{ISOPropertyType} or subclass
#'  }
#'  \item{\code{delCharacteristic(characteristic)}}{
#'    Deletes a characteristic as object of class \code{ISOPropertyType} or subclass
#'  }
#' }
#'
#' @examples 
#'  #featuretype
#'  md <- ISOFeatureType$new()
#'  md$setTypeName("typeName")
#'  md$setDefinition("definition")
#'  md$setCode("code")
#'  md$setIsAbstract(FALSE)
#'  md$addAlias("alias1")
#'  md$addAlias("alias2")
#'  
#'  #add feature attributes
#'  for(i in 1:3){
#'    #create attribute
#'    fat <- ISOFeatureAttribute$new()
#'    fat$setMemberName(sprintf("name %s",i))
#'    fat$setDefinition(sprintf("definition %s",i))
#'    fat$setCardinality(lower=1,upper=1)
#'    fat$setCode(sprintf("code %s",i))
#'    
#'    #add measurement unit
#'    gml <- GMLBaseUnit$new(id = "ID%")
#'    gml$setDescriptionReference("someref")
#'    gml$setIdentifier("identifier", "codespace")
#'    gml$addName("name1", "codespace")
#'    gml$addName("name2", "codespace")
#'    gml$setQuantityTypeReference("someref")
#'    gml$setCatalogSymbol("symbol")
#'    gml$setUnitsSystem("somelink")
#'    fat$setValueMeasurementUnit(gml)
#'    
#'    #add listed values
#'    val1 <- ISOListedValue$new()
#'    val1$setCode("code1")
#'    val1$setLabel("label1")
#'    val1$setDefinition("definition1")
#'    fat$addListedValue(val1)
#'    val2 <- ISOListedValue$new()
#'    val2$setCode("code2")
#'    val2$setLabel("label2")
#'    val2$setDefinition("definition2")
#'    fat$addListedValue(val2)
#'    fat$setValueType("typeName")
#'    
#'    #add feature attribute as carrierOfCharacteristic
#'    md$addCharacteristic(fat)
#'  }
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureType <- R6Class("ISOFeatureType",
   inherit = ISOAbstractObject,
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
     featureCatalogue = NA,
     #+ constrainedBy [0..*]: ISOConstraint
     constrainedBy = list(),
     #+ definitionReference [0..*]: ISODefinitionReference
     definitionReference = list(),
     #+ carrierOfCharacteristics [0..*]: ISOCarrierOfCharacteristics
     carrierOfCharacteristics = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setTypeName
     setTypeName = function(typeName){
       if(!is(typeName,"ISOLocalName")) typeName <- ISOLocalName$new(value = typeName)
       self$typeName <- typeName
     },
     
     #setDefinition
     setDefinition = function(definition, locales = NULL){
       self$definition <- definition
       if(!is.null(locales)){
         self$definition <- self$createLocalisedProperty(definition, locales)
       }
     },
     
     #setCode
     setCode = function(code, locales = NULL){
       self$code = code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
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
       if(!is(definitionReference, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference = definitionReference
     },
     
     #addCharacteristic
     addCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOCarrierOfCharacteristics")){
         stop("The argument should be an object of class extending 'ISOCarrierOfCharacteristics'")
       }
       return(self$addListElement("carrierOfCharacteristics", characteristic))
     },
     
     #delCharacteristic
     delCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOPropertyType")){
         stop("The argument should be an object of class extending 'ISOCarrierOfCharacteristics'")
       }
       return(self$delListElement("carrierOfCharacteristics", characteristic))
     }
   )         
)
