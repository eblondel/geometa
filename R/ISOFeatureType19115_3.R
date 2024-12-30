#' ISOFeatureType19115_3
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO FeatureType in ISO 19115-3
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'  #featuretype
#'  md <- ISOFeatureType$new()
#'  md$setTypeName("typeName")
#'  md$setDefinition("definition")
#'  md$setDesignation("designation")
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
#'   - ISO 19110 - GFC 1.1 https://schemas.isotc211.org/19110/gfc/1.1/gfc/#element_FC_FeatureType
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureType19115_3 <- R6Class("ISOFeatureType19115_3",
   inherit = ISOFeatureType,
   private = list(
     xmlElement = "FC_FeatureType",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field typeName typeName [1..1]: character
     typeName = NULL,
     #'@field definition definition [0..1]: character
     definition = NULL,
     #'@field code code [0..1]: character
     code = NULL,
     #'@field isAbstract isAbstract [1..1]: logical
     isAbstract = FALSE,
     #'@field aliases aliases [0..*]: character
     aliases = list(),
     #'@field designation designation [0..1]: character
     designation = NULL,
     #'@field carrierOfCharacteristics carrierOfCharacteristics [0..*]: ISOCarrierOfCharacteristics
     carrierOfCharacteristics = list(),
     #'@field inheritsFrom inheritsFrom [0..*]: ISOInheritanceRelation
     inheritsFrom = list(),
     #'@field inheritsTo inheritsTo [0..*]: ISOInheritanceRelation
     inheritsTo = list(),
     #'@field constrainedBy constrainedBy [0..*]: ISOConstraint
     constrainedBy = list(),
     #'@field definitionReference definitionReference [0..*]: ISODefinitionReference
     definitionReference = list(),
     #'@field featureCatalogue featureCatalogue: ISOFeatureCatalogue
     featureCatalogue = NA,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set type name
     #'@param typeName type name, object of class \link{character}
     setTypeName = function(typeName){
       typeName <- ISOElementSequence$new(value = typeName)
       self$typeName <- typeName
     },
     
     #'@description Set definition
     #'@param definition definition
     #'@param locales list of localized definitions. Default is \code{NULL}
     setDefinition = function(definition, locales = NULL){
       self$definition <- definition
       if(!is.null(locales)){
         self$definition <- self$createLocalisedProperty(definition, locales)
       }
     },
     
     #'@description Set code
     #'@param code definition
     #'@param locales list of localized codes. Default is \code{NULL}
     setCode = function(code, locales = NULL){
       self$code = code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
     },
     
     #'@description Set whether feature type is abstract
     #'@param isAbstract object of class \link{logical}
     setIsAbstract = function(isAbstract){
       if(!is.logical(isAbstract)) isAbstract <- as.logical(isAbstract)
       if(is.na(isAbstract)){
         stop("Value cannot be coerced to 'logical'")
       }
       self$isAbstract <- isAbstract
     },
     
     #'@description Adds alias
     #'@param alias object of class \link{ISOLocalName} (in ISO 19139 only) 
     #'or \link{character} (in ISO 19139 or ISO 19115-3)
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAlias = function(alias){
       alias <- ISOElementSequence$new(value = alias)
       return(self$addListElement("aliases", alias))
     },
     
     #'@param alias object of class \link{ISOLocalName} (in ISO 19139 only) 
     #'or \link{character} (in ISO 19139 or ISO 19115-3)
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAlias = function(alias){
       alias <- ISOElementSequence$new(value = alias)
       return(self$delListElement("aliases", alias))
     },
     
     #'@description Set designation
     #'@param designation designation
     #'@param locales list of localized designations. Default is \code{NULL}
     setDesignation = function(designation, locales = NULL){
       self$stopIfMetadataStandardIsNot("19115-3")
       self$designation <- designation
       if(!is.null(locales)){
         self$designation <- self$createLocalisedProperty(designation, locales)
       }
     },
     
     #'@description Adds characteristic
     #'@param characteristic characteristic, object inheriting class \link{ISOAbstractCarrierOfCharacteristics}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOAbstractCarrierOfCharacteristics")){
         stop("The argument should be an object of class extending 'ISOAbstractCarrierOfCharacteristics'")
       }
       return(self$addListElement("carrierOfCharacteristics", characteristic))
     },
     
     #'@description Deletes characteristic
     #'@param characteristic characteristic, object inheriting class \link{ISOAbstractCarrierOfCharacteristics}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCharacteristic = function(characteristic){
       if(!is(characteristic, "ISOAbstractCarrierOfCharacteristics")){
         stop("The argument should be an object of class extending 'ISOAbstractCarrierOfCharacteristics'")
       }
       return(self$delListElement("carrierOfCharacteristics", characteristic))
     },
     
     #'@description Adds 'inheritsFrom' relation
     #'@param rel rel, object of class \link{ISOInheritanceRelation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addInheritsFrom = function(rel){
       if(!is(rel, "ISOInheritanceRelation")){
         stop("Argument value should be an object of class 'ISOInheritanceRelation'")
       }
       return(self$addListElement("inheritsFrom", rel))
     },
     
     #'@description Deletes 'inheritsFrom' relation
     #'@param rel rel, object of class \link{ISOInheritanceRelation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delInheritsFrom = function(rel){
       if(!is(rel, "ISOInheritanceRelation")){
         stop("Argument value should be an object of class 'ISOInheritanceRelation'")
       }
       return(self$delListElement("inheritsFrom", rel))
     },
     
     #'@description Adds 'inheritsTo' relation
     #'@param rel rel, object of class \link{ISOInheritanceRelation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addInheritsTo = function(rel){
       if(!is(rel, "ISOInheritanceRelation")){
         stop("Argument value should be an object of class 'ISOInheritanceRelation'")
       }
       return(self$addListElement("inheritsTo", rel))
     },
     
     #'@description Deletes 'inheritsTo' relation
     #'@param rel rel, object of class \link{ISOInheritanceRelation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delInheritsTo = function(rel){
       if(!is(rel, "ISOInheritanceRelation")){
         stop("Argument value should be an object of class 'ISOInheritanceRelation'")
       }
       return(self$delListElement("inheritsTo", rel))
     },
           
     #'@description Adds constraint
     #'@param constraint constraint, object of class \link{ISOConstraint}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$addListElement("constrainedBy", constraint))
     },
     
     #'@description Deletes constraint
     #'@param constraint constraint, object of class \link{ISOConstraint}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$delListElement("constrainedBy", constraint))
     },
     
     #'@description Set definition reference
     #'@param definitionReference object of class \link{ISODefinitionReference}
     setDefinitionReference = function(definitionReference){
       if(!is(definitionReference, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference = definitionReference
     },
     
     #'@description Set feature catalogue
     #'@param fc object of class \link{ISOFeatureCatalogue}
     setFeatureCatalogue = function(fc){
       if(!is(fc, "ISOFeatureCatalogue")){
         stop("Argument value should be an object of class 'ISOFeatureCatalogue'")
       }
       self$featureCatalogue = fc
     }
   )         
)
