#' ISOFeatureCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO FeatureCatalogue
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @examples 
#'  fc <- ISOFeatureCatalogue$new(uuid = "my-fc-identifier")
#'  fc$setName("name")
#'  fc$addScope("scope1")
#'  fc$addScope("scope2")
#'  fc$addFieldOfApplication("field1")
#'  fc$addFieldOfApplication("field2")
#'  fc$setVersionNumber("1.0")
#'  fc$setVersionDate(ISOdate(2015, 1, 1, 1))
#'  
#'  producer <- ISOResponsibleParty$new()
#'  producer$setIndividualName("someone")
#'  fc$setProducer(producer)
#'  fc$setFunctionalLanguage("eng")
#'  
#'  cit <- ISOCitation$new()
#'  cit$setTitle("some citation title")
#'  fc$addDefinitionSource(cit)

#'  #'  #add featureType
#'  ft <- ISOFeatureType$new()
#'  ft$setTypeName("typeName")
#'  ft$setDefinition("definition")
#'  ft$setCode("code")
#'  ft$setIsAbstract(FALSE)
#'  ft$addAlias("alias1")
#'  ft$addAlias("alias2")
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
#'    gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
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
#'    ft$addCharacteristic(fat)
#'  }

#'  #add featureType to catalogue
#'  fc$addFeatureType(ft)
#'  
#'  xml <- fc$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureCatalogue <- R6Class("ISOFeatureCatalogue",
    inherit = ISOAbstractCatalogue,
    private = list(
      document = TRUE,
      xmlElement = "FC_FeatureCatalogue",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      #'@field attrs attrs
      attrs = list(),
      #'@field producer producer [1..1]: ISOResponsibleParty
      producer = NULL,
      #'@field functionalLanguage functionalLanguage [0..1]: character 
      functionalLanguage = NULL,
      #'@field featureType featureType [1..*]: ISOFeatureType
      featureType = list(),
      #'@field definitionSource definitionSource [0..*]: ISODefinitionSource
      definitionSource = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param uuid uuid
      initialize = function(xml = NULL, uuid = NULL){
        super$initialize(xml = xml)
        if(!is.null(uuid)) self$attrs[["uuid"]] <- as.character(uuid)
      },
      
      #'@description Set producer
      #'@param producer object of class \link{ISOResponsibleParty} (in ISO 19139) or \link{ISOResponsibility} (in ISO 19115-3)
      setProducer = function(producer){
        switch(getMetadataStandard(),
          "19139" = {
            if(!is(producer,"ISOResponsibleParty")){
              stop("The argument should be a 'ISOResponsibleParty' object")
            }
          },
          "19115-3" = {
            if(!is(producer,"ISOResponsibility")){
              stop("The argument should be a 'ISOResponsibility' object")
            }
          }
        )
        self$producer <- producer
      },
      
      #'@description Set functional language
      #'@param functionalLanguage functional language
      setFunctionalLanguage = function(functionalLanguage){
        if(!is(functionalLanguage,"character")) functionalLanguage <- as(functionalLanguage, "character")
        self$functionalLanguage <- functionalLanguage
      },
      
      #'@description Adds feature type
      #'@param featureType object of class \link{ISOFeatureType}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be a 'ISOFeatureType' object")
        }
        return(self$addListElement("featureType", featureType))
      },
      
      #'@description Deletes feature type
      #'@param featureType object of class \link{ISOFeatureType}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be a 'ISOFeatureType' object")
        }
        return(self$delListElement("featureType", featureType))
      },
      
      #'@description Adds definition source
      #'@param source object of class \link{ISODefinitionSource} or \link{ISOCitation}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addDefinitionSource = function(source){
        if(!is(source, "ISODefinitionSource")){
          if(is(source, "ISOCitation")){
            source <- ISODefinitionSource$new(source = source)
          }else{
            stop("The argument should be an object of class 'ISODefinitionSource' or 'ISOCitation'")
          }
        }
        return(self$addListElement("definitionSource", source))
      },
      
      #'@description Deletes definition source
      #'@param source object of class \link{ISODefinitionSource} or \link{ISOCitation}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delDefinitionSource = function(source){
        if(!is(source, "ISODefinitionSource")){
          if(is(source, "ISOCitation")){
            source <- ISODefinitionSource$new(source = source)
          }else{
            stop("The argument should be an object of class 'ISODefinitionSource' or 'ISOCitation'")
          }
        }
        return(self$delListElement("definitionSource", source))
      }
    )                        
)
