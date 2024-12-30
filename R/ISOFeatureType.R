#' ISOFeatureType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature type
#' @return Object of \code{\link{R6Class}} for modelling an ISO FeatureType
#' @format \code{\link{R6Class}} object.
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
#'   - ISO 19110 - GFC 1.0 https://schemas.isotc211.org/19110/-/gfc/1.0/gfc/#element_FC_FeatureType (in ISO 19139)
#'   
#'   - ISO 19110 - GFC 1.1 https://schemas.isotc211.org/19110/gfc/1.1/gfc/#element_FC_FeatureType (in ISO 19115-3)
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

     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )         
)

ISOFeatureType$new = function(xml = NULL){
  self <- switch(getMetadataStandard(),
   "19139" = ISOFeatureType19139$new(xml = xml),
   "19115-3" = ISOFeatureType19115_3$new(xml = xml)
  )
  return(self)
}
