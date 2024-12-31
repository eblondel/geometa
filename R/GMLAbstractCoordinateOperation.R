#' GMLAbstractCoordinateOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coordinate Operation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLAbstractCoordinateOperation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractCoordinateOperation <- R6Class("GMLAbstractCoordinateOperation",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "AbstractCoordinateOperation",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field domainOfValidity domainOfValidity [0..1]: character
     domainOfValidity = NULL,
     #'@field scope scope [1..*]: character
     scope = list(),
     #'@field operationVersion operationVersion [0..1]: character
     operationVersion = NULL,
     #'@field coordinateOperationAccuracy coordinateOperationAccuracy [0..1]: ISOPositionalAccuracy 
     coordinateOperationAccuracy = list(), #TODO
     #'@field sourceCRS sourceCRS [0..1]: subclass of GMLAbstractCRS
     sourceCRS = NULL,
     #'@field targetCRS targetCRS [0..1]: subclass of GMLAbstractCRS
     targetCRS = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults list of default values
     #'@param id id
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #'@description Set domain of validity
     #'@param domainOfValidity domain of validity, object extending \link{ISOExtent} class
     setDomainOfValidity = function(domainOfValidity){
       if(!is(domainOfValidity, "ISOExtent")) stop("Input should be an object of class 'ISOExtent'")
       self$domainOfValidity <- domainOfValidity
     },
     
     #'@description Adds scope
     #'@param scope scope
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addScope = function(scope){
       return(self$addListElement("scope", GMLElement$create("scope", value = scope)))
     },
     
     #'@description Removes scope
     #'@param scope scope
     #'@return \code{TRUE} if removed, \code{FALSE} otherwise
     delScope = function(scope){
       return(self$delListElement("scope", GMLElement$create("scope", value = scope)))
     },
     
     #'@description Set version
     #'@param version version
     setVersion = function(version){
       self$operationVersion <- GMLElement$create("operationVersion", value = version)
     },
     
     #'@description Adds accuracy
     #'@param accuracy accuracy, object inheriting class \link{ISOAbstractPositionalAccuracy}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAccuracy = function(accuracy){
       if(!inherits(accuracy, "ISOAbstractPositionalAccuracy")){
         stop("The argument value should be an object of class 'ISOPositionalAccuracy")
       }
       return(self$addListElement("coordinateOperationAccuracy",
                                  GMLElement$create("coordinateOperationAccuracy",
                                                    value = accuracy)))
     },
     
     #'@description Removes accuracy
     #'@param accuracy accuracy, object inheriting class \link{ISOAbstractPositionalAccuracy}
     #'@return \code{TRUE} if removed, \code{FALSE} otherwise
     delAccuracy = function(accuracy){
       if(!inherits(accuracy, "ISOAbstractPositionalAccuracy")){
         stop("The argument value should be an object of class 'ISOAbstractPositionalAccuracy")
       }
       return(self$delListElement("coordinateOperationAccuracy",
                                  GMLElement$create("coordinateOperationAccuracy",
                                                    value = accuracy)))
     },
     
     #'@description Set source CRS
     #'@param crs crs, object inheriting class \link{GMLAbstractSingleCRS}
     setSourceCRS = function(crs){
       if(!inherits(crs, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object extending 'GMLAbstractSingleCRS'")
       }
       self$sourceCRS <- GMLElement$create("sourceCRS", value = crs)
     },
     
     #'@description Set target CRS
     #'@param crs crs, object inheriting class \link{GMLAbstractSingleCRS}
     setTargetCRS = function(crs){
       if(!inherits(crs, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object extending 'GMLAbstractSingleCRS'")
       }
       self$targetCRS <- GMLElement$create("targetCRS", value = crs)
     }
     

   )
)
