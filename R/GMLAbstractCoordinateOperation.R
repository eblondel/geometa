#' GMLAbstractCoordinateOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coordinate Operation
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractCoordinateOperation
#' @format \code{\link{R6Class}} object.
#'
#' @field operationVersion
#' @field coordinateOperationAccuracy
#' @field sourceCRS
#' @field targetCRS
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{setVersion(version)}}{
#'    Sets version
#'  }
#'  \item{\code{addAccuracy(accuracy)}}{
#'    Adds coordinate operation accuracy, object extending \code{ISOAbstractPositionalAccuracy}
#'  }
#'  \item{\code{delAccuracy(accuracy)}}{
#'    Deletes coordinate operation accuracy, object extending \code{ISOAbstractPositionalAccuracy}
#'  }
#'  \item{\code{setSourceCRS(sourceCRS)}}{
#'    Sets the source CRS, object extending \code{GMLAbstractSingleCRS}
#'  }
#'  \item{\code{setTargetCRS(targetCRS)}}{
#'    Sets the target CRS, object extending \code{GMLAbstractSingleCRS}
#'  }
#' }
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
     
     #+ operationVersion [0..1]: character
     operationVersion = NULL,
     #+ coordinateOperationAccuracy [0..1]: ISOPositionalAccuracy 
     coordinateOperationAccuracy = list(), #TODO
     #+ sourceCRS [0..1]: subclass of GMLAbstractCRS
     sourceCRS = NULL,
     #+ targetCRS [0..1]: subclass of GMLAbstractCRS
     targetCRS = NULL,
     
     initialize = function(xml = NULL, defaults = list(), id = NA){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #setVersion
     setVersion = function(version){
       self$operationVersion <- GMLElement$create("operationVersion", value = version)
     },
     
     #addAccuracy
     addAccuracy = function(accuracy){
       if(!inherits(accuracy, "ISOPOsitionalAccuracy")){
         stop("The argument value should be an object of class 'ISOPositionalAccuracy")
       }
       return(self$addListElement("coordinateOperationAccuracy",
                                  GMLElement$create("coordinateOperationAccuracy",
                                                    value = accuracy)))
     },
     
     #delAccuracy
     delAccuracy = function(accuracy){
       if(!inherits(accuracy, "ISOAbstractPositionalAccuracy")){
         stop("The argument value should be an object of class 'ISOAbstractPositionalAccuracy")
       }
       return(self$delListElement("coordinateOperationAccuracy",
                                  GMLElement$create("coordinateOperationAccuracy",
                                                    value = accuracy)))
     },
     
     #setSourceCRS
     setSourceCRS = function(crs){
       if(!inherits(crs, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object extending 'GMLAbstractSingleCRS'")
       }
       self$sourceCRS <- GMLElement$create("sourceCRS", value = crs)
     },
     
     #setTargetCRS
     setTargetCRS = function(crs){
       if(!inherits(crs, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object extending 'GMLAbstractSingleCRS'")
       }
       self$targetCRS <- GMLElement$create("targetCRS", value = crs)
     }
     

   )
)