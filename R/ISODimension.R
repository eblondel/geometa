#' ISODimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO dimension
#' @return Object of \code{\link{R6Class}} for modelling an ISO Dimension
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create dimension
#'    md <- ISODimension$new()
#'    md$setName("row")
#'    md$setSize(1)
#'    md$setResolution(ISOLength$new(value=1,uom="m"))
#'    xml <- md$encode()
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODimension <- R6Class("ISODimension",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Dimension",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field dimensionName dimensionName [1..1]: ISODimensionNameType
     dimensionName = NULL,
     #'@field dimensionSize dimensionSize [1..1]: integer
     dimensionSize = NULL,
     #'@field resolution resolution [0..1]: ISOMeasure or subclass
     resolution = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
    
     #'@description Set name
     #'@param name object of class \link{ISODimensionNameType} or any \link{character}
     #'  among values returned by \code{ISODimensionNameType$values()}
     setName = function(name){
       if(!is(name, "ISODimensionNameType")){
         name <- ISODimensionNameType$new(value = name)
       }
       self$dimensionName <- name
     },
     
     #'@description Set size
     #'@param size object of class \link{integer}
     setSize = function(size){
       newSize <- as.integer(size)
       if(is.na(newSize)){
         stop(sprintf("Value '%s' cannot be coerced to 'integer'", size))
       }
       self$dimensionSize <- newSize
     },
     
     #'@description Sets the resolution
     #'@param resolution object of class \code{\link{ISOMeasure}} or any subclass
     #'   \code{\link{ISOLength}}, \code{\link{ISODistance}}, \code{\link{ISOAngle}}, \code{\link{ISOScale}}
     setResolution = function(resolution){
       if(!is(resolution, "ISOMeasure")){
         stop("Argument should be an object of class (or subclass of) 'ISOMeasure'")
       }
       self$resolution <- resolution
     }
     
   )                        
)