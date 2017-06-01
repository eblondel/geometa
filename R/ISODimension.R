#' ISODimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO dimension
#' @return Object of \code{\link{R6Class}} for modelling an ISO Dimension
#' @format \code{\link{R6Class}} object.
#'
#' @field dimensionName
#' @field dimensionSize
#' @field resolution
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODimension
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets the dimension name. Object of class \code{ISODimensionNameType} or
#'    any value from \code{ISODimensionNameType$values()}
#'  }
#'  \item{\code{setSize(size)}}{
#'    Sets the dimension size, object of class \code{integer}
#'  }
#'  \item{\code{setResolution(resolution)}}{
#'   Sets the resolution ie. object of class \code{ISOMeasure} or any subclass
#'   \code{ISOLength}, \code{ISODistance}, \code{ISOAngle}, \code{ISOScale}
#'  }
#' }
#' 
#' @examples
#'    #create dimension
#'    md <- ISODimension$new()
#'    md$setName("row")
#'    md$setSize(1)
#'    md$setResolution(ISOLength$new(value=1,uom="m"))
#'    xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODimension <- R6Class("ISODimension",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "MD_Dimension",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ dimensionName [1..1]: ISODimensionNameType
     dimensionName = NULL,
     #+ dimensionSize [1..1]: ISODimensionSize
     dimensionSize = NULL,
     #+ resolution [0..1]: ISOMeasure or subclass
     resolution = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
    
     #setName
     setName = function(name){
       if(!is(name, "ISODimensionNameType")){
         name <- ISODimensionNameType$new(value = name)
       }
       self$dimensionName <- name
     },
     
     #setSize
     setSize = function(size){
       newSize <- as.integer(size)
       if(is.na(newSize)){
         stop(sprintf("Value '%s' cannot be coerced to 'integer'", size))
       }
       self$dimensionSize <- newSize
     },
     
     #setResolution
     setResolution = function(resolution){
       if(!is(resolution, "ISOMeasure")){
         stop("Argument should be an object of class (or subclass of) 'ISOMeasure'")
       }
       self$resolution <- resolution
     }
     
   )                        
)