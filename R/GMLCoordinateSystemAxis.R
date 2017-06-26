#' GMLCoordinateSystemAxis
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML coordinate system axis
#' @return Object of \code{\link{R6Class}} for modelling an GMLCoordinateSystemAxis
#' @format \code{\link{R6Class}} object.
#'
#' @field axisAbbrev
#' @field axisDirection
#' @field minimumValue
#' @field maximumValue
#' @field rangeMeaning
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{setAbbrev(abbrev)}}{
#'    Sets the axis abbreviation
#'  }
#'  \item{\code{setDirection(direction, codeSpace)}}{
#'    Sets the axis direction
#'  }
#'  \item{\code{setMimimumValue(value)}}{
#'    Sets the minimum value
#'  }
#'  \item{\code{setMaximumValue(value)}}{
#'    Sets the maximum value
#'  }
#'  \item{\code{setRangeMeaning(meaning, codeSpace)}}{
#'    Sets the range meaning
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
GMLCoordinateSystemAxis <- R6Class("GMLCoordinateSystemAxis",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "CoordinateSystemAxis",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
   
     #+ axisAbbrev [1..1]: character
     axisAbbrev = NULL,
     #+ axisDirection [1..1]: character (with codeSpace)
     axisDirection = NULL,
     #+ minimumValue [0..1]: double
     minimumValue = NA,
     #+ maximumValue [0..1]: double
     maximumValue = NA,
     #+ rangeMeaning [0..1]: character (with codeSpace)
     rangeMeaning = NA,
     
     initialize = function(xml = NULL, defaults = list(), id = NA, uom = NA){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
         self$setAttr("uom", uom)
       }
     },
     
     #setAbbrev
     setAbbrev = function(abbrev){
       self$axisAbbrev <- GMLElement$create("axisAbbrev", value = abbrev)
     },
     
     #setDirection
     setDirection = function(direction, codeSpace = NULL){
       self$axisDirection <- GMLElement$create("axisDirection", value = direction, codeSpace = codeSpace)
     },
     
     #setMinimumValue
     setMinimumValue = function(value){
       self$minimumValue <- GMLElement$create("minimumValue", value = value)
     },
     
     #setMaximumValue
     setMaximumValue = function(value){
       self$maximumValue <- GMLElement$create("maximumValue", value = value)
     },
     
     #setRangeMeaning
     setRangeMeaning = function(meaning, codeSpace = NULL){
       self$rangeMeaning <- GMLElement$create("rangeMeaning", value = meaning, codeSpace = codeSpace)
     }
     
   )
)