#' GMLGridFunction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML GridFunction
#' @return Object of \code{\link{R6Class}} for modelling an GML grid function
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML grid function
#'  }
#'  \item{\code{setSequenceRule(sequenceRule)}}{
#'    Set the sequence rule, as object of class \code{character}
#'  }
#'  \item{\code{setStartPoint(x,y)}}{
#'    Set the start point
#'  }
#' }
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGridFunction <- R6Class("GMLGridFunction",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "GridFunction",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     sequenceRule = NULL,
     startPoint = matrix(NA,1,2),
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     },
     
     #setSequenceRule
     setSequenceRule = function(sequenceRule){
       allowedValues <- c("Linear","Boustrophedonic", "Cantor-diagonal",
                          "Spiral","Morton","Hilbert")
       if(!(sequenceRule %in% allowedValues)){
         stop(sprintf("The value should be one among [%s]", paste(allowedValues, collapse=",")))
       }
       self$sequenceRule <- GMLElement$create("sequenceRule", sequenceRule)
     },
     
     #setStartPoint
     setStartPoint = function(x,y){
       self$startPoint <- GMLElement$create("startPoint", matrix(c(x,y),1,2))
     }
   )
)