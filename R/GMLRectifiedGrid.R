#' GMLRectifiedGrid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link{R6Class}} for modelling an GML rectified grid
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited Methods:
#' \describe{
#'  \item{\code{setGridEnvelope(xmin, xmax, ymin, ymax)}}{
#'    Set the grid envelope limits with \code{xmin},\code{xmax},\code{ymin} and \code{ymax}.
#'  }
#'  \item{\code{setAxislabels(xlabel,ylabel)}}{
#'    Set the Axis labels
#'  }
#'  \item{\code{addAxisName(axisName)}}{
#'    Adds an axis name
#'  }
#'  \item{\code{delAxisName(axisName)}}{
#'    Deletes an axis name
#'  }
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element)}}{
#'    This method is used to instantiate a GML rectified grid
#'  }
#'  \item{\code{setOrigin(x,y)}}{
#'    Set the origin of the rectified grid
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
GMLRectifiedGrid <- R6Class("GMLRectifiedGrid",
   inherit = GMLGrid,
   private = list(
     xmlElement = "RectifiedGrid",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     origin = NULL,
     offsetVector = list(),
     initialize = function(xml = NULL){
       super$initialize(xml, element = private$xmlElement)
     },
     
     #setOrigin
     setOrigin = function(x,y){
       self$origin <- GMLPoint$new(sfg = sf::st_point(c(x,y)))
     },
     
     #addOffsetVector
     addOffsetVector = function(vec){
       if(!is.vector(vec)){ stop("Input should be a vector")}
       if(length(vec)!=self$attrs$dimension){
         stop(sprintf("Input vector length should equal to the number of dimensions",
                      self$attrs$dimension))
       }
       offsetVector <- GMLElement$create("offsetVector", list(matrix(vec,1,2)))
       return(self$addListElement("offsetVector", offsetVector))
     },
     
     #delOffsetVector
     delOffsetVector = function(vec){
       if(!is.vector(vec)){ stop("Input should be a vector")}
       if(length(vec)!=self$attrs$dimension){
         stop(sprintf("Input vector length should equal to the number of dimensions",
                      self$attrs$dimension))
       }
       offsetVector <- GMLElement$create("offsetVector", list(matrix(vec,1,2)))
       return(self$delListElement("offsetVector", offsetVector))
     }
   )
)