#' GMLRectifiedGrid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML rectified grid
#' @format \code{\link[R6]{R6Class}} object.
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
     #'@field origin origin
     origin = NULL,
     #'@field offsetVector offset vector
     offsetVector = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml, element = private$xmlElement)
     },
     
     #'@description Set origin
     #'@param x x
     #'@param y y
     setOrigin = function(x,y){
       self$origin <- GMLPoint$new(sfg = sf::st_point(c(x,y)))
     },
     
     #'@description Adds offset vector
     #'@param vec vec, object of class \link{vector}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOffsetVector = function(vec){
       if(!is.vector(vec)){ stop("Input should be a vector")}
       if(length(vec)!=self$attrs$dimension){
         stop(sprintf("Input vector length should equal to the number of dimensions",
                      self$attrs$dimension))
       }
       offsetVector <- GMLElement$create("offsetVector", list(matrix(vec,1,2)))
       return(self$addListElement("offsetVector", offsetVector))
     },
     #'@description Deletes offset vector
     #'@param vec vec, object of class \link{vector}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
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
