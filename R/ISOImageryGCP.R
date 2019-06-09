#' ISOImageryGCPCollection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery gcp collection
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery gcp collection
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryGCPCollection}}
#'  }
#'  \item{\code{setCollectionIdentification(id)}}{
#'    Set the identifier, object of class \code{integer}
#'  }
#'  \item{\code{setCollectionName(name, locales)}}{
#'    Sets a name (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setCoordinateReferenceSystem(crs)}}{
#'    Sets the crs, object of class \code{\link{ISOReferenceSystem}}
#'  }
#'  \item{\code{addGCP(gcp)}}{
#'    Adds a GCP, object of class \code{\link{ISOImageryGCP}}
#'  }
#'  \item{\code{delGCP(gcp)}}{
#'    Deletes a GCP, object of class \code{\link{ISOImageryGCP}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOImageryGCP$new()
#'   require(sf)
#'   pt <- sf::st_point(c(1,1))
#'   md$setGeographicCoordinates(sfg = pt) 
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGCP <- R6Class("ISOImageryGCP",
 inherit = GMLAbstractObject,
 private = list(
   xmlElement = "MI_GCP",
   xmlNamespacePrefix = "GMI"
 ),
 public = list(
   geographicCoordinates = matrix(NA_real_, 1, 2),
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #setGeographicCoordinates
   setGeographicCoordinates = function(sfg = NULL, m = NULL){
     if(!is.null(sfg)){
       if(!is(sfg, c("sfg","POINT"))) stop("Input 'sfg' object should be a 'point'")
       m <- as.matrix(sfg)
     }else if(!is.null(m)){
       if(!is.matrix(m)){
         stop("The argument 'm' should a matrix")
       }  
     }
     self$geographicCoordinates <- GMLElement$create(element = "geographicCoordinates", xmlNamespacePrefix = "GMI")
     self$geographicCoordinates$pos <- GMLElement$create(element = "pos", value = m)
   }
 )                        
)

