#' ISOImageryGCPCollection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery gcp collection
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery gcp collection
#' @format \code{\link{R6Class}} object.
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
   #'@field geographicCoordinates geographicCoordinates
   geographicCoordinates = matrix(NA_real_, 1, 2),
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml, wrap = TRUE)
   },
   
   #'@description Set geographic coordinates
   #'@param sfg simple feature object from \pkg{sf}
   #'@param m object of class \link{matrix}
   setGeographicCoordinates = function(sfg = NULL, m = NULL){
     if(!is.null(sfg)){
       if(!all(sapply(c("sfg","POINT"), function(x){is(sfg, x)}))) stop("Input 'sfg' object should be a 'point'")
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

