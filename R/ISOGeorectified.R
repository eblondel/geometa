#' ISOGeorectified
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO georectified
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Georectified
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Georectified}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_Georectified}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeorectified <- R6Class("ISOGeorectified",
   inherit = ISOGridSpatialRepresentation,
   private = list(
     xmlElement = "MD_Georectified",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MSR"
     )
   ),
   public = list(
     
     #'@field checkPointAvailability checkPointAvailability [1..1]
     checkPointAvailability = NULL,
     #'@field checkPointDescription checkPointDescription [0..1]
     checkPointDescription = NULL,
     #'@field cornerPoints cornerPoints [0..*]
     cornerPoints = list(),
     #'@field centerPoint centerPoint [0..1]
     centerPoint = NULL,
     #'@field pointInPixel pointInPixel [1..1]
     pointInPixel = NULL,
     #'@field transformationDimensionDescription transformationDimensionDescription [0..1]
     transformationDimensionDescription = NULL,
     #'@field transformationDimensionMapping transformationDimensionMapping [0..2]
     transformationDimensionMapping = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set check point availability
     #'@param availability object of class \link{logical}
     setCheckPointAvailability = function(availability){
       if(!is(availability, "logical")){
         stop("The argument should be of class 'logical' (TRUE/FALSE)")
       }
       self$checkPointAvailability = availability
     },
     
     #'@description Set check point description
     #'@param description object of class \link{character}
     #'@param locales list of localized descriptions. Default is \code{NULL}
     setCheckPointDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$checkPointDescription <- description
     },

     #'@description Adds corner point
     #'@param sfg simple feature object from \pkg{sf}
     #'@param m simple feature object of class \link{matrix}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCornerPoint = function(sfg = NULL, m = NULL){
       cornerPoint <- GMLPoint$new(sfg = sfg, m = m)
       return(self$addListElement("cornerPoints", cornerPoint))
     },
     
     #'@description Deletes corner point
     #'@param sfg simple feature object from \pkg{sf}
     #'@param m simple feature object of class \link{matrix}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCornerPoint = function(sfg = NULL, m = NULL){
       cornerPoint <- GMLPoint$new(sfg = sfg, m = m)
       return(self$delListElement("cornerPoints", cornerPoint))
     },
     
     #'@description Sets center point
     #'@param sfg simple feature object from \pkg{sf}
     #'@param m simple feature object of class \link{matrix}
     setCenterPoint = function(sfg = NULL, m = NULL){
       self$centerPoint <- GMLPoint$new(sfg = sfg, m = m)
     },
     
     #'@description Set pixel orientation
     #'@param pixelOrientation object of class \link{ISOPixelOrientation} or \link{character} among
     #'  values among those returned by \code{ISOPixelOrientation$values()}
     setPixelOrientation = function(pixelOrientation){
       if(is(pixelOrientation, "character")){
         pixelOrientation <- ISOPixelOrientation$new(value = pixelOrientation)
       }
       self$pointInPixel <- pixelOrientation
     },
     
     #'@description Set transformation dimension description
     #'@param description description
     #'@param locales list of localized descriptions. Default is \code{NULL}
     setTransformationDimensionDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$transformationDimensionDescription <- description
     },
     
     #'@description Adds transformation dimension mapping
     #'@param mapping mapping
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTransformationDimensionMapping = function(mapping){
       return(self$addListElement("transformationDimensionMapping", mapping))
     },
     
     #'@description Deletes transformation dimension mapping
     #'@param mapping mapping
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTransformationDimensionMapping = function(mapping){
       return(self$delListElement("transformationDimensionMapping", mapping))
     }
     
   )                        
)
