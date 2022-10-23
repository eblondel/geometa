#' GMLAbstractTimePrimitive
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML time primitive abstract
#' @return Object of \code{\link{R6Class}} for modelling an GML AbstractTimePrimitive
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractTimePrimitive <- R6Class("GMLAbstractTimePrimitive",
     inherit = GMLAbstractTimeObject,
     private = list(
       xmlElement = "AbstractTimePrimitive",
       xmlNamespacePrefix = "GML"
     ),
     public = list(
         
       #'@field relatedTime relatedTime
       relatedTime = list(),
                 
       #'@description Initializes object
       #'@param xml object of class \link{XMLInternalNode-class}
       #'@param defaults list of default values
       initialize = function(xml = NULL, defaults = list()){
         super$initialize(xml, defaults)
       },
       
       #'@description Adds related time
       #'@param time object of class \link{GMLTimeInstant}, \link{GMLTimePeriod}, \link{GMLTimeNode} or \link{GMLTimeEdge}
       #'@return \code{TRUE} if added, \code{FALSE} otherwise
       addRelatedTime = function(time){
         allowedClasses <- c("GMLTimeInstant", "GMLTimePeriod",
                             "GMLTimeNode", "GMLTimeEdge")
         if(!(class(time)[1] %in% allowedClasses)){
           stop(sprintf("The argument value should an object of class among the following classes %s",
                        paste(allowedClasses, collapse=", ")))
         }
         return(self$addListElement("relatedTime", time))
       },
       
       #'@description Deletes related time
       #'@param time object of class \link{GMLTimeInstant}, \link{GMLTimePeriod}, \link{GMLTimeNode} or \link{GMLTimeEdge}
       #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
       delRelatedTime = function(time){
         allowedClasses <- c("GMLTimeInstant", "GMLTimePeriod",
                             "GMLTimeNode", "GMLTimeEdge")
         if(!(class(time)[1] %in% allowedClasses)){
           stop(sprintf("The argument value should an object of class among the following classes %s",
                        paste(allowedClasses, collapse=", ")))
         }
         return(self$delListElement("relatedTime", time))
       }
     )                        
)