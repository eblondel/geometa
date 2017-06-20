#' GMLAbstractTimePrimitive
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML time primitive abstract
#' @return Object of \code{\link{R6Class}} for modelling an GML AbstractTimePrimitive
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate a GML AbstractTimePrimitive
#'  }
#'  \item{\code{setId(id)}}{
#'    Sets the id
#'  }
#'  \item{\code{addRelatedTime(time)}}{
#'    Adds related time, object of class among \code{GMLTimeInstant}, \code{GMLTimePeriod},
#'    \code{GMLTimeNode} or \code{GMLTimeEdge}
#'  }
#'  \item{\code{delRelatedTime(time)}}{
#'    Deletes related time
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
GMLAbstractTimePrimitive <- R6Class("GMLAbstractTimePrimitive",
     inherit = GMLAbstractTimeObject,
     private = list(
       xmlElement = "AbstractTimePrimitive",
       xmlNamespacePrefix = "GML"
     ),
     public = list(
       initialize = function(xml = NULL, defaults = list()){
         super$initialize(xml, defaults)
       },
       
       #addRelatedTime
       addRelatedTime = function(time){
         allowedClasses <- c("GMLTimeInstant", "GMLTimePeriod",
                             "GMLTimeNode", "GMLTimeEdge")
         if(!(class(time)[1] %in% allowedClasses)){
           stop(sprintf("The argument value should an object of class among the following classes %s",
                        paste(allowedClasses, collapse=", ")))
         }
         return(self$addListElement("relatedTime", time))
       },
       
       #delRelatedTime
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