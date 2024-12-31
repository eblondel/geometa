#' ISOExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Extent
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_EX_Extent}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gex/1.0/gex/#element_EX_Extent}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOExtent <- R6Class("ISOExtent",
   inherit = ISOAbstractObject,
   private = list(
      xmlElement = "EX_Extent",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "GEX"
      )
   ),
   public = list(
     #'@field geographicElement geographicElement [0..*]: ISOGeographicExtent
     geographicElement = list(),
     #'@field temporalElement temporalElement [0..*]: ISOTemporalExtent
     temporalElement = list(),
     #'@field verticalElement verticalElement [0..*]: ISOVerticalElement
     verticalElement = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds geographic element
     #'@param element object of class \link{ISOGeographicExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addGeographicElement = function(element){
       if(!is(element, "ISOGeographicExtent")){
         stop("The argument should extend 'ISOGeographicExtent' object")
       }
       return(self$addListElement("geographicElement", element))
     },
     
     #'@description Sets geographic element
     #'@param element object of class \link{ISOGeographicExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setGeographicElement = function(element){
        warning("Method 'setGeographicElement' is deprecated, please use 'addGeographicElement'!")
        return(self$addGeographicElement(element))
     },
     
     #'@description Deletes geographic element
     #'@param element object of class \link{ISOGeographicExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delGeographicElement = function(element){
       if(!is(element, "ISOGeographicExtent")){
         stop("The argument should extend 'ISOGeographicExtent' object")
       }
       return(self$delListElement("geographicElement", element))
     },
     
     #'@description Adds temporal element
     #'@param element object of class \link{ISOTemporalExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTemporalElement = function(element){
       if(!is(element, "ISOTemporalExtent")){
         stop("The argument should extend 'ISOTemporalExtent' object")
       }
       return(self$addListElement("temporalElement", element))
     },
     
     #'@description Deletes temporal element
     #'@param element object of class \link{ISOTemporalExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTemporalElement = function(element){
       if(!is(element, "ISOTemporalExtent")){
         stop("The argument should extend 'ISOTemporalExtent' object")
       }
       return(self$delListElement("temporalElement", element))
     },
     
     #'@description Adds vertical element
     #'@param element object of class \link{ISOVerticalExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addVerticalElement = function(element){
       if(!is(element, "ISOVerticalExtent")){
         stop("The argument should extend 'ISOVerticalExtent' object")
       }
       return(self$addListElement("verticalElement", element))
     },
     
     #'@description Deletes vertical element
     #'@param element object of class \link{ISOVerticalExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delVerticalElement = function(element){
       if(!is(element, "ISOVerticalExtent")){
         stop("The argument should extend 'ISOVerticalExtent' object")
       }
       return(self$delListElement("verticalElement", element))
     }
   )                        
)
