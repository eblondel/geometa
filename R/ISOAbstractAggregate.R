#' ISOAbstractAggregate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract aggregate
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractAggregate
#' @format \code{\link{R6Class}} object.
#' 
#' @note abstract class
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractAggregate <- R6Class("ISOAbstractAggregate",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "AbstractDS_Aggregate",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   #'@field composedOf composedOf [1..*]
   composedOf = list(),
   #'@field seriesMetadata seriesMetadata [1..*]
   seriesMetadata = list(),
   #'@field subset subset [0..*]
   subset = list(),
   #'@field superset superset [0..*]
   superset = list(),
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #'@description Adds a dataset 'composedOf' relationship
   #'@param composedOf object of class \link{ISODataSet}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addComposedOf = function(composedOf){
     if(is(composedOf, "ISODataSet")){
       stop("The argument should be an object of class 'ISODataSet")
     }
     return(self$addListElement("composedOf", composedOf))
   },
   
   #'@description Deletes a dataset 'composedOf' relationship
   #'@param composedOf object of class \link{ISODataSet}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delComposedOf = function(composedOf){
     if(is(composedOf, "ISODataSet")){
       stop("The argument should be an object of class 'ISODataSet")
     }
     return(self$delListElement("composedOf", composedOf))
   },
   
   #'@description Adds a series metadata
   #'@param metadata object of class \link{ISOMetadata}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addSeriesMetadata = function(metadata){
     if(!is(metadata, "ISOMetadata")){
       stop("The argument should be an object of class 'ISOMetadata'")
     }
     return(self$addListElement("seriesMetadata", metadata))
   },
   
   #'@description Deletes a series metadata
   #'@param metadata object of class \link{ISOMetadata}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   delSeriesMetadata = function(metadata){
     if(!is(metadata, "ISOMetadata")){
       stop("The argument should be an object of class 'ISOMetadata'")
     }
     return(self$delListElement("seriesMetadata", metadata))
   },
   
   #'@description Adds subset
   #'@param subset object of class inheriting \link{ISOAbstractAggregate}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addSubset = function(subset){
     if(!inherits(subset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$addListElement("subset", subset))
   },
   
   #'@description Deletes subset
   #'@param subset object of class inheriting \link{ISOAbstractAggregate}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delSubset = function(subset){
     if(!inherits(subset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$delListElement("subset", subset))
   },
   
   #'@description Adds superset
   #'@param superset object of class inheriting \link{ISOAbstractAggregate}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addSuperset = function(superset){
     if(!inherits(superset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$addListElement("superset", superset))
   },
   
   #'@description Deletes superset
   #'@param superset object of class inheriting \link{ISOAbstractAggregate}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delSuperset = function(superset){
     if(!inherits(superset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$delListElement("superset", superset))
   }
 )                        
)