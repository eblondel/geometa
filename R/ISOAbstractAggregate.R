#' ISOAbstractAggregate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract aggregate
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractAggregate
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAbstractAggregate
#'  }
#'  \item{\code{addComposedOf(composedOf)}}{
#'    Adds a dataset, object of class \code{ISODataSet}
#'  }
#'  \item{\code{delComposedOf(composedOf)}}{
#'    Deletes a dataset, object of class \code{ISODataSet}
#'  }
#'  \item{\code{addSeriesMetadata(metadata)}}{
#'    Adds a series metadata, object of class \code{ISOMetadata}
#'  }
#'  \item{\code{delSeriesMetadata(metadata)}}{
#'    Deletes a series metadata, object of class \code{ISOMetadata}
#'  }
#'  \item{\code{addSubset(subset)}}{
#'    Adds a subset, object that inherits from \code{ISOAbstractAggregate}
#'  }
#'  \item{\code{delSubset(subset)}}{
#'    Deletes a subset, object that inherits from \code{ISOAbstractAggregate}
#'  }
#'  \item{\code{addSuperset(superset)}}{
#'    Adds a superset, object that inherits from \code{ISOAbstractAggregate}
#'  }
#'  \item{\code{delSuperset(superset)}}{
#'    Deletes a superset, object that inherits from \code{ISOAbstractAggregate}
#'  }
#' }
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
   #composedOf [1..*]
   composedOf = list(),
   #seriesMetadata [1..*]
   seriesMetadata = list(),
   #subset [0..*]
   subset = list(),
   #superset [0..*]
   superset = list(),
   
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #addComposedOf
   addComposedOf = function(composedOf){
     if(is(composedOf, "ISODataSet")){
       stop("The argument should be an object of class 'ISODataSet")
     }
     return(self$addListElement("composedOf", composedOf))
   },
   
   #delComposedOf
   delComposedOf = function(composedOf){
     if(is(composedOf, "ISODataSet")){
       stop("The argument should be an object of class 'ISODataSet")
     }
     return(self$delListElement("composedOf", composedOf))
   },
   
   #addSeriesMetadata
   addSeriesMetadata = function(metadata){
     if(!is(metadata, "ISOMetadata")){
       stop("The argument should be an object of class 'ISOMetadata'")
     }
     return(self$addListElement("seriesMetadata", metadata))
   },
   
   #delSeriesMetadata
   delSeriesMetadata = function(metadata){
     if(!is(metadata, "ISOMetadata")){
       stop("The argument should be an object of class 'ISOMetadata'")
     }
     return(self$delListElement("seriesMetadata", metadata))
   },
   
   #addSubset
   addSubset = function(subset){
     if(!inherits(subset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$addListElement("subset", subset))
   },
   
   #delSubset
   delSubset = function(subset){
     if(!inherits(subset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$delListElement("subset", subset))
   },
   
   #addSuperset
   addSuperset = function(superset){
     if(!inherits(superset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$addListElement("superset", superset))
   },
   
   #delSuperset
   delSuperset = function(superset){
     if(!inherits(superset,"ISOAbstractAggregate")){
       stop("The argument should be an object inheriting 'ISOAbstractAggregate")
     }
     return(self$delListElement("superset", superset))
   }
 )                        
)