#' ISOSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Series
#' @return Object of \code{\link{R6Class}} for modelling an ISOSeries
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOSeries}}
#'  }
#' }
#' 
#' @section Inherited methods from \code{\link{ISOAbstractAggregate}}:
#' \describe{
#'  \item{\code{addComposedOf(composedOf)}}{
#'    Adds a dataset, object of class \code{\link{ISODataSet}}
#'  }
#'  \item{\code{delComposedOf(composedOf)}}{
#'    Deletes a dataset, object of class \code{\link{ISODataSet}}
#'  }
#'  \item{\code{addSeriesMetadata(metadata)}}{
#'    Adds a series metadata, object of class \code{\link{ISOMetadata}}
#'  }
#'  \item{\code{delSeriesMetadata(metadata)}}{
#'    Deletes a series metadata, object of class \code{\link{ISOMetadata}}
#'  }
#'  \item{\code{addSubset(subset)}}{
#'    Adds a subset, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#'  \item{\code{delSubset(subset)}}{
#'    Deletes a subset, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#'  \item{\code{addSuperset(superset)}}{
#'    Adds a superset, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#'  \item{\code{delSuperset(superset)}}{
#'    Deletes a superset, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#' }
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSeries <- R6Class("ISOSeries",
 inherit = ISOAbstractAggregate,
 private = list(
   xmlElement = "DS_Series",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   }
 )                        
)