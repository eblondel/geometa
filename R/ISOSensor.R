#' ISOSensor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Sensor
#' @return Object of \code{\link{R6Class}} for modelling an ISOSensor
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSensor
#'  }
#' }
#' 
#' @section Inherited methods from \code{ISOAbstractAggregate}:
#' \describe{
#'  \item{\code{addComposedOf(composedOf)}}{
#'    Adds a dataset, object of class \code{ISODataSet}
#'  }
#'  \item{\code{delComposedOf(composedOf)}}{
#'    Deletes a dataset, object of class \code{ISODataSet}
#'  }
#'  \item{\code{addSensorMetadata(metadata)}}{
#'    Adds a Sensor metadata, object of class \code{ISOMetadata}
#'  }
#'  \item{\code{delSensorMetadata(metadata)}}{
#'    Deletes a Sensor metadata, object of class \code{ISOMetadata}
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSensor <- R6Class("ISOSensor",
 inherit = ISOSeries,
 private = list(
   xmlElement = "DS_Sensor",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   }
 )                        
)