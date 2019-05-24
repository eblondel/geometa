#' ISODataSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO dataset
#' @return Object of \code{\link{R6Class}} for modelling an ISODataSet
#' @format \code{\link{R6Class}} object.
#' 
#' @field has [\code{\link{ISOMetadata}}] associated metadata
#' @field partOf [\code{\link{ISOAbstractAggregate}}] part of other aggregate(s)
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISODataSet}}
#'  }
#'  \item{\code{addHas(metadata)}}{
#'    Adds a metadata, object of class \\code{\link{ISOMetadata}}
#'  }
#'  \item{\code{delHas(metadata)}}{
#'    Deletes a metadata, object of class \code{\link{ISOMetadata}}
#'  }
#'  \item{\code{addPartOf(partOf)}}{
#'    Adds a partOf element, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#'  \item{\code{delPartOf(partOf)}}{
#'    Deletes a partOf element, object that inherits from \code{\link{ISOAbstractAggregate}}
#'  }
#' }
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataSet <- R6Class("ISODataSet",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DS_DataSet",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #has [1..*]
    has = list(),
    #partOf [0..*]
    partOf = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addHasMetadata
    addHasMetadata = function(metadata){
      if(!is(metadata, "ISOMetadata")){
        stop("The argument should be an object of class 'ISOMetadata'")
      }
      return(self$addListElement("has", metadata))
    },
    
    #delHasMetadata
    delHasMetadata = function(metadata){
      if(!is(metadata, "ISOMetadata")){
        stop("The argument should be an object of class 'ISOMetadata'")
      }
      return(self$delListElement("has", metadata))
    },
    
    
    #addPartOf
    addPartOf = function(partOf){
      if(!inherits(partOf,"ISOAbstractAggregate")){
        stop("The argument should be an object inheriting 'ISOAbstractAggregate")
      }
      return(self$addListElement("partOf", partOf))
    },
    
    #delPartOf
    delPartOf = function(partOf){
      if(!inherits(partOf,"ISOAbstractAggregate")){
        stop("The argument should be an object inheriting 'ISOAbstractAggregate")
      }
      return(self$delListElement("partOf", partOf))
    }
  )                        
)