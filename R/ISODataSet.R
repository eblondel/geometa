#' ISODataSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO dataset
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODataSet
#' @format \code{\link[R6]{R6Class}} object.
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
    #'@field has has [1..*]
    has = list(),
    #'@field partOf partOf [0..*]
    partOf = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds metadata
    #'@param metadata metadata, object of class \link{ISOMetadata}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addHasMetadata = function(metadata){
      if(!is(metadata, "ISOMetadata")){
        stop("The argument should be an object of class 'ISOMetadata'")
      }
      return(self$addListElement("has", metadata))
    },
    
    #'@description Deletes metadata
    #'@param metadata metadata, object of class \link{ISOMetadata}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delHasMetadata = function(metadata){
      if(!is(metadata, "ISOMetadata")){
        stop("The argument should be an object of class 'ISOMetadata'")
      }
      return(self$delListElement("has", metadata))
    },
    
    #'@description Adds aggregate dataset is part of
    #'@param partOf object inheriting class \link{ISOAbstractAggregate}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addPartOf = function(partOf){
      if(!inherits(partOf,"ISOAbstractAggregate")){
        stop("The argument should be an object inheriting 'ISOAbstractAggregate")
      }
      return(self$addListElement("partOf", partOf))
    },
    
    #'@description Deletes aggregate dataset is part of
    #'@param partOf object inheriting class \link{ISOAbstractAggregate}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delPartOf = function(partOf){
      if(!inherits(partOf,"ISOAbstractAggregate")){
        stop("The argument should be an object inheriting 'ISOAbstractAggregate")
      }
      return(self$delListElement("partOf", partOf))
    }
  )                        
)
