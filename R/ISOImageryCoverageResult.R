#' ISOImageryCoverageResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery coverage result
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery coverage result
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryCoverageResult <- R6Class("ISOImageryCoverageResult",
  inherit = ISOAbstractResult,
  private = list(
    xmlElement = "QE_CoverageResult",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #'@field spatialRepresentationType spatialRepresentationType [1..1] : ISOSpatialRepresentationType
    spatialRepresentationType = NULL,
    #'@field resultFile resultFile [1..1]: ISODataFile
    resultFile = NULL,
    #'@field resultSpatialRepresentation resultSpatialRepresentation  [1..1]: ISOSpatialRepresentation
    resultSpatialRepresentation = NULL,
    #'@field resultContentDescription resultContentDescription [1..1]: ISOCoverageDescription
    resultContentDescription = NULL,
    #'@field resultFormat resultFormat [1..1]: ISOFormat
    resultFormat = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set spatial representation type
    #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or \link{character}
    #' among values returned by \code{ISOSpatialRepresentationType$values()}
    setSpatialRepresentationType = function(spatialRepresentationType){
      if(is(spatialRepresentationType, "character")){
        spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
      }else{
        if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
          stop("The argument should be an object of class 'character' or 'ISOSpatialRepresentationType'")
        }
      }
      self$spatialRepresentationType <- spatialRepresentationType
    },
    
    #'@description Set result file
    #'@param resultFile object of class \link{ISODataFile}
    setResultFile = function(resultFile){
      if(!is(resultFile, "ISODataFile")){
        stop("The argument should be an object of class 'ISODataFile'")
      }
      self$resultFile <- resultFile
    },
    
    #'@description Set result spatial representation
    #'@param spatialRepresentation object of class \link{ISOSpatialRepresentation}
    setResultSpatialRepresentation = function(spatialRepresentation){
      if(!is(spatialRepresentation, "ISOSpatialRepresentation")){
        stop("The argument should be an object of class 'ISOSpatialRepresentation'")
      }
      self$resultSpatialRepresentation <- spatialRepresentation
    },
    
    #'@description Set result coverage description 
    #'@param coverageDescription object of class \link{ISOCoverageDescription}
    setResultCoverageDescription = function(coverageDescription){
      if(!is(coverageDescription, "ISOCoverageDescription")){
        stop("The argument should be an object of class 'ISOCoverageDescription")
      }
      self$coverageDescription <- coverageDescription
    },
    
    #'@description Set format
    #'@param format object of class \link{ISOFormat}
    setResultFormat = function(format){
      if(!is(format, "ISOFormat")){
        stop("The argument should be an object of class 'ISOFormat'")
      }
      self$resultFormat <- format
    }
  )                        
)