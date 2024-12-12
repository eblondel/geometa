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
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_QE_CoverageResult}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_QE_CoverageResult} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryCoverageResult <- R6Class("ISOImageryCoverageResult",
  inherit = ISOAbstractResult,
  private = list(
    xmlElement = "QE_CoverageResult",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@field resultScope resultScope [0..1]: ISOScope
    resultScope = NULL,
    #'@field dateTime dateTime [0..1]: POSIX/date
    dateTime = NULL,
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
    
    #'@description Set result scope
    #'@param scope object of class \link{ISOScope}
    setResultScope = function(scope){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(scope, "ISOScope")){
        stop("The argument should be a 'ISOScope' object")
      }
      self$resultScope = scope
    },
    
    #'@description Set date time
    #'@param dateTime date time, object of class \link{POSIXct}
    setDateTime = function(dateTime){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!all(class(dateTime) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be an 'POSIXct'/'POSIXt' object")
      }
      self$dateTime <- dateTime
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
    #'@param resultFile object of class \link{ISODataFile} (in ISO 19139) 
    #'or \link{ISOQualityResultFile} (in ISO 19115-3)
    setResultFile = function(resultFile){
      switch(getMetadataStandard(),
        "19139" = {
          if(!is(resultFile, "ISODataFile")){
            stop("The argument should be an object of class 'ISODataFile'")
          }
        },
        "19115-3" = {
          if(!is(resultFile, "ISOQualityResultFile")){
            stop("The argument should be an object of class 'ISOQualityResultFile'")
          } 
        }
      )
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