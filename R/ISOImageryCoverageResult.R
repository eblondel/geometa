#' ISOImageryCoverageResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery coverage result
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery coverage result
#' @format \code{\link{R6Class}} object.
#'
#' @field spatialRepresentationType [\code{\link{ISOSpatialRepresentationType}}]
#' @field resultFile [\code{\link{ISODataFile}}]
#' @field resultSpatialRepresentation [\code{\link{ISOSpatialRepresentation}}]
#' @field resultContentDescription [\code{\link{ISOCoverageDescription}}]
#' @field resultFormat [\code{\link{ISOFormat}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryCoverageResult}}
#'  }
#'  \item{\code{setSpatialRepresentationType(spatialRepresentationType)}}{
#'    Set the spatial representation type, object of class \code{\link{ISOSpatialRepresentationType}},
#'    or 'character' among values given by \code{ISOSpatialRepresentationType$values()} or any free text.
#'  }
#'  \item{\code{setResultFile(resultFile)}}{
#'    Set the result data file, object of class \code{\link{ISODataFile}}.
#'  }
#'  \item{\code{setResultSpatialRepresentation(spatialRepresentation)}}{
#'    Set the spatial representation, object of class \code{\link{ISOSpatialRepresentation}}.
#'  }
#'  \item{\code{setResultCoverageDescription(coverageDescription)}}{
#'    Set the coverage description, object of class \code{\link{ISOCoverageDescription}}
#'  }
#'  \item{\code{setResultFormat(format)}}{
#'    Set the result format, object of class \code{\link{ISOFormat}}
#'  }
#' }
#' 
#' @section Methods inherited from \code{\link{ISOAbstractResult}}:
#' See methods description at \code{\link{ISOAbstractResult}}
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
    
    #+ spatialRepresentationType [1..1] : ISOSpatialRepresentationType
    spatialRepresentationType = NULL,
    #+ resultFile [1..1]: ISODataFile
    resultFile = NULL,
    #+ resultSpatialRepresentation  [1..1]: ISOSpatialRepresentation
    resultSpatialRepresentation = NULL,
    #+ resultContentDescription [1..1]: ISOCoverageDescription
    resultContentDescription = NULL,
    #+ resultFormat [1..1]: ISOFormat
    resultFormat = NULL,
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setSpatialRepresentationType
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
    
    #setResultFile
    setResultFile = function(resultFile){
      if(!is(resultFile, "ISODataFile")){
        stop("The argument should be an object of class 'ISODataFile'")
      }
      self$resultFile <- resultFile
    },
    
    #setResultSpatialRepresentation
    setResultSpatialRepresentation = function(spatialRepresentation){
      if(!is(spatialRepresentation, "ISOSpatialRepresentation")){
        stop("The argument should be an object of class 'ISOSpatialRepresentation'")
      }
      self$resultSpatialRepresentation <- spatialRepresentation
    },
    
    #setResultCoverageDescription
    setResultCoverageDescription = function(coverageDescription){
      if(!is(coverageDescription, "ISOCoverageDescription")){
        stop("The argument should be an object of class 'ISOCoverageDescription")
      }
      self$coverageDescription <- coverageDescription
    },
    
    #setResultFormat
    setResultFormat = function(format){
      if(!is(format, "ISOFormat")){
        stop("The argument should be an object of class 'ISOFormat'")
      }
      self$resultFormat <- format
    }
  )                        
)