#' ISOImagerySource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery source
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery source
#' @format \code{\link{R6Class}} object.
#'
#' @field processedLevel [\code{\link{ISOMetaIdentifier}}]
#' @field resolution [\code{\link{ISOImageryNominalResolution}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImagerySource}}
#'  }
#'  \item{\code{setProcessedLevel(processedLevel)}}{
#'    Sets processed level, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setResolution(resolution)}}{
#'    Set the resolution, object of class \code{ISOImageryNominalResolution}
#'  }
#' }  
#' 
#' @examples
#'    md <- ISOImagerySource$new()
#'    md$setProcessedLevel("identifier")
#'    res <- ISOImageryNominalResolution$new()
#'    d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
#'    res$setScanningResolution(d)
#'    md$setResolution(res)
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImagerySource <- R6Class("ISOImagerySource",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LE_Source",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ processedLevel [0..1]: ISOMetaIdentifier
    processedLevel = NULL,
    #+ resolution [0..1]: ISOImageryNominalResolution
    resolution = NULL,
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setProcessedLevel
    setProcessedLevel = function(processedLevel){
      if(is(processedLevel, "character")){
        processedLevel <- ISOMetaIdentifier$new(code = processedLevel)
      }else{
        if(!is(processedLevel, "ISOMetaIdentifier")){
          stop("The argument should be an object of class 'character' or 'ISOMetaIdentifier'")
        }
      }
      self$processedLevel <- processedLevel
    },
    
    #setResolution
    setResolution = function(resolution){
      if(!is(resolution, "ISOImageryNominalResolution")){
        stop("The argument should be an object of class 'ISOImageryNominalResolution")
      }
      self$resolution <- resolution
    }
    
  )                        
)