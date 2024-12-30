#' ISOImagerySource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery source
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery source
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_LE_Source}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LE_Source}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImagerySource <- R6Class("ISOImagerySource",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LE_Source",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" =  "MRL"
    )
  ),
  public = list(
    
    #'@field processedLevel processedLevel [0..1]: ISOMetaIdentifier
    processedLevel = NULL,
    #'@field resolution resolution [0..1]: ISOImageryNominalResolution
    resolution = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set processed level
    #'@param processedLevel object of class \link{ISOMetaIdentifier} or \link{character}
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
    
    #'@description Set resolution
    #'@param resolution object of class \link{ISOImageryNominalResolution}
    setResolution = function(resolution){
      if(!is(resolution, "ISOImageryNominalResolution")){
        stop("The argument should be an object of class 'ISOImageryNominalResolution")
      }
      self$resolution <- resolution
    }
    
  )                        
)
