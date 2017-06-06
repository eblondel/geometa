#' ISOUnitOfMeasure
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract UnitOfMeasure
#' @format \code{\link{R6Class}} object.
#'
#' @field uomName
#' @field uomSymbol
#' @field measureType
#' @field nameStandardUnit
#' @field scaleToStandardUnit
#' @field offsetToStandardUnit
#' @field formula
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOUnitOfMeasure
#'  }
#' }
#' 
#' @note Abstract ISO class used internally by geometa
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUnitOfMeasure <- R6Class("ISOUnitOfMeasure",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "UnitOfMeasure",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    uomName = NULL,
    uomSymbol = NULL,
    measureType = NULL,
    nameStandardUnit = NULL,
    scaleToStandardUnit = NULL,
    offsetToStandardUnit = NULL,
    formula = NULL,
    initialize = function(xml = NULL, measureType){
      
      self$measureType <- measureType
      
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      
    },
    
    #setUomName
    setUomName = function(uomName){
      self$uomName <- uomName
    },
    
    #setUomSymbol
    setUomSymbol = function(uomSymbol){
      self$uomSymbol <- uomSymbol
    },
    
    #setNameStandardUnit
    setNameStandardUnit = function(nameStandardUnit){
      self$nameStandardUnit <- nameStandardUnit
    },
    
    #setScaleToStandardUnit
    setScaleToStandardUnit = function(scaleToStandardUnit){
      self$scaleToStandardUnit <- as.numeric(scaleToStandardUnit)
    },
    
    #setOffsetToStandardUnit
    setOffsetStandardUnit = function(offsetToStandardUnit){
      self$offsetToStandardUnit <- as.numeric(offsetToStandardUnit)
    }
  )                        
  
)