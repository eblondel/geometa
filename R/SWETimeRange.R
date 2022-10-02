#' SWETimeRange
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE Time Range
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWETimeRange <- R6Class("SWETimeRange",
  inherit = SWEAbstractSimpleComponent,
  private = list(
    xmlElement = "TimeRange",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@field uom uom
    uom = NULL,
    
    #'@field constraint constraint
    constraint = NULL,
    
    #'@field value  value
    value = matrix(NA_real_, 1, 2),
    
    #'@description Initializes an object of class \link{SWETimeRange}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param uom uom
    #'@param constraint constraint
    #'@param start start time
    #'@param end end time
    #'@param updatable updatable
    #'@param optional optional
    #'@param definition definition
    initialize = function(xml = NULL, 
                          uom = NULL, constraint = NULL, start = NULL, end = NULL,
                          updatable = NULL, optional = FALSE, definition = NULL){
      super$initialize(xml, element = private$xmlElement,
                       updatable = updatable, optional = optional, definition = definition)
      if(is.null(xml)){
        self$setUom(uom)
        self$setConstraint(constraint)
        self$setValue(start = start, end = end)
      }
    },
    
    #'@description setUom
    #'@param uom uom
    setUom = function(uom){
      uomElem <- SWEElement$create(element = "uom")
      uomElem$setAttr("code", uom)
      self$uom <- uomElem
    },
    
    #'@description setConstraint
    #'@param constraint constraint
    setConstraint = function(constraint){
      self$constraint <- constraint
    },
    
    #'@description setValue
    #'@param start start time
    #'@param end end time
    setValue = function(start, end){
      self$value <- matrix(list(start, end), nrow = 1, ncol = 2, byrow = TRUE)
    }
  )                        
)