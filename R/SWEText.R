#' SWEText
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE Text
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEText <- R6Class("SWEText",
  inherit = SWEAbstractSimpleComponent,
  private = list(
    xmlElement = "Text",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@field constraint constraint
    constraint = NULL,
    
    #'@field value  value
    value = NULL,
    
    #'@description Initializes an object of class \link{SWEText}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param constraint constraint
    #'@param value value
    #'@param updatable updatable
    #'@param optional optional
    #'@param definition definition
    initialize = function(xml = NULL, 
                          constraint = NULL, value = NULL,
                          updatable = NULL, optional = FALSE, definition = NULL){
      super$initialize(xml, element = private$xmlElement,
                       updatable = updatable, optional = optional, definition = definition)
      if(is.null(xml)){
        self$setConstraint(constraint)
        self$setValue(value)
      }
    },
    
    #'@description setConstraint
    #'@param constraint constraint
    setConstraint = function(constraint){
      self$constraint <- SWEElement$create(element = "constraint", value = constraint)
    },
    
    #'@description setValue
    #'@param value value
    setValue = function(value){
      self$value <- SWEElement$create(element = "value", value = value)
    }
  )                        
)