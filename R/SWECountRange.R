#' SWECountRange
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE CountRange
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWECountRange <- R6Class("SWECountRange",
  inherit = SWEAbstractSimpleComponent,
  private = list(
    xmlElement = "CountRange",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@field constraint constraint
    constraint = NULL,
    
    #'@field value  value
    value = matrix(NA_integer_, 1, 2),
    
    #'@description Initializes an object of class \link{SWECountRange}
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
      if(!is.numeric(value)){
        stop("Values should be integer")
      }
      if(is.vector(value)){
        if(length(value)!="2"){
          stop("Vector of values should of length 2")
        }
      }else if(is.matrix(value)){
        if(!all(dim(value)==c(1,2))){
          stop("Matrix of values should be of dimensions 1,2")
        }
      }else{
        stop("Value should be either a vector or matrix")
      }
      self$value <- SWEElement$create(element = "value", value = value)
    }
  )                        
)
