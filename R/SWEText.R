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
#'   OGC Geography Markup Language. https://www.ogc.org/standards/swecommon
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
    initialize = function(xml = NULL, constraint = NULL, value = NULL){
      super$initialize(xml, element = private$xmlElement)
      if(is.null(xml)){
        self$constraint <- constraint
        self$value <- value
      }
    },
    
    #'@description setConstraint
    #'@param constraint
    setConstraint = function(constraint){
      self$constraint <- constraint
    },
    
    #'@description setValue
    #'@param value
    setValue = function(value){
      self$value <- value
    }
  )                        
)