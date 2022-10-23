#' GMLConversion
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML conversion
#' @return Object of \code{\link{R6Class}} for modelling an GMLConversion
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLConversion <- R6Class("GMLConversion",
    inherit = GMLAbstractGeneralConversion,
    private = list(
      xmlElement = "Conversion",
      xmlNamespacePrefix = "GML"
    ),
    public = list(
      
      #'@field method method [1..1]: GMLOperationMethod
      method = NULL,
      #'@field parameterValue parameterValue [0..*]: GMLParameterValue
      parameterValue = list(),
      
      #'@description Set method
      #'@param method method, object of class \link{GMLOperationMethod}
      setMethod = function(method){
        if(!is(method, "GMLOperationMethod")){
          stop("The argument value should be an object of class 'GMLOperationMethod'")
        }
        self$method <- GMLElement$create("method", method)
      },
      
      #'@description Adds parameter value
      #'@param paramValue parameter value, object class inheriting \link{GMLAbstractGeneralParameterValue}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addParameterValue = function(paramValue){
        if(!inherits(paramValue, "GMLAbstractGeneralParameterValue")){
          stop("The argument value should be an object of class 'GMLParameterValue' or 'GMLParameterValueGroup'")
        }
        return(self$addListElement("parameterValue", paramValue))
      },
      
      #'@description Deletes parameter value
      #'@param paramValue parameter value, object class inheriting \link{GMLAbstractGeneralParameterValue}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delParameterValue = function(paramValue){
        if(!inherits(paramValue, "GMLAbstractGeneralParameterValue")){
          stop("The argument value should be an object of class 'GMLParameterValue' or 'GMLParameterValueGroup'")
        }
        return(self$delListElement("parameterValue", paramValue))
      }
      
    )
)