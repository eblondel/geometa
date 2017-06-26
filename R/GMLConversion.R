#' GMLConversion
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML conversion
#' @return Object of \code{\link{R6Class}} for modelling an GMLConversion
#' @format \code{\link{R6Class}} object.
#'
#' @field method
#' @field parameterValue
#'
#' @section Inherited methods from \code{GMLAbstractCoordinateOperation}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Conversion
#'  }
#'  \item{\code{setMethod(method)}}{
#'    Sets the method
#'  }
#'  \item{\code{addParameterValue(paramValue)}}{
#'    Adds a parameter value
#'  }
#'  \item{\code{delParameterValue(paramValue)}}{
#'    Deletes a parameter value
#'  }
#' }
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
      
      #+ method [1..1]: #TODO
      method = NULL,
      #+ parameterValue [0..*]: #TODO
      parameterValue = list(),
      
      #setMethod
      setMethod = function(method){
        self$method <- GMLElement$create("method", method)
      },
      
      #addParameterValue
      addParameterValue = function(paramValue){
        return(self$addListElement("parameterValue", paramValue))
      },
      
      #delParameterValue
      delParameterValue = function(paramValue){
        return(self$delListElement("parameterValue", paramValue))
      }
      
    )
)