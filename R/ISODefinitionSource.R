#' ISODefinitionSource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO definition source
#' @return Object of \code{\link{R6Class}} for modelling an ISODefinitionSource
#' @format \code{\link{R6Class}} object.
#'
#' @field source [\code{\link{ISOCitation}}] source citation
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, source)}}{
#'    This method is used to instantiate an \code{\link{ISODefinitionSource}}
#'  }
#'  \item{\code{setSource(source)}}{
#'    Sets the source as object of class \code{\link{ISOCitation}}
#'  }
#' }
#' 
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODefinitionSource <- R6Class("ISODefinitionSource",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "FC_DefinitionSource",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      #+ source [0..1]: ISOCitation
      source = NULL,
      initialize = function(xml = NULL, source = NULL){
        super$initialize(xml = xml)
        if(!is.null(source)){
          self$setSource(source)
        }
      },
      
      #setSource
      setSource = function(source){
        if(!is(source, "ISOCitation")){
          stop("The argument should be an object of class 'ISOCitation'")
        }
        self$source <- source
      }
    )                        
)