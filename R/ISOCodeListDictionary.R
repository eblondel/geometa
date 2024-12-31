#' ISOCodeListDictionary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code element
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Metadata codelist dictionary
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodeListDictionary <- R6Class("ISOCodeListDictionary",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "CodeListDictionary",
      xmlNamespacePrefix = list(
        "19139" = "GMX"
      )
    ),
    public = list(
      #'@field identifier identifier
      identifier = NA,
      #'@field description description
      description = NA,
      #'@field codeEntry code entries
      codeEntry = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Converts to \link{ISOCodelist}
      #'@return an object of class \link{ISOCodelist}
      toISOCodelist = function(){
        cl = ISOCodelist$new()
        identifier = ISOScopedName$new(value = self$identifier$value)
        identifier$setCodeSpace(self$identifier$attrs$codeSpace)
        cl$identifier = identifier
        cl$description = self$description$value
        cl$codeEntry = lapply(self$codeEntry, function(codeEntry){
          codeEntry$toISOCTCodelistValue()
        })
        return(cl)
      }
    )                       
)
