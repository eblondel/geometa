#' ISOFileName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file name
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO FileName
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOFileName$new(file = "someuri", name = "filename")
#'   xml <- md$encode()
#' 
#' @references
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmx/1.0/gmx/#element_FileName}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gcx/1.0/gcx/#element_FileName}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFileName <- R6Class("ISOFileName",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "FileName",
    xmlNamespacePrefix = list(
      "19139" = "GMX",
      "19115-3" = "GCX"
    )
  ),
  public = list(
    #'@field attrs attrs
    attrs = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param file file
    #'@param name name
    initialize = function(xml = NULL, file = NULL, name = NULL){
      super$initialize(xml = xml)
      if(!is.null(file) & !is.null(name)){
        self$attrs$src <- file
        self$value <- name
      }
    }
  )                        
)
