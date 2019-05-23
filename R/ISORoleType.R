#' ISORoleType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO roleType
#' @return Object of \code{\link{R6Class}} for modelling an ISO RoleType
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISORoleType}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISORoleType$values(labels = TRUE)
#'   
#'   #some charset
#'   ordinaryType <- ISORoleType$new(value = "ordinary")
#' 
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORoleType <- R6Class("ISORoleType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "FC_RoleType",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISORoleType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISORoleType, labels))
}