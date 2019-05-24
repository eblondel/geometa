#' ISOCountry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO country
#' @return Object of \code{\link{R6Class}} for modelling an ISO Country
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOCountry}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOCountry$values(labels = TRUE)
#'   
#'   #some charset
#'   charset <- ISOCountry$new(value = "utf8")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCountry <- R6Class("ISOCountry",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "Country",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOCountry$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCountry, labels))
}