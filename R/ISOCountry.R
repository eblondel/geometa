#' ISOCountry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO country
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Country
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOCountry$values(labels = TRUE)
#'   
#'   #some charset
#'   charset <- ISOCountry$new(value = "utf8")
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_Country}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_CountryCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCountry <- R6Class("ISOCountry",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = list(
       "19139" = "Country",
       "19115-3" = "CountryCode"
     ),
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "LAN"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOCountry$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOCountry, labels))
}
