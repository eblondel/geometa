#' ISODCPList
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO DCP
#' @return Object of \code{\link{R6Class}} for modelling an ISO DCPList
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISODCPList$values(labels = TRUE)
#'   
#'   #example
#'   javaDCP <- ISODCPList$new(value = "JAVA")
#'   
#' @references 
#'   ISO 19119:2005 - Geographic information -- Service
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODCPList <- R6Class("ISODCPList",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "DCPList",
     xmlNamespacePrefix = "SRV"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, 
                        description = description)
     }
   )                        
)

ISODCPList$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISODCPList, labels))
}