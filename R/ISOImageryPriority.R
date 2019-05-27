#' ISOImageryPriority
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery priority
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery priority
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPriority}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryPriority$values(labels = TRUE)
#'   
#'   #some def
#'   highImp <- ISOImageryPriority$new(value = "highImportance")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryPriority <- R6Class("ISOImageryPriority",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MI_PriorityCode",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOImageryPriority$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryPriority, labels))
}