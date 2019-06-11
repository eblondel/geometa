#' ISOImageryObjectiveType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery ObjectiveType
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery ObjectiveType
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryObjectiveType}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryObjectiveType$values(labels = TRUE)
#'   
#'   #some def
#'   survey <- ISOImageryObjectiveType$new(value = "survey")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryObjectiveType <- R6Class("ISOImageryObjectiveType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_ObjectiveTypeCode",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                       addCodeSpaceAttr = FALSE)
    }
  )                        
)

ISOImageryObjectiveType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryObjectiveType, labels))
}