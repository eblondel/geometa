#' ISOProgress
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO status
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO progress status
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOProgress$values(labels = TRUE)
#'   
#'   #pending status
#'   pending <- ISOProgress$new(value = "pending")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#'   ISO/TS 19115-3:2016 - Geographic information — Metadata — Part 3: XML schema implementation for fundamental concepts
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOProgress<- R6Class("ISOProgress",
  inherit = ISOCodeListItem,
  private = list(
     xmlElement = "MD_ProgressCode",
     xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MCC"
     )
  ),
  public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}  
     #'@param value value
     #'@param description description
     initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                         setValue = FALSE, addCodeSpaceAttr = FALSE)
     }
  )                        
)

ISOProgress$values <- function(labels = FALSE){
   return(ISOCodeListItem$values(ISOProgress, labels))
}

#' ISOStatus
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO status
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO progress status
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOStatus$values(labels = TRUE)
#'   
#'   #pending status
#'   pending <- ISOStatus$new(value = "pending")
#'   
#' @note deprecated - use \link{ISOProgress} instead  
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ProgressCode}
#' 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_MD_ProgressCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStatus<- R6Class("ISOStatus",
   inherit = ISOProgress,
   private = list(
     deprecated = TRUE,
     xmlElement = "MD_ProgressCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCC"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, value = value, description = description)
        warnings("Class 'ISOStatus' is deprecated, please use 'ISOProgress' instead!")
     }
   )                        
)

ISOStatus$values <- function(labels = FALSE){
  return(ISOProgress$values(labels = labels))
}
