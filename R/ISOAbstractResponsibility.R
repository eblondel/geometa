#' ISOAbstractResponsibility
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract responsibility
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract responsibility
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   ISO 19115-1:2014 Geographic information — Metadata Part 1: Fundamentals
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractResponsibility <- R6Class("ISOAbstractResponsibility",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_Responsibility",
     xmlNamespacePrefix = list(
       "19115-3" = "MCC"
     )
   ),
   public = list(

     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)