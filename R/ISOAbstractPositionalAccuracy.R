#' ISOAbstractPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractPositionalAccuracy
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods from \code{ISODataQualityAbstractElement}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAbstractPositionalAccuracy
#'  }
#' }
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractPositionalAccuracy <- R6Class("ISOAbstractPositionalAccuracy",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "AbstractDQ_PositionalAccuracy",
     xmlNamespacePrefix = "GMD"
   ),
   public = list()
)