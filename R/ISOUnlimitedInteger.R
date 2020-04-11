#' ISOUnlimitedInteger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unlimited integer
#' @return Object of \code{\link{R6Class}} for modelling an ISO UnlimitedInteger
#' @format \code{\link{R6Class}} object.
#'
#' @field value [\code{\link{integer}}|\code{\link{numeric}}] the integer
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOUnlimitedInteger}}
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUnlimitedInteger <- R6Class("ISOUnlimitedInteger",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "UnlimitedInteger",
      xmlNamespacePrefix = "GCO"
    ),
    public = list(
      value = NA,
      attrs = list(),
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml)
        if(is.null(xml)){
          if(!is(value, "integer") & !is.infinite(value)){
            value <- as.integer(value)
          }
          self$value = value
          self$attrs[["isInfinite"]] <- tolower(as.character(is.infinite(value)))
          if(is.infinite(value)){
            self$value <- NULL
            self$attrs[["xsi:nil"]] <- "true"
          }
        }else{
          isInf <- xmlGetAttr(xml, "isInfinite")
          self$attrs[["isInfinite"]] <- isInf
          if(as.logical(isInf)==TRUE){
            self$value <- NULL
            self$attrs[["xsi:nil"]] <- "true"
          }else{
            self$attrs[["xsi:nil"]] <- NULL
          }
        }
      }
    )                        
)