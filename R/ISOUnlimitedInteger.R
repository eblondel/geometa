#' ISOUnlimitedInteger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unlimited integer
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO UnlimitedInteger
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_UnlimitedInteger}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_UnlimitedInteger}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUnlimitedInteger <- R6Class("ISOUnlimitedInteger",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "UnlimitedInteger",
      xmlNamespacePrefix = list(
        "19139" = "GCO",
        "19115-3" = "GCO"
      )
    ),
    public = list(
      #'@field value value
      value = NA,
      #'@field attrs attrs
      attrs = list(),
      
      #'@description Initialize object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param value value
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
