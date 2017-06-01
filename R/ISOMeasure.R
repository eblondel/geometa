#' ISOMeasure
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure
#' @return Object of \code{\link{R6Class}} for modelling an ISO Measure
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOMeasure
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMeasure <- R6Class("ISOMeasure",
    inherit = ISOMetadataElement,
    private = list(
      xmlElement = "Measure",
      xmlNamespacePrefix = "GCO"
    ),
    public = list(
      value = NA,
      attrs = list(),
      initialize = function(xml = NULL, element = NULL, namespace = NULL, value, uom){
        if(is.null(element)) element <- private$xmlElement
        if(is.null(namespace)) namespace <- getISOMetadataNamespace(private$xmlNamespacePrefix)
        super$initialize(
          xml = xml,
          element = element,
          namespace = namespace
        )
        if(is.null(xml)){
          if(!is(value, "double")){
            value <- as.double(value)
          }
          self$value = value
          self$attrs[["uom"]] <- uom
        }else{
          uomId <- XML::xmlGetAttr(xml, "uom")
          if(!is.null(uomId)){
            self$attrs[["uom"]] <- uomId
          }
        }
      }
    )                        
)