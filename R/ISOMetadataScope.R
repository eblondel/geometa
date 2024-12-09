#' ISOMetadataScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata scope
#' @return Object of \code{\link{R6Class}} for modelling an ISO MetadataScope
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'  md <- ISOMetadataScope$new(
#'   resourceScope = "service",
#'   name = "Internal service"
#'  )
#'  xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_MetadataScope}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mdb/2.0/mdb/#element_MD_MetadataScope}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataScope <- R6Class("ISOMetadataScope",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_MetadataScope",
      xmlNamespacePrefix = list(
        "19115-3" = "MDB"
      )
    ),
    public = list(
      #'@field resourceScope resource scope
      resourceScope = NULL,
      #'@field name name
      name = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param resourceScope resource scope
      #'@param name name
      initialize = function(xml = NULL, resourceScope = NULL, name = NULL){
        super$initialize(xml = xml)
        if(is.null(xml)){
          self$setResourceScope(resourceScope)
          self$setName(name)
        }
      },
      
      #'@description Set resource scope
      #'@param resourceScope resource scope
      setResourceScope = function(resourceScope){
        if(is(resourceScope, "character")){
          resourceScope <- ISOScopeCode$new(value = resourceScope)
        }
        self$resourceScope = resourceScope
      },
      
      #'@description Set name
      #'@param name name
      #'@param locales a list of localized names. Default is \code{NULL}
      setName = function(name, locales = NULL){
        self$name <- as.character(name)
        if(!is.null(locales)){
          self$name <- self$createLocalisedProperty(name, locales)
        }
      }
      
    )                        
)