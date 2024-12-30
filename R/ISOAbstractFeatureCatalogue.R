#' ISoAbstractFeatureCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO abstract feature catalogue
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19110/-/fcc/2.2/fcc/#element_Abstract_FeatureCatalogue}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISoAbstractFeatureCatalogue <- R6Class("ISoAbstractFeatureCatalogue",
   inherit = ISOAbstractCatalogue,
   private = list(
     xmlElement = "Abstract_FeatureCatalogue",
     xmlNamespacePrefix = list(
       "19115-3" = "FCC"
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
