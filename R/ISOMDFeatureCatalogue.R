#' ISOMDFeatureCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO feature catalogue
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_FeatureCatalogue}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMDFeatureCatalogue <- R6Class("ISOMDFeatureCatalogue",
   inherit = ISOAbstractContentInformation,
   private = list(
     xmlElement = "MD_FeatureCatalogue",
     xmlNamespacePrefix = list(
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field featureCatalogue featureCatalogue [1..*] : ISOAbstractFeatureCatalogue
     featureCatalogue = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds feature catalogue
     #'@param featureCatalogue object inheriting class \link{ISOAbstractFeatureCatalogue}
     #'@return \code{TRUE} if added, \code{FALSe} otherwise
     addFeatureCatalogue = function(featureCatalogue){
       if(!is(featureCatalogue, "ISOAbstractFeatureCatalogue")){
         stop("The argument should be an object of class 'ISOAbstractFeatureCatalogue'")
       }
       return(self$addListElement("featureCatalogue", featureCatalogue))
     },
     
     #'@description Adds feature catalogue
     #'@param featureCatalogue object inheriting class \link{ISOAbstractFeatureCatalogue}
     #'@return \code{TRUE} if deleted, \code{FALSe} otherwise
     delFeatureCatalogue = function(featureCatalogue){
       if(!is(featureCatalogue, "ISOAbstractFeatureCatalogue")){
         stop("The argument should be an object of class 'ISOAbstractFeatureCatalogue'")
       }
       return(self$delListElement("featureCatalogue", featureCatalogue))
     }
   )                        
)
