#' ISODataIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_DataIdentification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataIdentification19115_3 <- R6Class("ISODataIdentification19115_3",
   inherit = ISOIdentification19115_3,
   private = list(
     xmlElement = "MD_DataIdentification",
     xmlNamespacePrefix = list(
       "19115-3" = "MRI"
     )
   ),
   public = list(
     #'@field defaultLocale defaultLocale [0..1]: ISOLocale
     defaultLocale = NULL,
     #'@field otherLocale otherLocale [0..*]: ISOLocale
     otherLocale = list(),
     #'@field environmentDescription environment description [0..1]: character
     environmentDescription = NULL,
     #'@field supplementalInformation supplementalInformation [0..1]: character
     supplementalInformation = NULL, 
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml, defaults = list())
     },
     
     #'@description Set default locale
     #'@param locale object of class \link{ISOLocale}
     setDefaultLocale = function(locale){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       self$defaultLocale = locale
     },
     
     #'@description Adds locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOtherLocale = function(locale){
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$addListElement("otherLocale", locale))
     },
     
     #'@description Deletes locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delOtherLocale = function(locale){
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$delListElement("otherLocale", locale))
     },
     
     #'@description Set environment description
     #'@param environmentDescription environment description
     #'@param locales a list of localized information. Default is \code{NULL}
     setEnvironmentDescription = function(environmentDescription, locales = NULL){
       self$environmentDescription = as.character(environmentDescription)
       if(!is.null(locales)){
         self$environmentDescription <- self$createLocalisedProperty(environmentDescription, locales)
       }
     },
     
     #'@description Set supplemental information
     #'@param supplementalInformation supplemental information
     #'@param locales a list of localized information. Default is \code{NULL}
     setSupplementalInformation = function(supplementalInformation, locales = NULL){
       self$supplementalInformation = as.character(supplementalInformation)
       if(!is.null(locales)){
         self$supplementalInformation <- self$createLocalisedProperty(supplementalInformation, locales)
       }
     }
     
   )                        
)