#' ISOFeatureCatalogueDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue description
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOFeatureCatalogue
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOFeatureCatalogueDescription$new()
#'   md$setComplianceCode(FALSE)
#'   md$addLanguage("eng")
#'   md$setIncludedWithDataset(FALSE)
#'   
#'   cit = ISOCitation$new()
#'   contact = ISOContact$new()
#'   fcLink <- ISOOnlineResource$new()
#'   fcLink$setLinkage("http://somelink/featurecatalogue")
#'   contact$setOnlineResource(fcLink)
#'   rp = ISOResponsibleParty$new()
#'   rp$setContactInfo(contact)
#'   cit$addCitedResponsibleParty(rp)
#'   md$addFeatureCatalogueCitation(cit)
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_FeatureCatalogueDescription}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_FeatureCatalogueDescription}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureCatalogueDescription <- R6Class("ISOFeatureCatalogueDescription",
   inherit = ISOAbstractContentInformation,
   private = list(
     xmlElement = "MD_FeatureCatalogueDescription",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field complianceCode complianceCode: logical
     complianceCode = NULL,
     #'@field language language [0..*]: character
     language = list(),
     #'@field locale locale [0..*]: ISOLocale
     locale = list(),
     #'@field includedWithDataset includedWithDataset: logical
     includedWithDataset = FALSE,
     #'@field featureTypes featureTypes [0..*]: GenericName #TODO?
     featureTypes = list(),
     #'@field featureCatalogueCitation featureCatalogueCitation [1..*]: ISOCitation
     featureCatalogueCitation = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set compliance code
     #'@param compliance compliance, object of class \link{logical}
     setComplianceCode = function(compliance){
       if(!is(compliance, "logical")){
         compliance <- as.logical(compliance)
         if(is.na(compliance)) stop("Value cannot be coerced to 'logical'")
       }
       self$complianceCode <- compliance
     },
     
     #'@description Adds language
     #'@param lang lang
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addLanguage = function(lang){
       return(self$addListElement("language", lang))
     },
     
     #'@description Deletes language
     #'@param lang lang
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delLanguage = function(lang){
       return(self$delListElement("language", lang))
     },
     
     #'@description Adds locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addLocale = function(locale){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$addListElement("locale", locale))
     },
     
     #'@description Deletes locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delLocale = function(locale){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$delListElement("locale", locale))
     },
     
     #'@description Set included with dataset
     #'@param include include, object of class \link{logical}
     setIncludedWithDataset = function(include){
       if(!is(include, "logical")){
         include <- as.logical(include)
         if(is.na(include)) stop("Value cannot be coerced to 'logical'")
       }
       self$includedWithDataset <- include
     },
     
     #'@description Adds feature catalogue citation
     #'@param citation, object of class \link{ISOCitation}
     #'@param uuid uuid
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addFeatureCatalogueCitation = function(citation, uuid = NULL){
       if(!is.null(uuid)) citation$parentAttrs <- list(uuidref = uuid)
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$addListElement("featureCatalogueCitation", citation))
     },
     
     #'@description Deletes feature catalogue citation
     #'@param citation, object of class \link{ISOCitation}
     #'@param uuid uuid
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delFeatureCatalogueCitation = function(citation, uuid = NULL){
       if(!is.null(uuid)) citation$parentAttrs <- list(uuidref = uuid)
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$delListElement("featureCatalogueCitation", citation))
     }

   )         
)
