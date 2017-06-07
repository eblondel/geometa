#' ISOFeatureCatalogueDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue description
#' @return Object of \code{\link{R6Class}} for modelling an ISOFeatureCatalogue
#' @format \code{\link{R6Class}} object.
#'
#' @field complianceCode
#' @field language
#' @field includeWithDataset
#' @field featureCatalogueCitation
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureCatalogueDescription
#'  }
#'  \item{\code{setComplianceCode(compliance)}}{
#'    Sets the compliance. TRUE if compliant, FALSE otherwise
#'  }
#'  \item{\code{addLanguage(lang)}}{
#'    Adds a language
#'  }
#'  \item{\code{delLanguage(lang)}}{
#'    Deletes a language
#'  }
#'  \item{\code{setIncludeWithDataset(include)}}{
#'    Sets TRUE if included with dataset, FALSE otherwise
#'  }
#'  \item{\code{addFeatureCatalogueCitation(citation)}}{
#'   Adds an object of class \code{ISOCitation} referencing the link
#'   to Feature Catalogue
#'  }
#'  \item{\code{delFeatureCatalogueCitation(citation)}}{
#'   Deletes an object of class \code{ISOCitation} referencing the link
#'   to Feature Catalogue
#'  }
#' }
#' 
#' @examples 
#'   md <- ISOFeatureCatalogueDescription$new()
#'   md$setComplianceCode(FALSE)
#'   md$addLanguage("eng")
#'   md$setIncludeWithDataset(FALSE)
#'   
#'   cit = ISOCitation$new()
#'   contact = ISOContact$new()
#'   fcLink <- ISOOnlineResource$new()
#'   fcLink$setLinkage("http://somelink/featurecatalogue")
#'   contact$setOnlineResource(fcLink)
#'   rp = ISOResponsibleParty$new()
#'   rp$setContactInfo(contact)
#'   cit$setCitedResponsibleParty(rp)
#'   md$addFeatureCatalogueCitation(cit)
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureCatalogueDescription <- R6Class("ISOFeatureCatalogueDescription",
   inherit = ISOContentInformation,
   private = list(
     xmlElement = "MD_FeatureCatalogueDescription",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ complianceCode: logical
     complianceCode = NULL,
     #+ language [0..*]: character
     language = list(),
     #+ includeWithDataset: logical
     includeWithDataset = NULL,
     #+ featureTypes [0..*]: GenericName #TODO?
     featureTypes = list(),
     #+ featureCatalogueCitation [1..*]: ISOCitation
     featureCatalogueCitation = list(),
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setComplianceCode
     setComplianceCode = function(compliance){
       if(!is(compliance, "logical")){
         compliance <- as.logical(compliance)
         if(is.na(compliance)) stop("Value cannot be coerced to 'logical'")
       }
       self$complianceCode <- compliance
     },
     
     #addLanguage
     addLanguage = function(lang){
       return(self$addListElement("language", lang))
     },
     
     #delLanguage
     delLanguage = function(lang){
       return(self$delListElement("language", lang))
     },
     
     #setIncludeWithDataset
     setIncludeWithDataset = function(include){
       if(!is(include, "logical")){
         include <- as.logical(include)
         if(is.na(include)) stop("Value cannot be coerced to 'logical'")
       }
       self$includeWithDataset <- include
     },
     
     #addFeatureCatalogueCitation
     addFeatureCatalogueCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$addListElement("featureCatalogueCitation", citation))
     },
     
     #delFeatureCatalogueCitation
     delFeatureCatalogueCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$delListElement("featureCatalogueCitation", citation))
     }

   )         
)