#' ISOKeywordClass
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO keywordclass
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO keyword class
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_KeywordClass}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywordClass <- R6Class("ISOKeywordClass",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_KeywordClass",
     xmlNamespacePrefix = list(
       "19115-3" = "MRI"
     )
   ),
   public = list(
     #'@field className className
     className = NULL,
     #'@field conceptIdentifier conceptIdentifier
     conceptIdentifier = NULL,
     #'@field ontology ontology
     ontology = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set class name
     #'@param className className
     #'@param locales list of localized texts. Default is \code{NULL}
     setClassName = function(className, locales = NULL){
       if(!is.null(locales)){
         className <- self$createLocalisedProperty(className, locales)
       }
       self$className = className
     },
     
     #'@description Set concept identifier
     #'@param conceptIdentifier conceptIdentifier, object of class \link{ISOURI}
     setConceptIdentifier = function(conceptIdentifier){
       if(!is(conceptIdentifier, "ISOURI")){
         stop("The argument should be an object of class 'ISOURI'")
       }
       self$conceptIdentifier = conceptIdentifier
     },
     
     #'@description Set ontology
     #'@param ontology ontology, object inheriting class \link{ISOAbstractParty}
     setOntology = function(ontology){
       if(!is(ontology, "ISOAbstractParty")){
         stop("The argument should be an object of class 'ISOAbstractParty'")
       }
       self$ontology = ontology
     }
   )                        
)
