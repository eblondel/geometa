#' ISOFeatureCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature catalogue
#' @return Object of \code{\link{R6Class}} for modelling an ISO FeatureCatalogue
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureCatalogue
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets the name
#'  }
#' }
#'
#' @examples 
#'  fc <- ISOFeatureCatalogue$new()
#'  xml <- fc$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureCatalogue <- R6Class("ISOFeatureCatalogue",
    inherit = ISOMetadataElement,
    private = list(
      xmlElement = "FC_FeatureCatalogue",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      name = NULL,
      scope = list(),
      fieldOfApplication = list(),
      versionNumber = NULL,
      versionDate = NULL,
      producer = NULL,
      functionalLanguage = NULL,
      initialize = function(xml = NULL){
        super$initialize(
          xml = xml,
          element = private$xmlElement,
          namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
        )
      },
      
      #setName
      setName = function(name){
        if(!is(name,"character")) name <- as(name, "character")
        self$name <- name
      },
      
      #addScope
      addScope = function(scope){
        return(self$addListElement("scope", scope))
      },
      
      #delScope
      delScope = function(scope){
        return(self$delListElement("scope", scope))
      },
      
      #addFieldOfApplication
      addFieldOfApplication = function(fieldOfApplication){
        return(self$addListElement("fieldOfApplication", fieldOfApplication))
      },
      
      #delFieldOfApplication
      delFieldOfApplication = function(fieldOfApplication){
        return(self$delListElement("fieldOfApplication", fieldOfApplication))
      },
      
      #setVersionNumber
      setVersionNumber = function(versionNumber){
        if(!is(versionNumber,"character")) versionNumber <- as(versionNumber, "character")
        self$versionNumber <- versionNumber
      },
      
      #setVersionDate
      setVersionDate = function(versionDate){
        self$versionDate <- versionDate
      },
      
      #setProducer
      setProducer = function(producer){
        if(!is(producer,"ISOResponsibleParty")){
          stop("The argument should be a 'ISOResponsibleParty' object")
        }
        self$producer <- producer
      },
      
      #setFunctionalLanguage
      setFunctionalLanguage = function(functionalLanguage){
        if(!is(functionalLanguage,"character")) functionalLanguage <- as(functionalLanguage, "character")
        self$functionalLanguage <- functionalLanguage
      }
      
    )                        
)