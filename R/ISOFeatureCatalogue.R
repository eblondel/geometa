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
#'  \item{\code{addScope(scope)}}{
#'    Adds scope (object of class \code{character})
#'  }
#'  \item{\code{delScope(scope)}}{
#'    Deletes scope
#'  }
#'  \item{\code{addFieldOfApplication(fieldOfApplication)}}{
#'    Adds a field of application (object of class \code{character})
#'  }
#'  \item{\code{delFieldOfApplication(fieldOfApplication)}}{
#'    Deletes fieldOfApplication
#'  }
#'  \item{\code{setVersionNumber(versionNumber)}}{
#'    Sets version number (object of class \code{character})
#'  }
#'  \item{\code{setVersionDate(versionDate)}}{
#'    Sets version date
#'  }
#'  \item{\code{setProducer(producer)}}{
#'    Sets an object of class \code{ISOResponsibleParty} as producer
#'  }
#'  \item{\code{setFunctionalLanguage(functionalLanguage)}}{
#'    Sets the functional language
#'  }
#'  \item{\code{addFeatureType(featureType)}}{
#'    Adds a feature type, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{delFeatureType(featureType)}}{
#'    Deletes a feature type, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{addDefinitionSource(source)}}{
#'    Adds a definition source, object of class \code{ISODefinitionSource} or
#'    \code{ISOCitation}
#'  }
#'  \item{\code{delDefinitionSource(source)}}{
#'    Deletes a definition source, object of class \code{ISODefinitionSource} or
#'    \code{ISOCitation}
#'  }
#' }
#'
#' @examples 
#'  fc <- ISOFeatureCatalogue$new()
#'  fc$setName("name")
#'  fc$addScope("scope1")
#'  fc$addScope("scope2")
#'  fc$addFieldOfApplication("field1")
#'  fc$addFieldOfApplication("field2")
#'  fc$setVersionNumber("1.0")
#'  fc$setVersionDate(ISOdate(2015, 1, 1, 1))
#'  
#'  producer <- ISOResponsibleParty$new()
#'  producer$setIndividualName("someone")
#'  fc$setProducer(producer)
#'  fc$setFunctionalLanguage("eng")
#'
#'  cit <- ISOCitation$new()
#'  cit$setTitle("some citation title")
#'  fc$addDefinitionSource(cit)
#'  
#'  xml <- fc$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
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
      
      #+ name [1..1]: character
      name = NULL,
      #+ scope [1..*]: character
      scope = list(),
      #+ fieldOfApplication [0.*]: character
      fieldOfApplication = list(),
      #+ versionNumber [1..1]: character
      versionNumber = NULL,
      #+ versionDate [1..1]: character
      versionDate = NULL,
      #+ producer [1..1]: ISOResponsibleParty
      producer = NULL,
      #+ functionalLanguage [0..1]: character 
      functionalLanguage = NULL,
      #+ featureType [1..*]: ISOFeatureType
      featureType = list(),
      #+ definitionSource [0..*]: ISODefinitionSource
      definitionSource = list(),
      
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
      },
      
      #addFeatureType
      addFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be a 'ISOFeatureType' object")
        }
        return(self$addListElement("featureType", featureType))
      },
      
      #delFeatureType
      delFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be a 'ISOFeatureType' object")
        }
        return(self$delListElement("featureType", featureType))
      },
      
      #addDefinitionSource
      addDefinitionSource = function(source){
        if(!is(source, "ISODefinitionSource")){
          if(is(source, "ISOCitation")){
            source <- ISODefinitionSource$new(source = source)
          }else{
            stop("The argument should be an object of class 'ISODefinitionSource' or 'ISOCitation'")
          }
        }
        return(self$addListElement("definitionSource", source))
      },
      
      #delDefinitionSource
      delDefinitionSource = function(source){
        if(!is(source, "ISODefinitionSource")){
          if(is(source, "ISOCitation")){
            source <- ISODefinitionSource$new(source = source)
          }else{
            stop("The argument should be an object of class 'ISODefinitionSource' or 'ISOCitation'")
          }
        }
        return(self$delListElement("definitionSource", source))
      }
    )                        
)