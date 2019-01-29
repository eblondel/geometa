#' ISOAbstractCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract catalogue
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstracCatalogue
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field scope
#' @field fieldOfApplication
#' @field versionNumber
#' @field versionDate
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAbstractCatalogue
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets the name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{addScope(scope, locales)}}{
#'    Adds scope (object of class \code{character}). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{delScope(scope, locales)}}{
#'    Deletes scope. Locale names can be  specified as \code{list} with 
#'    the \code{locales} argument. Local names should match those of 
#'    the scope to be deleted, otherwise nothing will be deleted.
#'  }
#'  \item{\code{addFieldOfApplication(fieldOfApplication, locales)}}{
#'    Adds a field of application (object of class \code{character}). Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{delFieldOfApplication(fieldOfApplication, locales)}}{
#'    Deletes fieldOfApplication. Locale names can be  specified as \code{list} with 
#'    the \code{locales} argument. Local names should match those of 
#'    the field of application to be deleted, otherwise nothing will be deleted.
#'  }
#'  \item{\code{setVersionNumber(versionNumber)}}{
#'    Sets version number (object of class \code{character})
#'  }
#'  \item{\code{setVersionDate(versionDate)}}{
#'    Sets version date
#'  }
#' }
#'  
#' @references 
#'   ISO 19139:2007 Metadata - XML schema implementation
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCatalogue <- R6Class("ISOAbstractCatalogue",
   inherit = ISOAbstractObject,
   private = list(
     document = TRUE,
     xmlElement = "AbstractCT_Catalogue",
     xmlNamespacePrefix = "GMX"
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
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setName
     setName = function(name, locales = NULL){
       if(!is(name,"character")) name <- as(name, "character")
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       self$name <- name
     },
     
     #addScope
     addScope = function(scope, locales = NULL){
       if(!is.null(locales)){
         scope <- self$createLocalisedProperty(scope, locales)
       }
       return(self$addListElement("scope", scope))
     },
     
     #delScope
     delScope = function(scope, locales = NULL){
       if(!is.null(locales)){
         scope <- self$createLocalisedProperty(scope, locales)
       }
       return(self$delListElement("scope", scope))
     },
     
     #addFieldOfApplication
     addFieldOfApplication = function(fieldOfApplication, locales = NULL){
       if(!is.null(locales)){
         fieldOfApplication <- self$createLocalisedProperty(fieldOfApplication, locales)
       }
       return(self$addListElement("fieldOfApplication", fieldOfApplication))
     },
     
     #delFieldOfApplication
     delFieldOfApplication = function(fieldOfApplication){
       if(!is.null(locales)){
         fieldOfApplication <- self$createLocalisedProperty(fieldOfApplication, locales)
       }
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
     }
   )                        
)