#' ISOConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO  constraints
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract Constraints
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Abstract ISO class
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Constraints}
#' 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mco/1.0/mco/#element_MD_Constraints}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConstraints<- R6Class("ISOConstraints",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Constraints",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MCO"
    )
  ),
  public = list(
    #'@field useLimitation useLimitation [0..*]: character
    useLimitation = list(),
    
    #TODO 19115-3 fields (and associated methods)
    #'@field constraintApplicationScope constraintApplicationScope [0..1]: ISOScope (=> ISO 19115-3)
    constraintApplicationScope = NULL,
    #'@field graphic graphic [0..*]: ISOBrowseGraphic (=> ISO 19115-3)
    graphic = list(),
    #'@field reference reference [0..*]: ISOCitation (=> ISO 19115-3)
    reference = list(), #cit:CI_Citation
    #'@field releasability releasability [0..1]: ISOReleasability (=> ISO 19115-3)
    releasability = NULL, #mco:MD_Releasability
    #'@field responsibleParty responsibleParty [0..*]: ISOAbstractResponsibility (=> ISO 19115-3)
    responsibleParty = list(), #cit:CI_Responsibility
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param defaults list of default values
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults = defaults)
    },
    
    #'@description Adds a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$addListElement("useLimitation", useLimitation))
    },
    
    #'@description Adds a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    setUseLimitation = function(useLimitation, locales = NULL){
      warning("Method 'setUseLimitation' is deprecated, please use 'addUseLimitation' instead!")
      self$useLimitation <- list()
      return(self$addUseLimitation(useLimitation, locales))
    },
    
    #'@description Deletes a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$delListElement("useLimitation", useLimitation))
    },
    
    #'@description Set scope
    #'@param scope object of class \link{ISOScope}
    setScope = function(scope){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(scope, "ISOScope")){
        stop("The argument 'scope' should be an object of class 'ISOScope'")
      }
      self$constraintApplicationScope = scope
    },
    
    #'@description Adds a graphic
    #'@param graphic graphic
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addGraphic = function(graphic){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(graphic, "ISOBrowseGraphic")){
        stop("The argument 'graphic' should be an object of class 'ISOBrowseGraphic'")
      }
      return(self$addListElement("graphic", graphic))
    },
    
    #'@description Deletes a graphic
    #'@param graphic graphic
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delGraphic = function(graphic){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(graphic, "ISOBrowseGraphic")){
        stop("The argument 'graphic' should be an object of class 'ISOBrowseGraphic'")
      }
      return(self$delListElement("graphic", graphic))
    },
    
    #'@description Adds a reference
    #'@param reference reference
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addReference = function(reference){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(reference, "ISOCitation")){
        stop("The argument 'reference' should be an object of class 'ISOCitation'")
      }
      return(self$addListElement("reference", reference))
    },
    
    #'@description Deletes a reference
    #'@param reference reference
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delReference = function(reference){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(reference, "ISOCitation")){
        stop("The argument 'reference' should be an object of class 'ISOCitation'")
      }
      return(self$delListElement("reference", reference))
    },
    
    #'@description Set releasbility
    #'@param releasability object of class \link{ISOReleasability}
    setReleasability = function(releasability){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(releasability, "ISOReleasability")){
        stop("The argument 'releasability' should be an object of class 'ISOReleasability'")
      }
      self$releasability = releasability
    },
    
    #'@description Adds a responsible party
    #'@param party party object of class inheriting \link{ISOAbstractResponsibility}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addResponsibleParty = function(party){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(party, "ISOAbstractResponsibility")){
        stop("The argument 'party' should be an object inheriting class 'ISOAbstractResponsibility'")
      }
      return(self$addListElement("responsibleParty", party))
    },
    
    #'@description Deletes a responsible party
    #'@param party party object of class inheriting \link{ISOAbstractResponsibility}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delResponsibleParty = function(party){
      self$checkMetadataStandardCompliance("19115-3")
      if(!is(party, "ISOAbstractResponsibility")){
        stop("The argument 'party' should be an object inheriting class 'ISOAbstractResponsibility'")
      }
      return(self$delListElement("responsibleParty", party))
    }
  )                                          
)
