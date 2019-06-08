#' ISOImageryGCPCollection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery gcp collection
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery gcp collection
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryGCPCollection}}
#'  }
#'  \item{\code{setCollectionIdentification(id)}}{
#'    Set the identifier, object of class \code{integer}
#'  }
#'  \item{\code{setCollectionName(name, locales)}}{
#'    Sets a name (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setCoordinateReferenceSystem(crs)}}{
#'    Sets the crs, object of class \code{\link{ISOReferenceSystem}}
#'  }
#'  \item{\code{addGCP(gcp)}}{
#'    Adds a GCP, object of class \code{\link{ISOImageryGCP}}
#'  }
#'  \item{\code{delGCP(gcp)}}{
#'    Deletes a GCP, object of class \code{\link{ISOImageryGCP}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOImageryGCPCollection$new()
#'   md$setCollectionIdentification(1L)
#'   md$setCollectionName("name")
#'   rs <- ISOReferenceSystem$new()
#'   rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'   rs$setReferenceSystemIdentifier(rsId)
#'   md$setCoordinateReferenceSystem(rs)
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGCPCollection <- R6Class("ISOImageryGCPCollection",
  inherit = ISOImageryAbstractGeolocationInformation,
  private = list(
    xmlElement = "MI_GCPCollection",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ collectionIdentification [1..1]: integer
    collectionIdentification = NULL,
    #+ collectionName [1..1]: character|ISOLocalisedCharacterString
    collectionName = NULL,
    #+ coordinateReferenceSystem [1..1]: ISOReferenceSystem
    coordinateReferenceSystem = NULL,
    #+ gcp [0..*]: list of ISOImageryGCP
    gcp = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setCollectionIdentification
    setCollectionIdentification = function(id){
      identifier <- id
      if(!is(identifier, "integer")){
        identifier <- as.integer(id)
      }
      if(is.na(identifier)){
        stop("The argument should be an object of class 'integer' or coercable to 'integer'")
      }
      self$collectionIdentification <- identifier
    },
    
    #setCollectionName
    setCollectionName = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      self$collectionName <- name
    },
    
    #setCoordinateReferenceSystem
    setCoordinateReferenceSystem = function(crs){
      if(!is(crs, "ISOReferenceSystem")){
        stop("The argument should be an object of class 'ISOReferenceSystem'")
      }
      self$coordinateReferenceSystem <- crs
    },
    
    #addGCP
    addGCP = function(gcp){
      if(!is(gcp, "ISOImageryGCP")){
        stop("The argument should be an object of class 'ISOImageryGCP'")
      }
      return(self$addListElement("gcp", gcp))
    },
    
    #delGCP
    delGCP = function(gcp){
      if(!is(gcp, "ISOImageryGCP")){
        stop("The argument should be an object of class 'ISOImageryGCP'")
      }
      return(self$delListElement("gcp", gcp))
    }
  )                        
)

