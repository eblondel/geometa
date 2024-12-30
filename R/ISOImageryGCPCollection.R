#' ISOImageryGCPCollection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery gcp collection
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery gcp collection
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_GCPCollection}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MI_GCPCollection}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGCPCollection <- R6Class("ISOImageryGCPCollection",
  inherit = ISOImageryAbstractGeolocationInformation,
  private = list(
    xmlElement = "MI_GCPCollection",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MSR"
    )
  ),
  public = list(
    
    #'@field collectionIdentification collectionIdentification [1..1]: integer
    collectionIdentification = NULL,
    #'@field collectionName collectionName [1..1]: character|ISOLocalisedCharacterString
    collectionName = NULL,
    #'@field coordinateReferenceSystem coordinateReferenceSystem [1..1]: ISOAbstractReferenceSystem
    coordinateReferenceSystem = NULL,
    #'@field gcp gcp [0..*]: list of ISOImageryGCP
    gcp = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set collection identification id
    #'@param id object of class \link{integer}
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
    
    #'@description Set collection name
    #'@param name object of class \link{character}
    #'@param locales list of localized names. Default is \code{NULL}
    setCollectionName = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      self$collectionName <- name
    },
    
    #'@description Set coordinate reference system
    #'@param crs object of class inheriting \link{ISOAbstractReferenceSystem}
    setCoordinateReferenceSystem = function(crs){
      if(!is(crs, "ISOAbstractReferenceSystem")){
        stop("The argument should be an object inheriting class 'ISOAbstractReferenceSystem'")
      }
      self$coordinateReferenceSystem <- crs
    },
    
    #'@description Adds GCP
    #'@param gcp object of class \link{ISOImageryGCP}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addGCP = function(gcp){
      if(!is(gcp, "ISOImageryGCP")){
        stop("The argument should be an object of class 'ISOImageryGCP'")
      }
      return(self$addListElement("gcp", gcp))
    },
    
    #'@description Deletes GCP
    #'@param gcp object of class \link{ISOImageryGCP}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delGCP = function(gcp){
      if(!is(gcp, "ISOImageryGCP")){
        stop("The argument should be an object of class 'ISOImageryGCP'")
      }
      return(self$delListElement("gcp", gcp))
    }
  )                        
)

