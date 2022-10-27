#' ISOMedium
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO medium
#' @return Object of \code{\link{R6Class}} for modelling an ISO Citation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'  md <- ISOMedium$new()
#'  md$setName("satellite")
#'  md$addDensity(1.0)
#'  md$setDensityUnits("string")
#'  md$setVolumes(1L)
#'  md$addMediumFormat("tar")
#'  md$setMediumNote("some note")
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMedium<- R6Class("ISOMedium",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Medium",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field name name
    name = NULL,
    #'@field density density
    density = list(),
    #'@field densityUnits density units
    densityUnits = NULL,
    #'@field volumes volumes
    volumes = NULL,
    #'@field mediumFormat medium format
    mediumFormat = list(),
    #'@field mediumNote medium note
    mediumNote = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set name
    #'@param name name object of class \link{ISOMediumName} or \link{character} among
    #' values returned by \code{ISOMediumName$values()}
    setName = function(name){
      if(is(name, "character")){
        name <- ISOMediumName$new(value = name)
      }
      self$name <- name
    },
    
    #'@description Adds density
    #'@param density object of class \link{numeric}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDensity = function(density){
      dens <- as.numeric(density)
      if(is.na(dens)){
        stop("The density should be of class 'numeric' or coerceable to 'numeric'")
      }
      density <- dens
      return(self$addListElement("density", density))
    },
    
    #'@description Deletes density
    #'@param density object of class \link{numeric}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDensity = function(density){
      dens <- as.numeric(density)
      if(is.na(dens)){
        stop("The density should be of class 'numeric' or coerceable to 'numeric'")
      }
      density <- dens
      return(self$delListElement("density", density))
    },
    
    #'@description Set density units
    #'@param densityUnits densityUnits
    setDensityUnits = function(densityUnits){
      self$densityUnits <- densityUnits
    },
    
    #'@description Set volumes
    #'@param volumes object of class \link{integer}
    setVolumes = function(volumes){
      vol <- as.integer(volumes)
      if(is.na(vol)){
       stop("The volumes should be of class 'integer' or coerceable to 'integer'") 
      }
      self$volumes <- vol
    },
    
    #'@description Adds medium format
    #'@param mediumFormat object of class \link{ISOMediumFormat} or \link{character}
    #' among values returned by \code{ISOMediumFormat$values()}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addMediumFormat = function(mediumFormat){
      if(is(mediumFormat, "character")){
        mediumFormat <- ISOMediumFormat$new(value = mediumFormat)
      }
      return(self$addListElement("mediumFormat", mediumFormat))
    },
    
    #'@description Deletes medium format
    #'@param mediumFormat object of class \link{ISOMediumFormat} or \link{character}
    #' among values returned by \code{ISOMediumFormat$values()}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delMediumFormat = function(mediumFormat){
      if(is(mediumFormat, "character")){
        mediumFormat <- ISOMediumFormat$new(value = mediumFormat)
      }
      return(self$delListElement("mediumFormat", mediumFormat))
    },
    
    #'@description Set medium note
    #'@param mediumNote medium note
    #'@param locales list of localized notes. Default is \code{NULL}
    setMediumNote = function(mediumNote, locales = NULL){
      if(!is.null(locales)){
        mediumNote <- self$createLocalisedProperty(mediumNote, locales)
      }
      self$mediumNote <- mediumNote
    }
    
  )                                          
)
