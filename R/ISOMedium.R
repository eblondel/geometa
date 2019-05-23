#' ISOMedium
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO medium
#' @return Object of \code{\link{R6Class}} for modelling an ISO Citation
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{ISOMediumName}}|\code{\link{character}}] name
#' @field density [\code{\link{numeric}}] density
#' @field densityUnits \code{\link{character}} density unit
#' @field volumes [\code{\link{integer}}] volumes
#' @field mediumFormat [\code{\link{ISOMediumFormat}}|\code{\link{character}}] format
#' @field mediumNode \code{\link{character}} note
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOMedium}}
#'  }
#'  \item{\code{setName(name)}}{
#'    Set the medium name, object of class 'character' or \code{\link{ISOMediumName}}
#'  }
#'  \item{\code{addDensity(density)}}{
#'    Adds a density
#'  }
#'  \item{\code{delDensity(density)}}{
#'    Deletes density
#'  }
#'  \item{\code{setDensityUnits(densityUnits)}}{
#'    Set density unit
#'  }
#'  \item{\code{setVolumes(volumes)}}{
#'    Set volumes
#'  }
#'  \item{\code{addMediumFormat(mediumFormat)}}{
#'    Adds a medium format, object of class 'character' or \code{\link{ISOMediumFormat}}
#'  }
#'  \item{\code{delMediumFormat(mediumFormat)}}{
#'    Deletes a medium format, object of class 'character' or \code{\link{ISOMediumFormat}}
#'  }
#'  \item{\code{setMediumNote(mediumNote, locales)}}{
#'    Set a medium note. Locale names can be specified as \code{list} with 
#'    the \code{locales} argument.
#'  }
#' }
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
    name = NULL,
    density = list(),
    densityUnits = NULL,
    volumes = NULL,
    mediumFormat = list(),
    mediumNote = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setName
    setName = function(name){
      if(is(name, "character")){
        name <- ISOMediumName$new(value = name)
      }
      self$name <- name
    },
    
    #addDensity
    addDensity = function(density){
      dens <- as.numeric(density)
      if(is.na(dens)){
        stop("The density should be of class 'numeric' or coerceable to 'numeric'")
      }
      density <- dens
      return(self$addListElement("density", density))
    },
    
    #delDensity
    delDensity = function(density){
      dens <- as.numeric(density)
      if(is.na(dens)){
        stop("The density should be of class 'numeric' or coerceable to 'numeric'")
      }
      density <- dens
      return(self$delListElement("density", density))
    },
    
    #setDensityUnits
    setDensityUnits = function(densityUnits){
      self$densityUnits <- densityUnits
    },
    
    #setVolumes
    setVolumes = function(volumes){
      vol <- as.integer(volumes)
      if(is.na(vol)){
       stop("The volumes should be of class 'integer' or coerceable to 'integer'") 
      }
      self$volumes <- vol
    },
    
    #addMediumFormat
    addMediumFormat = function(mediumFormat){
      if(is(mediumFormat, "character")){
        mediumFormat <- ISOMediumFormat$new(value = mediumFormat)
      }
      return(self$addListElement("mediumFormat", mediumFormat))
    },
    
    #delMediumFormat
    delMediumFormat = function(mediumFormat){
      if(is(mediumFormat, "character")){
        mediumFormat <- ISOMediumFormat$new(value = mediumFormat)
      }
      return(self$delListElement("mediumFormat", mediumFormat))
    },
    
    #setMediumNote
    setMediumNote = function(mediumNote, locales = NULL){
      if(!is.null(locales)){
        mediumNote <- self$createLocalisedProperty(mediumNote, locales)
      }
      self$mediumNote <- mediumNote
    }
    
  )                                          
)
