#' ISOImageryProcessStepReport
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery ProcessStepReport
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery ProcessStepReport
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field description [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field fileType [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryProcessStepReport}}
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets a name (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets a description (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setFileType(fileType, locales)}}{
#'    Sets a fileType (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#' }  
#' 
#' @examples
#'    md <- ISOImageryProcessStepReport$new()
#'    md$setName("my_report")
#'    md$setDescription("description")
#'    md$setFileType("md")
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryProcessStepReport <- R6Class("ISOImageryProcessStepReport",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LE_ProcessStepReport",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
  
    #+ name [1..1]: character|ISOLocalisedCharacterString
    name = NULL,
    #+ description [0..1]: character|ISOLocalisedCharacterString
    description = NULL,
    #+ fileType [0..1]: character|ISOLocalisedCharacterString
    fileType = NULL,
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setName
    setName = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      self$name <- name
    },
    
    #setDescription
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    },
    
    #setFileType
    setFileType = function(fileType, locales = NULL){
      if(!is.null(locales)){
        fileType <- self$createLocalisedProperty(fileType, locales)
      }
      self$fileType <- fileType
    }
    
  )
)