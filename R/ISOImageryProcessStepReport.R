#' ISOImageryProcessStepReport
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery ProcessStepReport
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery ProcessStepReport
#' @format \code{\link{R6Class}} object.
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
  
    #'@field name name [1..1]: character|ISOLocalisedCharacterString
    name = NULL,
    #'@field description description [0..1]: character|ISOLocalisedCharacterString
    description = NULL,
    #'@field fileType fileType [0..1]: character|ISOLocalisedCharacterString
    fileType = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set name
    #'@param name name
    #'@param locales list of localized texts. Default is \code{NULL}
    setName = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      self$name <- name
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    },
    
    #'@description Set file type
    #'@param fileType file type
    #'@param locales list of localized texts. Default is \code{NULL}
    setFileType = function(fileType, locales = NULL){
      if(!is.null(locales)){
        fileType <- self$createLocalisedProperty(fileType, locales)
      }
      self$fileType <- fileType
    }
    
  )
)