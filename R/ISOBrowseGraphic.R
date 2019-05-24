#' ISOBrowseGraphic
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO BrowseGraphic
#' @format \code{\link{R6Class}} object.
#'
#' @field fileName [\code{\link{character}}] file name
#' @field fileDescription [\code{\link{character}}] file description
#' @field fileType [\code{\link{character}}] file type
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, fileName, fileDescription, fileType)}}{
#'    This method is used to instantiate an \code{\link{ISOBrowseGraphic}}
#'  }
#'  \item{\code{setFileName(fileName, locales)}}{
#'    Set file name. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setFileDescription(fileDescription, locales)}}{
#'    Set file description. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setFileType(fileType, locales)}}{
#'    Set file type. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples 
#'  md <- ISOBrowseGraphic$new(
#'   fileName = "http://wwww.somefile.org/png",
#'   fileDescription = "Map Overview",
#'   fileType = "image/png"
#'  )
#'  xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBrowseGraphic <- R6Class("ISOBrowseGraphic",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_BrowseGraphic",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     fileName = NULL,
     fileDescription = NULL,
     fileType = NULL,
     initialize = function(xml = NULL, fileName = NULL, fileDescription = NULL, fileType = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$fileName <- fileName
         self$fileDescription <- fileDescription
         self$fileType <- fileType
       }
     },
     
     #setFileName
     setFileName = function(fileName, locales = NULL){
       self$fileName <- as.character(fileName)
       if(!is.null(locales)){
         self$fileName <- self$createLocalisedProperty(fileName, locales)
       }
     },
     
     #setFileDescription
     setFileDescription = function(fileDescription, locales = NULL){
       self$fileDescription <- as.character(fileDescription)
       if(!is.null(locales)){
         self$fileDescription <- self$createLocalisedProperty(fileDescription, locales)
       }
     },
     
     #setFileType
     setFileType = function(fileType, locales = NULL){
       self$fileType <- as.character(fileType)
       if(!is.null(locales)){
         self$fileType <- self$createLocalisedProperty(fileType, locales)
       }
     }
     
   )                        
)