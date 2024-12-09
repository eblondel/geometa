#' ISOBrowseGraphic
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO BrowseGraphic
#' @format \code{\link{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_BrowseGraphic}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_MD_BrowseGraphic}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBrowseGraphic <- R6Class("ISOBrowseGraphic",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_BrowseGraphic",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCC"
     )
   ),
   public = list(
     #'@field fileName file name
     fileName = NULL,
     #'@field fileDescription file description
     fileDescription = NULL,
     #'@field fileType file type
     fileType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param fileName file name
     #'@param fileDescription file description
     #'@param fileType file type
     initialize = function(xml = NULL, fileName = NULL, fileDescription = NULL, fileType = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$fileName <- fileName
         self$fileDescription <- fileDescription
         self$fileType <- fileType
       }
     },
     
     #'@description Set file name
     #'@param fileName file name
     #'@param locales a list of localized names. Default is \code{NULL}
     setFileName = function(fileName, locales = NULL){
       self$fileName <- as.character(fileName)
       if(!is.null(locales)){
         self$fileName <- self$createLocalisedProperty(fileName, locales)
       }
     },
     
     #'@description Set file description
     #'@param fileDescription file description
     #'@param locales a list of localized descriptions. Default is \code{NULL}
     setFileDescription = function(fileDescription, locales = NULL){
       self$fileDescription <- as.character(fileDescription)
       if(!is.null(locales)){
         self$fileDescription <- self$createLocalisedProperty(fileDescription, locales)
       }
     },
     
     #'@description Set file type
     #'@param fileType file type
     #'@param locales a list of localized types. Default is \code{NULL}
     setFileType = function(fileType, locales = NULL){
       self$fileType <- as.character(fileType)
       if(!is.null(locales)){
         self$fileType <- self$createLocalisedProperty(fileType, locales)
       }
     }
     
   )                        
)