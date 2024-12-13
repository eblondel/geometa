#' ISOQualityResultFile
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO quality result file
#' @return Object of \code{\link{R6Class}} for modelling an ISO quality result file
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_QualityResultFile}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOQualityResultFile <- R6Class("ISOQualityResultFile",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "QualityResultFile",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@field fileName fileName [1]: ISOFileName
     fileName = NULL,
     #'@field fileType fileType [1]: ISOMimeFileType
     fileType = NULL,
     #'@field fileDescription fileDescription [1]: character
     fileDescription = NULL,
     #'@field fileFormat fileFormat [1]: ISOFormat
     fileFormat = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set file name
     #'@param fileName filename object of class \link{ISOFileName}
     setFileName = function(fileName){
       if(!is(fileName, "ISOFileName")){
         stop("The argument 'fileName' should be an object of class 'ISOFileName'")
       }
       self$fileName = fileName
     },
     
     #'@description Set file type
     #'@param fileType fileType object of class \link{ISOMimeFileType} or \link{character}
     setFileType = function(fileType){
       if(!is(fileType, "ISOMimeFileType")){
         if(is(fileType, "character")){
           fileType = ISOMimeFileType$buildFrom(mimetype = fileType)
         }else{
           stop("The argument 'fileType' should be an object of class 'ISOMimeFileType' or 'character'") 
         }
       }
       self$fileType = fileType
     },
     
     #'@description Set file description
     #'@param fileDescription fileDescription object of class \link{character}
     #'@param locales list of localized file description. Default is \code{NULL}
     setFileDescription = function(fileDescription, locales = NULL){
       self$fileDescription <- fileDescription
       if(!is.null(locales)){
         self$fileDescription <- self$createLocalisedProperty(fileDescription, locales)
       }
     },
     
     #'@description Set file format
     #'@param fileFormat fileFormat = object of class \link{ISOFormat} or \link{character}
     setFileFormat = function(fileFormat){
       if(!is(fileFormat, "ISOFormat")){
         if(is(fileFormat, "character")){
           fileFormat = ISOFormat$buildFrom(mimetype = fileFormat)
         }else{
           stop("The argument 'fileFormat' should be an object of class 'ISOFormat' or 'character'") 
         }
       }
       self$fileFormat = fileFormat
     }
   )                        
)