#' ISOApplicationSchemaInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO application schema information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ApplicationSchemaInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ApplicationSchemaInformation}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mas/1.0/mas/#element_MD_ApplicationSchemaInformation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOApplicationSchemaInformation <- R6Class("ISOApplicationSchemaInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_ApplicationSchemaInformation",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MAS"
     )
   ),
   public = list(
     
     #'@field name name [1..1]
     name = NULL,
     #'@field schemaLanguage chemaLanguage [1..1]
     schemaLanguage = NULL,
     #'@field constraintLanguage constraintLanguage [1..1]  
     constraintLanguage = NULL,
     #'@field schemaAscii schemaAscii [0..1]
     schemaAscii = NULL,
     #'@field graphicsFile graphicsFile [0..1]: ISOOnlineResource
     graphicsFile = NULL,
     #'@field softwareDevelopmentFile softwareDevelopmentFile [0..1]: ISOOnlineResource
     softwareDevelopmentFile = NULL,
     #'@field softwareDevelopmentFileFormat softwareDevelopmentFileFormat [0..1]
     softwareDevelopmentFileFormat = NULL,
    
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set name
     #'@param name name
     setName = function(name){
       if(!is(name, "ISOCitation")){
         stop("The argument 'name' should be an object of class 'ISOCitation")
       }
       self$name <- name
     },
     
     #'@description Set schema language
     #'@param schemaLanguage schema language
     setSchemaLanguage = function(schemaLanguage){
       self$schemaLanguage <- schemaLanguage
     },
     
     #'@description Set constraint language
     #'@param constraintLanguage constraint language
     setConstraintLanguage = function(constraintLanguage){
       self$constraintLanguage <- constraintLanguage
     },
     
     #'@description Set schema Ascii
     #'@param schemaAscii schema Ascii
     setSchemaAscii = function(schemaAscii){
       self$schemaAscii <- schemaAscii
     },
     
     #'@description Set graphics file
     #'@param graphicsFile graphics file
     setGraphicsFile = function(graphicsFile){
       self$graphicsFile <- ISOBinary$new(value = graphicsFile)
     },
     
     #'@description Set software development file
     #'@param file file
     setSoftwareDevelopmentFile = function(file){
       self$softwareDevelopmentFile <- ISOBinary$new(value = file)
     },
     
     #'@description Set software development file format
     #'@param format file format
     setSoftwareDevelopmentFileFormat = function(format){
       self$softwareDevelopmentFileFormat <- format
     }
   )                        
)
