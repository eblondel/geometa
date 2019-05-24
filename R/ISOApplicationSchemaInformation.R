#' ISOApplicationSchemaInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO application schema information
#' @return Object of \code{\link{R6Class}} for modelling an ISO ApplicationSchemaInformation
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{ISOCitation}}]
#' @field schemaLanguage [\code{\link{character}}]
#' @field constraintLanguage [\code{\link{character}}]
#' @field schemaAscii [\code{\link{character}}]
#' @field graphicsFile [\code{\link{ISOBinary}}]
#' @field softwareDevelopmentFile [\code{\link{ISOBinary}}]
#' @field softwareDevelopmentFileFormat [\code{\link{character}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOApplicationSchemaInformation}}
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets name, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setSchemaLanguage(schemaLanguage)}}{
#'    Sets schema language
#'  }
#'  \item{\code{setConstraintLanguage(constraintLanguage)}}{
#'    Sets constraint language
#'  }
#'  \item{\code{setSchemaAscii(schemaAscii)}}{
#'    Sets schema Ascii
#'  }
#'  \item{\code{setGraphicsFile(graphicsFile)}}{
#'    Sets graphics file
#'  }
#'  \item{\code{setSoftwareDevelopmentFile(file)}}{
#'    Sets software development file
#'  }
#'  \item{\code{setSoftwareDevelopmentFileFormat(format)}}{
#'    Sets software development file format
#'  }
#' }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOApplicationSchemaInformation <- R6Class("ISOApplicationSchemaInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_ApplicationSchemaInformation",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #name [1..1]
     name = NULL,
     #schemaLanguage [1..1]
     schemaLanguage = NULL,
     #constraintLanguage [1..1]  
     constraintLanguage = NULL,
     #schemaAscii [0..1]
     schemaAscii = NULL,
     #graphicsFile [0..1] 
     graphicsFile = NULL,
     #softwareDevelopmentFile [0..1]
     softwareDevelopmentFile = NULL,
     #softwareDevelopmentFileFormat [0..1]
     softwareDevelopmentFileFormat = NULL,
    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setName
     setName = function(name){
       if(!is(name, "ISOCitation")){
         stop("The argument 'name' should be an object of class 'ISOCitation")
       }
       self$name <- name
     },
     
     #setSchemaLanguage
     setSchemaLanguage = function(schemaLanguage){
       self$schemaLanguage <- schemaLanguage
     },
     
     #setConstraintLanguage
     setConstraintLanguage = function(constraintLanguage){
       self$constraintLanguage <- constraintLanguage
     },
     
     #setSchemaAscii
     setSchemaAscii = function(schemaAscii){
       self$schemaAscii <- schemaAscii
     },
     
     #setGraphicsFile
     setGraphicsFile = function(graphicsFile){
       self$graphicsFile <- ISOBinary$new(value = graphicsFile)
     },
     
     #setSoftwareDevelopmentFile
     setSoftwareDevelopmentFile = function(file){
       self$softwareDevelopmentFile <- ISOBinary$new(value = file)
     },
     
     #setSoftwareDevelopmentFileFormat
     setSoftwareDevelopmentFileFormat = function(format){
       self$softwareDevelopmentFileFormat <- format
     }
   )                        
)