#' INSPIREMetadataValidator
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords INSPIRE metadata validator validation
#' @return Object of \code{\link{R6Class}} for setting an INSPIREMetadataValidator
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate an INSPIRE Metadata validator
#'  }
#'  \item{\code{getValidationReport(obj, file, raw)}}{
#'    Get validation report for a metadata specified either as R object of class
#'    \code{ISOMetadata} (from \pkg{geometa} package) or \code{XMLInternalDocument} 
#'    (from \pkg{XML} package), or as XML file, providing the path of the XML file
#'    to be sent to the INSPIRE metadata validator web-service. By default, a summary
#'    report is returned. To append the raw response of INSPIRE validation web-service
#'    to the summary report, set \code{raw = TRUE}.
#'  }
#' }
#' 
#' @references 
#'   INSPIRE Geoportal Metadata Validator Web Service (http://inspire-geoportal.ec.europa.eu/validator2/html/usingaswebservice.html)
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
INSPIREMetadataValidator <- R6Class("INSPIREMetadataValidator",
   private = list(
     host = "http://inspire-geoportal.ec.europa.eu",
     endpoint = "GeoportalProxyWebServices/resources/INSPIREResourceTester"
   ),
   public = list(
     url = NULL,
     initialize = function(){
       if(!require("httr")){
         stop("The INSPIRE metadata validator requires the installation of 'httr' package")
       }
       self$url <- paste(private$host, private$endpoint, sep = "/")
     },
     
     #getValidationReport
     getValidationReport = function(obj = NULL, file = NULL, raw = FALSE){
       
       #check args & read data
       xmlObj <- NULL
       if(!is.null(obj)){
         if(is(obj,"ISOMetadata")){
          xmlObj <- md$encode()
         }else if(is(obj,"XMLInternalDocument")){
           xmlObj <- obj 
         }else{
           stop("'obj' should be an object of class 'ISOMetadata' (from 'geometa') or 'XMLInternalDocument' (from 'XML')")
         }
       }else{
         if(!is.null(file)){
           xmlObj <- try(XML::xmlParse(file))
           if(class(xmlObj)=="try-error"){
             stop("Error while parsing XML file")
           }
         }else{
           stop("Either object (XML or geometa) or XML file should be provided")
         }
       }
       
       #post metadata XML to INSPIRE web-service
       req <- httr::POST(
         url = self$url,
         httr::add_headers("Accept" = "application/json", "Content-Type" = "text/plain"),
         body = as(xmlObj, "character")
       )
       if(httr::status_code(req)!=201){
         stop("Error during communication with INSPIRE validation web-service")
       }
       loc <- httr::headers(req)$location
       resp <- content(req)$value
       report <- list(
         url = loc,
         creationDate = as.POSIXct(resp$AuditRecord$CreationDate/1000, origin = "1970-01-01"),
         lastUpdateDate = as.POSIXct(resp$AuditRecord$LastUpdateDate/1000, origin = "1970-01-01"),
         validity = list(
           completeness = list(percentage = resp$CompletenessIndicator, status = resp$CompletenessIndicator == 100),
           interoperability = list(percentage = resp$InteroperabilityIndicator, status = resp$InteroperabilityIndicator == 100)
         )
       )
       if(raw) report$raw <- resp
       return(report)
     }
   )
)
