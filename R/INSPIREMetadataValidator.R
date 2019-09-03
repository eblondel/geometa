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
#'  \item{\code{uploadFile(path)}}{
#'    Upload a XML metadata file to INSPIRE web-service. Method called internally through
#'    \code{getValidationReport}.
#'  }
#'  \item{\code{getValidationReport(obj, file, raw)}}{
#'    Get validation report for a metadata specified either as R object of class
#'    \code{ISOMetadata} (from \pkg{geometa} package) or \code{XMLInternalNode} 
#'    (from \pkg{XML} package), or as XML file, providing the path of the XML file
#'    to be sent to the INSPIRE metadata validator web-service. By default, a summary
#'    report is returned. To append the raw response of INSPIRE validation web-service
#'    to the summary report, set \code{raw = TRUE}.
#'  }
#' }
#' 
#' @examples
#'  \donttest{
#'   inspireValidator <- INSPIREMetadataValidator$new()
#'   inspireReport <- inspireValidator$getValidationReport(obj = ISOMetadata$new())
#'  }
#' 
#' @references 
#'   INSPIRE Reference Validator Web Service (http://inspire.ec.europa.eu/validator/swagger-ui.html)
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
INSPIREMetadataValidator <- R6Class("INSPIREMetadataValidator",
  inherit = geometaLogger,
  private = list(
    host = "http://inspire.ec.europa.eu",
    endpoint = "validator/v2"
  ),
  public = list(
    url = NULL,
    running = FALSE,
    initialize = function(){
      if(!require("httr")){
        stop("The INSPIRE metadata validator requires the installation of 'httr' package")
      }
      self$url <- paste(private$host, private$endpoint, sep = "/")
      ping <- status_code(GET(paste(self$url, "status", sep = "/")))
      self$running <- if(ping==200) TRUE else FALSE
    },
    
    #uploadFile
    uploadFile = function(path){
      req <- POST(
        sprintf("%s/TestObjects?action=upload", self$url),
        body = list(fileupload = httr::upload_file(path = path))
      )
      if(status_code(req)!=200){
        errMsg <- sprintf("Error while upload file '%s' to INSPIRE reference validator web-service", path)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      out <- content(req)
      return(out)
    },
    
    #getValidationReport
    getValidationReport = function(obj = NULL, file = NULL, raw = FALSE){
      
      #check args & read data
      xml_file <- NULL
      if(!is.null(obj)){
        xml_file <- tempfile(fileext = ".xml")
        if(!is(obj, "ISOMetadata") && !is(obj, "XMLInternalNode")){
          stop("'obj' should be an object of class 'ISOMetadata' (from 'geometa') or 'XMLInternalNode' (from 'XML')")
        }
        if(is(obj,"ISOMetadata")){
          obj$save(xml_file, validate = FALSE)
        }else{
          XML::saveXML(xml, file = xml_file)
        }
      }else{
        if(!is.null(file)){
          xml_file <- file
        }else{
          stop("Either object (XML or geometa) or XML file should be provided")
        }
      }
      
      #upload file
      uploaded <- self$uploadFile(path = xml_file)
      
      #post metadata XML to INSPIRE web-service
      self$INFO("Sending metadata file to INSPIRE metadata validation web-service...")
      req <- httr::POST(
        url = sprintf("%s/TestRuns", self$url),
        httr::add_headers(
          "User-Agent" = paste("geometa/",as.character(packageVersion("geometa")),sep=""),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          label = unbox("Test run for ISO/TC 19139:2007 based INSPIRE metadata records."),
          executableTestSuiteIds = "EID59692c11-df86-49ad-be7f-94a1e1ddd8da",
          arguments = list(
            files_to_test = unbox(".*"),
            tests_to_execute = unbox(".*")
          ),
          testObject = list(
            id = unbox(unlist(strsplit(uploaded$testObject$id, "EID"))[2])
          )
        ), auto_unbox = FALSE)
      )
      
      resp <- NULL
      if(httr::status_code(req)!=201){
        errorMsg <- "Error while creating INSPIRE validation  Test run!"
        self$INFO(errorMsg)
        stop(errorMsg)
      }else{
        self$INFO("INSPIRE metadata validation test...")
        pb <- txtProgressBar(min = 0, max = 100, style = 3)
        resp <- content(req)$EtfItemCollection
        testRunId <- resp$testRuns$TestRun$id
        progress <- 0
        while(progress < 100){
          prog_req <- httr::GET(sprintf("%s/TestRuns/%s/progress", self$url, testRunId))
          if(status_code(prog_req)!=200){
            errMsg <- sprintf("Error while getting progress for Test Run '%s'", testRunId)
            #self$ERROR(errMsg)
            stop(errMsg)
          }
          progress <- round(as.integer(content(prog_req)$val) / as.integer(content(prog_req)$max) * 100, 2)
          setTxtProgressBar(pb, value = progress)
          Sys.sleep(1)
        }
        close(pb)
        cat("\n")
        self$INFO("INSPIRE validation test run completed!")
      }
      #report content
      resp <- jsonlite::read_json(resp$ref)
      resp <- resp$EtfItemCollection
      
      sections <- resp$referencedItems$testTaskResults$TestTaskResult$testModuleResults$TestModuleResult$testCaseResults$TestCaseResult
      result_status <- do.call("rbind", lapply(sections, function(section){
        status <- sapply(section$testStepResults$TestStepResult$testAssertionResults$TestAssertionResult, function(x){x$status})
        status <- as.data.frame(table(status), stringsAsFactors =  FALSE)
        return(status)
      }))
      result_status <- aggregate(.~status, data = result_status, FUN = sum)
      failed <- result_status[result_status$status=="FAILED", "Freq"]
      if(length(failed)==0) failed <- "0"
      passed <- result_status[result_status$status=="PASSED", "Freq"]
      if(length(passed)==0) passed <- "0"
      completeness <- passed / sum(result_status$Freq) * 100
      report <- list(
        "Status" = resp$testRuns$TestRun$status,
        "Completeness" = paste0(completeness,"% (", passed," PASSED, ", failed," FAILED)"),
        "Test Run ID" = resp$testRuns$TestRun$id,
        "Log" = resp$testRuns$TestRun$logPath,
        "Ref URI" = resp$ref,
        "HTML Report" = gsub(".json", ".html", resp$ref)
      )
      if(raw) report$raw <- resp
      return(report)
    }
  )
)
