#' ISOImageryProcessStep
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery process step
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery process step
#' @format \code{\link{R6Class}} object.
#'
#' @field processingInformation [\code{\link{ISOImageryProcessing}}]
#' @field output [\code{list} of \code{\link{ISOImagerySource}}]
#' @field report [\code{list} of \code{\link{ISOImageryProcessStepReport}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryProcessStep}}
#'  }
#'  \item{\code{setProcessingInformation(processingInfo)}}{
#'    Set the processing information, object of class \code{\link{ISOImageryProcessing}}
#'  }
#'  \item{\code{addOutput(output)}}{
#'    Add output, object of class \code{\link{ISOSource}}
#'  }
#'  \item{\code{delOutput(output)}}{
#'    Deletes output, object of class \code{\link{ISOSource}}
#'  }
#'  \item{\code{addReport(report)}}{
#'    Add report, object of class \code{\link{ISOImageryProcessStepReport}}
#'  }
#'  \item{\code{delReport(report)}}{
#'    Deletes report, object of class \code{\link{ISOImageryProcessStepReport}}
#'  }
#' }
#' 
#' @section Methods inherited from \code{\link{ISOProcessStep}}:
#' See methods description at \code{\link{ISOProcessStep}}
#' 
#' @examples 
#'    ps <- ISOImageryProcessStep$new()
#'    ps$setDescription("description")
#'    ps$setRationale("rationale")
#'    ps$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
#'    rp <- ISOResponsibleParty$new()
#'    rp$setIndividualName("someone") #and more responsible party properties..
#'    ps$addProcessor(rp)
#'    
#'    #specific methods to ISO 19115-2
#'    process <- ISOImageryProcessing$new()
#'    
#'    #add citation
#'    rp1 <- ISOResponsibleParty$new()
#'    rp1$setIndividualName("someone1")
#'    rp1$setOrganisationName("somewhere1")
#'    rp1$setPositionName("someposition1")
#'    rp1$setRole("pointOfContact")
#'    contact1 <- ISOContact$new()
#'    phone1 <- ISOTelephone$new()
#'    phone1$setVoice("myphonenumber1")
#'    phone1$setFacsimile("myfacsimile1")
#'    contact1$setPhone(phone1)
#'    address1 <- ISOAddress$new()
#'    address1$setDeliveryPoint("theaddress1")
#'    address1$setCity("thecity1")
#'    address1$setPostalCode("111")
#'    address1$setCountry("France")
#'    address1$setEmail("someone1@@theorg.org")
#'    contact1$setAddress(address1)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact1$setOnlineResource(res)
#'    rp1$setContactInfo(contact1)
#'    
#'    #citation
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(ISOdate(2015,1,1))
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp1)
#'    
#'    process$setIdentifier("identifier")
#'    process$setProcedureDescription("some description")
#'    process$addSoftwareReference(ct)
#'    process$addDocumentation(ct)
#'    process$setRunTimeParameters("params")
#'    ps$setProcessingInformation(process)
#'   
#'    #output
#'    trg <- ISOImagerySource$new()
#'    trg$setProcessedLevel("level")
#'    res <- ISOImageryNominalResolution$new()
#'    d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
#'    res$setScanningResolution(d)
#'    trg$setResolution(res)
#'    ps$addOutput(trg)
#'    
#'    #report
#'    rep <- ISOImageryProcessStepReport$new()
#'    rep$setName("report")
#'    rep$setDescription("description")
#'    rep$setFileType("filetype")
#'    ps$addReport(rep)
#'    
#'    xml <- ps$encode()  
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryProcessStep <- R6Class("ISOImageryProcessStep",
  inherit = ISOProcessStep,
  private = list(
    xmlElement = "LE_ProcessStep",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    #+ processingInformation [0..1]: ISOImageryProcessing
    processingInformation = NULL,
    #+ output [0..*]: list of ISOImagerySource
    output = list(),
    #+ report [0..*]: list of ISOImageryProcessStepReport 
    report = list(),

    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setProcessingInformation
    setProcessingInformation = function(processingInfo){
      if(!is(processingInfo, "ISOImageryProcessing")){
        stop("The argument should be an object of class 'ISOImageryProcessing'")
      }
      self$processingInformation <- processingInfo
    },
    
    #addOutput
    addOutput = function(output){
      if(!is(output, "ISOImagerySource")){
        stop("The argument should be an object of class 'ISOImagerySource")
      }
      return(self$addListElement("output", output))
    },
    
    #delOutput
    delOutput = function(output){
      if(!is(output, "ISOImagerySource")){
        stop("The argument should be an object of class 'ISOImagerySource")
      }
      return(self$delListElement("output", output))
    },
    
    #addReport
    addReport = function(report){
      if(!is(report, "ISOImageryProcessStepReport")){
        stop("The argument should be an object of class 'ISOImageryProcessStepReport")
      }
      return(self$addListElement("report", report))
    },
    
    #delReport
    delReport = function(report){
      if(!is(report, "ISOImageryProcessStepReport")){
        stop("The argument should be an object of class 'ISOImageryProcessStepReport")
      }
      return(self$delListElement("report", report))
    }
  )                        
)