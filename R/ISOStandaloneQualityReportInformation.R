#' ISOStandaloneQualityReportInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO standalone quality report information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO standalone quality report information
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_StandaloneQualityReportInformation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStandaloneQualityReportInformation <- R6Class("ISOStandaloneQualityReportInformation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DQ_StandaloneQualityReportInformation",
    xmlNamespacePrefix = list(
      "19115-3" = "MDQ"
    )
  ),
  public = list( 
    
    #'@field reportReference reportReference [1]: ISOCitation
    reportReference = NULL,
    #'@field abstract abstract [1]: character
    abstract = NULL,
    #'@field elementReport elementReport [0..*]: ISODataQualityAbstractElement
    elementReport = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description set Report reference
    #'@param reportReference object of class \link{ISOCitation}
    setReportReference = function(reportReference){
      if(!is(reportReference, "ISOCitation")){
        stop("The argument should be an object of class 'ISOCitation'")
      }
      self$reportReference = reportReference
    },
    
    #'@description Set abstract
    #'@param abstract abstract
    #'@param locales list of localized abstract Default is \code{NULL}
    setAbstract = function(abstract, locales = NULL){
      self$abstract <- abstract
      if(!is.null(locales)){
        self$abstract <- self$createLocalisedProperty(abstract, locales)
      }
    },
    
    #'@description Adds element report
    #'@param elementReport object of class \link{ISODataQualityAbstractElement}
    #''@return \code{TRUE} if added, \code{FALSE} otherwise
    addElementReport = function(elementReport){
      if(!is(elementReport, "ISODataQualityAbstractElement")){
        stop("The argument 'elementReport' should inherit class 'ISODataQualityAbstractElement'")
      }
      return(self$addListElement("elementReport", elementReport))
    },
    
    #'@description Deletes element report
    #'@param elementReport object of class \link{ISODataQualityAbstractElement}
    #''@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delElementReport = function(elementReport){
      if(!is(elementReport, "ISODataQualityAbstractElement")){
        stop("The argument 'elementReport' should inherit class 'ISODataQualityAbstractElement'")
      }
      return(self$addListElement("elementReport", elementReport))
    }
    
  )
)
