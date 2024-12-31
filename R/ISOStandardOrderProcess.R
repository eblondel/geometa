#' ISOStandardOrderProcess
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO StandardOrderProcess
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO StandardOrderProcess
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOStandardOrderProcess$new()
#'   md$setFees("fees")
#'   md$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
#'   md$setOrderingInstructions("instructions")
#'   md$setTurnaround("turnaround")
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_StandardOrderProcess}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrd/1.0/mrd/#element_MD_StandardOrderProcess}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStandardOrderProcess <- R6Class("ISOStandardOrderProcess",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_StandardOrderProcess",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRD"
     )
   ),
   public = list(
     
     #'@field fees fees [0..1]: character
     fees = NULL,
     #'@field plannedAvailableDateTime plannedAvailableDateTime [0..1]: 'POSIXct/POSIXlt'
     plannedAvailableDateTime = NULL,
     #'@field orderingInstructions orderingInstructions [0..1]: character
     orderingInstructions = NULL,
     #'@field turnaround turnaround [0..1]: character
     turnaround = NULL,
     #'@field orderOptionsType orderOptionsType [0..1]: ISORecordType (=> ISO 19115-3) 
     orderOptionsType = NULL,
     #'@field orderOptions orderOptions [0..1]: ISORecord (=> ISO 19115-3)
     orderOptions = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml)
     },
     
     #'@description Set fees
     #'@param fees fees
     #'@param locales list of localized texts. Default is \code{NULL}
     setFees = function(fees, locales = NULL){
       self$fees <- as.character(fees)
       if(!is.null(locales)){
         self$fees <- self$createLocalisedProperty(fees, locales)
       }
     },
     
     #'@description Set planned available date time
     #'@param dateTime object of class \link{POSIXct}
     setPlannedAvailableDateTime = function(dateTime){
       if(!all(class(dateTime)==c("POSIXct","POSIXt"))){
         stop("The argument value should be of class ('POSIXct','POSIXt')")
       }
       self$plannedAvailableDateTime <- dateTime
     },
     
     #'@description Set ordering instructions
     #'@param instructions instructions
     #'@param locales list of localized texts. Default is \code{NULL}
     setOrderingInstructions = function(instructions, locales = NULL){
       self$orderingInstructions <- as.character(instructions)
       if(!is.null(locales)){
         self$orderingInstructions <- self$createLocalisedProperty(instructions, locales)
       }
     },
     
     #'@description Set turnaround
     #'@param turnaround turnaround
     #'@param locales list of localized texts. Default is \code{NULL}
     setTurnaround = function(turnaround, locales = NULL){
       self$turnaround <- as.character(turnaround)
       if(!is.null(locales)){
         self$turnaround <- self$createLocalisedProperty(turnaround, locales)
       }
     },
     
     #'@description Set order options type
     #'@param orderOptionsType orderOptionsType object of class \link{ISORecordType} or \link{character}
     setOrderOptionsType = function(orderOptionsType){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(orderOptionsType, "ISORecordType")){
         orderOptionsType = ISORecordType$new(value = orderOptionsType)
       }
       self$orderOptionsType = orderOptionsType
     },
     
     #'@description Set order options
     #'@param orderOptions orderOptions object of class \link{ISORecord} or \link{character}
     setOrderOptions = function(orderOptions){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(orderOptions, "ISORecord")){
         orderOptions = ISORecord$new(value = orderOptions)
       }
       self$orderOptions = orderOptions
     }
   )                        
)
