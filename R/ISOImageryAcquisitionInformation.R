#' ISOImageryAcquisitionInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import XML
#' @export
#' @keywords ISO imagery AcquisitionInformation element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Imagery AcquisitionInformation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'     md = ISOImageryAcquisitionInformation$new()
#'     
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- AcquisitionInformation -- Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryAcquisitionInformation <- R6Class("ISOImageryAcquisitionInformation",
    inherit = ISOAbstractObject,
    private = list(
      document = TRUE,
      xmlElement = "MI_AcquisitionInformation",
      xmlNamespacePrefix = "GMI"
    ),
    public = list(
      
      #'@field instrument instrument [0..*]: ISOImageryInstrument
      instrument = list(),
      #'@field operation operation [0..*]: ISOImageryOperation
      operation = list(),
      #'@field platform platform [0..*]: ISOImageryPlatform
      platform = list(),
      #'@field acquisitionPlan acquisitionPlan [0..*]: ISOImageryPlan
      acquisitionPlan = list(),
      #'@field objective objective [0..*]: ISOImageryObjective
      objective = list(),
      #'@field acquisitionRequirement acquisitionRequirement [0..*]: ISOImageryRequirement
      acquisitionRequirement = list(),
      #'@field environmentalConditions environmentalConditions [0..1]: ISOImageryEnvironmentalRecord
      environmentalConditions = NULL,

      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Adds instrument
      #'@param instrument object of class \link{ISOImageryInstrument}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addInstrument = function(instrument){
        if(!is(instrument, "ISOImageryInstrument")){
          stop("The argument should be an object of class 'ISOImageryInstrument")
        }
        return(self$addListElement("instrument", instrument))
      },

      #'@description Deletes instrument
      #'@param instrument object of class \link{ISOImageryInstrument}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delInstrument = function(instrument){
        if(!is(instrument, "ISOImageryInstrument")){
          stop("The argument should be an object of class 'ISOImageryInstrumenet")
        }
        return(self$delListElement("instrument", instrument))
      },
      
      #'@description Adds operation
      #'@param operation object of class \link{ISOImageryOperation}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addOperation = function(operation){
        if(!is(operation, "ISOImageryOperation")){
          stop("The argument should be an object of class 'ISOImageryOperation")
        }
        return(self$addListElement("operation", operation))
      },
      
      #'@description Deletes operation
      #'@param operation object of class \link{ISOImageryOperation}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delOperation = function(operation){
        if(!is(operation, "ISOImageryOperation")){
          stop("The argument should be an object of class 'ISOImageryOperation")
        }
        return(self$delListElement("operation", operation))
      },
      
      #'@description Adds platform
      #'@param platform object of class \link{ISOImageryPlatform}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addPlatform = function(platform){
        if(!is(platform, "ISOImageryPlatform")){
          stop("The argument should be an object of class 'ISOImageryPlatform")
        }
        return(self$addListElement("platform", platform))
      },
      
      #'@description Deletes platform
      #'@param platform object of class \link{ISOImageryPlatform}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delPlatform = function(platform){
        if(!is(platform, "ISOImageryPlatform")){
          stop("The argument should be an object of class 'ISOImageryPlatform")
        }
        return(self$delListElement("platform", platform))
      },
      
      
      #'@description Adds plan
      #'@param plan object of class \link{ISOImageryPlan}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addPlan = function(plan){
        if(!is(plan, "ISOImageryPlan")){
          stop("The argument should be an object of class 'ISOImageryPlan")
        }
        return(self$addListElement("aquisitionPlan", plan))
      },
      
      #'@description Deletes plan
      #'@param plan object of class \link{ISOImageryPlan}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delPlan = function(plan){
        if(!is(plan, "ISOImageryPlan")){
          stop("The argument should be an object of class 'ISOImageryPlan")
        }
        return(self$delListElement("acquisitionPlan", plan))
      },
      
      #'@description Adds objective
      #'@param objective object of class \link{ISOImageryObjective}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addObjective = function(objective){
        if(!is(objective, "ISOImageryObjective")){
          stop("The argument should be an object of class 'ISOImageryObjective")
        }
        return(self$addListElement("objective", objective))
      },
      
      #'@description Deletes objective
      #'@param objective object of class \link{ISOImageryObjective}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delObjective = function(objective){
        if(!is(objective, "ISOImageryObjective")){
          stop("The argument should be an object of class 'ISOImageryObjective")
        }
        return(self$delListElement("objective", objective))
      },
      
      #'@description Adds requirement
      #'@param requirement object of class \link{ISOImageryRequirement}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addRequirement = function(requirement){
        if(!is(requirement, "ISOImageryRequirement")){
          stop("The argument should be an object of class 'ISOImageryRequirement")
        }
        return(self$addListElement("aquisitionRequirement", requirement))
      },
      
      #'@description Deletes requirement
      #'@param requirement object of class \link{ISOImageryRequirement}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delRequirement = function(requirement){
        if(!is(requirement, "ISOImageryRequirement")){
          stop("The argument should be an object of class 'ISOImageryRequirement")
        }
        return(self$delListElement("acquisitionRequirement", requirement))
      },
      
      #'@description Set environment conditions 
      #'@param conditions object of class \link{ISOImageryEnvironmentalRecord}
      setEnvironmentConditions = function(conditions){
        if(!is(conditions, "ISOImageryEnvironmentalRecord")){
          stop("The argument should be an object of class 'ISOImageryEnvironmentalRecord")
        }
        self$environmentCondition <- conditions
      }
      
    )                        
)
