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
#' @field instrument [\code{list} of \code{\link{ISOImageryInstrument}}]
#' @field operation [\code{list} of \code{\link{ISOImageryOperation}}]
#' @field platform [\code{list} of \code{\link{ISOImageryPlatform}}]
#' @field acquisitionPlan [\code{list} of \code{\link{ISOImageryPlan}}]
#' @field objective [\code{list} of \code{\link{ISOImageryObjective}}]
#' @field acquisitionRequirement [\code{list} of \code{\link{ISOImageryRequirement}}]
#' @field environmentConditions [\code{\link{ISOImageryEnvironmentalRecord}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryAcquisitionInformation}}
#'  }
#'  \item{\code{addInstrument(instrument)}}{
#'    Adds instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{delInstrument(instrument)}}{
#'    Deletes instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{addOperation(operation)}}{
#'    Adds instrument, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{delOperation(operation)}}{
#'    Deletes operation, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{addPlatform(platform)}}{
#'    Adds platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{delPlatform(platform)}}{
#'    Deletes platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{addPlan(plan)}}{
#'    Adds plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{delPlan(plan)}}{
#'    Deletes plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{addObjective(objective)}}{
#'    Adds objective, object of class \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{delObjective(objective)}}{
#'    Deletes objective, object of class \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{addRequirement(requirement)}}{
#'    Adds requirement, object of class \code{\link{ISOImageryRequirement}}
#'  }
#'  \item{\code{delRequirement(requirement)}}{
#'    Deletes requirement, object of class \code{\link{ISOImageryRequirement}}
#'  }
#'  \item{\code{setEnvironmentConditions(conditions)}}{
#'    Set environment conditions, object of class \code{\link{ISOImageryEnvironmentalRecord}}
#'  }
#' }
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
      
      #+ instrument [0..*]: ISOImageryInstrument
      instrument = list(),
      #+ operation [0..*]: ISOImageryOperation
      operation = list(),
      #+ platform [0..*]: ISOImageryPlatform
      platform = list(),
      #+ acquisitionPlan [0..*]: ISOImageryPlan
      acquisitionPlan = list(),
      #+ objective [0..*]: ISOImageryObjective
      objective = list(),
      #+ acquisitionRequirement [0..*]: ISOImageryRequirement
      acquisitionRequirement = list(),
      #+ environmentalConditions [0..1]: ISOImageryEnvironmentalRecord
      environmentalConditions = NULL,

      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #addInstrument
      addInstrument = function(instrument){
        if(!is(instrument, "ISOImageryInstrument")){
          stop("The argument should be an object of class 'ISOImageryInstrument")
        }
        return(self$addListElement("instrument", instrument))
      },

      #delInstrument
      delInstrument = function(instrument){
        if(!is(instrument, "ISOImageryInstrument")){
          stop("The argument should be an object of class 'ISOImageryInstrumenet")
        }
        return(self$delListElement("instrument", instrument))
      },
      
      #addOperation
      addOperation = function(operation){
        if(!is(operation, "ISOImageryOperation")){
          stop("The argument should be an object of class 'ISOImageryOperation")
        }
        return(self$addListElement("operation", operation))
      },
      
      #delOperation
      delOperation = function(operation){
        if(!is(operation, "ISOImageryOperation")){
          stop("The argument should be an object of class 'ISOImageryOperation")
        }
        return(self$delListElement("operation", operation))
      },
      
      #addPlatform
      addPlatform = function(platform){
        if(!is(platform, "ISOImageryPlatform")){
          stop("The argument should be an object of class 'ISOImageryPlatform")
        }
        return(self$addListElement("platform", platform))
      },
      
      #delPlatform
      delPlatform = function(platform){
        if(!is(platform, "ISOImageryPlatform")){
          stop("The argument should be an object of class 'ISOImageryPlatform")
        }
        return(self$delListElement("platform", platform))
      },
      
      #addPlan
      addPlan = function(plan){
        if(!is(plan, "ISOImageryPlan")){
          stop("The argument should be an object of class 'ISOImageryPlan")
        }
        return(self$addListElement("aquisitionPlan", plan))
      },
      
      #delPlan
      delPlan = function(plan){
        if(!is(plan, "ISOImageryPlan")){
          stop("The argument should be an object of class 'ISOImageryPlan")
        }
        return(self$delListElement("acquisitionPlan", plan))
      },
      
      #addObjective
      addObjective = function(objective){
        if(!is(objective, "ISOImageryObjective")){
          stop("The argument should be an object of class 'ISOImageryObjective")
        }
        return(self$addListElement("objective", objective))
      },
      
      #delObjective
      delObjective = function(objective){
        if(!is(objective, "ISOImageryObjective")){
          stop("The argument should be an object of class 'ISOImageryObjective")
        }
        return(self$delListElement("objective", objective))
      },
      
      #addRequirement
      addRequirement = function(requirement){
        if(!is(requirement, "ISOImageryRequirement")){
          stop("The argument should be an object of class 'ISOImageryRequirement")
        }
        return(self$addListElement("aquisitionRequirement", requirement))
      },
      
      #delRequirement
      delRequirement = function(requirement){
        if(!is(requirement, "ISOImageryRequirement")){
          stop("The argument should be an object of class 'ISOImageryRequirement")
        }
        return(self$delListElement("acquisitionRequirement", requirement))
      },
      
      #setEnvironmentConditions
      setEnvironmentConditions = function(conditions){
        if(!is(conditions, "ISOImageryEnvironmentalRecord")){
          stop("The argument should be an object of class 'ISOImageryEnvironmentalRecord")
        }
        self$environmentCondition <- conditions
      }
      
    )                        
)
