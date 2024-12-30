#' ISOReleasability
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO releasability
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOReleasability
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mco/1.0/mco/#element_MD_Releasability}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReleasability <- R6Class("ISOReleasability",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_Releasability",
      xmlNamespacePrefix = list(
        "19115-3" = "MCO"
      )
    ),
    public = list(
      
      #'@field addressee addressee [0..*]: ISOAbstractResponsibility
      addressee = list(),
      #'@field statement statement [0..1]: character
      statement = NULL,
      #'@field disseminationConstraints disseminationConstraints [0..*]: ISORestriction
      disseminationConstraints = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Adds addressee
      #'@param addressee addressee of class \link{ISOAbstractResponsibility}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addAddressee = function(addressee){
        if(!is(addressee, "ISOAbstractResponsibility")){
          stop("The argument should be an object inheriting class 'ISOAbstractResponsibility'")
        }
        return(self$addListElement("addressee", addressee))
      },
      
      #'@description Deletes addressee
      #'@param addressee addressee of class \link{ISOAbstractResponsibility}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delAddressee = function(addressee){
        if(!is(addressee, "ISOAbstractResponsibility")){
          stop("The argument should be an object inheriting class 'ISOAbstractResponsibility'")
        }
        return(self$delListElement("addressee", addressee))
      },
    
      #'@description Set statement
      #'@param statement statement
      #'@param locales list of localized texts. Default is \code{NULL}
      setStatement = function(statement, locales = NULL){
        self$statement = statement
        if(!is.null(locales)){
          self$statement <- self$createLocalisedProperty(statement, locales)
        }
      },
      
      #'@description Adds constraint
      #'@param constraint constraint of class \link{ISORestriction}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addConstraint = function(constraint){
        if(!is(constraint, "ISORestriction")){
          if(is(constraint, "character")){
            constraint = ISORestriction$new(value = constraint)
          }else{
            stop("The argument should be an object of class 'ISORestriction' or 'character'")
          }
        }
        return(self$addListElement("disseminationConstraints", constraint))
      },
      
      #'@description Deletes constraint
      #'@param constraint constraint of class \link{ISORestriction}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delConstraint = function(constraint){
        if(!is(constraint, "ISORestriction")){
          if(is(constraint, "character")){
            constraint = ISORestriction$new(value = constraint)
          }else{
            stop("The argument should be an object of class 'ISORestriction' or 'character'")
          }
        }
        return(self$delListElement("disseminationConstraints", constraint))
      }
      
    )                        
)
