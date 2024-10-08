#' ISOImageryRevision
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery revision
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery revision
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Revision}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryRevision <- R6Class("ISOImageryRevision",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MI_Revision",
      xmlNamespacePrefix = list(
        "19115-3" = "MAC"
      )
    ),
    public = list(
      
      #'@field description description [0..1] : character
      description = NULL,
      #'@field author author [1..1] : ISOAbstractResponsibility
      author = NULL,
      #'@field dateInfo dateInfo [1..1] : ISOAbstractTypedDate
      dateInfo = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Set description
      #'@param description description
      #'@param locales list of localized editions. Default is \code{NULL}
      setDescription = function(description, locales = NULL){
        if(!is.null(locales)){
          description = self$createLocalisedProperty(description, locales)
        }else{
          description = as.character(description)
        }
        self$description = description
      },
      
      #'@description Set author
      #'@param author author
      setAuthor = function(author){
        if(!is(author, "ISOAbstractResponsibility")){
          stop("The argument should be an object inheriting class 'ISOAbstractResponsibility")
        }
        self$author = author
      },
      
      #'@description Set date info
      #'@param dateInfo dateInfo
      setDateInfo = function(dateInfo){
        if(!is(author, "ISOAbstractTypedDate")){
          stop("The argument should be an object inheriting class 'ISOAbstractTypedDate")
        }
        self$dateInfo = dateInfo
      }
      
    )                        
)