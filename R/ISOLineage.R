#' ISOLineage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO lineage
#' @return Object of \code{\link{R6Class}} for modelling an ISO Lineage
#' @format \code{\link{R6Class}} object.
#'
#' @field statement
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLineage
#'  }
#'  \item{\code{setStatement(statement)}}{
#'    Sets the statement
#'  }
#' }
#' 
#' @examples 
#'   lineage <- ISOLineage$new()
#'   lineage$setStatement("statement")
#'   xml <- lineage$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLineage<- R6Class("ISOLineage",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "LI_Lineage",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    statement = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setStatement
    setStatement = function(statement){
      self$statement <- as.character(statement)
    }
  )                        
)