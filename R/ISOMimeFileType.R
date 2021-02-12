#' ISOMimeFileType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO mime file type
#' @return Object of \code{\link{R6Class}} for modelling an ISO MimeFileType
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, type, name)}}{
#'    This method is used to instantiate an \code{\link{ISOMimeFileType}}
#'  }
#'  \item{\code{setName(name)}}{
#'    Set name
#'  }
#'  \item{\code{setType(type)}}{
#'   Set type
#'  }
#' }
#' 
#' @examples
#'   md <- ISOMimeFileType$new(type = "somemimetype", name = "Mime type name")
#'   xml <- md$encode()
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMimeFileType <- R6Class("ISOMimeFileType",
     inherit = ISOAbstractObject,
     private = list(
       xmlElement = "MimeFileType",
       xmlNamespacePrefix = "GMX"
     ),
     public = list(
       initialize = function(xml = NULL, type = NULL, name = NULL){
         super$initialize(xml = xml)
         if(!is.null(type) & !is.null(name)){
          self$attrs$type <- type
          self$value <- name
         }
       },
       
       #setName
       setName = function(name){
         self$value <- name
       },
       
       #setType
       setType = function(type){
         self$attrs$type <- type
       }
     )                        
)

ISOMimeFileType$buildFrom = function(ext, add_iana_uri = TRUE){

  mime1 <- data.frame(mime::mimemap)
  mime1 <- data.frame(ext = row.names(mime1), mime = as.character(mime1[,1L]),
                      stringsAsFactors = FALSE)
  mime2 <- data.frame(mime:::mimeextra)
  mime2 <- data.frame(ext = row.names(mime2), mime = as.character(mime2[,1L]),
                      stringsAsFactors = FALSE)
  mime.list <- rbind(mime1, mime2)
  
  mime.sel <- mime.list[mime.list$ext == ext,]
  if(nrow(mime.sel)==0) return(NULL)
  
  mft <- ISOMimeFileType$new()
  mft$setType(mime.sel$mime)
  mft$setName(mime.sel$mime)
  
  return(mft)
}