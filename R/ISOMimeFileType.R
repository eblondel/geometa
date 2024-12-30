#' ISOMimeFileType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO mime file type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO MimeFileType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOMimeFileType$new(type = "somemimetype", name = "Mime type name")
#'   xml <- md$encode()
#' 
#' @references
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmx/1.0/gmx/#element_MimeFileType}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gcx/1.0/gcx/#element_MimeFileType}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMimeFileType <- R6Class("ISOMimeFileType",
     inherit = ISOAbstractObject,
     private = list(
       xmlElement = "MimeFileType",
       xmlNamespacePrefix = list(
         "19139" = "GMX",
         "19115-3" = "GCX"
       )
     ),
     public = list(
       
       #'@description Initializes object
       #'@param xml object of class \link{XMLInternalNode-class}
       #'@param type type
       #'@param name name
       initialize = function(xml = NULL, type = NULL, name = NULL){
         super$initialize(xml = xml)
         if(!is.null(type) & !is.null(name)){
          self$attrs$type <- type
          self$value <- name
         }
       },
       
       #'@description Set name
       #'@param name name
       setName = function(name){
         self$value <- name
       },
       
       #'@description Set type
       #'@param type type
       setType = function(type){
         self$attrs$type <- type
       }
     )                        
)

ISOMimeFileType$buildFrom = function(mimetype, add_iana_uri = TRUE){

  mime1 <- data.frame(mime::mimemap)
  mime1 <- data.frame(ext = row.names(mime1), mime = as.character(mime1[,1L]),
                      stringsAsFactors = FALSE)
  mime2 <- data.frame(mime:::mimeextra)
  mime2 <- data.frame(ext = row.names(mime2), mime = as.character(mime2[,1L]),
                      stringsAsFactors = FALSE)
  mime.list <- rbind(mime1, mime2)
  
  mime.sel <- mime.list[mime.list$mime == mimetype,]
  if(nrow(mime.sel)==0) mime.sel <- mime.list[mime.list$ext == mimetype,]
  if(nrow(mime.sel)>1) mime.sel <- mime.sel[1L,]
  if(nrow(mime.sel)==0) return(NULL)
  
  mft <- ISOMimeFileType$new()
  mft$setType(mime.sel$mime)
  mft$setName(mime.sel$mime)
  
  return(mft)
}
