#' SWETextEncoding
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE text encoding object
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWETextEncoding <- R6Class("SWETextEncoding",
  inherit = SWEAbstractEncoding,
  private = list(
    xmlElement = "TextEncoding",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@description Initializes a SWE Text Encoding element
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param collapseWhiteSpaces Indicates whether white spaces (i.e. space, tab, CR, LF) 
    #'  should be collapsed with separators when parsing the data stream. Default is \code{TRUE}
    #'@param decimalSeparator Character used as the decimal separator. Default is \code{TRUE}
    #'@param tokenSeparator Character sequence used as the token separator (i.e. between two successive values). Required
    #'@param blockSeparator Character sequence used as the block separator (i.e. between two successive blocks in the data set. 
    #'  The end of a block is reached once all values from the data tree have been encoded once). Required
    #'
    initialize = function(xml = NULL, collapseWhiteSpaces = TRUE, decimalSeparator = ".",
                          tokenSeparator = NULL, blockSeparator = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$setAttr("collapseWhiteSpaces", tolower(collapseWhiteSpaces))
        self$setAttr("decimalSeparator", decimalSeparator)
        self$setAttr("tokenSeparator", tokenSeparator)
        self$setAttr("blockSeparator", blockSeparator)
      }
    }
  )                        
)
