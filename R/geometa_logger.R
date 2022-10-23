#' geometaLogger
#'
#' @docType class
#' @export
#' @keywords logger
#' @return Object of \code{\link{R6Class}} for modelling a simple logger
#' @format \code{\link{R6Class}} object.
#' 
#' @note Logger class used internally by geometa
#'
geometaLogger <- R6Class("geometaLogger",
   private = list(
     logger = function(type, text){
       cat(sprintf("[geometa][%s] %s \n", type, text))
     }
   ),
   public = list(
     #'@description Logger to report information. Used internally
     #'@param text text
     INFO = function(text){private$logger("INFO", text)},
     
     #'@description Logger to report warnings Used internally
     #'@param text text
     WARN = function(text){private$logger("WARN", text)},
     
     #'@description Logger to report errors Used internally
     #'@param text text
     ERROR = function(text){private$logger("ERROR", text)},
     
     #'@description Initializes object
     initialize = function(){}
   )
)