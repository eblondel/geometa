#pivot_format
pivot_format <- R6Class("pivot_format",
  public = list(
    id = NULL,
    pkg = NULL,
    reader = NULL,
    checker = NULL,
    constructor = NULL,
    initialize = function(id, pkg, reader = NULL, checker = NULL, constructor = NULL){
      self$id <- id
      self$pkg <- pkg
      self$reader <- reader
      self$checker <- checker
      if(!is.null(constructor)){
        out_constructor <- eval(parse(text=constructor))
        if(!is(out_constructor, "function"))
          stop(sprintf("Invalid constructor '%s', cannot be evaluated as function", constructor))
        self$constructor <- constructor
      }
    }
  )                        
)

#pivot_converter
pivot_converter <- R6Class("pivot_converter",
  public = list(
   from = NULL,
   to = NULL,
   initialize = function(from, to){
     if(!is(from,"pivot_format")) stop("'from' should be an object of class 'pivot_format'")
     if(!is(to,"pivot_format")) stop("'to' should be an object of class 'pivot_format'")
     self$from <- from
     self$to <- to
   }
  )                           
)

list_metadata_formats <- function(){
  formats <- list(
    pivot_format$new(
      id = "geometa", pkg = "geometa", 
      reader = "%s[[%s]]", checker = "!is.null(%s[[%s]])", 
      constructor = "ISOMetadata$new"
    ),
    pivot_format$new(
      id = "eml", pkg = "EML", 
      reader = "%s[[%s]]", checker = "!is.null(%s[[%s]])",
      constructor = "eml$eml"
    ),
    pivot_format$new(
      id = "ncdf", pkg = "ncdf4", 
      reader = "ncatt_get(%s,0,%s)$value", checker = "ncatt_get(%s,0,%s)$hasatt",
      constructor = NULL
    )
  )
  return(formats)
}


#pivot_format_rule
pivot_format_rule <- R6Class("pivot_format_rule",
  public = list(
   items = list(),
   initialize = function(str){
     items <- unlist(strsplit(str,"\\/(?![^[]*])", perl=T))
     the_items <- list()
     if(str != "$" & length(items)>0){
       the_items <- lapply(1:length(items), function(i){
         out_item <- list()
         item <- items[[i]]
         item_parts <- unlist(strsplit(item,"\\$"))
         out_item$class <- item_parts[1]
         if(out_item$class=="") out_item$class <- NULL
         out_item$field <- item_parts[2]
         if(endsWith(out_item$field,"*")){
           out_item$field <- substr(out_item$field,1,nchar(out_item$field)-1)
           out_item$islist <- TRUE
         }else{
           out_item$islist <- FALSE
         }
         
         out_item$attrs <- list()
         hasAttributes <- regexpr("\\[", item)>0 & endsWith(item, "]")
         if(hasAttributes){
           value_splits <- unlist(strsplit(item, "\\[")) #split on '['
           value_splits[2] <- substr(value_splits[2], 1, nchar(value_splits[2])-1) #delete ']'
           out_item$field <- unlist(strsplit(value_splits[1],"\\$"))[2]
           item_attrs <- value_splits[2]
           item_attrs <- unlist(strsplit(item_attrs, "\\|")) # | is the separator of properties
           item_attrs_names <- sapply(item_attrs, function(x){unlist(strsplit(x,"="))[1]})
           item_attrs <- lapply(item_attrs, function(x){unlist(strsplit(x,"="))[2]})
           names(item_attrs) <- item_attrs_names
           out_item$attrs <- item_attrs[!startsWith(names(item_attrs),"$")]
           if(!is.null(out_item$attrs$index)){
             out_item$attrs$index <- as.integer(out_item$attrs$index)
             if(is.na(out_item$attrs$index)){
               stop(sprintf("Format mapping rule '%s' includes an non-integer index attribute", item))
             }
           }
           if(!is.null(out_item$attrs$item)){
             out_item$attrs$item <- as.integer(out_item$attrs$item)
             if(is.na(out_item$attrs$item)){
               stop(sprintf("Format mapping rule '%s' includes an non-integer index attribute", item))
             }
           }
           #property element
           if(any(startsWith(names(item_attrs),"$"))){
             out_item$attrs$element <- list(
               key = names(item_attrs)[startsWith(names(item_attrs),"$")][1],
               value = item_attrs[startsWith(names(item_attrs),"$")][[1]]
             )
           }
         }
         return(out_item)
       })
     }
     self$items <- the_items
   }
  )                        
)

#pivot_format_mapping
pivot_format_mapping <- R6Class("pivot_format_mapping",
  public = list(
    from_format = NULL,
    from = NULL,
    to_format = NULL,
    to = NULL,
    initialize = function(from_format, from, to_format, to){
      self$from_format <- from_format
      self$to_format <- to_format
      if(is(from, "pivot_format_rule")){
        self$from <- from
      }else if(is(from, "character")){
        self$from <- pivot_format_rule$new(str = from)
      }else{
        stop("Argument 'from' should be an object of class 'pivot_format_rule' or a character string")
      }
      if(is(to, "pivot_format_rule")){
        self$to <- to
      }else if(is(to, "character")){
        self$to <- pivot_format_rule$new(str = to)
      }else{
        stop("Argument 'to' should be an object of class 'pivot_format_rule' or a character string")
      }
    }
  )
)

#get_pivot_source_object
get_pivot_source_object <- function(mapping, obj, verbose = FALSE){
  
  if(verbose){
    cat(sprintf("Try to get source object from '%s'\n", mapping$from_format$id))
  }
  
  from_obj <- obj
  invisible(lapply(1:length(mapping$from$items), function(i){
    if(is.null(from_obj)) return(NULL)
    item <- mapping$from$items[[i]]
    
    #condition !is.null(names(from_obj)) makes sure that we are in a named list object (vs. a list of objects)
    #!is.list(from_obj)|!item$islist) & !is.null(names(from_obj))
    if(!is.null(names(from_obj))){
      item_obj <- NULL
      #case of non-list item
      item_check <- sprintf(mapping$from_format$checker, "from_obj", paste0("\"",item$field, "\""))
      if(eval(parse(text=item_check))){
        item_read <- sprintf(mapping$from_format$reader, "from_obj", paste0("\"",item$field, "\""))
        item_obj <- eval(parse(text = item_read))  #from_obj[[item$field]]
      }
      if(is.null(item_obj)){
        from_obj <<- NULL
        return(NULL)
      }
      if(length(item_obj)==0){
        from_obj <<- NULL
        return(NULL)
      }
      if(!item$islist && is.null(item_obj)){
        from_obj <<- NULL
        return(NULL)
      }
      
      if(!item$islist && suppressWarnings(is.na(item_obj))){
        from_obj <<-NULL
        return(NULL)
      }
      
      if(!is.null(item$attrs$sep)){
        item_obj <- switch(class(item_obj),
                           "character" = unlist(strsplit(item_obj, item$attrs$sep)),
                           "list" = lapply(item_obj, function(x){unlist(strsplit(x, item$attrs$sep))})
        )
      }
      if(!is.null(item$attrs$index)){
        if(is.list(item_obj)){
          if(is.null(item$attrs$sep)){
            item_obj <- item_obj[[item$attrs$index]]
          }else{
            item_obj <- lapply(item_obj, function(x){item_obj[[item$attrs$index]]})
          }
        }else{
          item_obj <- item_obj[item$attrs$index]
        }
      }
      if(!is.null(item$attrs$element)){
        if(verbose) cat(sprintf("Try to filter list of items based on element '%s' with value '%s'\n",
                                item$attrs$element$key, item$attrs$element$value))
        element_mapping <- pivot_format_mapping$new(
          from_format = mapping$from_format, 
          from = item$attrs$element$key,
          to_format = mapping$to_format,
          to = "$" #not needed here
        )
        from_element_obj <- get_pivot_source_object(element_mapping, from_obj)
        if(!is.null(from_element_obj)){
          if(from_element_obj != item$attrs$element$value){
            return(NULL)
          }
        }else{
          return(NULL)
        }
      }
      from_obj <<- item_obj
    }else{
      from_obj_list <- lapply(from_obj, function(x){
        item_obj <- NULL
        item_check <- sprintf(mapping$from_format$checker, "x", paste0("\"",item$field, "\""))
        if(eval(parse(text=item_check))){
          item_read <- sprintf(mapping$from_format$reader, "x", paste0("\"",item$field, "\""))
          item_obj <- eval(parse(text = item_read))  #from_obj[[item$field]]
        }
        if(is.null(item_obj)){
          return(NULL)
        }
        if(length(item_obj)==0){
          return(NULL)
        }
        if(!item$islist && is.null(item_obj)){
          return(NULL)
        }
        if(!item$islist && suppressWarnings(is.na(item_obj))){
          return(NULL)
        }
        
        #management of attributes
        if(!is.null(item$attrs$element)){
          if(verbose) cat(sprintf("Try to filter list of items based on element '%s' with value '%s'\n",
                                  item$attrs$element$key, item$attrs$element$value))
          element_mapping <- pivot_format_mapping$new(
            from_format = mapping$from_format, 
            from = item$attrs$element$key,
            to_format = mapping$to_format,
            to = "$" #not needed here
          )
          from_element_obj <- get_pivot_source_object(element_mapping, x)
          if(!is.null(from_element_obj)){
            if(from_element_obj != item$attrs$element$value){
              return(NULL)
            }
          }
        }
        if(!is.null(item$attrs$sep)){
          item_obj <- switch(class(item_obj),
                             "character" = unlist(strsplit(item_obj, item$attrs$sep)),
                             "list" = lapply(item_obj, function(x){unlist(strsplit(x, item$attrs$sep))})
          )
        }
        if(!is.null(item$attrs$index)){
          if(is.list(item_obj)){
            if(is.null(item$attrs$sep)){
              item_obj <- item_obj[[item$attrs$index]]
            }else{
              item_obj <- lapply(item_obj, function(x){x[item$attrs$index]})
            }
          }else{
            item_obj <- item_obj[item$attrs$index]
          }
        }
        return(item_obj)
      })

      if(!is.environment(from_obj_list)) from_obj_list <- from_obj_list[!sapply(from_obj_list, is.null)]
      if(length(from_obj_list)==0) from_obj_list <- NULL
      if(!is.null(item$attrs$item)) from_obj_list <- list(from_obj_list[[item$attrs$item]])
      if(length(from_obj_list)==1 | (item$islist & !is.null(item$attrs$element) & length(from_obj_list)>0)) from_obj_list <- from_obj_list[[1]] #if a list et présence d'un filtre
      from_obj <<- from_obj_list
    }
  }))
  if(is.null(from_obj)) return(NULL) #the value we try to find doesn't exist, we stop here
  return(from_obj)
}

#create_pivot_target_object
create_pivot_target_object <- function(mapping, from_obj, verbose = FALSE){
  out_obj <- NULL
  invisible(lapply(1:length(mapping$to$items), function(j){
    new_obj <- NULL
    item <- rev(mapping$to$items)[[j]]	
    if(!is.null(item$class)){
      class_constructor <- eval(parse(text=item$class))
      new_obj <- switch(class(class_constructor),
                        "R6ClassGenerator" = class_constructor$new(),
                        "list" = list()
      )
    }else{
      new_obj <- list()
    }
    if(j==1){
      new_obj_item <- from_obj
      if(is.list(new_obj_item) & length(new_obj_item)>1){
        new_obj_list <- lapply(new_obj_item, function(x){
          new_obj_x <- NULL
          if(!is.null(item$class)){
            class_constructor <- eval(parse(text=item$class))
            new_obj_x <- switch(class(class_constructor),
                                "R6ClassGenerator" = class_constructor$new(),
                                "list" = list()
            )
          }else{
            new_obj_x <- list()
          }
          
          if(!is.null(item$attrs$element)){
            if(verbose) cat(sprintf("Enrich target object with element '%s' value '%s'\n",item$attrs$element$key,item$attrs$element$value))
            if(!is.null(item$class)){
              element_mapping <- pivot_format_mapping$new(
                from_format = mapping$from_format, 
                from = "$", #not needed here
                to_format = mapping$to_format,
                to = paste0(item$class, item$attrs$element$key)
              )
              to_element_obj <- create_pivot_target_object(element_mapping, item$attrs$element$value)
              new_obj_x <- to_element_obj
            }else{
              eval(parse(text=paste0("new_obj_x",item$attrs$element$key," <- item$attrs$element$value")))
            }
          }
          
          new_obj_x[[item$field]] <- x
          
          if(!is.null(item$attrs$formatter)){
            if(regexpr("<-",item$attrs$formatter)>0){
              if(verbose) cat(sprintf("Applying expression-based formatter '%s' to single list item\n",item$attrs$formatter))
              eval(parse(text=sprintf(item$attrs$formatter,"new_obj_x[[item$field]]")))
            }else{
              if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to single list item\n",item$attrs$formatter))
              new_obj_x[[item$field]] <- eval(parse(text=sprintf(item$attrs$formatter,"new_obj_x[[item$field]]")))
            }
          }
          
          return(new_obj_x)
        })
        new_obj <- new_obj_list
      }else{
        #identify a vector of values (object not a list, not an env), should be coerced as list before setting to a single property in new_obj
        if(!is.environment(new_obj_item) & !is.list(new_obj_item)) if(length(new_obj_item)>1) new_obj_item <- as.list(new_obj_item)
        
        if(!is.null(item$attrs$element)){
          if(verbose) cat(sprintf("Enrich target object with element '%s' value '%s'\n",item$attrs$element$key,item$attrs$element$value))
          if(!is.null(item$class)){
            element_mapping <- pivot_format_mapping$new(
              from_format = mapping$from_format, 
              from = "$", #not needed here
              to_format = mapping$to_format,
              to = paste0(item$class, item$attrs$element$key)
            )
            to_element_obj <- create_pivot_target_object(element_mapping, item$attrs$element$value)
            new_obj <- to_element_obj
          }else{
            eval(parse(text=paste0("new_obj",item$attrs$element$key," <- item$attrs$element$value")))
          }
        }
        
        new_obj[[item$field]] <- new_obj_item
        
        if(!is.null(item$attrs$formatter)){
          if(is.list(new_obj[[item$field]])){
            if(regexpr("<-",item$attrs$formatter)>0){
              if(verbose) cat(sprintf("Applying expression-based formatter '%s' to a list of items\n",item$attrs$formatter))
              new_obj[[item$field]] <- lapply(new_obj[[item$field]], function(x){
                x_out <- x
                eval(parse(text=sprintf(item$attrs$formatter,"x_out")))
                return(x_out)
              })
            }else{
              if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to a list of items\n",item$attrs$formatter))
              new_obj[[item$field]] <- lapply(new_obj[[item$field]], function(x){
                x_out <- eval(parse(text=sprintf(item$attrs$formatter,"x")))
                return(x_out)
              })
            }
          }else{
            if(regexpr("<-",item$attrs$formatter)>0){
              if(verbose) cat(sprintf("Applying expression-based formatter '%s' to single item\n",item$attrs$formatter))
              eval(parse(text=sprintf(item$attrs$formatter,"new_obj[[item$field]]")))
            }else{
              if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to single item\n",item$attrs$formatter))
              new_obj[[item$field]] <- eval(parse(text=sprintf(item$attrs$formatter,"new_obj[[item$field]]")))
            }
          }
        }
        
      }
    }else{
      new_obj_item <- out_obj
      if(item$islist) new_obj[[item$field]] <- list()
      if(is.list(new_obj[[item$field]]) && is.null(names(new_obj_item))){
        #here field content should be the list of items (out_obj)
        new_obj[[item$field]] <- new_obj_item
      }else{
        if(is.null(names(new_obj_item))){
          #here the field content is not a list of items
          #but still we manipulate of list of items, need to propagate the list to the upper level
          new_obj_list <- lapply(new_obj_item, function(x){
            new_obj_x <- NULL
            if(!is.null(item$class)){
              class_constructor <- eval(parse(text=item$class))
              new_obj_x <- switch(class(class_constructor),
                                  "R6ClassGenerator" = class_constructor$new(),
                                  "list" = list()
              )
            }else{
              new_obj_x <- list()
            }
            
            if(!is.null(item$attrs$element)){
              if(verbose) cat(sprintf("Enrich target object with element '%s' value '%s'\n",item$attrs$element$key,item$attrs$element$value))
              if(!is.null(item$class)){
                element_mapping <- pivot_format_mapping$new(
                  from_format = mapping$from_format, 
                  from = "$", #not needed here
                  to_format = mapping$to_format,
                  to = paste0(item$class, item$attrs$element$key)
                )
                to_element_obj <- create_pivot_target_object(element_mapping, item$attrs$element$value)
                new_obj_x <- to_element_obj
              }else{
                eval(parse(text=paste0("new_obj_x",item$attrs$element$key," <- item$attrs$element$value")))
              }
            }
            
            new_obj_x[[item$field]] <- x
            
            if(!is.null(item$attrs$formatter)){
              if(regexpr("<-",item$attrs$formatter)>0){
                if(verbose) cat(sprintf("Applying expression-based formatter '%s' to single list item\n",item$attrs$formatter))
                eval(parse(text=sprintf(item$attrs$formatter,"new_obj_x[[item$field]]")))
              }else{
                if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to single list item\n",item$attrs$formatter))
                new_obj_x[[item$field]] <- eval(parse(text=sprintf(item$attrs$formatter,"new_obj_x[[item$field]]")))
              }
            }
            
            return(new_obj_x)
          })
          new_obj <- new_obj_list
        }else{
          if(!is.null(item$attrs$element)){
            if(verbose) cat(sprintf("Enrich target object with element '%s' value '%s'\n",item$attrs$element$key,item$attrs$element$value))
            if(!is.null(item$class)){
              element_mapping <- pivot_format_mapping$new(
                from_format = mapping$from_format, 
                from = "$", #not needed here
                to_format = mapping$to_format,
                to = paste0(item$class, item$attrs$element$key)
              )
              to_element_obj <- create_pivot_target_object(element_mapping, item$attrs$element$value)
              new_obj <- to_element_obj
            }else{
              eval(parse(text=paste0("new_obj",item$attrs$element$key," <- item$attrs$element$value")))
            }
          }
          
          new_obj[[item$field]] <- new_obj_item
          
          if(!is.null(item$attrs$formatter)){
            if(is.list(new_obj[[item$field]])){
              if(regexpr("<-",item$attrs$formatter)>0){
                if(verbose) cat(sprintf("Applying expression-based formatter '%s' to a list of items\n",item$attrs$formatter))
                new_obj[[item$field]] <- lapply(new_obj[[item$field]], function(x){
                  x_out <- x
                  eval(parse(text=sprintf(item$attrs$formatter,"x_out")))
                  return(x_out)
                })
              }else{
                if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to a list of items\n",item$attrs$formatter))
                new_obj[[item$field]] <- lapply(new_obj[[item$field]], function(x){
                  x_out <- eval(parse(text=sprintf(item$attrs$formatter,"x")))
                  return(x_out)
                })
              }
            }else{
              if(regexpr("<-",item$attrs$formatter)>0){
                if(verbose) cat(sprintf("Applying expression-based formatter '%s' to single item\n",item$attrs$formatter))
                eval(parse(text=sprintf(item$attrs$formatter,"new_obj[[item$field]]")))
              }else{
                if(verbose) cat(sprintf("Applying assignation-based formatter '%s' to single item\n",item$attrs$formatter))
                new_obj[[item$field]] <- eval(parse(text=sprintf(item$attrs$formatter,"new_obj[[item$field]]")))
              }
            }
          }
        }
      }
    }
    out_obj <<- new_obj
  }))
  return(out_obj)
}

#feed_pivot_target_data
feed_pivot_target_data <- function(mapping, out_obj, out, verbose = FALSE){
  #check if out contains object
  #TODO this part needs further consolidation
  last_previous <- ""
  last <- ""
  for(i in 1:length(mapping$to$items)){
    item <- mapping$to$items[[i]]
    last_previous <- last
    last <- paste0(last, "$", item$field)
    previousIsList <- eval(parse(text= paste0("is.null(names(out", last_previous,"))")))
    out_item_code <- paste0("out", last)
    out_item <- try(eval(parse(text=out_item_code)), silent = TRUE)
    empty <- all(class(out_item)=="try-error") | length(out_item)==0
    if(empty | i==length(mapping$to$items)){
      #if(item$islist | length(out_item)==0) if(!endsWith(last,"]]")) eval(parse(text = paste0("out",last," <- list()")))
      out_obj_item_txt <- paste0("out_obj",ifelse(previousIsList, last_previous, last))
      out_obj_item <- eval(parse(text=out_obj_item_txt))
      list_or_env <- is.list(out_obj_item) | is.environment(out_obj_item)
      if(list_or_env){
        if(verbose) cat("Processing a list or environment...\n")
        if(!previousIsList | is.environment(out_obj_item)){
          if(verbose) cat("Filling first time a list of elements...\n")
          eval(parse(text = paste0("out",last," <- out_obj_item")))
        }else{
          if(verbose) cat("Filling an existing a list of elements...\n")
          out_item_previous <- try(eval(parse(text=paste0("out",last_previous))), silent = TRUE)
          eval(parse(text = paste0("out",last_previous," <- lapply(1:length(out_item_previous), function(k){
                                   out_item_new <- out_item_previous[[k]]
                                   out_item_new[[\"",item$field,"\"]] <- out_obj_item[[k]][[\"",item$field,"\"]] #here we take the first list element (~ item$field)
                                   return(out_item_new)
        })")))
				}
        break
      }else{
        if(verbose) cat("Processing a non-list/environment object...\n")
        out_obj_item_native <- out_obj_item
        #if(((is(out_obj_item,"character")&class(out_obj_item)[1]=="character")|is(out_obj_item, "POSIXt")|is(out_obj_item,"Date")) out_obj_item <- paste0("\"",out_obj_item,"\"")
        if(eval(parse(text=paste0("is.null(out",last,")|length(out",last,")==0")))){
          if(verbose) if(verbose) cat("First non-list/environment object creation...\n")
          if(item$islist){
            eval(parse(text = paste0("out",last,"[[",eval(parse(text=paste0("out",last))),"]] <- ",out_obj_item)))
          }else{
            eval(parse(text = paste0("out",last," <- out_obj_item")))
          }
        }else{
          if(is.null(item$attrs$sep)){
            if(verbose) cat("No separator for propriety collapsing, ~ first object creation...\n")
            eval(parse(text = paste0("out",last," <- out_obj_item")))
          }else{
            if(verbose) cat("Collapsing with separator ...\n")
            sep <- paste0("\"",item$attrs$sep,"\"")
            if(eval(parse(text=paste0("out",last))) != out_obj_item_native){
              if(is(out_obj_item,"character")) out_obj_item <- paste0("\"", out_obj_item, "\"")
              eval(parse(text = paste0("out",last," <- paste0(out",last,",",sep,",",out_obj_item,")")))
            } 
          }
        }
      }
      }
  }
  return(out)
}

#apply_format_mapping
apply_format_mapping <- function(mapping, obj, out, verbose = FALSE){
  
  #we go recursively into the mapping rule items to pick up source value
  from_obj <- get_pivot_source_object(mapping, obj, verbose = verbose)
  if(verbose) cat(sprintf("=> Source object read: %s\n", from_obj))
  if(is.null(from_obj)) return(NULL)
  
  #we go recursively into the mapping rule items to create nested elements
  #for last item of target format rule we assign from_obj
  out_obj <- create_pivot_target_object(mapping, from_obj, verbose = verbose)
  if(verbose) cat("=> Target object read:\n")
  if(verbose) print(out_obj)
  
  #feed out target object
  out <- feed_pivot_target_data(mapping, out_obj, out)
  return(out)
  
} 


#convert_metadata
convert_metadata <- function(obj, from, to, mappings, verbose = FALSE){
  
  available_metadata_formats <- list_metadata_formats()
  format_ids <- sapply(available_metadata_formats, function(x){x$id})
  if(!(from %in% format_ids))
    stop(sprintf("The source format '%s' is not among known formats. Check the list of possible formats with list_metadata_formats()", from))
  if(!(to %in% format_ids))
    stop(sprintf("The target format '%s' is not among known formats. Check the list of possible formats with list_metadata_formats()", to))
  
  format_from <- available_metadata_formats[sapply(available_metadata_formats, function(x){x$id == from})][[1]]
  format_to <- available_metadata_formats[sapply(available_metadata_formats, function(x){x$id == to})][[1]]
  if(is.null(format_to$constructor))
    stop("The format '%s' cannot be used as target because no constructor is defined", to)
  out_constructor <- eval(parse(text=format_to$constructor))
  
  if(!is(mappings, "data.frame"))
    stop("The mappings argument is expected to be an object of class 'data.frame'")
  if(!(from %in% colnames(mappings)))
    stop(sprintf("The source format '%s' is not among mappings columns!",from))
  if(!(to %in% colnames(mappings)))
    stop(sprintf("The target format '%s' is not among mappings columns!",to))
  
  #mappings
  mappings <- mappings[,c(from, to)]	
  mappings <- lapply(1:nrow(mappings), function(i){
    if(is.na(mappings[i,from])|is.na(mappings[i,to])) return(NULL)
    return(pivot_format_mapping$new(
      from_format = format_from,
      from = mappings[i,from],
      to_format = format_to,
      to = mappings[i,to]
    ))
  })
  mappings <- mappings[!sapply(mappings, is.null)]
  
  #create target object
  out <- out_constructor()
  
  #run mappings
  mappings_nb <- length(mappings)
  lapply(1:mappings_nb, function(i){
    mapping <- mappings[[i]]
    if(verbose) cat(sprintf("Applying mapping %s\n", i))
    out_mapping <- apply_format_mapping( mapping, obj, out, verbose = verbose)
    if(!is.null(out_mapping)) out <<- out_mapping
  })
  return(out)
}

#' @name registerMappings
#' @aliases registerMappings
#' @title registerMappings
#' @export
#' @description \code{MappingFile} allows to register in  \pkg{geometa} a \code{data.frame} 
#' containing mappings rules to convert from/to other metadata formats (currently 
#' EML/emld objects and NetCDF-CF/ncdf4 objects)
#' 
#' @usage registerMappings(x)
#' 
#' @param x a \code{data.frame} containing the metadata mapping rules
#'
registerMappings <- function(x){
  if(class(x)!="data.frame"){
    stop("The object containing the mapping rules should be a 'data.frame'")
  }
  .geometa.mappings$rules <- x
}

#setters

setAs("ISOMetadata", "emld", function(from){
  out_eml <- convert_metadata(from, from = "geometa", to = "eml", 
                              mappings = .geometa.mappings$rules, verbose = FALSE)
  out_emld <- emld::as_emld(out_eml)
  return(out_emld)
})

setAs("emld", "ISOMetadata", function(from){
  in_from <- from
  class(in_from) <- "list"
  out_md <- convert_metadata(in_from, from = "eml", to = "geometa", 
                             mappings = .geometa.mappings$rules, verbose = FALSE)
  return(out_md)
})

setAs("ncdf4", "ISOMetadata", function(from){
  out_md <- convert_metadata(from, from = "eml", to = "geometa", 
                             mappings = .geometa.mappings$rules, verbose = FALSE)
  return(out_md)
})
