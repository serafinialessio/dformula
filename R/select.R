select <- function(from, formula, na.remove = FALSE, ...)
{
  if(!inherits(from, "data.frame"))
  {
    from <- data.frame(from)
  }

  if(!inherits(formula, "formula"))
  {
    stop("formula must be a formula object")
  }

  formula <- check_formula(formula = formula)


  if(!any(rhs(formula$getsTransf) == "."))
  {
    from_new <- model.frame(formula = formula$getsTransf, data = from,
                            drop.unused.levels = FALSE, na.action = NULL,...)
    from_new <- unlist(from_new)

  }else{
    from_new <- rep(TRUE,nrow(from))
  }


  if(!is.null(formula$getsVars))
  {
    from <- from[from_new,formula$getsVars, drop = FALSE]
  }

  if(isTRUE(na.remove))
  {
    from <- na.omit(from)
  }


  class(from) <- class(from)
  return(from)

}



# remove <- function(from, formula, na.remove = FALSE, na.exclude = FALSE, ...)
# {
#
#   if(!inherits(from, "data.frame"))
#   {
#     from <- data.frame(from)
#   }
#
#   if(!inherits(formula, "formula"))
#   {
#     stop("formula must be a formula object")
#   }
#
#   if(isTRUE(na.remove))
#   {
#     na.remove <- na.omit
#
#   }else if(isFALSE(na.remove))
#   {
#     na.remove <- NULL
#   }
#
#   formula <- check_formula(formula = formula)
#
#   if(is.null(formula$getsVars) & any(rhs(formula$getsTransf) == "."))
#   {
#     warning("'.' are not allowd in the formula. The original data are returned.")
#
#   }else{
#     from_new <- model.frame(formula = formula$getsTransf, data = from,
#                             drop.unused.levels = FALSE, na.action = na.remove,...)
#
#     from_new <- unlist(!from_new)
#
#     if(isTRUE(na.exclude))
#     {
#       from_new[is.na(from_new)] <- FALSE
#     }
#
#     if(!is.null(formula$getsVars))
#     {
#       from <- from[from_new,!(colnames(from) %in% formula$getsVars)]
#     }else{
#       from <- from[from_new, , drop = FALSE]
#     }
#   }
#
#   if(isTRUE(na.remove))
#   {
#     from <- na.omit(from)
#   }
#
#   class(from) <- class(from)
#   return(from)
#
# }
#
