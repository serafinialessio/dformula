
add <- function(from, formula, names = NULL,
                position = c("right", "left"),
                na.remove = FALSE, convert_logic = TRUE,...)
{

  position <- match.arg(position, choices = eval(formals(add)$position))


  if(!inherits(from, "data.frame"))
  {
    from <- data.frame(from)
  }

  if(!inherits(formula, "formula"))
  {
    stop("formula must be a formula object")
  }


  formula <- check_formula_add(formula = formula, from = from)

#   whc <- formula$getsVars
#
# if(is.null(formula$getsVars))
# {
#   whc <- colnames(from)
# }

  # from_new <- model.frame(formula = formula$getsTransf, data = from[whc],
  #                         drop.unused.levels = FALSE, na.action = NULL,...)
  from_new <- as.matrix(formula$model_frame)

  if(isTRUE(convert_logic))
  {
    tp <- sapply(1:ncol(from_new), function(x) typeof(from_new[,x]))
    from_new[,which(tp == "logical")] <- as.numeric(from_new[,which(tp == "logical")])
  }

  # if(is.null(na.exclude))
  # {
  #   from_new[is.na(from_new)] <- FALSE
  # }

  nChanges <- ncol(from_new)

  if(is.null(names))
  {
    names <- paste0("Var.",1:nChanges)

  }else if(nChanges != length(names)){

    warning(paste("The new variable are", nChanges, " and new names are ", length(names)),call. = FALSE)
    names <- paste0("Var.",1:nChanges)
  }

  colnames(from_new) <- names

  if(position == "left")
  {
    from <- data.frame(from_new, from, check.names = FALSE, ...)

  }else if(position == "right"){

    from <- data.frame(from, from_new, check.names = FALSE, ...)

  }

  if(isTRUE(na.remove))
  {
    from <- na.omit(from)
  }

  class(from) <- class(from)
  return(from)
}


