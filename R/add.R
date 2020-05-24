
add <- function(from, formula, as = NULL,
                position = c("right", "left"),
                na.remove = FALSE, logic_convert = TRUE,...)
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
  from_new <- as.matrix(formula$model_frame)

  if(isTRUE(logic_convert))
  {
    tp <- sapply(1:ncol(from_new), function(x) typeof(from_new[,x]))
    from_new[,which(tp == "logical")] <- as.numeric(from_new[,which(tp == "logical")])
  }

  nChanges <- ncol(from_new)

  if(is.null(as))
  {
    as <- paste0("Var.",1:nChanges)

  }else if(nChanges != length(as)){

    warning(paste("The new variable are", nChanges, " and new names are ", length(as)),call. = FALSE)
    as <- paste0("Var.",1:nChanges)
  }

  colnames(from_new) <- as

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


