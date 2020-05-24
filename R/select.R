select <- function(from, formula = .~., as = NULL,
                   na.remove = FALSE, na.return = FALSE, ...)
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
  }else{

    from <- from[from_new, ,drop = FALSE]

  }

  nChanges <- ncol(from_new)

  if(!is.null(as)){

    if(nChanges != length(as))
    {
      warning(paste("The new variable are", nChanges, " and new names are ", length(as)),call. = FALSE)
      as <- NULL
    }

    colnames(from_new) <- as

  }


  if(isTRUE(na.return))
  {

    from <- from[!complete.cases(from),]

  }else{

    if(isTRUE(na.remove))
    {
      from <- na.omit(from)
    }

  }



  class(from) <- class(from)
  return(from)

}

