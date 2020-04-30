remove <- function(from, formula, na.remove = FALSE, ...)
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


  if(any(rhs(formula$getsTransf) == "."))
    #if(is.null(formula$getsVars) & any(rhs(formula$getsTransf) == "."))
  {
    #warning("'.' are not allowd in the formula. The original data are returned.")
    from_new <- rep(TRUE,nrow(from))
  }else{
    from_new <- model.frame(formula = formula$getsTransf, data = from,
                            drop.unused.levels = FALSE, na.action = NULL,...)

    from_new <- unlist(!from_new)
}
    if(!is.null(formula$getsVars))
    {
      from <- from[from_new,!(colnames(from) %in% formula$getsVars)]
    }else{
      from <- from[from_new, , drop = FALSE]
    }


  if(isTRUE(na.remove))
  {
    from <- na.omit(from)
  }

  class(from) <- class(from)
  return(from)

}
