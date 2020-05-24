
transform <- function(from, formula, as = NULL,
                      na.remove = FALSE, logic_convert = TRUE, ...)
{
  if(!inherits(from, "data.frame"))
  {
    from <- data.frame(from)
  }

  if(!inherits(formula, "formula"))
  {
    stop("formula must be a formula object")
  }


  objclass <- class(from)

  formula <- check_formula_transf(formula = formula, from = from)

  if(formula$getsTransf == "~.")
  {
    if(isFALSE(na.remove))
    {
      from <- na.omit(from)
    }

    return(from)

  }else if(is.null(formula$getsVars) & length(rhs.vars(formula$getsTransf)) != 1)
  {
    stop("Number of transformations must be equal to number of variables to transform", call. = FALSE)

  }else if(length(formula$getsVars) != ncol(formula$model_frame))
  {
    stop("Number of transformations must be equal to number of variables to transform", call. = FALSE)
  }

  # whc <- formula$getsVars
  # if(!is.null(formula$getsVars))
  # {
  #   whc <- colnames(from)
  # }
  #
  #
  # from_new <- model.frame(formula = formula$getsTransf, data = from[whc],
  #                         drop.unused.levels = FALSE, na.action = NULL,...)
  # from_new <- as.matrix(from_new)
  from_new <- as.matrix(formula$model_frame)

  if(isTRUE(logic_convert))
  {
    tp <- sapply(1:ncol(from_new), function(x) typeof(from_new[,x]))
    from_new[,which(tp == "logical")] <- as.numeric(from_new[,which(tp == "logical")])
  }

  vNames <- formula$getsVars

  cnames <- colnames(from)



  from[,cnames %in% vNames] <- from_new[,, drop = TRUE]

  if(is.null(as))
  {
    colnames(from) <- cnames
  }else{
    if(length(cnames) != as)
    {
      warning("lenght of names is different from the number of column of data")
      as <- NULL
    }
    colnames(from) <- as
  }



  if(isTRUE(na.remove))
  {
    from <- na.omit(from)
  }

  class(from) <- objclass
  return(from)
}
