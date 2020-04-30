

rename <- function(from, formula, ...)
{
  if(!inherits(from, "data.frame"))
  {
    from <- data.frame(from)
  }

  if(!inherits(formula, "formula"))
  {
    stop("formula must be a formula object")
  }

  formula <- check_formula_names(formula = formula)

  vNames <- formula$getsVars
  vNames_New <- formula$getsTransf
  if(is.null(vNames) & all(vNames_New != ".")){vNames <- colnames(from)}

  if(!all(formula$getsTransf == ".") | !is.null(formula$getsVars) )
  {
    nChanges <- length(vNames)

    if(nChanges != length(vNames_New)){
      stop(paste("The name to change are", nChanges, " and new names are ", length(vNames_New)),call. = FALSE)
    }
    name_pos <- colnames(from) %in% vNames
    colnames(from)[name_pos] <- vNames_New
  }


return(from)

}
