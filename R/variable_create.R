
variable_create <- function(var = NULL, from, startsWith = NULL, endsWith = NULL)
{
  if(is.null(var))
  {
    var <- colnames(from)

    if(!is.null(startsWith) & !is.null(endsWith))
    {
      stop("both true startsWith or endsWith are not allow")
    }


    if(!is.null(startsWith))
    {

      var <- var[startsWith(var, startsWith)]

    }else if(!is.null(endsWith))
    {
      var <- var[endsWith(var, endsWith)]
    }


  }else if(is.character(var)){

    if(!is.null(startsWith) & !is.null(endsWith))
    {
      stop("both true startsWith or endsWith are not allow")
    }


    if(!is.null(startsWith)){

      var <- var[startsWith(var, startsWith)]


    }else if(!is.null(endsWith))
    {
      var <- var[endsWith(var, endsWith)]

    }

  }else if(is.numeric(var))
  {
    var <- colnames(from)[var]
  }

  fm <- paste0(var, collapse = "+")


  return(fm)
}



