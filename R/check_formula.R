check_formula <- function(formula)
{

  ##### Left
  lf <- lhs(formula)
  lhC <-as.character(lf)

  if(is.null(lf))
  {

  }else if(any(lhC == "."))
  {

    lhs(formula) <- quote(NULL)

  }

  ##### Right

  rh <- rhs(formula)
  rhC <-as.character(rh)

  if(is.null(rh))
  {
    rhs(formula) <- quote(.)

  }else if(any(rhC == "."))
  {

    if(is.null(lhs(formula)))
    {
      formula <- ~.
    }else{
      rhs(formula) <- quote(.)
    }

  }

  if(!is.null(lhs(formula)))
  {
    lhV <- lhs.vars(formula)

    if(is.null(attr(lhV,"term.labels"))){
      check <- NA
      for(i in lhV)
      {
        #nch <- nchar(i)
        check <- (substring(i,1,2) == "I(")

        if(check)
        {
          stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
        }
      }
    }else{
      stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
    }

  }
  if(rhs(formula) != ".")
  {
    rhV <- rhs.vars(formula)

    check <- NA
    for(i in rhV)
    {
      nch <- nchar(i)
      check <- (substring(i,1,2) == "I(" & substring(i,nch,nch) == ")")

      if(!check)
      {
        stop(paste0("The right and of the formula requires the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
      }
    }
  }

  getsVars <- lhs.vars(formula)
  getsTransf <- formula
  lhs(getsTransf) <- quote(NULL)
  return(list(formula = formula,
         getsVars = getsVars,
         getsTransf = getsTransf))


}



check_formula_names <- function(formula)
{
  ##### Left
  lf <- lhs(formula)
  lhC <-as.character(lf)

  if(is.null(lf))
  {

  }else if(any(lhC == "."))
  {

    lhs(formula) <- quote(NULL)
    #stop("'.' together with names in the left side of the formula are not allowed.", call. = FALSE)
  }

  ##### Right

  rh <- rhs(formula)
  rhC <-as.character(rh)

  if(is.null(rh))
  {
    rhs(formula) <- quote(.)
    #stop("NULL values are not allowed. Please indicate the names of the new varaibles in the right end of the formula.", call = FALSE)

  }else if(any(rhC == "."))
  {
    #stop("'.' together with names are not allowed. All variable are considered", call. = FALSE)
  }

  getsVars <- NULL
  getsTransf <- "."

if(!is.null(lhs(formula)) | !rhs(formula)  == ".")
{
  if(!is.null(lhs(formula)))
  {
    lhV <- lhs.vars(formula)

    if(is.null(attr(lhV,"term.labels"))){
      check <- NA
      for(i in lhV)
      {
        #nch <- nchar(i)
        check <- (substring(i,1,2) == "I(")

        if(check)
        {
          stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
        }
      }
    }else{
      stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
    }

  }
  if(!is.null(rhs(formula)))
  {
    rhV <- lhs.vars(formula)

    check <- NA
    for(i in rhV)
    {
      #nch <- nchar(i)
      check <- (substring(i,1,2) == "I(")

      if(check)
      {
        stop(paste0("The right hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
      }
    }
  }
  getsVars <- lhs.vars(formula)
  getsTransf <- rhs.vars(formula)
}




  return(list(formula = formula,
              getsVars = getsVars,
              getsTransf = getsTransf))

}


check_formula_add <- function(formula, from)
{

  ##### Left
  lf <- lhs(formula)
  lhC <-as.character(lf)

  if(!is.null(lf))
  {
    if(any(lhC == "."))
    {

      lhs(formula) <- quote(NULL)

    }
  }

  ##### Right

  rh <- rhs(formula)
  rhC <-as.character(rh)

  if(is.null(rh))
  {
    rhs(formula) <- quote(.)
    warning("Right side of formula does no allow 'NULL'. The original data are returned.")

  }else if(any(rhC == "."))
  {

    warning("Right side of formula does no allow '.'. The original data are returned.")

    if(is.null(lhs(formula)))
    {
      formula <- ~.
    }else{
      rhs(formula) <- quote(.)
    }

  }

  if(!is.null(lhs(formula)))
  {
    lhV <- lhs.vars(formula)

    if(is.null(attr(lhV,"term.labels"))){
      check <- NA
      for(i in lhV)
      {
        #nch <- nchar(i)
        check <- (substring(i,1,2) == "I(")

        if(check)
        {
          stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
        }
      }
    }else{
      stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
    }

  }


  count <- 0


  if(rhs(formula) != ".")
  {
    rhV <- rhs.vars(formula)

    check <- rep(NA,length(rhV))
    #


    for(i in rhV)
    {

      count <-  count + 1
      nch <- nchar(i)
      check[count] <- (substring(i,1,2) == "C(")

      #check <- (substring(i,1,2) == "I(" & substring(i,nch,nch) == ")")

      if(check[count])
      {
        rhV[count] <- paste("constant(","x=",substring(i,3,nch-1), ",nr=",nrow(from),")" ,sep = "")
      }
    }






    if(is.null(lhs(formula)))
    {
      colNames <- colnames(from)
      getsVars <- colNames

      formula <- as.formula(paste("~", paste(rhV,collapse="+"),sep = ""))
      #rhV <- rhs.vars(formula)

    }else{
      colNames <- (lhs.vars(formula))
      getsVars <- lhs.vars(formula)

      formula <- as.formula(paste("~", paste(rhV,collapse="+"),sep = ""))
      #rhV <- rhs.vars(formula)

    }

    count <- length(rhV)
    err <- NULL

    #if(length(colNames) != count & count == 1)
      if(count == 1)

    {

      formula1 <- terms(formula, data = from)
      env <- environment(formula)
      vars <- attr(formula1, "variables")

      err <- try(eval(vars, from, env), silent = TRUE)

      if(class(err) == "try-error")
      {
        formula <- lh_formula_internal(Lnames = colNames, rhs_vars = rhV)
      }
    }

  }

  getsTransf <- formula
  lhs(getsTransf) <- quote(NULL)



  if(is.list(err))
  {

    if(any(check))
    {
      model_frame <- matrix(err[[1]], ncol = length(getsVars), nrow = nrow(from), byrow = FALSE)
    }else{
      model_frame <- matrix(err[[1]], ncol = 1, nrow = nrow(from), byrow = FALSE)
    }


  }else{


    whc <- getsVars

    if(is.null(getsVars))
    {
      whc <- colnames(from)
    }
    model_frame <-   model.frame(formula = getsTransf, data = from[whc],
                                 drop.unused.levels = FALSE, na.action = NULL)
  }


  return(list(formula = formula,
              getsVars = getsVars,
              getsTransf = getsTransf,
              model_frame = model_frame))


}






check_formula_transf <- function(formula, from)
{

  ##### Left
  lf <- lhs(formula)
  lhC <-as.character(lf)

  if(!is.null(lf))
  {
    if(any(lhC == "."))
    {

      lhs(formula) <- quote(NULL)

    }
  }

  ##### Right

  rh <- rhs(formula)
  rhC <-as.character(rh)

  if(is.null(rh))
  {
    rhs(formula) <- quote(.)
    warning("Right side of formula does no allow 'NULL'. The original data are returned.")

  }else if(any(rhC == "."))
  {

    warning("Right side of formula does no allow '.'. The original data are returned.")

    if(is.null(lhs(formula)))
    {
      formula <- ~.
    }else{
      rhs(formula) <- quote(.)
    }

  }

  if(!is.null(lhs(formula)))
  {
    lhV <- lhs.vars(formula)

    if(is.null(attr(lhV,"term.labels"))){
      check <- NA
      for(i in lhV)
      {
        #nch <- nchar(i)
        check <- (substring(i,1,2) == "I(")

        if(check)
        {
          stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
        }
      }
    }else{
      stop(paste0("The left hand of the formula does not accept the following format: I(expression). Check the following entries: \n", i), call. = FALSE)
    }

  }


  count <- 0


  if(rhs(formula) != ".")
  {
    rhV <- rhs.vars(formula)

    check <- rep(NA,length(rhV))
    #


    for(i in rhV)
    {

      count <-  count + 1
      nch <- nchar(i)
      check[count] <- (substring(i,1,2) == "C(")

      #check <- (substring(i,1,2) == "I(" & substring(i,nch,nch) == ")")

      if(check[count])
      {
        rhV[count] <- paste("constant(","x=",substring(i,3,nch-1), ",nr=",nrow(from),")" ,sep = "")
      }
    }






    if(is.null(lhs(formula)))
    {
      colNames <- colnames(from)
      getsVars <- colNames

      formula <- as.formula(paste("~", paste(rhV,collapse="+"),sep = ""))
      #rhV <- rhs.vars(formula)

    }else{
      colNames <- (lhs.vars(formula))
      getsVars <- lhs.vars(formula)

      formula <- as.formula(paste("~", paste(rhV,collapse="+"),sep = ""))
      #rhV <- rhs.vars(formula)

    }

    count <- length(rhV)
    err <- NULL

    #if(length(colNames) != count & count == 1)
    if(count == 1)

    {

      formula1 <- terms(formula, data = from)
      env <- environment(formula)
      vars <- attr(formula1, "variables")

      err <- try(eval(vars, from, env), silent = TRUE)

      if(class(err) == "try-error")
      {
        formula <- lh_formula_internal(Lnames = colNames, rhs_vars = rhV)
      }
    }

  }

  getsTransf <- formula
  lhs(getsTransf) <- quote(NULL)



  if(is.list(err))
  {

    # if(count == 1 & isTRUE(check[1]) & !is.null(getsVars))
    # {
    #   model_frame <- matrix(err[[1]], ncol = length(getsVars), nrow = nrow(from), byrow = FALSE)
    # }else{
    #   model_frame <- matrix(err[[1]], ncol = 1, nrow = nrow(from), byrow = FALSE)
    # }

    model_frame <- matrix(err[[1]], ncol = length(getsVars), nrow = nrow(from), byrow = FALSE)

  }else{


    whc <- getsVars

    if(is.null(getsVars))
    {
      whc <- colnames(from)
    }
    model_frame <-   model.frame(formula = getsTransf, data = from[whc],
                                 drop.unused.levels = FALSE, na.action = NULL)
  }


  return(list(formula = formula,
              getsVars = getsVars,
              getsTransf = getsTransf,
              model_frame = model_frame))


}


lh_formula_internal <- function(Lnames, rhs_vars)
{

  lh <- paste(Lnames, collapse = "+")

  ncfunc <- nchar(rhs_vars)
  #substring(rhs_vars,ncfunc,ncfunc)

  if(substring(rhs_vars,1,2) == "I(")
  {
    if(substring(rhs_vars,ncfunc-1,ncfunc-1) == ")")
    {

      func <- substring(rhs_vars, 3,ncfunc-1)

      temp <- grep("\\(",unlist(strsplit(func, "")))[1]
      subL <- substring(func, 1,temp)
      subR <- substring(func, temp+1,nchar(func))
      rh <- paste0("I(",subL,Lnames,",",subR,")", collapse  = "+")

    }else{

      func <- substring(rhs_vars, 3, ncfunc-1)
      rh <- paste0("I(",func,"(",Lnames,"))", collapse  = "+")
    }
  }else{

    if(substring(rhs_vars,ncfunc,ncfunc) == ")")
    {

      func <- rhs_vars

      temp <- grep("\\(",unlist(strsplit(func, "")))[1]
      subL <- substring(func, 1,temp)
      subR <- substring(func, temp+1,nchar(func))
      rh <- paste0(subL,Lnames,",",subR, collapse  = "+")

    }else{

      func <- substring(rhs_vars, 1, ncfunc-2)
      rh <- paste0(func,"(",Lnames,")", collapse  = "+")
    }


  }


  formula <- as.formula(paste(lh,rh,sep = "~"))

  return(formula)
}



constant <- function(x, nr,...)
{

  if((length(x)==1))
  {
    x <- rep(x, nr)
  }
  #if(missing(x)){stop(" ",call. = FALSE)}
  return(x)
}
