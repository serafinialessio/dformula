\name{add}
\alias{add}

\title{Add variables}

\description{
Add new variables by mutating the input variables using a formula.
}
\usage{
add(from, formula, as = NULL,
    position = c("right", "left"),
    na.remove = FALSE, logic_convert = TRUE,...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
  \item{as}{a character vector with names of new variables.}
  \item{position}{if the new varaibles are positioned at the begining (\code{right}) or at the left (\code{left}) of the data in input.}
  \item{na.remove}{a logical value indicating whether NA values should be removed.}
\item{logic_convert}{logical value indicating if the new logical varaible are convertet to \code{0} or \code{1}}
  \item{\dots}{further arguments}
}
\details{

The formula is composed of two part:

\code{ ~ new_variables}

the right-hand are the new varaible to add starting from the existing varaibles, using the \code{I()} function.

For example:

\code{ ~ I(log(column_names1)) + I(column_names2/100)}

the \code{column_names1} and \code{log(column_names1)} are added to the data.

If \code{na.remove} is set ti \code{TRUE}, new variables are created, added to the dataset in input and then the observation with missing are removed.
}

\value{
Returns a data.frame object with the original and the new varaibles.
}

\author{Alessio Serafini}


\examples{

data("airquality")
dt <- airquality

head(add(from = dt, formula =   ~ log(Ozone)))
head(add(from = dt, formula =   ~ log(Ozone) +  log(Wind)))
head(add(from = dt, formula =   ~ log(Ozone), as = "Ozone_1"))


head(add(from = dt, formula =  Ozone + Wind ~ log()))
head(add(from = dt, formula =  ~ log()))
head(add(from = dt, formula =  .~ log(), position = "left"))

head(add(from = dt, formula =  .~ log(), na.remove = TRUE))

head(add(from = dt, formula =   ~ I((Ozone>5))))
head(add(from = dt, formula =   ~ I((Ozone>5)), logic_convert = FALSE ))

head(add(from = dt, formula = Ozone + Wind ~ C(Ozone-Ozone)))
head(add(from = dt, formula =  ~ C(log(Ozone))))
head(add(from = dt, formula =  ~ C(5)))
head(add(from = dt, formula = Ozone + Wind ~ C(Ozone-Ozone)))
head(add(from = dt, formula =  Ozone + Wind ~ C(log(Ozone))))



foo <- function(x, a = 100){return(x-x + a)}

head(add(from = dt, formula =  Ozone + Month~ I(foo(a = 100))))
head(add(from = dt, formula =  Ozone + Month~ foo()))
head(add(from = dt, formula =  ~ I(foo(Ozone, a = 100))))

}

