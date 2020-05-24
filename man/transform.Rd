\name{transform}
\alias{transform}

\title{Transform varibles}
\description{Mutate input variables  using a formula.
}
\usage{
transform(from, formula, as = NULL,
          na.remove = FALSE, logic_convert = TRUE, ...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
  \item{as}{a character vector with names of new variables.}
  \item{na.remove}{a logical value indicating whether NA values should be removed.}
\item{logic_convert}{logical value indicating if the new logical varaible are converted to \code{0} or \code{1}}
  \item{\dots}{further arguments}
}
\details{

The formula is composed of two part:

\code{column_names ~ trasformed_variables}

the left-hand side are the names of the column to transform, and the right-hand the operations applied to the selected columns, using the \code{I()} function.

For example:

\code{column_names1 + column_names2 ~ I(log(column_names1)) + I(column_names2/100)}

the \code{column_names1} is mutated in \code{log(column_names1)} and \code{column_names2} is divided by 100.


If \code{na.remove} is set to \code{TRUE},  variables are mutaded, and then the observation with missing are removed.
}


\value{
Returns the original data.frame object with mutaded varaibles.
}

\author{Alessio Serafini}

\examples{

data("airquality")
dt <- airquality

head(transform(from = dt, Ozone ~ I(Ozone-Ozone)))
head(transform(from = dt, Ozone ~ log(Ozone)))
head(transform(from = dt, Ozone ~ I(Ozone>5)))
head(transform(from = dt, Ozone ~ I(Ozone>5), logic_convert = TRUE))


head(transform(from = dt,  ~ log()))
head(transform(from = dt, . ~ log()))
head(transform(from = dt, NULL ~ log()))

head(transform(from = dt, Ozone + Day ~ log()))
head(transform(from = dt, Ozone + Day ~ log(Ozone/100) + exp(Day)))
head(transform(from = dt, Ozone ~ log()))

head(transform(from = dt,Ozone + Wind ~ C(log(1))))
head(transform(from = dt,Ozone + Wind ~ log(Ozone) + C(10)))


head(transform(from = dt, Ozone + Wind~ C(log(Ozone))))


foo <- function(x, a = 100){return(x-x + a)}
head(transform(from = dt, Ozone + Wind ~ foo(a = 100)))
head(transform(from = dt, . ~ foo(a = 100)))

head(transform(from = dt, Ozone + Wind ~ log(log(1))))

}
