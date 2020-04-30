\name{transform}
\alias{transform}

\title{Transform varibles}
\description{Mutate input variables  using a formula.
}
\usage{
transform(from, formula, names = NULL,
          na.remove = FALSE, convert_logic = TRUE, ...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
  \item{names}{a character vector with names of new variables.}
  \item{na.remove}{a logical value indicating whether NA values should be removed.}
\item{convert_logic}{logical value indicating if the new logical varaible are convertet to \code{0} or \code{1}}
  \item{\dots}{further arguments}
}
\details{
If \code{na.remove} is set to \code{TRUE},  variables are mutated, and then the observation with missing are removed.
}


\value{
Returns the original data.frame object with mutaded varaibles.
}

\author{Alessio Serafini}

\examples{

head(transform(from = dt, Ozone ~ I(Ozone-Ozone)))
head(transform(from = dt, Ozone ~ log(Ozone)))
head(transform(from = dt, Ozone ~ I(Ozone>5)))
head(transform(from = dt, Ozone ~ I(Ozone>5), convert_logic = TRUE))


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
