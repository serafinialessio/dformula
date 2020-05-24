\name{select}
\alias{select}

\title{Select a subset}
\description{
Selects the row and the varaibles by specifing a condition using a formula.}
\usage{
select(from, formula = .~., as = NULL, na.remove = FALSE, na.return = FALSE,...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion}
    \item{as}{a character vector with names of new variables.}
 \item{na.remove}{a logical value indicating whether NA values should be removed}
 \item{na.return}{a logical value indicating whether only the observation with NA values should be shown}

  \item{\dots}{further arguments}
}
\details{

The formula is composed of two part:

\code{column_names ~ row_conditions}

the left-hand side are the names of the column to select, and the right-hand the operations to select the rows, using the \code{I()} function.

For example:

\code{column_names1 + column_names2 ~ I(column_names1 == "a") + I(column_names2 > 4)}

first the rows are selected if the observation in the \code{column_names1} are equal to \code{a} and if the observation in the \code{column_names2} are biggers than \code{4}, then the \code{column_names1} and \code{column_names2} are returned.

If \code{na.remove} is set to \code{TRUE}, after the subsetting the observations with missing are removed.
}

\value{
Returns a data.frame object containing the selected elements.
}


\author{Alessio Serafini}


\examples{
data("airquality")
dt <- airquality

## Selects columns and filter rows

select(from = dt, formula = .~ I(Ozone > 10 & Wind > 10))
select(from = dt, formula = Ozone ~ I(Wind > 10))
select(from = dt, formula = Ozone + Wind~ I(Ozone > 10))

## All rows and filter columns

select(from = dt, formula = Ozone ~ .)
select(from = dt, formula = Ozone + Wind ~ NULL)




}

