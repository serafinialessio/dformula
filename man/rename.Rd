\name{rename}
\alias{rename}

\title{Rename variables}

\description{Rename variables using formulas}

\usage{
rename(from, formula, ...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
  \item{\dots}{further arguments}

}
\details{

The formula is composed of two part:

\code{column_names ~ new_variables_name}

the left-hand side select the columns to change the names, and the right-hand the new names of the selected columns

For example:

\code{column_names1 + column_names2 ~ new_variables_name1 + new_variables_name2}

the name of the column \code{1} and the name of the column \code{2} are changed in  \code{new_variables_name1} and \code{new_variables_name2}

}

\value{
The original data.frame with changed column names
}

\author{Alessio Serafini}


\examples{

data("airquality")
dt <- airquality

head(rename(from = dt, Ozone ~ Ozone1))
head(rename(from = dt, Ozone + Wind ~ Ozone_new + Wind_new))
}
