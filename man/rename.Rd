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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The original data.frame with changed column names
}

\author{Alessio Serafini}


\examples{
head(rename(from = dt, Ozone ~ Ozone1))
head(rename(from = dt, Ozone + Wind ~ Ozone_new + Wind_new))
}
