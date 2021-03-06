% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CsvfWriter.R
\docType{class}
\name{CsvfWriter}
\alias{CsvfWriter}
\title{Csv file writer which can write the file row by row}
\format{\code{\link{R6Class}} object}
\usage{
CsvfWriter
}
\value{
Object of \code{\link{R6Class}} with methods write csv file row by row
}
\description{
Csv file writer which can write the file row by row
}
\examples{
writer <- CsvfWriter$new("hello.csv")
df <- data.frame(a=1:3, b=c("hello", "world", "CsvfWriter"))
writer$writeLines(df)
writer$close()

#' @section Methods
\\describe{
  \\item{\\code{new(...)}}{constructor, the same arguments as \\code{\\link{write.csv}}}
  \\item{\\code{writeLines(data)}}{write \\code{data} to file}
}

}
\seealso{
\code{\link{CsvfReader}}
}
\keyword{data}
