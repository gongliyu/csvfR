% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CsvfReader.R
\docType{class}
\name{CsvfReader}
\alias{CsvfReader}
\title{Csv file reader which can read the file row by row}
\format{\code{\link{R6Class}} object}
\usage{
CsvfReader
}
\value{
Object of \code{\link{R6Class}} with methods read csv file row by row
}
\description{
Csv file reader which can read the file row by row
}
\examples{
df <- data.frame(a=1:2, b=c("hello", "world"))
write.csv(df, "hello.csv")
reader <- CsvfReader$new("hello.csv")
reader$readLines(1)
reader$readLines(1)

#' @section Methods:
\\describe{
  \\item{\\code{new(...)}}{constructor for creating object, the same arguments as \\code{\\link{read.csv}}}
  \\item{\\code{readLines(n=1)}}{read \\code{n} lines from file}
  \\item{\\code{getColNames()}}{return the column names as a character vector}
}

}
\seealso{
\code{\link{CsvfWriter}}
}
\keyword{data}
