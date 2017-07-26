#' Csv file writer which can write the file row by row
#'
#' @docType class
#' @export
#' @import R6
#' @importFrom base file open close writeLines
#' @importFrom utils write.table
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods write csv file row by row
#' @format \code{\link{R6Class}} object
#' @seealso
#'   \code{\link{CsvfReader}}
#'
#' @examples
#' writer <- CsvfWriter$new("hello.csv")
#' df <- data.frame(a=1:3, b=c("hello", "world", "CsvfWriter"))
#' writer$writeRows(df)
#' writer$close()
#'
#' #' @section Methods
#' \describe{
#'   \item{\code{new(...)}}{constructor, the same arguments as \code{\link{write.csv}}}
#'   \item{\code{writeRows(data)}}{write \code{data} to file}
#' }
#' 


CsvfWriter <- R6::R6Class(
  "CsvfWriter",
  public = list(

    initialize = function(file, ...)
    {
      private$args <- list(...)
      private$originalArgs <- private$args
      private$originalArgs$file <- file

      ## check the type of argument "file", if it is a file name
      ## as a character string, open it. If it is a connection
      ## but not open, open it.
      ## If file is already not open before attached to this object,
      ## then mark a flag to indicate whether we need to close it
      ## if this object is garbage collected.
      if(is.character(file))
      {
        file <- file(file, open="wt")
        private$needToClose <- TRUE
      }
      else if(is(file, "connection"))
      {
        if(!isOpen(file))
        {
          open(file, open="wt")
          private$needToClose <- TRUE
        }
      }
      else
        stop("input file should be a file name or a connection")
      private$args$file <- file

      ## check argument row.names
      if(!is.null(private$originalArgs$row.names) &
           !is.logical(private$originalArgs$row.names))
        stop("argument row.names should be TRUE or FALSE.")
    },

    finalize = function()
    {
      if(private$needToClose)
        self$close()
    },
    
    close = function()
    {
      if(!is.null(private$args$file))
      {
        close(private$args$file)
        private$args$file = NULL
      }
      private$needToClose <- F
    },
    
    writeRows = function(data)
    {
      if(private$nRowsWritten == 0)
      {
        ## This is the first time to write since the object
        ## is initialized, we need to do the following things
        ## 1) get the column names from data
        ## 2) get the number of columns from data
        if(isTRUE(private$originalArgs$col.names))
          private$columnNames <- names(data)

        private$nColumns <- ncol(data)
        do.call(write.csv, c(list(x=data),private$args))
        private$args$col.names <- F
      }
      else
      {
        private$validateData(data)
        if(is.null(private$args$sep)) private$args$sep <- ","
        do.call(write.table, c(list(x=data),private$args))
      }
      private$nRowsWritten <- private$nRowsWritten + nrow(data)
    }
  ),
  
  private = list(
    validateData = function(data)
    {
      stopifnot(!is.null(private$nColumns))
      
      if(private$nColumns != ncol(data))
        stop("number of columns does not match.")

      if(isTRUE(private$originalArgs$col.names) &
           !identical(private$columnNames, names(data)))
        stop("column names are different from previous written data.")
        
    },

    originalArgs = NULL,
    args = list(),
    file = NULL,
    nColumns = NULL,
    columnNames = NULL,
    nRowsWritten = 0,
    needToClose = F)
)
