#' Csv file reader which can read the file row by row
#'
#' @docType class
#' @export
#' @import R6
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods read csv file row by row
#' @format \code{\link{R6Class}} object
#'
#' @seealso
#'   \code{\link{CsvfWriter}}
#' 
#' @examples
#' df <- data.frame(a=1:2, b=c("hello", "world"))
#' write.csv(df, "hello.csv")
#' reader <- CsvfReader$new("hello.csv")
#' reader$readRows(1)
#' reader$readRows(1)
#' 
#' #' @section Methods:
#' \describe{
#'   \item{\code{new(...)}}{constructor for creating object, the same arguments as \code{\link{read.csv}}}
#'   \item{\code{readRows(n=1)}}{read \code{n} lines from file}
#'   \item{\code{getColNames()}}{return the column names as a character vector}
#' }
#' 
CsvfReader <- R6::R6Class(
  "CsvfReader",
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
        file <- file(file, open="rt")
        private$needToClose <- TRUE
      }
      else if(is(file, "connection"))
      {
        if(!isOpen(file))
        {
          open(file)
          private$needToClose <- TRUE
        }
      }
      else
        stop("input file should be a file name or a Connection")
      private$file <- file

      if(isTRUE(private$originalArgs$stringsAsFactors))
      {
        warning("option 'stringsAsFactors' is not supported") 
        private$originalArgs$stringsAsFactors <- FALSE
      }
      private$args$stringsAsFactors <- FALSE
      
      if(isTRUE(private$originalArgs$header))
      {
        private$readHeader()
        private$args$header <- FALSE
      }

    },

    finalize = function()
    {
      if(isTRUE(private$needToClose))
        self$close()
    },
    

    close = function()
    {
      if(!is.null(private$file))
      {
        close(private$file)
        private$file = NULL
      }
      private$needToClose = FALSE
    },
    
    readRows = function(n=1)
    {
      stopifnot(is(private$file, "connection") & isOpen(private$file))
      lines <- readLines(private$file, n=n)
      do.call(read.csv, c(list(file=textConnection(lines)), private$args))
    },

    getColNames = function()
    {
      private$args$col.names
    }),
  
  private = list(
    readHeader = function()
    {
      line <- readLines(private$file, n=1)
      header <- read.csv(textConnection(line), header=T)
      if(is.null(private$args$col.names))
      {
        private$args$col.names <- names(header)
      }
    },
    
    file = NULL,
    args = NULL,
    originalArgs = NULL,
    needToClose = F
  ))


