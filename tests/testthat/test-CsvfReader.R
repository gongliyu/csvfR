context("CsvfReader")

## TODO: Rename context
## TODO: Add more tests
fname <- tempfile()
df <- data.frame(a=1:3, b=c("hello", "world", "hi"),
                 stringsAsFactors = F)
write.csv(df, fname, row.names=F)

test_that("readHeader", {
  expect_equal(c("a","b"), CsvfReader$new(fname, header=T)$getColNames())
  expect_equal(c("a","b"), CsvfReader$new(file(fname), header=T)$getColNames())
  fid <- file(fname); open(fid)
  expect_equal(c("a","b"), CsvfReader$new(fid, header=T)$getColNames())
  close(fid)
  
  expect_true(is.null(CsvfReader$new(fname, header=F)$getColNames()))
  expect_true(is.null(CsvfReader$new(file(fname), header=F)$getColNames()))
  fid <- file(fname); open(fid)
  expect_true(is.null(CsvfReader$new(fid, header=F)$getColNames()))
  close(fid)
})

test_that("getColNames", {
  ColNames <- c("a","b","c")
  expect_equal(ColNames, CsvfReader$new(fname, header=F,col.names=ColNames)$getColNames())
  expect_equal(ColNames, CsvfReader$new(fname, header=T,col.names=ColNames)$getColNames())
  reader <- CsvfReader$new(fname, header=T, col.names=ColNames)
  reader$readRows(1)
  expect_equal(ColNames, reader$getColNames())
})

test_that("readRows", {
  expect_equal(
    data.frame(a=1, b="hello", stringsAsFactors = F),
    CsvfReader$new(fname, header=T)$readRows(1))
  
  expect_equal(df, CsvfReader$new(fname, header=T)$readRows(3))
  reader <- CsvfReader$new(fname, header=T)
  reader$readRows(2)
  expect_equal(data.frame(a=3,b="hi",stringsAsFactors=F), reader$readRows(1))
})
