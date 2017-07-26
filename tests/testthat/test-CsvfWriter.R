context("CsvfWriter")

## TODO: Rename context
## TODO: Add more tests
fname <- tempfile()

test_that("Write one time", {
  df <- data.frame(a=1:2, b=3:4)
  
  writer <- CsvfWriter$new(fname, row.names = TRUE)
  writer$writeRows(df)
  writer$close()
  expect_equal(read.csv(fname, row.names=1), df)

  writer <- CsvfWriter$new(fname, row.names = FALSE)
  writer$writeRows(df)
  writer$close()
  expect_equal(read.csv(fname), df)
  
  writer <- CsvfWriter$new(file(fname), row.names = FALSE)
  writer$writeRows(df)
  writer$close()
  expect_equal(read.csv(fname), df)

  writer <- CsvfWriter$new(file(fname, open='wt'), row.names = FALSE)
  writer$writeRows(df)
  writer$close()
  expect_equal(read.csv(fname), df)
})

test_that("Write multiple times", {
  writer <- CsvfWriter$new(fname, row.names = FALSE)
  writer$writeRows(data.frame(a=1:2, b=5:6))
  writer$writeRows(data.frame(a=3:4, b=7:8))
  writer$close()
  expect_equal(read.csv(fname), data.frame(a=1:4, b=5:8))
})
