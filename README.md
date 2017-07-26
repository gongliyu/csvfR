# csvfR
A pure R package to handle large csv file: split, merge, read/write row by row

## Install
```R
devtools::install_github("gongliyu/csvfR")
```

## Usage
### Read csv file row by row
```R
library(csvfR)
reader <- CsvfReader$new("hello.csv")
x <- reader$readRows(n=2)
y <- reader$readRows(n=2)
```
### write csv file row by row
```R
library(csvfR)
writer <- CsvfWriter$new("hello.csv", row.names=F)
writer$writeRows(data.frame(a=1:2, b=c("hello","world"))
writer$writeRows(data.frame(a=3:4, b=c("hi","you"))
writer$close()
```
