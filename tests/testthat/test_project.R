# project test


library(testthat)


test_that("",{
  #
  
  file=paste(getwd(),"/tests/testdata/testproject/layout_test.xls",sep="")
  xls=read.xls(file, stringsAsFactors=FALSE, blank.lines.skip=FALSE,header=FALSE)
  
  file=paste(getwd(),"/tests/testdata/testproject/layout_test.xlsx",sep="")
  xlsx=read.xls(file, stringsAsFactors=FALSE, blank.lines.skip=FALSE,header=FALSE)
  
  file=paste(getwd(),"/tests/testdata/testproject/layout_test.ods",sep="")
  ods=read.ods(file)
  
  xls
  xlsx
  ods
  
  
  xlsData=readLayoutFile(file=paste(getwd(),"/tests/testdata/testproject/layout_test.xls",sep=""))
  odsData=readLayoutFile()
  xlsxDatareadLayoutFile()
  
})