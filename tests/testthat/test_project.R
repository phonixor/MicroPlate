# project test


library(testthat)


test_that("",{
  #
  # xlsFile=paste(getwd(),"/tests/testdata/project/layout.xls",sep="")
  # xlsxFile=paste(getwd(),"/tests/testdata/project/layout.xlsx",sep="")
  # odsFile=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")  
  xlsFile=paste(getwd(),"/../testdata/project/layout.xls",sep="")
  xlsxFile=paste(getwd(),"/../testdata/project/layout.xlsx",sep="")
  odsFile=paste(getwd(),"/../testdata/project/layout.ods",sep="")

  xls=readLayoutFile(xlsFile)
  xlsx=readLayoutFile(xlsxFile)
  ods=readLayoutFile(odsFile)
  
  # test is everyting is the same
  expect_true(all(xls[]==xlsx[], na.rm=T))
  expect_true(all(xls[]==ods[], na.rm=T))
  
   
   
#   xls
#   xlsx
#   ods


  
})