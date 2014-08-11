# project test


library(testthat)


test_that("",{
  #
  

  xls=paste(getwd(),"/../testdata/project/layout.xls",sep="")
  xlsx=paste(getwd(),"/../testdata/project/layout.xlsx",sep="")
  ods=paste(getwd(),"/../testdata/project/layout.ods",sep="")

  xls=readLayoutFile(xls)
  xlsx=readLayoutFile(xlsx)
  ods=readLayoutFile(ods)
  
  # test is everyting is the same
  expect_true(all(xls[]==xlsx[], na.rm=T))
  expect_true(all(xls[]==ods[], na.rm=T))
  
   
   
#   xls
#   xlsx
#   ods


  
})