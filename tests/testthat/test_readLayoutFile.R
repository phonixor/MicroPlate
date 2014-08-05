
library(testthat)

test_that("readLayoutFile.R_basic_tests",{
  # test differnt file format
  # TODO test different parsers
  #
  xlsFile=paste(getwd(),"/../testdata/project/layout.xls",sep="")
  xlsxFile=paste(getwd(),"/../testdata/project/layout.xlsx",sep="")
  odsFile=paste(getwd(),"/../testdata/project/layout.ods",sep="")
  
  xls=readLayoutFile(xlsFile)
  xlsx=readLayoutFile(xlsxFile)
  ods=readLayoutFile(odsFile)
   
  # test is everyting is the same
  expect_true(all(xls[]==xlsx[], na.rm=T))
  expect_true(all(xls[]==ods[], na.rm=T))  
  
  expect_true(  all(dim(ods)==c(24000,9))  )
  
  # test merge
  ods=merge(ods,xls) # note that by default the 2nd microplate will be added to the first, and the 2nd will then be deleted
  expect_false(exists("xls"))
  expect_true(all(dim(ods[level=3])==c(2,1)))
  expect_true(all(dim(ods[level=2])==c(192,6)))
  expect_true(all(dim(ods[level=1])==c(48000,9)))
  ods=merge(ods,xlsx,removeOther = F) # test if it works twice
  expect_true(all(dim(ods[level=3])==c(3,1)))
  expect_true(all(dim(ods[level=2])==c(288,6)))
  expect_true(all(dim(ods[level=1])==c(72000,9)))
  expect_true(exists("xlsx"))
  
  # test data first then add layout
  kdata=/../testdata/project/KineticData.xls
  parser="..."
  
  
})
  