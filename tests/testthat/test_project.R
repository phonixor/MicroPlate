# project test


library(testthat)


test_that("",{
  #
  # xlsFile=paste(getwd(),"/tests/testdata/project/layout.xls",sep="")
  # xlsxFile=paste(getwd(),"/tests/testdata/project/layout.xlsx",sep="")
#   xlsFile=paste(getwd(),"/../testdata/project/layout.xls",sep="")
#   xlsxFile=paste(getwd(),"/../testdata/project/layout.xlsx",sep="")
#   xls=readLayoutFile(xlsFile)
#   xlsx=readLayoutFile(xlsxFile)
#   # test is everyting is the same
#   expect_true(all(xls[]==xlsx[], na.rm=T))
#   expect_true(all(xls[]==ods[], na.rm=T))

  # Read Data
  # odsFile=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")  
  odsFile=paste(getwd(),"/../testdata/project/layout.ods",sep="")
  ods=readLayoutFile(odsFile)
  
  # initial data inspection
  
  # remove bias
#   .. # show results

# growth curves
#   .. # show restuls

  # some simple/excruciatingly complex compare stuff
  
  # some fancy images and graphs
  
  
})