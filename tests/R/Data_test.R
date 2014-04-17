# load the package
library(microplate)
library(testthat)
#
# Test Data.R
#
#
#
test_that("Data.R_ basic tests",{
  # add data test
  test=new("Data")
  test$test
  test$test=1234567
  test$test
  expect_equal(test$test,1234567) # test the data
  expect_equal(colnames(test),"test") # test the colname
  colnames(test)="cookies"
  expect_equal(colnames(test),"cookies") # test if the colname was changed
  expect_equal(test$cookies,1234567) # test if the data also changed...
  
  #
  # change in one instance effectes the other
  test2=test
  test$cookies=123
  expect_equal(test2$cookies,123) # note that we changed test and check test2
  #
  # test that you can have multiple instances that dont influence eachother
  test3=new("Data")
  test3$cookies=1234
  expect_false(test2$cookies==1234) 
})
#
# some more tests
test_that("Data.R_ novastar",{
  workspace = getwd()
  testdir=file.path(workspace, "tests/testdata/enzymeAssays")
  file=file.path(testdir, "3263.dbf")
  test=novostar.dbf(path=file)
  testData=new("Data")
  testData=addData(testData,newData=test)
  
  
  
})
