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
  test$test=1234567
  expect_equal(test$test,1234567) # test the data
  expect_equal(colnames(test),"test") # test the colname
  expect_warning((colnames(test)="cookies"))
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
  # prepare
  workspace = getwd()
  testdir=file.path(workspace, "tests/testdata/enzymeAssays")
  file=file.path(testdir, "3263.dbf")
  test=novostar.dbf(path=file)
  testData=new("Data")
  testData=addData(testData,newData=test)
  # begin the tests
  expect_equal(colnames(testData),c("row","column","content","value","time","temp"))
  expect_equal(testData@.data$level,c("well","measurement"))
  expect_equal(600,length(testData$value))
  expect_equal(600,length(testData$content))

  
  testData@.data$colLevel
  testData@.data$colNames
  testData@.data$colType
  testData@.data$level
  testData@.data$levelSize
  
  
  
  testData[] # everything
  testData[1] # first col
  testData[1,] # first row
  testData[,1] # first col
  testData[1,2] # first row 2nd col
  testData[,] # everything
  
  expect_error(testData["a",1]) # not a row number
  expect_error(testData[1000000000,1]) # to many rows
  # TODO test if you give multiple rows... starting with a valid one
  # TODO test half a row???
  expect_error(testData[testData]) # column not num or char
  expect_error(testData[1,"col"]) # unspecified column name
  testData[1,c("row","col","test")]
  testData[1,"column"]
  testData["column"]
  
  testData$content
  testData$value
  length(testData$value)
  
  tdf=testData[]
  
  testData[[]]
  
  
  system.time(length(testData[1:6]))
  system.time(length(tdf[1:6]))
  
  system.time(replicate(10000,testData[1:6]))
  system.time(replicate(10000,tdf[1:6]))
  
  system.time(testData["value"])
  system.time(testData$value)
  
  
  system.time(replicate(10000,testData["content"]))
  system.time(replicate(10000,testData$content))
  system.time(replicate(10000,tdf$content))
  
  system.time(length(testData$value))
  system.time(length(testData$content))
  ttt=NULL
  ttt$test=1:600
  ttt
  system.time(replicate(100000,length(ttt$test))) # 0.123 sec
  system.time(replicate(100000,length(testData$content)))
  system.time(replicate(100000,length(testData$value)))
#   system.time(length(testData$value))
  
  system.time(replicate(100000,length(1:600/600))) # 0.123 sec


})
