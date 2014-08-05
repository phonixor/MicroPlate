# load the package
library(microplate)
library(testthat)
#
# Test MicroPlate.R
#
#
#
test_that("MicroPlate.R_ basic tests",{
  
  file=paste(getwd(),"/../testdata/project/KineticData.xls",sep="")
  testData=novostar.xls(file)


  testData$test=1234567 # at plate level
  expect_equal(testData$test,1234567)
  expect_true(any(colnames(testData)=="test")) # test the colname
  expect_error((colnames(testData)="cookies")) # only 1 element while data has 8 columns # NOTE: currently it just returns an error in any case
#   expect_warning((colnames(testData)=c(1,"cookies",3,4,5,6,7,8)))
#   expect_true(any(colnames(testData)=="cookies")) # test if the colname was changed
#   expect_equal(testData$cookies,1234567) # test if the data also changed...
   
#   # change in one instance effectes the other
  testData2=testData
  testData$cookies=123 # once again at plate level
  expect_equal(testData2$cookies,123) # note that we changed test and check test2

  # test that you can have multiple instances that dont influence eachother
  testData3=novostar.xls(file)
  testData3$cookies=1234
  expect_false(testData2$cookies==1234) 
  
#   # test reading [
  expect_equal(dim(testData[]),c(24000,9))    # everything
  expect_equal(testData[[1]],"KineticData.xls") # first col # plateName
  expect_equal(dim(testData[1,]),c(1,9)) # first row
#   expect_equal(testData[,1],"KineticData.xls") # first col
  expect_equal(testData[1,2], 1234567)   # first row 2nd col # the test i just added
  expect_equal(dim(testData[,]),c(24000,9))  # everything
# 
  expect_equal(testData[level=1],testData[level="measurement"]) # test level select
  expect_equal(testData[level=2],testData[level="well"])
# #   testData[level=2] # TODO decide what todo if levelSize well=measurement...testData[level=2] # TODO decide what todo if levelSize well=measurement...
  expect_equal(testData[level=3],testData[level="plate"])
  expect_equal(dim(testData[c(1,3,5)])[2],3) # test column select
  expect_equal(dim(testData[,c(1,3,5)])[2],3) # test column select
  expect_equal(dim(testData[c(1,2),])[1],2) # test row select 
  
  # test writing [<- 
  testData=novostar.xls(file)
  expect_error(testData[93:97,"row"]) #  only 96 at well level...
  expect_error((testData[93:97,"row"]=93:97))
  testData[2:3,"row"]=10:11 # should just work
  expect_true(all(testData[2:3,"row"]==10:11))

#   # test levelSize of well=plate=measurement
#   testData=new("MicroPlate")
#   test=list(row=1,column=1,measurement=list(list(value=1,temp=1,time=1)))
#   testData=addPlate(testData,newData=test)
#   testData$test=1234567 # new column size=1
#   testData["test2"]="love for cookies"
#   expect_equal[]
# 
#   # test levelSize of well=plate
#   testData=new("MicroPlate")
#   test=list(row=1,column=1,measurement=list(list(value=1:10,temp=1:10,time=1:10)))
#   testData=addPlate(testData,newData=test)
#   testData$test=1234567 # new column size=1
#   testData["test2"]="love for cookies"
#   # is it possible that plate>well?? i think not...  
#   testData[]
# 
#   # test plateName
#   # with platename first
#   testData=new("MicroPlate") 
#   test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
#   testData=addPlate(testData,newData=test, plateName="first plate")
#   testData=addPlate(testData,newData=test, plateName="second plate")
#   testData=addPlate(testData,newData=test)
#   testData[]
#   # without platename first
#   testData=new("MicroPlate")
#   test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
#   testData=addPlate(testData,newData=test)
#   testData=addPlate(testData,newData=test)
#   testData=addPlate(testData,newData=test, plateName="third plate")
# 
#   # test remove function df[colname]=NULL
#   testData=new("MicroPlate")
#   test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
#   testData=addPlate(testData,newData=test)
# #   testData["plateName"]=NULL # should make this give an error... # it already does :P
#   testData["row"]=NULL
#   expect_equal(dim(testData[]),c(2,5))
#   testData["temp"]=NULL
#   expect_equal(dim(testData[]),c(2,4))
#   testData[]
#   
#   # more remove
#   expect_error(removeColumn(testData,"124123")) # invalid column name
#   expect_error(removeColumn(testData,1000000000)) # invalid column number
#   expect_error(removeColumn(testData,testData)) # wrong data type
#   
#   # multiple row delete
#   testData=new("MicroPlate")
#   test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
#   testData=addPlate(testData,newData=test)
#   testData[c("temp","row")]=NULL
#   expect_equal(dim(testData[]),c(2,4))

})


test_that("MicroPlate.R_additional_functions",{
#   testData=new("MicroPlate")
#   test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
#   testData=addPlate(testData,newData=test)
#   # make the copy
#   testCopy=copy(testData)
#   expect_true(all(testCopy[]==testData[]))
#   # check if they behave indepently
#   testCopy@.data$data$column=c(2,2)
#   expect_true(!all(testCopy[]==testData[]))
#   testData@.data$data$column=c(2,2)
#   expect_true(all(testCopy[]==testData[]))
#   #
#   
  
})


#
# some more tests
test_that("MicroPlate.R_novastar",{
  # prepare
#   workspace = getwd()
#   testdir=file.path(workspace, "tests/testdata/enzymeAssays")
#   file=file.path(testdir, "3263.dbf")
#   test=novostar.dbf(path=file)
#   testData=new("MicroPlate")
#   testData=addPlate(testData,newData=test)
#   # begin the tests
#   
#  
#   
#   # test meta data
#   expect_equal(testData@.data$colLevel,c("plate","well","well","well","measurement","measurement","measurement"))
#   expect_equal(testData@.data$colNames,c("plateName", "row", "column", "content", "value", "time", "temp"))
#   expect_equal(testData@.data$colLevelNr,c(3, 2, 2, 2, 1, 1, 1))
#   expect_equal(testData@.data$level,c("plate","well","measurement"))
#   expect_equal(testData@.data$levelNr,c(3,2,1))
#   expect_equal(testData@.data$levelSize,c(1,12,600))
#   
#   # other tests
#   expect_equal(colnames(testData),c("plateName","row","column","content","value","time","temp"))
#   expect_equal(600,length(testData$value))
#   expect_equal(12,length(testData$content))
#   
#   expect_error(testData["a",1]) # not a row number
#   expect_error(testData[1000000000,1]) # to many rows
#   # TODO test if you give multiple rows... starting with a valid one
#   # TODO test half a row???
#   expect_error(testData[testData]) # column not num or char
#   expect_error(testData[1,"idonotexist"]) # unspecified column name
#   
#   testData=addPlate(testData,newData=test) # add more data
#   expect_equal(testData@.data$levelSize,c(1,12,600)*2) # test if things got doubled
#   
#   
#   
#   # well reading
#   expect_equal(dim(testData[level=2]),c(24,4))
# 
#   
#   
#   testData[level="well"]==testData[level=2]
#   
#   expect_equal(dim(testData["content"])[1])
#   
#   
# 
#   
#   
#   
#   # well writing
#   testData["content"]=1:12 # this should throw an error as content has 24 rows now!
#   testData["content"]=1:24
#   testData["plateName"]="COOKIES!!!"
#   
#   
#   
#   
#   
# 
#   testData@.data$colLevel
#   testData@.data$colNames
#   testData@.data$colLevelNr
#   testData@.data$level
#   testData@.data$levelNr
#   testData@.data$levelSize
#   
#   plotPerWell(testData)
#   plotPerPlate(testData)
#   
#   testData[] # everything
#   testData[1] # first col
#   testData[1,] # first row
#   testData[,1] # first col
#   testData[1,2] # first row 2nd col
#   testData[,] # everything
#   
#   testData["content"]=1:12
#   
#   
#   testData[1,c("row","col","test")]
#   testData[1,"column"]
#   testData["column"]
#   
#   testData["plateName"]
#   testData[c("plateName","column")]
#   testData[c("plateName","content","value")]
#   
#   testData[level=3] # everything
# 
#   testData$content
#   testData$value
#   length(testData$value)
#   
#   tdf=testData[]
#   
#   testData[[]]
#   
#   
#   system.time(length(testData[1:6]))
#   system.time(length(tdf[1:6]))
#   
#   system.time(replicate(10000,testData[1:6]))
#   system.time(replicate(10000,tdf[1:6]))
#   
#   system.time(testData["value"])
#   system.time(testData$value)
#   
#   
#   system.time(replicate(1000,testData$value))
#   
#   
#   system.time(replicate(10000,testData["content"])) # 4.864 sec
#   system.time(replicate(10000,testData$content)) # 1.367 sec
#   
#   system.time(replicate(10000,testData["value"])) # 5.671 sec
#   system.time(replicate(10000,testData$value)) # 0.948 sec
#   tft=testData[] #put it in a data.frame
#   system.time(replicate(10000,tft["value"])) # 0.505 sec
#   system.time(replicate(10000,tft$value)) # 0.201 sec
#   
# 
#   
#   system.time(replicate(10000,tdf$content)) # 0.011
#   
#   system.time(length(testData$value))
#   system.time(length(testData$content))
#   ttt=NULL
#   ttt$test=1:600
#   ttt
#   system.time(replicate(100000,length(ttt$test))) # 0.123 sec
#   system.time(replicate(100000,length(testData$content)))
#   system.time(replicate(100000,length(testData$value)))
# #   system.time(length(testData$value))
#   
#   system.time(replicate(100000,length(1:600/600))) # 0.123 sec


})




test_that("MicroPlate.R_ stress/compare tests",{
  # its probably a bad idea to keep this in the stress test
  
#   file="../testdata/"
#   workspace = getwd()
#   testdir=file.path(workspace, "tests/testdata/enzymeAssays")
#   file=file.path(testdir, "GJS_layout3263.tab")
#   layoutData=readLayoutFile(file=file)
#   file2=file.path(testdir, "3263.dbf")
#   newData=novostar.dbf(path=file2)
#   testData=new("MicroPlate")
#   
#   system.time(replicate(50, addPlate(testData,newData=newData,layoutData=layoutData)))
#   tdf=testData[] # 2MB ish
#   
#   system.time(replicate(1000,testData$value)) # 3 sec
#   system.time(replicate(1000,tdf$value)) # .4 sec
#   # about Data = 10x slower then data.frame
#   
#   system.time(replicate(1000,testData["value"])) # 24 sec
#   system.time(replicate(1000,tdf["value"])) # .3 sec
#   # many many times slower
#   
#   system.time(replicate(1000,testData["content"])) # .5 sec
#   system.time(replicate(1000,tdf["content"])) # 1.2 sec #... that is ... weird...
#   # that is ... weird... is this factor vs string?
#   # oooh crap... this is 600 vs 30000 rows....
#   system.time(replicate(1000,testData["content",level="measurement"])) # 55 sec
#   # that is ... many many times slower
#   
#   system.time(replicate(1000,testData["row",level="measurement"])) # 27 sec
#   system.time(replicate(1000,tdf["row"])) # .4 sec
#   # eeeugh....
#   
#   
#   testData=new("Data")
#   system.time(replicate(100, addPlate(testData,newData=newData,layoutData=layoutData))) # 5sec
#   testData
#   
#   
#   tdf=testData[] # 2MB ish
#   system.time(replicate(10,testData$value))
#   system.time(replicate(10,tdf$value))
#   
#   system.time(replicate(10000,testData["value"]))
#   system.time(replicate(10000,tdf["value"]))
#   
#   
#   testData[]
#   testData
  
  
})
