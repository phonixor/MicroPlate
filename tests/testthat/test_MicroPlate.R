# load the package
library(microplate)
library(testthat)
library(plyr)
#
# Test MicroPlate.R
#
#
#
test_that("MicroPlate.R_$_tests",{
  # file=paste(getwd(),"/tests/testdata/parsers/novostar.xls/KineticData.xls",sep="")
  file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
  testData=novostar.xls(file)
  
  ### $ tests
  # Plate
  testData$test=1234567 # write
  expect_equal(testData$test,1234567) # read
  #TODO ADD OVERWRITE TEST...
  testData$test=NULL # test remove
  suppressWarnings(expect_true(is.null(testData$test))) # does give a warning
  # well
  testData$testw=1:96 # write
  expect_true(all(testData$testw==1:96)) # read
  testData$testw=20 # overwrite all same value
  expect_true(all(testData$testw==20)) # read
  testData$testw=NULL # test remove
  suppressWarnings(expect_true(is.null(testData$testw))) # does give a warning
  # measurement
  testData$testm=1:24000 # write
  expect_true(all(testData$testm==1:24000)) # read
  testData$testm=20 # overwrite all same value
  expect_true(all(testData$testm==20)) # read
  testData$testm=NULL # test remove
  suppressWarnings(expect_true(is.null(testData$testm))) # does give a warning
})

test_that("MicroPlate.R_basic_tests",{
  file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
  testData=novostar.xls(file)
  ### colnames
  expect_true(any(colnames(testData)=="value")) # test the colname
#   expect_error((colnames(testData)="cookies")) # only 1 element while data has 8 columns # NOTE: currently it just returns an error in any case
  #   expect_warning((colnames(testData)=c(1,"cookies",3,4,5,6,7,8)))
  #   expect_true(any(colnames(testData)=="cookies")) # test if the colname was changed
  #   expect_equal(testData$cookies,1234567) # test if the data also changed...
  
  ### dim
  #TODO level support?
  expect_true(all(dim(testData)==c(24000,7)))

  ### instance tests
  # change in one instance effectes the other
  testData2=testData
  testData$cookies=123 # once again at plate level
  expect_equal(testData2$cookies,123) # note that we changed test and check test2
  
  # test that you can have multiple instances that dont influence eachother
  testData3=novostar.xls(file)
  testData3$cookies=1234
  expect_false(testData2$cookies==1234) 

  ### copy
  testData4=copy(testData)
  testData4$cookies=123456 
  expect_false(testData$cookies==123456) # same kinda instance test



})


test_that("MicroPlate.R_[]_tests",{
  # file=paste(getwd(),"/tests/testdata/parsers/novostar.xls/KineticData.xls",sep="")
  file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
  testData=novostar.xls(file)
  
  ### singel column
  # plate
  testData["newColumn"]=1
  expect_equal(testData[["newColumn"]],1)
  testData["newColumn"]=2 # overwrite
  expect_equal(testData[["newColumn"]],2)
  expect_error((testData["newColumn"]=1:24000))# try to add data at wrong level
  testData["newColumn"]=NULL # remove
  suppressWarnings(expect_true(is.null(testData$newColumn))) # does give a warning
  
  # well
  testData["newColumn"]=1:96 # reuse column name at different level
  expect_true(all(testData["newColumn"]==1:96))
  testData["newColumn"]=500 # single value overwrite
  expect_true(all(testData["newColumn"]==rep(500,96)))
  expect_error((testData["newColumn"]=1:24000))# try to add data at wrong level
  testData["newColumn"]=NULL
  suppressWarnings(expect_true(is.null(testData$newColumn))) # does give a warning
  
  # measurement
  testData["newColumn"]=1:24000 # GOES WITH BLAZING SPEED!
  expect_true(all(testData["newColumn"]==1:24000))
  testData["newColumn"]=500 # single value overwrite -- yup the new underlying structure makes this much faster!
  expect_true(all(testData["newColumn"]==rep(500,24000)))
  expect_error((testData["newColumn"]=1:96))# try to add data at wrong level
  testData["newColumn"]=NULL
  suppressWarnings(expect_true(is.null(testData$newColumn))) # does give a warning
  
  
  ### row
  # plate
  testData=novostar.xls(file)
  testData[1,"newColumn",level="plate"]=5
  expect_equal(testData[1,"newColumn"],5)
  expect_error((testData[1,"newColumn"]=NULL)) # you are not allowed to delete individual values
  testData[1,"newColumn",level=3]=50
  expect_error(testData[10,"newColumn"]) # out of range
  expect_error((testData[2,"newColumn"]=5)) # out of range assign
  
  # well
  expect_error((testData[5,"newColumn",level="well"]=5))
  testData=novostar.xls(file)
  testData[5,"newColumn",level="well"]=5
  expect_equal(testData[[5,"newColumn"]],5)
  testData[6,"newColumn",level=2]=50
  expect_error(testData[97,"newColumn"]) # out of range
  expect_error((testData[97,"newColumn"]=5)) # out of range assign
  
  # measurement
  expect_error((testData[5,"newColumn",level="measurement"]=5))
  testData=novostar.xls(file)
  testData[15,"newColumn",level="measurement"]=5
  testData[18,"newColumn",level=1]=55
  expect_equal(testData[[18,"newColumn"]],55)
  expect_error(testData[24010,"newColumn"]) # out of range
  expect_error((testData[24001,"newColumn"]=5)) # out of range assign
  
  ### multiple column
  # plate
  testData=novostar.xls(file)
  testData$newColumn=1
  testData[7:8]=matrix(1,1,2) # change 
  expect_true(all(testData[7:8]==c(1,1)))
  testData[7:8]=1:2 # change 
  expect_true(all(testData[7:8]==1:2))
  expect_error((testData[7:8]=1)) # you cant overwrite a block of data...
  testData[c("plateName","evenNewerColumn")]=10:11 # 50% new!
  expect_true(all(testData[c("plateName","evenNewerColumn")]==10:11))
#   testData[c("lalala","lalalala")]=10:11# 100% new!  # TODO MAKE THIS WORK!!
#   expect_true(all(testData[c("lalala","lalalala")]==10:11))
  testData[c("lalala","lalalala"),level=3]=10:11# 100% new
  expect_true(all(testData[c("lalala","lalalala")]==10:11))
  testData[c("lalala","lalalala")]=NULL # multi column delete
  expect_error(testData[c("lalala","lalalala")]) # error cause rows are deleted

  # well
  testData=novostar.xls(file)
  testData[5:6]=matrix(1,96,2)
  expect_true(all(testData[5:6]==1))
  expect_error((testData[5:6]=1:192)) # 2D selection requires 2D data! i will not shape the data for you!  that is crazy!
  testData[c("content","evenNewerColumn")]= matrix(1,96,2) # 50% new!
  expect_true(all(testData[c("content","evenNewerColumn")]==1))
  testData[c("lalala","lalalala"),level="well"]=matrix(2,96,2) # 100% new
  expect_true(all(testData[c("lalala","lalalala")]==2))
  testData[c("lalala","lalalala")]=NULL # multi column delete
  expect_error(testData[c("lalala","lalalala")]) # error cause rows are deleted

  # measurement
  testData=novostar.xls(file)
  testData[1:2]=matrix(1,24000,2)
  expect_true(all(testData[1:2]==1))
  testData[c("temp","evenNewerColumn")]= matrix(2,24000,2) # 50% new!
  expect_true(all(testData[c("temp","evenNewerColumn")]==2))  
  testData[c("lalala","lalalala"),level="measurement"]=matrix("cookies!",24000,2) # 100% new
  expect_true(all(testData[c("lalala","lalalala")]=="cookies!"))
  testData[c("lalala","lalalala")]=NULL # multi column delete
  expect_error(testData[c("lalala","lalalala")]) # error cause rows are deleted


  ### multiple column+row
  # general
  testData=novostar.xls(file)
  expect_equal(class(testData[1:7,1:7]),"data.frame")
  expect_error((testData[1:7,1:7]=matrix(1,7,7))) # you cant change data at multiple levels in 1 go
  expect_true(all(dim(testData[1:7,1:7])==c(7,7))) # read columns different levels
  expect_error((testData[1:7,1:7]=1)) #assign wrong format
  expect_error((testData[1:7,1:7]=1:7))

  # plate
  testData=novostar.xls(file)
  testData=merge(testData,testData,removeOther = F)
  testData["newPlateData"]=1:2
  expect_true(all(testData[1:2,7:8][,2]==1:2))
  testData[1:2,7:8]=matrix(5,2,2)
  expect_true(all(testData[1:2,7:8]==5))

  # well
  testData=novostar.xls(file)
  expect_true(all(dim(testData[12:44,4:6])==c(33,3)))
  testData[12:44,4:6]=matrix(123,33,3)
  expect_true(all(testData[12:44,4:6]==123))

  # measurement
  expect_error((testData[1:7,1:2]=1)) #assign wrong format
  expect_error((testData[1:7,1:2]=1:7))
  testData[1:7,1:3]=matrix("cookies",7,3)
  expect_true(all(testData[1:7,1:3]=="cookies")) # my favorite kinda test


  
  ### boolean selection
  # plate
  testData=novostar.xls(file)
  expect_equal(testData[testData$plateName=="KineticData.xls","plateName"],"KineticData.xls")
  expect_error((testData[testData$plateName=="KineticData.xls"]="plateOfDoom"))# should give error as i do not specify what column 
  testData[testData$plateName=="KineticData.xls","plateName"]="plateOfDoom"
  expect_equal(testData[["plateName"]],"plateOfDoom")

  # well
  expect_warning((testData[testData$row>10,"plateName"]=="KineticData.xls")) # wrong level!... and nothing selected...
  expect_error((testData[testData$row>2,"plateName"]=="KineticData.xls")) # wrong level!... 
  testData[testData$row>2,"content"]="NEW CONTENT!"
  expect_true(sum(testData$content=="NEW CONTENT!")==72) 

  # measurement
#   expect_true(sum(testData[testData$value>0.5,"value"])==7498.442)# FUCK YOU R!!! DONT HIDE STUFF FROM ME!
  expect_true(sum(testData[testData$value>0.5,"value"])==7498.4418)
  expect_true(max(testData[testData$value>0.5,"value"])==0.8514)
  testData[testData$value>0.4,"value"]=100
  expect_true(all(testData[testData$value>0.4,"value"]==100))

  ### diffrent level then col selection
  # plate
  testData=novostar.xls(file)
  expect_true(length(testData[["plateName",level="well"]])==96)
  expect_error((testData["plateName",level="well"]=1:96))
  expect_error((testData[1:96,"plateName",level="well"]=1:96)) # say i want well and give well level data, but its a plate level column
  expect_true(length(testData[["plateName",level=1]])==24000)
  expect_error((testData["plateName",level="measurement"]=1:96))

  # well
  expect_error(testData[["row",level=3]])
  expect_true(length(testData[["row",level=1]])==24000)
  expect_error(testData[["row",level="measurement"]]=1:96) # say i want well but give measurement level
  expect_error((testData[["row",level="measurement"]]=1:24000))

  # measurement
  expect_error(testData[["value",level=3]]) # data level lower then requested level
  expect_error(testData[["value",level=2]]) 
  
  # restricted column names.. plate measurement etc...
  # plate
  expect_error((testData["plate"]=1))
  expect_error((testData["measurement"]=1))
  expect_error((testData["well"]=1))
  
  


#   # test reading [
#   expect_equal(dim(testData[]),c(24000,9))    # everything
#   expect_equal(testData[[1]],"KineticData.xls") # first col # plateName
#   expect_equal(dim(testData[1,]),c(1,9)) # first row
# #   expect_equal(testData[,1],"KineticData.xls") # first col
#   expect_equal(testData[1,2], 1234567)   # first row 2nd col # the test i just added
#   expect_equal(dim(testData[,]),c(24000,9))  # everything
#  
#   expect_equal(testData[level=1],testData[level="measurement"]) # test level select
#   expect_equal(testData[level=2],testData[level="well"])
# #   testData[level=2] # TODO decide what todo if levelSize well=measurement...testData[level=2] # TODO decide what todo if levelSize well=measurement...
#   expect_equal(testData[level=3],testData[level="plate"])
#   expect_equal(dim(testData[c(1,3,5)])[2],3) # test column select
#   expect_equal(dim(testData[,c(1,3,5)])[2],3) # test column select
#   expect_equal(dim(testData[c(1,2),])[1],2) # test row select 
#   #
#   # test logical
#   testData[testData$value>0.4,]
#   testData[testData$value>0.4,"value"]=0.4
# 
# 
#   # test writing [<- 
#   testData=novostar.xls(file)
#   expect_error(testData[93:97,"row"]) #  only 96 at well level...
#   expect_error((testData[93:97,"row"]=93:97))
#   testData[2:3,"row"]=10:11 # should just work
#   expect_true(all(testData[2:3,"row"]==10:11))

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
#   # multiple column delete
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
