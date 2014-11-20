# load the package
library(MicroPlate)
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
  expect_true(all(dim(testData)==c(24000,8)))

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
  # test both [] and []<-
  #
  #
  # file=paste(getwd(),"/tests/testdata/parsers/novostar.xls/KineticData.xls",sep="")
  file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
  testData=novostar.xls(file)
  ### just level
  expect_equal(testData[level=1],testData[level="measurement"])
  expect_equal(testData[level=2], testData[level="well"])
  expect_equal(testData[level=3], testData[level="plate"])
  expect_error(testData[level=4])
  expect_error(testData[level="COOKIESSS!!!"]) # todo: add cookie level
  
  ### singel column
  # plate
  testData["newColumn"]=1
  expect_equal(testData["newColumn"],1)
  testData["newColumn"]=2 # overwrite
  expect_equal(testData["newColumn"],2)
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
  expect_equal(testData[5,"newColumn"],5)
  testData[6,"newColumn",level=2]=50
  expect_error(testData[97,"newColumn"]) # out of range
  expect_error((testData[97,"newColumn"]=5)) # out of range assign
  
  # measurement
  expect_error((testData[5,"newColumn",level="measurement"]=5))
  testData=novostar.xls(file)
  testData[15,"newColumn",level="measurement"]=5
  testData[18,"newColumn",level=1]=55
  expect_equal(testData[18,"newColumn"],55)
  expect_error(testData[24010,"newColumn"]) # out of range
  expect_error((testData[24001,"newColumn"]=5)) # out of range assign
  
  ### just row
  # TODO increase this section!!!
  # boolean select
  testData=novostar.xls(file)
  expect_true(all(testData[1,]==c(0.2663,0,600,1,1,"Sample X1",1,"KineticData.xls"))) # first row
  expect_true(all(dim(testData[testData$row==1,])==c(12,5))) # boolean selection
  
  
  
  ### multiple column
  # plate
  testData=novostar.xls(file)
  testData$newColumn=1
  testData[8:9]=matrix(1,1,2) # change 
  expect_true(all(testData[8:9]==c(1,1)))
  testData[8:9]=1:2 # change 
  expect_true(all(testData[8:9]==1:2))
  expect_error((testData[8:9]=1)) # you cant overwrite a block of data...
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
  expect_true(all(testData[1:2,8:9][,2]==1:2))
  testData[1:2,8:9]=matrix(5,2,2)
  expect_true(all(testData[1:2,8:9]==5))

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
  expect_equal(testData["plateName"],"plateOfDoom")

  # well
  expect_error((testData[testData$row>10,"plateName"]=="KineticData.xls")) # wrong level!... and nothing selected...
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
  expect_true(length(testData["plateName",level="well"])==96)
  expect_error((testData["plateName",level="well"]=1:96))
  expect_error((testData[1:96,"plateName",level="well"]=1:96)) # say i want well and give well level data, but its a plate level column
  expect_true(length(testData["plateName",level=1])==24000)
  expect_error((testData["plateName",level="measurement"]=1:96))

  # well
  expect_error(testData["row",level=3])
  expect_true(length(testData["row",level=1])==24000)
  expect_error((testData["row",level="measurement"]=1:96)) # say i want well but give measurement level
  expect_error((testData["row",level="measurement"]=1:24000))

  # measurement
  expect_error(testData["value",level=3]) # data level lower then requested level
  expect_error(testData["value",level=2]) 
  
  # restricted column names.. plate measurement etc...
  # plate
  expect_error((testData["plate"]=1))
  expect_error((testData["measurement"]=1))
#   expect_error((testData["well"]=1)) # might need to change this
  

})


test_that("MicroPlate.R_[]_tests_2nd_mode",{
  ###################
  #  2nd mode test  #
  ###################
  
  # mp[colNamesYouWant, colname=content]
  # mp[,well=96]
  # mp[,well=4:12]
  
  # file=paste(getwd(),"/tests/testdata/parsers/novostar.xls/KineticData.xls",sep="")
  file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
  testData=novostar.xls(file)
  
  ### well=
  expect_true(all(dim(testData[well=10])==c(250,8)))
  expect_true(all(testData[well=10]==testData[well="A10"]))
  expect_true(all(dim(testData[well=12:80])==c(17250 ,8)))
  testData["content",well=10]="COOKIES!!!"
  expect_equal(testData["content",well=10],"COOKIES!!!")
  expect_error((testData["content",well=10]=1:250))
  expect_error((testData["content",well=10,level="measurement"]=1:250))
  expect_true(all(dim(testData[well=10,level=2])==c(1,5)))

  #   testData[well=10,level=2]=c(1,10,"lalalala") # does not work... should it?
  #   expect_error(testData[well=100])#out of range ... dunno if i should throw an error or return nothing...
  
#   testData[well=10]
#   testData[well=10,level=1] # works
#   testData[well=10,level=2] # works
#   testData[well=10,level=2]=c(1,10,"lalalala") # does not work... should it?
#   testData[,well=10,level=2]# works
#   testData["content",well=10,level=2] # works
#   testData[well=10]
  
  
  
#   testData[well=10]=1 # TODO: needs better error
  
  
  ### all kind of sexy combinations...
  expect_equal(testData["row",well=4,level=2],1)
  expect_equal(length(testData["content",well=10:23,level=2]),14)
  expect_true(all(testData[c("row","column","content"),well=4,level=2] == c(1,4,"Sample X4")))
  
  
#   
#   testData[well=4,level=2]
#   testData[well=8]
#   testData[,well=8]
#   testData[,,well=8]
# 
# 
#   testData["value",well=8]
#   
#   testData["content",well="B6",level=1]#should give error!
#   testData["content",well="B6",level=2]
#   testData["content",well="B6",level=3]#should crash
#   testData["content",row=2,column=6,level=2]
#   testData["content",column=2,level=2]
  
  
  
  
})


test_that("MicroPlate.R_ stress/compare tests",{
  # its probably a bad idea to keep this in the stress test
  # ... stress unit test sounds like a silly idea in general..
  
#   # file=paste(getwd(),"/tests/testdata/parsers/novostar.xls/KineticData.xls",sep="")
#   file=paste(getwd(),"/../testdata/parsers/novostar.xls/KineticData.xls",sep="")
#   testData=novostar.xls(file)
#   
  
  
  ### OLD
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


