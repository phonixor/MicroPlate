# Tests all the different parsers
#
#
###
library(testthat)

test_that("novostar.dbf",{
  # file=paste(getwd(),"/tests/testdata/parsers/novostar.dbf/2773.dbf",sep="")
  file=paste(getwd(),"/../testdata/parsers/novostar.dbf/2773.dbf",sep="")
  dbf=novostar.dbf(file)
  #TODO WAY MORE TESTS!!!
  
})


test_that("novostar.xls",{
  
  
  
}) 

test_that("spectramax.txt",{
  # file=paste(getwd(),"/tests/testdata/parsers/spectramax.txt/20100114_data.txt",sep="")
  file=paste(getwd(),"/../testdata/parsers/spectramax.txt/20100114_data.txt",sep="")
  txt=spectramax.txt(file)
  
  
  
  txt[] # figure out why this gives a warning!?!?!?
  
}) 

# TODO is their a spextramax.xml????


test_that("combine",{
  
  # dbfFile=paste(getwd(),"/tests/testdata/parsers/novostar.dbf/2773.dbf",sep="")
  dbfFile=paste(getwd(),"/../testdata/parsers/novostar.dbf/2773.dbf",sep="")
  dbf=novostar.dbf(dbfFile)
  
  # txtFile=paste(getwd(),"/tests/testdata/parsers/spectramax.txt/20100114_data.txt",sep="")
  txtFile=paste(getwd(),"/../testdata/parsers/spectramax.txt/20100114_data.txt",sep="")
  txt=spectramax.txt(txtFile)
  
  merge(txt,dbf,removeOther = F)
  dim(txt) 
  
  
})

