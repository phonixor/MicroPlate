# example on how the data will be stored!

workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "3263.dbf")
test=novostar.dbf(path=file)
testData=new("Data")
testData=addData(testData,newData=test)
testData@.data


#add experiment....
removeBlank(data=testData, column="content", identifier="B")
