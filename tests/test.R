# load the package
library(microplate)

# example on how the data will be stored!
workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "3263.dbf")
test=novostar.dbf(path=file)
testData=new("Data")
testData=addData(testData,newData=test)
#testData@.data


# ok this should be the only thing i call... the rest is hould be able to call from this class... 
# not the other way around...
currentExperiment=new("MicroplateExperiment")
currentExperiment=setData(currentExperiment,testData)
#currentExperiment@.mainData

# rb=new("RemoveBlank")
rb=new("RemoveBlank", column="content", identifier="B")
# removeBlank(rb, data=testData, column="content", identifier="B")


#add operation to the experiment...
currentExperiment=addOperation(currentExperiment,rb)
currentExperiment=runAll(currentExperiment)


