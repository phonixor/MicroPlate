##
# this should contains a *real* experiment
# it should also act as like a mini tutorial
# 

#
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


co=new("CustomOperation", novostar.dbf, file)





workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "GJS_layout3263.tab")
layoutData=readLayoutFile(file=file)
file2=file.path(testdir, "3263.dbf")
newData=novostar.dbf(path=file2)
testData=new("Data")
testData=addData(testData,newData=newData,layoutData=layoutData)
testData[]

result=NULL
result=MPApply(testData,fun=smoothEpisode)
result=MPApply(testData,fun=smoothEpisode,df=5, compact=TRUE, episode=c(300,700))


result=MPApply(testData,fun=mean)

result





tdf=testData[]
index=(tdf$row==1 & tdf$column==4)
plot(x=tdf[index,"time"],y=tdf[index,"value"])

# import doesnt seam to work for non s4 functions... :(
derivs=smoothEpisode(x=tdf[index,"time"],y=tdf[index,"value"])
plot(derivs$x,derivs$y)



