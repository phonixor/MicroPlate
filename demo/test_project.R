# project test

# TODO: this demo needs work.....


# Read Data
odsFile=paste(path.package("MicroPlate"),"/extdata/demo/project/layout.ods",sep="")
mp=readLayoutFile(odsFile)

# initial data inspection
plotPerPlate(mp)

### remove blanc
# get avarage (TODO per time point)
averageBlanc=aggregate(value~time, data=mp[mp["basic",level="measurement"]=="blanc",] , mean)
plot(averageBlanc)


# remove bias
#   .. # show results

# growth curves
#   .. # show restuls

# some simple/excruciatingly complex compare stuff
  
# some fancy images and graphs
  
  

# file=paste(getwd(),"/tests/testdata/project/KineticData.xls",sep="")
#
# file=paste(getwd(),"/tests/testdata/project/140106rawdata.xls",sep="")
# mp1=novostar.xls(file)
# plotPerPlate(mp1)
# 
# file=paste(getwd(),"/tests/testdata/project/140801rawdata.xls",sep="")
# mp2=novostar.xls(file)
# plotPerPlate(mp2)
