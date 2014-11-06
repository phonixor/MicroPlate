

#todo figure out demo paths after install!

# test data from Iraes Rabbers, TY!
file=paste(path.package("microplate"),"/extdata/demo/project3/layout.xls",sep="")
mp=readLayoutFile(file)


# initial data inspection
plotPerPlate(mp)

### remove blanc
# get avarage (TODO per time point)
averageBlanc=aggregate(value~time, data=mp[mp["basic",level="measurement"]=="blanc",] , mean)
plot(averageBlanc)
# remove blanc from data
mp$corValue=mp$value-averageBlanc[match(mp$time, averageBlanc$time),2] # remove blanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... well least it removes negatives..

### growth curves
wellSelection=mp$basic!="blanc"
result=getGrowthRate(mp,wellSelection, valueColumn = "corValue") # call grofit package
# settings=grofit.control(log.y.gc=F,interactive=F)
# resultsNoLog=getGrowthRate(mp,wellSelection, valueColumn = "corValue", settings = settings)


averagePerCondition=aggregate(grofit.growthRate~basic, data=mp[mp["basic"]!="blanc",] , mean)
plot(averagePerCondition,log="x")



getSlope(mp,14,25,43)
getSlope(mp,14,45,80)

slopeGraph(mp,14)

slopeGraph(mp,14,5)
slopeGraph(mp,14,7)
slopeGraph(mp,14,10)
slopeGraph(mp,14,20)

slopeGraph(mp,15)



