# todo figure out demo paths after install!

# test data from Iraes Rabbers, TY!
file=paste(getwd(),"/tests/testdata/project3/layout.xls",sep="")
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


averagePerCondition=aggregate(grofit.growthRate~basic, data=mp[mp["basic"]!="blanc",] , mean)
plot(averagePerCondition,log="x")




