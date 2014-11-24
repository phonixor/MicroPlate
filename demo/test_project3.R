devAskNewPage(ask=F) # i decide where users wait... not plot...

### import data
# test data from: Filipe Branco dos Santos & Parsa Mahallehyousefi - TY!
# 96 well plate, 250 measurements
file=paste(path.package("MicroPlate"),"/extdata/demo/project3/layout.xls",sep="")
mp=readLayoutFile(file)
# initial data inspection
showWellNrs(mp)
readline("press any key to continue")
plotPerPlate(mp)
readline("press any key to continue")

### remove blanc
# plot blanc over time
plot(mp[c("time","value"),basic="blanc"]) # WTF!
readline("press any key to continue")
# ok that was weird
# lets investigate where this gap comes from
# is it row based?
plot(mp[c("time","value"),basic="blanc",column=1] , type="l")
# ok that looks interestingre 
readline("press any key to continue")
# is it col based?
plot(mp[c("time","value"),basic="blanc",row=1],type="l")
# apperently not...
readline("press any key to continue")
#
# lets look at the first time points of each well
# plot first time points
install.package("scatterplot3d")
selection=mp$time==min(mp$time) # select first time points
origenalPar=par(no.readonly = T)#next function is not implemented cleanly
scatterplot3d(x=mp[selection,"row",level=1],y=mp[selection,"column",level=1],z=mp[selection,"value"],xlab = "row",ylab="col",zlab="OD")
suppressWarnings(par(origenalPar)) # restore pars... this can give warnings for some reason..
readline("press any key to continue")
plot(mp[selection,c("row","value")],ylab="OD")
readline("press any key to continue")
plot(mp[selection,c("column","value")],ylab = "OD")
readline("press any key to continue")
# so apperently 

#looking at a few wells
plot(mp[c("time","value"),well=14])
plot(mp[c("time","value"),well=16])
plot(mp[c("time","value"),well=20])
readline("press any key to continue")
# its safe to take atleast the first 5 values
firstFiveTimePoints=unique(mp$time)[1:5]
medium=aggregate(value~well,data=mp[time=firstFiveTimePoints],mean)
aggregate(value~well,data=mp[time=firstFiveTimePoints],sd) # not perfect but not horrible either
mp$corValue=mp$value-medium$value[mp["well",level=1]] # remove medium
mp[mp$corValue<=0,"corValue"]=0.0001  # remove negative values.. (there are none but still :) )
readline("press any key to continue")


### growth curves
wellSelection=mp$basic!="blanc"
result=getGrowthRates(mp,wellSelection, valueColumn = "corValue",nrOfTimePointsForSlope = "10%") # call grofit package

mp$doublingTime=log(2)/mp$growthRate # doublingTime
mp[level=2]

readline("press any key to continue")

# plot per concentration
averagePerCondition=aggregate(growthRate~basic, data=mp[mp["basic"]!="blanc",] , mean)
plot(averagePerCondition,log="x",xlab="initial nr of cells of x in mM",ylab="growthRate (min)") 
averagePerCondition=aggregate(doublingTime~basic, data=mp[mp["basic"]!="blanc",] , mean)
plot(averagePerCondition,log="x",xlab="initial nr of cells of x in mM",ylab="doublingTime (min)") 

# a clear indication that i probably did not have proper values for my layout file

