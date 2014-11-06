devAskNewPage(ask=F) # i decide where users wait... not plot...

### import data
# test data from: Filipe Branco dos Santos & Parsa Mahallehyousefi - TY!
# 96 well plate, 250 measurements
file=paste(path.package("microplate"),"/extdata/demo/project3/layout.xls",sep="")
mp=readLayoutFile(file)
# initial data inspection
plotPerPlate(mp) 
readline("press any key to continue")

### remove blanc
# plot blanc over time
plot(mp[c("time","value"),basic="blanc"]) # WTF!



nonBlancWellNrs=(1:96)[mp$basic=="blanc"]
plot(xlab=c(min(mp$time),max(mp$value)),ylab=c(min()))
for(i in nonBlancWellNrs){
  xy=mp[c("time","value"),well=i]
  lines(xy,type="l")
}


plot(mp[c("time","value"),basic="blanc",column=1])
plot(mp[c("time","value"),basic="blanc",row=1])



firstTimePoint=min(mp$time)



# plot first time points
install.package("scatterplot3d")
selection=mp$time==min(mp$time) # select first time points
scatterplot3d(x=mp[selection,"row",level=1],y=mp[selection,"column",level=1],z=mp[selection,"value"])
plot(mp[selection,c("row","value")])
plot(mp[selection,c("column","value")])




# get blanc per time point

averageBlanc=aggregate(value~time, data=mp[basic="blanc"] , mean)
plot(averageBlanc)
# remove blanc from data
mp$corValue=mp$value-averageBlanc[match(mp$time, averageBlanc$time),2] # remove blanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... well least it removes negatives..
readline("press any key to continue")


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



