# todo figure out demo paths after install!

# test data from Iraes Rabbers, TY!
file=paste(getwd(),"/tests/testdata/project2/project2.ods",sep="")
mp=readLayoutFile(file)


# show data
plotPerPlate(mp)

### remove blanc
# get avarage (TODO per time point)
averageBlanc=aggregate(value~time, data=mp[mp["strain",level="measurement"]=="blanc",] , mean)
plot(averageBlanc)
mp$corValue=mp$value-averageBlanc[match(mp$time, averageBlanc$time),2] # remove blanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... well least it removes negatives..
# 
# # take natural logarithm of corOD/corOD(t=0) 
# f$lncorOD <- log(f$corOD/f$corOD[1,])

### growth curves
# next part will take a while
readline("press any key to continue")
wellSelection=mp$strain!="blanc"
result=getGrowthRate(mp,wellSelection, valueColumn = "corValue") # call grofit package

### plot growth overview
library(gplots)
# 
succinate=mp$Sugar=="succinate"
glucose=mp$Sugar=="glucose"
LM3113=mp$strain=="LM3113"
LM3118=mp$strain=="LM3118"
  
LM3113_succinate_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3113,],mean)
LM3113_succinate_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3113,],sd)
LM3118_succinate_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3118,],mean)
LM3118_succinate_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3118,],sd)
# control
LM3118_glucose_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[glucose&LM3118,],mean)
LM3118_glucose_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[glucose&LM3118,],sd)

plotCI(LM3113_succinate_iptg_average[[1]],LM3113_succinate_iptg_average[[2]],LM3113_succinate_iptg_sd[[2]],col='black')
plotCI(LM3118_succinate_iptg_average[[1]],LM3118_succinate_iptg_average[[2]],LM3118_succinate_iptg_sd[[2]],add=TRUE,col='blue')
title("Growth on succinate")

plotCI(LM3118_glucose_iptg_average[[1]],LM3118_glucose_iptg_average[[2]],LM3118_glucose_iptg_sd[[2]],col='black')
title("Growth on glucose")


### plot yield overview
#
LM3113_succinate_iptg_average=aggregate(grofit.yield~IPTG,data=mp[succinate&LM3113,],mean)
LM3113_succinate_iptg_sd=aggregate(grofit.yield~IPTG,data=mp[succinate&LM3113,],sd)
LM3118_succinate_iptg_average=aggregate(grofit.yield~IPTG,data=mp[succinate&LM3118,],mean)
LM3118_succinate_iptg_sd=aggregate(grofit.yield~IPTG,data=mp[succinate&LM3118,],sd)
# control
LM3118_glucose_iptg_average=aggregate(grofit.yield~IPTG,data=mp[glucose&LM3118,],mean)
LM3118_glucose_iptg_sd=aggregate(grofit.yield~IPTG,data=mp[glucose&LM3118,],sd)

plotCI(LM3113_succinate_iptg_average[[1]],LM3113_succinate_iptg_average[[2]],LM3113_succinate_iptg_sd[[2]],col='black')
plotCI(LM3118_succinate_iptg_average[[1]],LM3118_succinate_iptg_average[[2]],LM3118_succinate_iptg_sd[[2]],add=TRUE,col='blue')
title("Yield on succinate")

plotCI(LM3118_glucose_iptg_average[[1]],LM3118_glucose_iptg_average[[2]],LM3118_glucose_iptg_sd[[2]],col='black')
title("Yield on glucose")


