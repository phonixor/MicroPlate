devAskNewPage(ask=F) # i decide where users wait... not plot...

### import data
# test data from Iraes Rabbers, TY!
# 1 plate - 96 well - 250 measurements per well
file=paste(path.package("MicroPlate"),"/extdata/demo/project2/project2.ods",sep="")
mp=readLayoutFile(file)
# show data
plotPerPlate(mp)
readline("press any key to continue")

### remove blanc
# get avarage per time point
averageBlanc=aggregate(value~time, data=mp[strain="blanc"] , mean)
plot(averageBlanc)
mp$corValue=mp$value-averageBlanc[match(mp$time, averageBlanc$time),2] # remove blanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... well least it removes negatives..
# the corrected values are added to mp under the new colname "corValue"
readline("press any key to continue")

### growth curves
# next part will take a while
wellSelection=mp$strain!="blanc"
result=getGrowthRate(mp,wellSelection, valueColumn = "corValue") # call grofit package
# creates a picture for each none blanc well
# mp will now have new slope related values
head(mp[level="well"])
readline("press any key to continue")

### plot growth overview
install.package("gplots")
# 
succinate=mp$Sugar=="succinate"
glucose=mp$Sugar=="glucose"
LM3113=mp$strain=="LM3113"
LM3118=mp$strain=="LM3118"
  
LM3113_succinate_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3113,],mean)
LM3113_succinate_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3113,],sd)
LM3118_succinate_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3118,],mean)
LM3118_succinate_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[succinate&LM3118,],sd)

xlabelplot<-expression(paste("IPTG concentration (",mu,"M)"))
ylabelplot<-expression(mu[max] ~ (h^{-1}))
ylim=c(0, max(LM3113_succinate_iptg_average[[2]],LM3118_succinate_iptg_average[[2]]) )

plotCI(LM3113_succinate_iptg_average[[1]],LM3113_succinate_iptg_average[[2]],uiw=LM3113_succinate_iptg_sd[[2]],liw=LM3113_succinate_iptg_sd[[2]],col='black',xlab=xlabelplot,ylab=ylabelplot,err="y",gap=0,pch=25,type="l",ylim=ylim)
plotCI(LM3118_succinate_iptg_average[[1]],LM3118_succinate_iptg_average[[2]],uiw=LM3118_succinate_iptg_sd[[2]],liw=LM3118_succinate_iptg_sd[[2]],add=TRUE,col='blue',err="y",gap=0,pch=15,type="l")
title("Growth on succinate")

readline("press any key to continue")

# plotWithErrorBars(LM3113_succinate_iptg_average[[1]],LM3113_succinate_iptg_average[[2]],LM3113_succinate_iptg_sd[[2]])

# control
LM3118_glucose_iptg_average=aggregate(grofit.growthRate~IPTG,data=mp[glucose&LM3118,],mean)
LM3118_glucose_iptg_sd=aggregate(grofit.growthRate~IPTG,data=mp[glucose&LM3118,],sd)

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

xlabelplot<-expression(paste("IPTG concentration (",mu,"M)"))
ylabelplot<-expression(mu[max] ~ (h^{-1}))
ylim=c(0, max(LM3113_succinate_iptg_average[[2]],LM3118_succinate_iptg_average[[2]]) )

plotCI(LM3113_succinate_iptg_average[[1]],LM3113_succinate_iptg_average[[2]],uiw=LM3113_succinate_iptg_sd[[2]],liw=LM3113_succinate_iptg_sd[[2]],col='black',xlab=xlabelplot,ylab=ylabelplot,err="y",gap=0,pch=25,ylim=ylim)
plotCI(LM3118_succinate_iptg_average[[1]],LM3118_succinate_iptg_average[[2]],uiw=LM3118_succinate_iptg_sd[[2]],liw=LM3118_succinate_iptg_sd[[2]],add=TRUE,col='blue',err="y",gap=0,pch=15)
title("Yield on succinate")



plotCI(LM3118_glucose_iptg_average[[1]],LM3118_glucose_iptg_average[[2]],LM3118_glucose_iptg_sd[[2]],col='black',xlab=xlabelplot,ylab=ylabelplot)
title("Yield on glucose")

# #add trendlines
# smoothplot=0.3
# #smooth spline for titratable strain
# fitLM3113 <- smooth.spline(LM3113_succinate_iptg_average[[1]] ~ LM3113_succinate_iptg_average[[2]],spar=smoothplot)
# lines(fitLM3113, col="blue", lwd=4)
# # smooth spline trendline for wildtype
# fitLM3118 <- smooth.spline(LM3118_succinate_iptg_average[[1]] ~ LM3118_succinate_iptg_average[[2]],spar=smoothplot)
# lines(fitLM3118, col="black", lwd=4)

