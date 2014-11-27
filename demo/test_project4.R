devAskNewPage(ask=F) # i decide where users wait... not plot...
# file=paste(path.package("MicroPlate"),"/extdata/demo/project4/parsaData.xlsx",sep="")

file=paste(path.package("MicroPlate"),"/extdata/demo/project4/layout.xls",sep="")
mp=readLayoutFile(file)
showWellNrs(mp)
plotPerPlate(mp)
# notice that these both work per plate...

colnames(mp)


# now normally we can use well to get all data in a well...
# but now that is no longer enough...
# row and column combined are... but thats a pain to work with...
# so we make a new identifier
mp$wellNr=paste(mp$row,mp$column,sep="_")
mp$wellNr
length(mp["value", wellNr="1_1" ,level=1]) # all 406 measurements... so it works

# ok this data is a bit silly cause now you have time series of a single well over multiple plates
# and they don't have the correct time stamp on them yet...
# so lets fix that first
corTime=c((0:249)*10, ((250:255)*10), ((256:405)*10)+5 ) 
corTime=corTime/60 # guess you prefere hours
length(corTime) # 406 time points...check!
corTime
# now for each "well" add that time
for(well in unique(mp$wellNr)){
  mp["corTime", wellNr=well ,level=1]=corTime 
}# this takes a few seconds my function is not the fastest

plot(mp[c("corTime","value"),startingCells=0]) 
# very weird spike!
abline(v=corTime[251]) #start 2nd plate
abline(v=corTime[257]) #start 3rd plate
# at the start of the thirth plate
# looking at the data its the first 4 time points of the 3th plate....
timeSelection=corTime[c(1:256,261:406)]
length(timeSelection) # 402... which is 406-4 so good!
# lets check if that fixes things
plot(mp[c("corTime","value"),startingCells=0,corTime=timeSelection]) # much better!
# note: currently you cannot delete data rows with my package... so we need to keep using the timeSelection...
#
#
# lets look at the first time points again
firstXTimePoints=mp$corTime%in%unique(mp$corTime)[1:20] # select first time points rows...
install.package("scatterplot3d")
origenalPar=par(no.readonly = T)#next function is not implemented cleanly
scatterplot3d(x=mp[firstXTimePoints,"column",level=1],y=mp[firstXTimePoints,"row",level=1],z=mp[firstXTimePoints,"value"],xlab = "row",ylab="col",zlab="OD")
scatterplot3d(x=mp[firstXTimePoints,"row",level=1],y=mp[firstXTimePoints,"column",level=1],z=mp[firstXTimePoints,"value"],xlab = "row",ylab="col",zlab="OD") # different angle
par(origenalPar)
# this makes you question the plate reader....
# it looks like a plane inside the cube...
# values differ from 0.26~0.4 - so they are definitly relevant
# you dont have enough blanks to compensate for this...
# so you use the first time point themselve
# lets calculate the plane
meanFirst=aggregate(value~row+column,data=mp[firstXTimePoints,c("row","column","value"),level=1],FUN = mean) 
meanFirst
# this step is not really necessery as you can just do plane... but this is more convienent later on
plane=lm(formula=value~row+column, data=meanFirst)
plane
plane$coefficients
meanFirst$positionalBias=plane$fitted.values # we want to remove these from our data....
meanFirst$wellNr=paste(meanFirst$row,meanFirst$column,sep="_") # make sure you have the same identifier
meanFirst
#
values=mp[firstXTimePoints,"value"]
scatterplot3d(x=mp[firstXTimePoints,"column",level=1],y=mp[firstXTimePoints,"row",level=1],z=values,xlab = "row",ylab="col",zlab="OD",zlim=c(min(values),max(values)) )
par(new=TRUE)
scatterplot3d(x=meanFirst$column,y=meanFirst$row,z=meanFirst$positionalBias,xlab = "row",ylab="col",zlab="OD",color="blue",type="l",zlim=c(min(values),max(values)))

#other way around
scatterplot3d(x=mp[firstXTimePoints,"row",level=1],y=mp[firstXTimePoints,"column",level=1],z=values,xlab = "row",ylab="col",zlab="OD",zlim=c(min(values),max(values)) )
par(new=TRUE)
scatterplot3d(x=meanFirst$row,y=meanFirst$column,z=meanFirst$positionalBias,xlab = "row",ylab="col",zlab="OD",color="blue",type="l",zlim=c(min(values),max(values)))
par(origenalPar)
#this function cant draw a plane :(

# looks good enough to remove
mp$corValue=mp$value-meanFirst$positionalBias[match(mp["wellNr",level=1],meanFirst$wellNr)]
# let us check the results!

scatterplot3d(x=mp[firstXTimePoints,"row",level=1],y=mp[firstXTimePoints,"column",level=1],z=mp[firstXTimePoints,"corValue",level=1],xlab = "row",ylab="col",zlab="OD")
par(origenalPar)
# was in the range of 0.26~0.4 and now it is in the range of -0.04 to 0.03 so from a max differnece of 0.14 vs 0.07
# about a 50% improvement


# lets look at the effect of the medium
blankValues=mp[c("corValue"),startingCells=0,corTime=timeSelection]
plot(mp[c("corTime","corValue"),startingCells=0,corTime=timeSelection,medium="galactose"],col="blue",ylim =c( min(blankValues), max(blankValues) ) )
points(mp[c("corTime","corValue"),startingCells=0,corTime=timeSelection,medium="glucose"],col="red")
# there is stil a small medium effect...
#
# now you have to decide if you count that as a result or as a bias...
#
# lets remove that
mediumEffect=aggregate(formula=corValue~medium,data=mp[startingCells=0,corTime=timeSelection],FUN = mean)
mediumEffect
mp$corValue=mp$corValue-mediumEffect$corValue[match(mp["medium",level=1],mediumEffect$medium)]
# lets check
plot(mp[c("corTime","corValue"),startingCells=0,corTime=timeSelection,medium="galactose"],col="blue",ylim =c( min(blankValues), max(blankValues) ) )
points(mp[c("corTime","corValue"),startingCells=0,corTime=timeSelection,medium="glucose"],col="red")
#
# ok from this:
plot(mp[c("corTime","value"),corTime=timeSelection])
# to this:
plot(mp[c("corTime","corValue"),corTime=timeSelection])
# 
# can still remove first time points from eveything
# which is probably much more effective...
# doing just this as bias corrction also works... but you might throw away the baby with the bath water...
meanFirstPerWell=aggregate(corValue~wellNr,mp[firstXTimePoints,c("corValue","wellNr")],mean)
meanFirstPerWell
mp$corValue=mp$corValue-meanFirstPerWell$corValue[match(mp["wellNr",level=1],meanFirstPerWell$wellNr)]
mp$corValue[mp$corValue<0]=0.000001 # remove negative and 0 values... else log(value) will be problematic
# 
plot(mp[c("corTime","corValue"),corTime=timeSelection])
#
colnames(mp)
unique(mp$premedium)
unique(mp$medium)
#
# plot 
value=mp["corValue",corTime=timeSelection]
ylim=c(min(value),max(value))
plot(mp[c("corTime","corValue"),medium="glucose",corTime=timeSelection],col="blue",ylim=ylim)
points(mp[c("corTime","corValue"),medium="galactose",corTime=timeSelection],col="red",ylim=ylim)
legend("topleft", legend=c("glucose","galactose"), fill=c("blue","red"))
# well they like glucose better.... even though they where pregrown on galactose
#
# and premedium?
plot(mp[c("corTime","corValue"),premedium="galactose",corTime=timeSelection],col="blue",ylim=ylim)
points(mp[c("corTime","corValue"),premedium="galactose+AHM",corTime=timeSelection],col="red",ylim=ylim)
legend("topleft", legend=c("galactose","galactose+AHM"), fill=c("blue","red"))

# ok that is less clear
#
# lets get the growth rates... 
# remove the blanks
nonBlanc=unique(mp[mp$startingCells!=0,"wellNr"])
plot(mp[c("corTime","corValue"), corTime=timeSelection, wellNr=nonBlanc]) # still blanc like values
plotPerPlate(mp) # ok 5 10 starting cells don't allways growh.... so remove them...
nonBlanc=unique(mp[mp$startingCells>10,"wellNr"])
plot(mp[c("corTime","corValue"), corTime=timeSelection, wellNr=nonBlanc]) # perfect!


# my getGrowthRates function works per well...
# my getGrowthRate function works with x and y values... so that is possible
results=data.frame(matrix(NA,nrow = length(nonBlanc),ncol = 6)) #reserve space
results[,1]=nonBlanc
colnames(results)=c("wellNr","slope","base","r2","doublingTime","timeZero")
results
index=1
for(wellNr in nonBlanc){
  result=getGrowthRate(values=mp["corValue", corTime=timeSelection,wellNr=wellNr],time=timeSelection, nrOfTimePointsForSlope = "5%",plotTitle=paste("well:",wellNr))
#   print(result)
  results[index,2:6]=result
  index=index+1
}
results
# i am actualy pleasently suprised on how well a lot of these fit....
# there are some in which the black line stars rounding a lot quicker...but on the straigth part they are the same...
# lets look at some of them:3
# 1_10-1_12, 2_10-2_12 and 3_10-3_12 for instance
getGrowthRate(values=mp["corValue", corTime=timeSelection, wellNr="3_10"], time=timeSelection, nrOfTimePointsForSlope = "5%")
# increasing the area over which you calculate the slope does not imporve the fit!
getGrowthRate(values=mp["corValue", corTime=timeSelection, wellNr="3_10"], time=timeSelection, nrOfTimePointsForSlope = "20%")
# what you see here is that there appear to be 2 different exponential growths, as the log(value) has 2 different slopes
# lets do it manual then...
getGrowthRateBetween(values=mp["corValue",corTime=timeSelection,wellNr="3_10"], time=timeSelection, start=165, end=190) # fits the straight part
getGrowthRateBetween(values=mp["corValue",corTime=timeSelection,wellNr="3_10"], time=timeSelection, start=130, end=150) # fits the bendy part 
# it appears that each time the algorithm picks the straight part, as this apperently has the steepest slope with the normal data...
#
# i am not sure what todo... so i just ignore it for now :)
# add results back to the microplate object
mp[names(results)[2:6]]=results[match(mp["wellNr"],results$wellNr),2:6]
head(mp[level=2])



colFunc=colorRampPalette(c("white", "lightgreen"))

# ok now the data has been cleaned, and the growth rates determined...
# lets display them per condition
stuff=aggregate(slope~medium+premedium+startingCells,data=mp[level=2],mean)
stuff$slopeSD=aggregate(slope~medium+premedium+startingCells,data=mp[level=2],sd)$slope
stuff

# 2 much stuff!
#
# lets look at the effect of nr of starting cells!
stuff=aggregate(slope~startingCells,data=mp[level=2],mean)
stuff$slopeSD=aggregate(slope~startingCells,data=mp[level=2],sd)$slope
stuff
bp=barplot(stuff$slope, names.arg=stuff$startingCells,ylab="slope",xlab="nr of starting cells",col=colFunc(6))
text(bp, c(stuff$slope), round(stuff$slope,4),pos = 1) 
#
stuff=aggregate(slope~startingCells+medium,data=mp[level=2],mean)
stuff$slopeSD=aggregate(slope~startingCells+medium,data=mp[level=2],sd)$slope
stuff


# medium
stuff=aggregate(slope~medium,data=mp[level=2],mean)
stuff$slopeSD=aggregate(slope~medium,data=mp[level=2],sd)$slope
stuff

bp=barplot(stuff$slope, names.arg=stuff$medium,ylab="slope",xlab="medium")
text(bp, c(stuff$slope), round(stuff$slope,4),pos = 1) 

#### medium + premedium
stuff=aggregate(slope~medium+premedium,data=mp[level=2],mean)
stuff$slopeSD=aggregate(slope~medium+premedium,data=mp[level=2],sd)$slope
stuff
# bp=barplot(stuff$slope, names.arg=stuff$medium,ylab="slope",xlab="medium")
data=tapply(stuff$slope, list(stuff$medium, stuff$premedium), mean)
data
bp=barplot(data, beside=T, ylab="slope", col=colFunc(2))
legend("topright", legend=c("galactose","glucose"),fill=colFunc(2),bty="n")




# lets display them per condition
mp$combinedMediumCondition=paste(mp$premedium,mp$medium,sep="_")
data=tapply(mp$slope, list(mp$startingCells, mp$combinedMediumCondition), mean)
data
data=data[4:9,]
data
# bp=barplot(data, beside=T,col=colFunc(6))
bp=barplot(data, beside=T,col=colFunc(6),names.arg=c("galactose\ngalactose","galactose\nglucose","galactose+AHM\ngalactose","galactose+AHM\nglucose"))
legend("topleft", legend=rownames(data),fill=colFunc(6),bty="n",title="nr of starting cells")
# text(bp, 0, rownames(data),pos = 3 ,cex=.5) 
text(bp, 0, round(data,3),pos = 3 ,cex=.5) 
title("growthRate")


#
# display of graphs per condition
plot(mp[c("corTime","corValue"),medium="galactose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc],col="blue")
points(mp[c("corTime","corValue"),medium="galactose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc],col="red")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc],col="green")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc],col="purple")

plot(mp[c("corTime","corValue"),medium="galactose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=100],col="blue")
points(mp[c("corTime","corValue"),medium="galactose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=500],col="red")
points(mp[c("corTime","corValue"),medium="galactose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=1000],col="green")

plot(mp[c("corTime","corValue"),medium="galactose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=100],col="blue")
points(mp[c("corTime","corValue"),medium="galactose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=500],col="red")
points(mp[c("corTime","corValue"),medium="galactose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=1000],col="green")

plot(mp[c("corTime","corValue"),medium="glucose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=100],col="blue")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=500],col="red")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose+AHM",corTime=timeSelection,wellNr=nonBlanc,startingCells=1000],col="green")

plot(mp[c("corTime","corValue"),medium="glucose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=100],col="blue")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=500],col="red")
points(mp[c("corTime","corValue"),medium="glucose",premedium="galactose",corTime=timeSelection,wellNr=nonBlanc,startingCells=1000],col="green")




mp[level=2]
mp[c("wellNr","medium","slope")]


nonBlanc

results

