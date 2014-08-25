# todo figure out demo paths after install!

# test data from Iraes Rabbers, TY!
file=paste(getwd(),"/tests/testdata/project2/project2.ods",sep="")
mp=readLayoutFile(file)

# show data
plotPerPlate(mp)

### remove blanc
# get avarage
# TODO MAKE IT PER TIME POINT!
averageBlanc=mean(mp$value[mp["strain",level="measurement"]=="blanc"])
# remove per time point
mp$corValue=mp$value-averageBlanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... no clue
# 
# # take natural logarithm of corOD/corOD(t=0) 
# f$lncorOD <- log(f$corOD/f$corOD[1,])



### growth curves
wellSelection=mp$strain!="blanc"
#settings

# next part will take a while
readline("press any key to continue")

# result=getGrowthRate(self = mp,wellNrs = wellSelection,timeColumn = "time",valueColumn = "corValue",experimentIdentifierColumn = "strain",additionalInformationColumn = "Sugar",concentrationOfSubstrateColumn = "IPTG")

result=getGrowthRate(mp,wellSelection)


index=getWellsMeasurementIndex(mp,wellNrs[1])
plot(mp@.data$measurement$time[index],mp@.data$measurement$corValue[index])
plot(result$gcFittedModels[[1]])
plot(result$gcFittedSpline[[1]])


for(i in 1:nrOfWells){
  plot(result$gcFittedModels[[i]])
}

for(i in 1:nrOfWells){
  plot(result$gcFittedSpline[[i]])
}

summary=result$gcTable

# summ=summary(result)
#head(summ) = head(result$gcTable)



