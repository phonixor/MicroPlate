
# TODO maybe make this a demo instead of a test

# test data from Iraes Rabbers, TY!
# file=paste(getwd(),"/tests/testdata/project2/project2.ods",sep="")
file=paste(getwd(),"/,,/testdata/project2/project2.ods",sep="")
mp=readLayoutFile(file)

# show data

### remove blanc
# get avarage
# TODO MAKE IT PER TIME POINT!
averageBlanc=mean(mp$value[mp["strain",level="measurement"]=="blanc"])
# remove per time point
mp$corValue=mp$value-averageBlanc
mp$corValue[mp$corValue<0.008]=0.008  # minimal detection limit of platereader ... no clue
# 
# # take natural logarithm of corOD/corOD(t=0) # why give it the log?
# f$lncorOD <- log(f$corOD/f$corOD[1,])


### growth curves
wellSelection=mp$strain!="blanc"
#settings
myopt <- grofit.control(nboot.gc=100,interactive=F,suppress.messages=T,model.type=c("gompertz"))
# time

wellNrs=(1:(mp@.data$levelSize[2]))[wellSelection]
nrOfWells=sum(wellSelection)
nrOfTimePoints=length(getWellsMeasurementIndex(mp,wellNrs[1]))
  
# TODO error not all wells have same time points... 


time=matrix(0,nrow = nrOfWells, ncol = nrOfTimePoints)
data=data.frame(matrix(0,nrow = nrOfWells, ncol = nrOfTimePoints+3))
data[,1]=mp@.data$well[["strain"]][wellSelection]
data[,2]=mp@.data$well[["Sugar"]][wellSelection]
data[,3]=mp@.data$well[["IPTG"]][wellSelection]
index=0
for(i in wellNrs){# for each well
  index=index+1
  data[index,(4:(nrOfTimePoints+3))]=mp@.data$measurement[["corValue"]][getWellsMeasurementIndex(mp,i)]
  time[index,1:nrOfTimePoints]=mp@.data$measurement$time[getWellsMeasurementIndex(mp,i)]
}
# head(data)


result=gcFit(time = time,data=data,control = myopt)



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



