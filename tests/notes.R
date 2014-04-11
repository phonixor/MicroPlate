





# default shiney does localhost
# you need to change it to your own ip if you want to change that
ip="192.168.178.10"
runApp("../microplate",host=ip)

runApp("../microplate")






# example on how the data will be stored!
library(gtools)
test=data.frame(a=1:5,b=2:6,c=3:7)
test2=data.frame(b=7:8,a=6:7,d=1:2)
test=smartbind(test,test2)
test


testData=new("Data",test) # does not work
testData$dataMatrix
testData=new("Data",.data=test) # works!
testData@.data
typeof(test)
typeof(testData)

testData=new("Data")# works
addData(testData,newData=test)# and works... well no
testData@.data
testData
# AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGH!!!

testData=new("Data")# works
testData=addData(testData,newData=test) # this works.... 
testData@.data
#
# *CRIES*
# R SUCKS MONKEY BALLS!!!!
# 
# if R had OO it would go from:
# testData=addData(testData,newData=test)
# to:
# testData.addData(newData=test)
# or:
# testData.addData(test)

workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "3263.dbf")
test=novostar.dbf(path=file)
testData=new("Data")
testData=addData(testData,newData=test)
testData@.data

test3=by(test$value,test$content,mean)
test3=as.data.frame(test3) # requires other package... taRifx
test3

test3=by(test$value,test$content=="b",mean)# kinda works
test3

df=test

df
aggregate(value ~ content,df,mean)
test4


library(data.table)
dt=data.table(df)
dt




experiment=new("MicroplateExperiment",.mainData=testData)





head(state.x77)
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          mean)


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
head(chickwts)
aggregate(weight ~ feed, data = chickwts, mean)
head(warpbreaks)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
head(airquality)
cbind(airquality$Ozone, airquality$Temp)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
head(esoph)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
head(iris)
aggregate(. ~ Species, data = iris, mean)
head(ToothGrowth)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))









head(df)
aggregate(df[""],df["content"],mean)
aggregate(value~content,df,mean)

dfmean=aggregate(value~content,df,mean)
dfmean$sd=aggregate(value~content,df,sd)$value
dfmean

rownames(dfmean)=dfmean$content
dfmean
plot(dfmean$value,labels=dfmean$content)


df=test

df[c("time","content")] # this way you can select multiple stuff...


aggregate(value ~ row+column,df,mean)



df
library("Hmisc")
dfmean=aggregate(value ~ time,df,mean)
dfmean$sd=aggregate(value ~ time,df,sd)$value
dfmean
plot(dfmean$time,dfmean$value)
errbar(dfmean$time,dfmean$value,dfmean$value+dfmean$sd,dfmean$value-dfmean$sd)



df
dfmean=aggregate(value~content,df,mean)
dfmean$sd=aggregate(value~content,df,sd)$value
dfmean
len=1:length(dfmean$content)
plot(len,dfmean$value)
library("Hmisc")
errbar(len,dfmean$value,dfmean$value+dfmean$sd,dfmean$value-dfmean$sd)


dfmean$value+dfmean$sd
dfmean$value
dfmean$value-dfmean$sd

x  = 1:5
y  = c(1.1, 1.5, 2.9, 3.8, 5.2)
sd = c(0.1, 0.3, 0.2, 0.2, 0.4)

plot (x, y)
errbar(x,y,y+sd,y-sd)






d = data.frame(
  x  = c(1:5)
  , y  = c(1.1, 1.5, 2.9, 3.8, 5.2)
  , sd = c(0.2, 0.3, 0.2, 0.0, 0.4)
)

##install.packages("Hmisc", dependencies=T)
library("Hmisc")

# add error bars (without adjusting yrange)
plot(d$x, d$y, type="n")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=T, pch=1, cap=.1)
)

# new plot (adjusts Yrange automatically)
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=F, pch=1, cap=.015, log="x")
)





------------
  
  
  names=data$method
x = 1:13*2-1
CI.up = as.numeric(data$mean)+as.numeric(data$ci)
CI.dn = as.numeric(data$mean)-as.numeric(data$ci)
plot(data$mean~x, cex=1.5,xaxt='n',ylim=c(0.3,0.40), xlab='',ylab='lalala!', main='blahblahblah',col='blue',pch=16)
axis(1, at=x, labels=names)
arrows(x,CI.dn,x,CI.up,code=3,length=0.2,angle=90,col='red')
legend("bottomleft",paste(names,": S.E=",data$se),ncol=6,text.width=1)

-------------
  
  
# http://stackoverflow.com/questions/11562656/averaging-column-values-for-specific-sections-of-data-corresponding-to-other-col

df=test  
aggregate(value~content=="b",df,mean)## case sensitivy ignrored??????

blaad="value"
aggregate(blaad~content=="b",df,mean)#yeah... i was afraid of that...



apply
sapply
lapply



df=data.frame(a=1:6,content=c("B","B","x1","x1","x2","x2"), value=1:6)
df


blaad=sapply(df$value[df$content=="B"],mean)
blaad
print(blaad)

df$content
df$content=="B"
mean(df$value[df$content=="B"])

mean(df$blaad[df$content=="B"])

df[blaad] # this works....
df[blaad][df$content=="B"]# this does not
df[blaad[df$content=="B"]] # nope
df[blaad,df$content=="B"] # nope

df
blaad="value"
bro="content"
cow="B"


df[bro]==cow # works
df[blaad] # works
df[blaad,df[bro]==cow] # nope
df[blaad][df[bro]==cow] # YEAAAAH!!!!

mean(df[blaad][df[bro]==cow]) #whooohoooo!!

s <- split(df, df$content)

sapply( s, function(x) mean(x$speed) )




