### test the new stuff

file=paste(getwd(),"/tests/testdata/project/KineticData.xls",sep="")
mp=novostar.xls(file)


#################################

testData@.data$data$measurement[[1]][[1,"newColumn"]]=(1:24000)[1]
testData@.data$data$measurement[[1]][[1,"newColumn"]]=1


#############################

xlsFile=paste(getwd(),"/tests/testdata/project/layout.xls",sep="")
xlsxFile=paste(getwd(),"/tests/testdata/project/layout.xlsx",sep="")
odsFile=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")

xls=readLayoutFile(xlsFile)
xlsx=readLayoutFile(xlsxFile)
ods=readLayoutFile(odsFile)

all(xls[]==xlsx[], na.rm=T)
all(xls[]==ods[], na.rm=T)


file=paste(getwd(),"/tests/testdata/project/KineticData.xls",sep="")
mp=novostar.xls(file)

test=NULL
test$test1="123"
test$test2="456"
test$test3="5323"
# first try this
colNames=names(test)
mp[1,colNames,level="plate"]=test[colNames]

#somehow this is done
utest=unlist(test)
colNames=names(utest)
mp[1,colNames,level="plate"]=utest[colNames]

# ok... both don't work
# lets see how data.frame handles it...
df=data.frame(a=1,b=2)
df[1,colNames]=test[colNames]#ok that doesnt work
df[[1,colNames]]=test[colNames]#thats worse!
df[1,colNames]=utest[colNames] # same error as the first
# error: Error in `*tmp*`[[j]] : recursive indexing failed at level 2
df[colNames]=test[colNames] # ... this does work!?!?!?! WHY?!??!?!
# i already hated R and data.frames....
# but... WHY?!?!??!?!?!?



df=data.frame(a=1:2,b=2:3)
df[1,3]=1# this is actually allowed...

df=data.frame(a=1:2,b=2:3)
df[1,3:4]=1 # is not
df[1,3:4]=1:2 # is not
df[1:2,3:4]=1:4 # is not



test=NULL
test$test1=1:4
test$test2=2:5
test$test3=3:6
test





#################################


file=paste(path.package("microplate"),"/extdata/test.xlsx",sep="")

# warnings are still thrown even in a try catch... FUCKING R!
gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)



tryCatch(expr={gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)},error=function(e)T,warning=function(w)F)

tryCatch(expr={gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)},error=function(e)F)





file=paste(getwd(),"/tests/testdata/project/layout.xls",sep="")
xls=read.xls(file, stringsAsFactors=FALSE, blank.lines.skip=FALSE,header=FALSE)

xls



file=paste(getwd(),"/tests/testdata/project/layout.xlsx",sep="")
xlsx=read.xls(file, stringsAsFactors=FALSE, blank.lines.skip=FALSE,header=FALSE)

file=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")
ods=read.ods(file)

xlsFile=paste(getwd(),"/tests/testdata/project/layout.xls",sep="")
xlsxFile=paste(getwd(),"/tests/testdata/project/layout.xlsx",sep="")
odsFile=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")

xls=readLayoutFile(xlsFile)
xlsx=readLayoutFile(xlsxFile)
ods=readLayoutFile(odsFile)

all(xls[]==xlsx[], na.rm=T)
all(xls[]==ods[], na.rm=T)



file=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")
test=readLayoutFile(file)

test


test2=copy(test)
test=merge(test,test2)



test2=merge(test2,test)

test=merge(test,test2)
# plate data NA
test[]

test@.data$data$plate # here is the cause... now to find the cause of the cause...




showNonASCII( readLines(f))



####

file=paste(getwd(),"/tests/testdata/project/layout.ods",sep="")
test=readLayoutFile(file)

file=paste(getwd(),"/tests/testdata/project/KineticData.xls",sep="")

novostar.xls(file)


###

file=paste(getwd(),"/tests/testdata/testproject/layout_test.ods",sep="")
test=readLayoutFile(file)

file=paste(getwd(),"/tests/testdata/testproject/layout_test.ods",sep="")
read.ods(file)



file=paste(getwd(),"/tests/testdata/testproject/layout.xls",sep="")
test=readLayoutFile(file)



################
test=c(1,2,3,4)
test=append(test,c(5,6,7))
test




##############

test=list(row=1:2,column=1:2,measurement=list( list(value=1,temp=1,time=1),list(value=2,temp=1,time=1) ) )
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
colnames(testData)

testData[]
testData[level="well"]
testData[level="plate"]

testData





testData=new("MicroPlate")
test=list(row=1,column=1,measurement=list( list(value=1,temp=1,time=1)) )
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
tdf=testData[]

tdf[1]
tdf[1,1]
tdf[,1]

testData[1]
testData[1,1] # leveled...
testData[,1]

tdf[2]
tdf[1,2]

testData[2]
testData[1,2] # wrong data.frame with 1 element

tdf[5]
tdf[1,5]

testData[5]
testData[1,5] # wrong data.frame with 1 element

# try again with multiple plates...
testData=new("MicroPlate")
test=list(row=1,column=1,measurement=list( list(value=1,temp=1,time=1)) )
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
testData=addPlate(testData,newData=test, test=1234, cookies=1423423)
tdf=testData[]
tdf[] # returns data.frame everything
tdf[1] # returns data.frame first column
tdf[1,1] # returns object as type of the object...
tdf[,1] # returns a vector of the first column
tdf[1,] # returns a data.frame of the first row # eeeeeugh... that is odd....
tdf[2:4] # returns a data.frame with 3 columns
tdf[1,2:4] # returns a data.frame of the first row of 3 colums
tdf[2:4,2:4] # returns a 3x3 data.frame
tdf[2:4,1] # returns a vector of 2:4 row and 1st column
tdf[,2:4] # returns a data.frame with 3 columns




(testData[1,2]+3)[1,1] # might not cause many bugs... but it looks awefull!








##############
test=data.frame(a=1:26, b=letters[1:26], c=rep("tada",26))
test

test=as.list(test)
test


##########
# compare more
# more compare

workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "GJS_layout3263.tab")
layoutData=readLayoutFile(file=file)
file2=file.path(testdir, "3263.dbf")
newData=novostar.dbf(path=file2)
testData=new("MicroPlate")
# testData=addPlate(testData,newData=newData)
testData=addPlate(testData,newData=newData,layoutData=layoutData)
tdf=testData[]


tdf
l=list()
l$measurement=tdf[c("value","time","temp")]
l$measurement$index=as.integer((0:599/50)+1)
head(l$measurement)
l$well=unique(tdf[c("row","column","content","basic","sample")])
l$well$plateName=rep(1,12)
head(l$well)
l$plate=data.frame(plateName=unique(tdf["plateName"]))

l

# new
test=function(list){
  l=list$measurement
  l=cbind(l,list$well[list$measurement$index,])
  return(l)
}
cookies=test(l)
cookies
dim(cookies)

system.time(test(l)) # 0.001
system.time(replicate(100,test(l))) # 0.079

system.time(testData[]) #0.003
system.time(replicate(100,testData[])) # 0.258

system.time(tdf) #0
system.time(replicate(100,tdf)) # 0.002

# new
test=function(list){
  temp=list$well
  l=list$measurement
  l=cbind(l,list$well[list$measurement$index,])
  return(l)
}

#############
# compare


workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "GJS_layout3263.tab")
layoutData=readLayoutFile(file=file)
file2=file.path(testdir, "3263.dbf")
newData=novostar.dbf(path=file2)
testData=new("MicroPlate")
# testData=addPlate(testData,newData=newData)
testData=addPlate(testData,newData=newData,layoutData=layoutData)
tdf=testData[]


tdf
l=list()
l$measurement=tdf[c("value","time","temp")]
head(l$measurement)
l$well=unique(tdf[c("row","column","content","basic","sample")])
head(l$well)
l$plate=data.frame(plateName=unique(tdf["plateName"]))
l$well$b=((1:12)*50)-49
l$well$e=((1:12)*50)
l
list=l
i=1

# new
test=function(list){
  l=list$measurement
  temp=data.frame()
  for(i in 1:dim(list$well)[1]){
    for(k in 1:(list$well$e[[i]]-list$well$b[[i]]+1) )
      temp=rbind(temp,list$well[i,])
  }
  l=cbind(temp,l)
  return(l)
}
cookies=test(l)
cookies
dim(cookies)

system.time(test(l)) # 0.295
system.time(replicate(100,test(l))) # 29.734

system.time(testData[]) #0.003
system.time(replicate(100,testData[])) # 0.258

system.time(tdf) #0
system.time(replicate(100,tdf)) # 0.002


test=function(list){
  l=list$measurement
  temp=data.frame()
  for(i in 1:12){
    for(k in 1:50)
      temp=rbind(temp,list$well[i,])
  }
  l=cbind(temp,l)
  return(l)
}
system.time(test(l)) # 0.317
#eeeugh

test=function(list){
  l=list$measurement
  temp=data.frame()
  for(i in 1:12){
    for(k in 1:50)
      temp=append(temp,list$well[i,])
  }
  l=cbind(temp,l)
  return(l)
}
system.time(test(l)) # 1.116
# eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeugh


temp=NULL
temp=list()
# temp=rep(append(temp,list$well[i,]),10)
temp=replicate(10,append(temp,list$well[i,]))
temp=t(temp)
temp

test=function(list){
  l=list$measurement
  temp=list()
  for(i in 1:12){
    test=list()
    test=replicate(50,append(test,list$well[i,]))
    temp=append(temp,test)
  }
  temp=t(temp)
  l=cbind(temp,l)
  return(l)
}
test(l)
system.time(test(l)) # 1.807





temp=data.frame()
temp=rbind(temp,list$well[i,])
temp=rbind(temp,list$well[i,])
temp

temp=data.frame()
temp=replicate(n=10,expr=rbind(temp,list$well[i,])) # why doesnt this work!!!
temp

temp=data.frame()
# temp=replicate(n=10,expr=rbind(temp,list$well[i,]),simplify=FALSE) # why doesnt this work!!!
temp=rbind(temp,replicate(n=10,list$well[i,],simplify=FALSE))
temp

test=data.frame()
replicate(n=10,list$well[i,])
replicate(n=10,list$well[i,],simplify=FALSE)

replicate(n=10,list$well[i,],simplify=FALSE)
t(replicate(n=10,as.list(list$well[i,])))

test=function(list){
  l=list$measurement
  temp=list()
  for(i in 1:12){
    temp=rbind(temp,t(replicate(n=50,as.list(list$well[i,]))))
  }
  l=cbind(temp,l)
  return(l)
}
test(l)
system.time(test(l)) # 0.073
system.time(replicate(100,test(l))) # 7.901

system.time(testData[]) #0.003
system.time(replicate(100,testData[])) # 0.258

#### why the speed difference????
tdf
x=testData

lalala=function(x){
  col=colnames(x)
  row=NULL
  returnValue=data.frame(matrix(nrow=600,ncol=9))
  colnames(returnValue)=col
  for(colnr in 1:length(col)){ # for each column
    # always first fill tempdata with the whole column (at measurement level)
    # then do the row select
    level=x@.data$colLevel[x@.data$colNames==col[colnr]]
    tempData=NULL
    if (level=="well"){
      # data at top level
      #
      # data has to be repeated for each measurement
      for (i in 1:length(x@.data$data$measurement)){ # for each measurement
        tempData=append(tempData,rep(x@.data$data[[col[colnr]]][[i]],length(x@.data$data$measurement[[i]][[1]])))
      }
    } else if(level=="measurement"){
      # get whole column
      for(i in 1:length(x@.data$data$measurement)){
        tempData=append(tempData, x@.data$data$measurement[[i]][[col[colnr]]])
      }
    } else if(level=="plate"){
      # repeat for each well*each measurement
      for(i in 1:length(x@.data$data[[1]])){ # for each well
        # get the corresponding plate values
        data=x@.data$plate[x@.data$data$plate[[i]],col[colnr]]
        tempData=append(tempData,rep(data,length(x@.data$data$measurement[[i]][[1]]))) # for each measurement
      }
      #         tempData=lapply(x@.data$data, function(x)returnValue=append(returnValue,x[[name]]))
      tempData=c(tempData,recursive=T)
    } else {
      stop("data at unknown level... this error means a coding error as it should have been cought above!")
    }
    
    if(is.null(row)){
      # whole column
      returnValue[,colnr]=tempData
    } else {
      # specific rows
      returnValue[,colnr]=tempData[row]
    }
  }
  return(returnValue)
}

lalala(testData)

system.time(lalala(testData)) # 0.003
system.time(replicate(100,lalala(testData))) # 0.246
# ok so its prop not a package=compiled or something thing...


temp=list()
temp=rbind(temp,replicate(n=50,as.list(list$well[i,])))
temp=rbind(temp,replicate(n=50,as.list(list$well[i,])))
dim(temp) # nope...
temp


tdf
l=list()
for(i in c("value","time","temp") ){
  l$measurement[[i]]=tdf[[i]]
}
tdf2=unique(tdf[c("row","column","content","basic","sample")])
for(i in c("row","column","content","basic","sample") ){
  l$well[[i]]=tdf2[[i]]
}
l$well$b=((1:12)*50)-49
l$well$e=((1:12)*50)
l$plate$plateName=unique(tdf[["plateName"]])
l
list=l

test=function(list){
  l=list$measurement
  for(i in names(list$well)){
    temp=list()
    for(j in 1:length(list$well[[i]])){
      l[[i]]=append(l[[i]],replicate(n=50,list$well[[i]][j]))
    }
  }
  return(data.frame(l))
}
test(l)

system.time(test(l)) # 0.012
system.time(replicate(100,test(l))) # 1.232
#... WTF!!!
# is it the append thing???

system.time(testData[]) # 0.003
system.time(replicate(100,testData[])) # 0.275

test=function(list){
  l=data.frame(matrix(nrow=length(list$measurement[[1]]),ncol=length(list$measurement)+length(list$well)))
  colnames(l)=append(names(list$measurement),names(list$well))
  for(i in names(list$measurement)){
    l[[i]]=list$measurement[[i]]
  }
  
  for(i in names(list$well)){
    index=1
    for(j in 1:length(list$well[[i]])){
      #       print(replicate(n=50,list$well[[i]][j]))
      #       print(c(list$well[["b"]][j],list$well[["e"]][j]))
      #       print(l[c(list$well[["b"]][j],list$well[["e"]][j]),i])
      l[index:(index+49),i]=replicate(n=50,list$well[[i]][j])
      index=index+50
    }
  }
  return(data.frame(l))
}
test(l)
system.time(test(l)) # 0.017
system.time(replicate(100,test(l))) # 1.927
# why is this still so slow compared to other stuff...



# 
# 
# 
# 
# 
# temp=append(temp,rep(list$well[i,],10))
# temp
# 
# temp=data.frame()
# temp=rep(append(temp,list$well[i,]),10)
# temp
# 
# temp=data.frame()
# temp=replicate(n=10,expr=append(temp,list$well[i,]))
# temp
# 
# temp=data.frame()
# for (k in 1:10){
#   temp=rbind(temp,list$well[i,])
# }
# temp



####################
# spectramax

workspace = getwd()
testdir=file.path(workspace, "tests/testdata/growth/")
file=file.path(testdir, "20100114_data.txt")


test=spectramax.txt(path=file)
test



############
# testing multi sheet xls files for layout
library(gdata)


workspace = getwd()
testdir=file.path(workspace, "tests/testdata/")
file=file.path(testdir, "layout_test.xls")

xls = read.xls(file,stringsAsFactors=FALSE, sheet=3) # FUCK FACTORS!!!
xls 

read.xls



#####################


# 
workspace = getwd()
testdir=file.path(workspace, "tests/testdata/xls/")
file=file.path(testdir, "KineticData.xls")
test=novostar.xls(path=file)
test

testData=new("MicroPlate")
testData=addPlate(testData,newData=test)

xls=read.xls(file, stringsAsFactors=FALSE)



#############################
### understanding formula ###
coplot # check source, cause it works with formula...


# functions created in coplot
deparen <- function(expr) {
  while (is.language(expr) && !is.name(expr) && deparse(expr[[1L]])[1L] == 
           "(") expr <- expr[[2L]]
  expr
}
getOp <- function(call) deparse(call[[1L]], backtick = FALSE)[[1L]]
bad.formula <- function() stop("invalid conditioning formula")
bad.lengths <- function() stop("incompatible variable lengths")


formula=Petal.Length ~ Petal.Width | Species
formula="Petal.Length ~ Petal.Width | Species"
# class(formula)="formula"
formula=as.formula(formula)
typeof(formula) # language
class(formula) # formula


data=iris

formula=deparen(formula) # appears to do nothing
if (!inherits(formula, "formula")) 
  bad.formula()
y <- deparen(formula[[2L]]) # Petal.Length
rhs <- deparen(formula[[3L]]) # Petal.Width | Species
if (getOp(rhs) != "|") 
  bad.formula()
x <- deparen(rhs[[2L]]) # Petal.Width
rhs <- deparen(rhs[[3L]]) # Species

x.name <- deparse(x)
x <- eval(x, data, parent.frame())
y <- eval(y, data, parent.frame())

y
rhs
x
formula

plot(x,y)


deparen("abc ~ 123")
deparen("((abc ~ 123")
deparen(call("abc ~ 123"))
deparen(call("((abc ~ 123"))
deparen(call("((abc)) ~ 123"))

call("iris","test")
deparse(call("iris","test"))






coplot(Petal.Length ~ Petal.Width | Species, data = iris)
plot(Petal.Length ~ Petal.Width, data = iris)

coplot(Petal.Width | Species, data = iris)

"~"(Petal.Length,Petal.Width, data= iris)
"~"(Petal.Length,Petal.Width, data= iris)



coplot(Petal.Length ~ Petal.Width | Species, data = iris)
coplot((Petal.Length ~ Petal.Width) | Species, data = iris) # gives error
coplot((Petal.Length ~ Petal.Width) | Species, data = iris) # gives error
coplot((Petal.Length ~ Petal.Width | Species), data = iris) # works
coplot(formula=(Petal.Length ~ Petal.Width | Species), data = iris) # works
coplot(formula=Petal.Length ~ Petal.Width | Species, data = iris) # works
coplot(Petal.Length ~ (Petal.Width | Species), data = iris) # works
coplot("~"(Petal.Length, (Petal.Width | Species)), data = iris) # works

coplot(Petal.Length ~ (Petal.Width | Species), data = iris) # works



test=lm(Petal.Length ~ Petal.Width | Species,data=iris)

lm(Petal.Length ~ Petal.Width ,data=iris)


lm(Petal.Length ~ Petal.Width | Species,data=iris)
lm(Petal.Length ~ Petal.Width, data=iris)

Petal.Length ~ Petal.Width | Species

eval


##########


testData=new("MicroPlate")
test=list(row=1:2,column=1:2,measurement=list( list(value=1:5,temp=1:5,time=1:5),list(value=2,temp=1,time=1) ) )
testData=addPlate(testData,newData=test)
testData=addPlate(testData,newData=test)

tdf=testData[]


plot(temp~time,data=tdf)
plot(temp~time,data=testData)
plot(temp~time,data=testData[]) # somehow [] is called twice...

plot(row~time,data=tdf)
plot(row~time,data=testData[])




tdf[] # everything
testData[]
tdf[1] # first col
testData[1] # first col
testData[1,level="measurement"]
testData[1,level=1]

tdf[5] # temp
testData[5] # temp

tdf[1,] # first row
tdf[,1] # first col
tdf[1,2] # first row 2nd col
tdf[,] # everything
# multi arg
tdf[c(1,3)] # returns first and 3th column
tdf[c(1,3),] # returns first and 3th row so its consistent...
tdf[c(),]








#########################################
# more data.frame behaviour
df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df
df[,c("a","b")]=c(1,2,3,4,5,6,7,8)
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c(1,2,3)]=df
df
df[c(2,3,4)]=df # names arent ignored for new naming, but they are for assigning
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c("a","d","e")]=df # note that the colnames of the new df are ignored here...
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c(3,4)]=df # yeah this doesnt work... 
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c(3,4,"5")]=df # c converts it all to chars... and since they are names abc not 123... eeugh
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
colnames(df)=c(1,2,3)
df[c(3,4,"5")]=df # and now for the magic, yup col 3 is overwritten now... = c("3","4","5")
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c(2,3)]=NULL # does not work!
df[2]=NULL # does work!
df

df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[]=c("123","456") # yeah this works... with rep() all the way... FUGLY!
df


################################
# WTF AM I DOING WRONG??!?!?!

currentName="a"
data=NULL
data[[currentName]]=append(data[[currentName]],"sss")
data[[currentName]]=append(data[[currentName]],"sss")


currentName="a"
data=NULL
data[currentName]=append(data[currentName],"sss")
data[currentName]=append(data[currentName],"sss")
data

currentName="a"
data=NULL
data[[currentName]]=append(data[currentName],"sss")
data[[currentName]]=append(data[currentName],"sss")
data


currentName="a"
data=NULL
data[currentName]=append(data[[currentName]],"sss")
data[currentName]=append(data[[currentName]],"sss")
data

currentName="a"
data=list()
data[currentName]=append(data[currentName],"sss")
data[currentName]=append(data[currentName],"sss")
data


currentName="a"
data=list()
data[[currentName]]=append(data[[currentName]],"sss")
data[[currentName]]=append(data[[currentName]],"sss")
data



data=NULL
data[["test"]]=append(data[["test"]],c(1,2,3))
data

data2=list(c("axxbc","xxdef","xxghi"))

data=NULL
data[["test"]]=append(data[["test"]],c("abc","def","ghi"))
data[["test"]]=append(data[["test"]],data2[[1]])
data



##########################

workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "GJS_layout3263.tab")
layoutData=readLayoutFile(file=file)
file2=file.path(testdir, "3263.dbf")
newData=novostar.dbf(path=file2)
testData=new("MicroPlate")
testData=addPlate(testData,newData=newData,layoutData=layoutData)
testData=addPlate(testData,newData=newData,layoutData=layoutData)
testData[]
testData
# 2 errors 
# 1: NA for 2nd plate
# 2: NA for 2nd plate not detected... cause testData[] crashes...



col
testData@.data$data[["sample"]][2]="A"


newData
layoutData
testData[]
testData@.data$data






###########################
# ...
boo=function(...)print(paste(...))
boo("b","O",0)

###########################


workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "3263.dbf")
test=novostar.dbf(path=file)
testData=new("Data")
testData=addData(testData,newData=test)
tdf=testData[]

tapply(tdf$value ,tdf$content, mean)
tapply(testData$value ,testData$content, mean)

tapply(tdf$value, tdf$content, mean)





require(stats)
by(warpbreaks, warpbreaks[,"tension"],   function(x) lm(breaks ~ wool, data = x))


###########
#memtest
g=1:1000000 # 3.8 Mb
g=1:10000000 # 38.1 Mb
g=1:100000000 # 381 Mb
g=1:300000000 # 1.1 Gb





g=1:10000000 # 38.1 Mb
temp=list()
for (i in 1:10){
  temp2=list()
  temp2$values=g
  temp2$temp=g
  temp=append(temp, c(experiment=letters[i],plate=1, measurement=list(temp2)))
} # 762 Mb
head(temp) # FUGLY!



# vs
g=1:10000000 # 38.1 Mb
temp=data.frame(values=rep(g,10)) # doesnt work anymore??? why???
temp$temp=rep(g,10)
experiment=NULL
for(i in 1:10){
  experiment=append(experiment,rep(letters[i], length(g)))
}
temp$experiment=experiment # doesnt work!
temp$plate=1 # also doesnt work!
#
head(temp)
dim(temp)

#
#
g=1:10000000 # 38.1 Mb
temp=list()
temp$value=rep(g,10)
temp2=temp
temp3=temp
temp4=temp
head(temp[["value"]])
temp[["value"]][1:100]=1
head(temp[["value"]])
temp[["value"]][1:10000]=1
temp
temp2
temp3
temp4
temp2[["value"]][1:10000]=2
rm(temp3)
rm(temp4)
temp2[["value"]][1:10000]=2

h=g # add more!
rm(h) # its removed from global... but comp still claims ram! :(
rm(g)

#################################################3

a=c("a","b","c")
b=c("c","d")
is.element(a,b)

#########################################################

# checking df behaviour
df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4),c=c(555,5,5,183447))
df[c(1,3),]
df[c(1,3)]
df[c("a","c")]

df$c
df$c(1,3)

# indexing is kinda weird...
df[] # everything
df[1] # first col
df[1,] # first row
df[,1] # first col
df[1,2] # first row 2nd col
df[,] # everything
# multi arg
df[c(1,3)] # returns first and 3th column
df[c(1,3),] # returns first and 3th row so its consistent...
df[c(),]
#
df[c("a","b")] 
df[c(1,2)]
df[c("a",2)] # does not work!
df[c(2,"a")] # also doesnt work!
#
dim(df) # returns nr of row,col
length(df) # returns nr of col

class(df) # returns "data.frame"
class(df[]) # returns "data.frame"


#####################################



B=setClass("new-data.frame", representation("data.frame")) 
a=new("new-data.frame")
a



### test DF inheritence
# http://stackoverflow.com/questions/2497111/r-how-can-i-use-apply-on-rows-of-a-data-frame-and-get-out-column-name
df<-data.frame(a=c("x","x","y","y"),b=c(1,2,3,4))
df<-data.frame(a=c("x","x","y","y","z","z"),b=c(1,2,3,4,1,2))

df
df$a
df$b
typeof(df$a)
class(df$a)

df$a==c("x","x","y","y","z","z")
(df$a==c("x","x","y","y","z","z"))&&T # ... eeeugh

factor(c("x","x","y","y"))

# ok it does not return a data.frame :P ... mmmh
library(plyr)
adply(df, 1, function (data.frame_in) print(data.frame_in$a))

# now on my Data instance
workspace = getwd()
testdir=file.path(workspace, "tests/testdata/enzymeAssays")
file=file.path(testdir, "3263.dbf")
test=novostar.dbf(path=file)
testData=new("Data")
testData=addData(testData,newData=test)
# testData=addData(testData,newData=df)
testData

tdf=testData[]
adply(tdf, 1, function (data.frame_in) print(data.frame_in$row))
adply(testData, 1, function (data.frame_in) print(data.frame_in$row))





################################################

test=new.env()
test
test[1]=1 # ok that doesnt work....
test[[1]]=1 # ok that doesnt work either....
test$test=1 # so this is the only way to acces....
test
test$test[2]
##################

# the love works!!!
test=new("Data")
colnames(test)
colnames(test)=1
colnames(test)
test2=new("Data")
colnames(test2)
test2=test
colnames(test)
colnames(test)=2
colnames(test2)

## 

test=new("Data")
print(test)
test$data
colnames(test)
test$test=1:3
test$test2=1:5
colnames(test)
colnames(test)="cookies"
colnames(test)
test@.data$colnames
test@.data$data


typeof(test@.data)
typeof(test@.data$data)
typeof(test@.data$data["test2"])

if(is.na(test@.data$data["test2"])){
  print("cookies!!!")
}else{print("crumble!!!")}



test=new.env()
test$data
test$data=NULL
test$data
test$data[test]=1:5
test$data


### testing environment
test=data.frame(a=1:5,b=2:6,c=3:7)
test2=data.frame(b=7:8,a=6:7,d=1:2)
test=smartbind(test,test2)
test

testData=new.env()
testData$Data=new("Data") # add magic..
testData2=testData
testData$Data
testData2$Data

# now change one
testData$Data@.data=test
testData$Data
testData2$Data
# magic! both have changed!!!

# now test if its a class with an "environment" instead of an enviroment witha class with a "data.frame"/"List"...
test=new.env()
test$a=1:5
test$b=2:6
test$c=3:7
test$a

testData=new("Data")
testData2=testData
testData@.data$a
testData2@.data$a
testData@.data=test
testData@.data$a
testData2@.data$a
# no magic! only one is changed!!!
testData2=testData
testData@.data$a=1
testData@.data$a
testData2@.data$a
# and magic again!!!
testData
testData2

## ok we learned that if you put an environment in a class and copy the class... it kinda works
## put if you change anything else in the class... you are screwed again!!! 
## 
## if working with this make sure to 
## never overwrite the environment! but rewrite the data inside the environment!!!...
##



# setGeneric("foo", function(a, b) standardGeneric("foo")) 
# setMethod("foo", signature("A1", "A2"), function(a, b) "1-2")
#



# $ test... how to turn it into EUR!
test=data.frame(a=1:5,b=2:6,c=3:7)
test   $   a #i never thougth about it before.. but this makes sense :)
test
rownames(test)
rownames(test)=c("r1","r2","r3","r4","r5")
rownames(test)
test$r1 #does not work!
# i wonder if there even is a row version of $




setClass("string", contains="character")
string <- function(obj) new("string", as.character(obj))

## this works
setMethod("+", signature(e1 = "character", e2 = "ANY"),
          function (e1, e2) string(paste(e1, as.character(e2), sep = "")))
## this doesnt!
setMethod("+", signature(aaa = "character", e2 = "ANY"),
          function (aaa, e2) string(paste(aaa, as.character(e2), sep = "")))
## check with
getGeneric("+")


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

testData=new("MicroPlate")# works
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
testData=new("MicroPlate")
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




