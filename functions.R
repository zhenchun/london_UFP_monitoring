filterCPCv1<-function(vectorTOfilter) {
  tempdat <- vectorTOfilter[-1] 
  validSize <- tempdat > 100
  Factor <- ratioV3(vectorTOfilter, 10)
  validFactor <- !Factor[,2]
  keepIndex <- validSize & validFactor
  tempdat[which(keepIndex == FALSE)] <- NA
  return(tempdat)
}

filterMDv4<-function(fileTOfilter){
  #  fileTOfilter<-MDdat2#for testing
  tempdat0<-fileTOfilter
  tempdat<-tempdat0[-1,] #first row (1-sec) removed to match output from 'ratioV2'
  
  ##SIZE RANGE CHANGES (MING 17-06-2015 email)
  validSize<-tempdat$Size>10 & tempdat$Size<300
  validFlow<-tempdat$Flow>=0.9 & tempdat$Flow<=1.1
  #MD error breakdown:
  #the following converts the MDerror value into a binary number with 32 places.
  #this is used to figure out what errors are present by column...
  md<-as.data.frame(do.call(rbind, lapply(tempdat$Error,function(x){as.integer(intToBits(x))})))
  names(md)<-c('MDw1','MDw2','MDw3','MDw4','MDw5','gap','MDe1','MDe2','MDe3','MDe4','MDe5','MDe6','MDe7','MDe8','MDe9','MDe10')
  Errors <- md$MDw1|md$MDw2|md$MDw3|md$MDw4|md$MDw5|md$MDe1|md$MDe2|md$MDe3|md$MDe4|md$MDe5|md$MDe6|md$MDe7|md$MDe8|md$MDe9|md$MDe10 
  notErrors<-!Errors  
  validCount<-tempdat$Number>100
  Factor<-ratioV3(tempdat0$Number,10)  #the # of rows will now match those of other valid* vars.
  validFactor<-!Factor[,4]
  keepIndex<-validSize & validFlow & notErrors & validCount & validFactor
  cleanData<-tempdat[keepIndex,]
  percOf24hrs<-dim(cleanData)[1]/60/60/24*100
  stats<-data.frame(orig_n=length(validSize),total_drop=sum(!keepIndex),size_drop=sum(!validSize),flow_drop=sum(!validFlow),Num_drop=sum(!validCount), Factor_drop=sum(!validFactor), error_drop=sum(Errors),perc_total_drop=sum(!keepIndex)/length(validSize)*100,perc_of_24hrs=percOf24hrs,MDw1=sum(md$MDw1),MDw2=sum(md$MDw2),MDw3=sum(md$MDw3),MDw4=sum(md$MDw4),MDw5=sum(md$MDw5),MDe1=sum(md$MDe1),MDe2=sum(md$MDe2),MDe3=sum(md$MDe3),MDe4=sum(md$MDe4),MDe5=sum(md$MDe5),MDe6=sum(md$MDe6),MDe7=sum(md$MDe7),MDe8=sum(md$MDe8),MDe9=sum(md$MDe9),MDe10=sum(md$MDe10))
  #remove the first row (1-second) of cleanData & stats as
  return(list(cleanData,stats))
  
}

ratioV3<-function(v,x){ #(vector v, factor x)
  vectorLen<-length(v)
  fact<-x
  tempdat0<-v[-vectorLen] #drop the last observation
  tempdat1<-v[-1]         #drop the first observation
  ratio<-tempdat1/tempdat0
  
  ratio1<-as.data.frame(cbind(ratio,ratio>fact|ratio<1/fact)) #the length of this vector is 1 less than the input
  
  
  cumsum1<-cumsum(ratio1[,2]) #cumulative sum.
  rle1<-rle(cumsum1) #run length encoding--Compute the lengths and values of runs of equal values
  tossDF<-as.data.frame(cbind(rle1$values,rle1$lengths)) #dataframe with values and their lengths
  names(tossDF)<-c('values','lengths')
  tossDF<-tossDF[-1,] #remove value of '0' from being tossed.
  index<-which(tossDF$lengths==1) #if a value occurs only once, that means that it is surrounded on both sides by values that are a factor of >10 different.
  valsToRm<-tossDF$values[index] #using the index, pick out the cumulative sums to be removed.
  to_remove<-is.element(cumsum1,valsToRm)                     
  
  ratio1<-cbind(ratio1,cumsum1,to_remove)
  names(ratio1)[2]<-paste('GTorLTfact_',fact,sep='')
  return(ratio1)
}

importMD_dm<-function(filename){
  #LOAD MINIDISC DATA INTO R:
  dat<-read.csv(file=filename,header=TRUE,skip=5,sep="\t")
  oldjava<-dim(dat)[2]==16
  #Following removes extra last column that shows up in dat thru javascript:
  if(dim(dat)[2]==16)rmcol<-16  #output file processed with earlier javascript 
  if(dim(dat)[2]==18)rmcol<-18  #output file processed with javascript v1.217.
  dat<-dat[,-rmcol] #remove extra \t column (turns out that somewhere between season 1&2, the javatool provides 2 additional columns (Surface & Mass); therefore, need to remove column 16 before, and column 18 after new columns introduced...
  timemd <- strptime(dat[,1],format="%d-%b-%Y %H:%M:%S") #CONVERT TIMESTAMP TO R time standard
  dat2<-cbind(PosixTime=timemd,dat)
  return(list(dat2,oldjava))
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
