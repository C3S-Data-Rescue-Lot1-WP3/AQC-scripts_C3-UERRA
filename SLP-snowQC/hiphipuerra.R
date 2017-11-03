### I am having a lot of trouble for numeric. 

## Hi ho lets go

### Bring in sequence control
### Set up a function that takes all the time series and makes statistics of % of equal values length of largest strike of equal values
### Implement smart correction for obvious errors, such as 10122 to 1012.2. Before accepting the value, check if it is inside de box
##					of the boxplot. 


# needs to install XLConnect. Careful with the JAVA installation. 


require(XLConnect)
master<-function(digitized='digitized.txt',metadata='metadata.txt',output='output.txt',textdir='./text/',name='output.pdf'){

# OBJECTIVE: opens a file containing a list of all the digitized excel files (full path is needed). Will open one by one
# and send them to different qc routines. 

  
  
pdf(name)
if(file.exists(output)){file.remove(output)}
if(!file.exists(textdir)){dir.create(textdir)}
list<-read.fwf(digitized,widths=300,stringsAsFactors=FALSE)
num<-nrow(list)
for(i in 1:num){
  badboy<-paste('Working with',list[i,1])
  namo <- unlist(strsplit(list[i,1],"[/.]+"))
  stname <- namo[length(namo)-1]
  print(noquote(paste('Working with',list[i,1])))
  write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
  wk <- loadWorkbook(list[i,1])
  myframe = readWorksheet(wk, 1,colTypes='numeric', forceConversion = TRUE)
  myframe<-notanumber(myframe,tag='SLP',output,stname)
  elpuma<-length(unlist(strsplit(list[i,1],'/')))
  pavoreal<-unlist(strsplit(list[i,1],'/'))[4]
  vivalanumerasion<-paste(textdir,substring(pavoreal,1,5),'0.txt',sep='')
  write.table(myframe,vivalanumerasion,sep='\t',col.names=TRUE,row.names=FALSE,quote=FALSE,na='-99.9')
  myframe<-read.table(vivalanumerasion,na.strings=c('-99.9','-88.8'),stringsAsFactors=FALSE,header=TRUE)
  myframe<-cleanframe(myframe)
  myframe<-duplidates(myframe,output)
#  myframe<-mmhgtohpa(myframe,output)
  write.table(myframe,vivalanumerasion,sep='\t',col.names=TRUE,row.names=FALSE,quote=FALSE,na='-99.9')
  myframe<-read.table(vivalanumerasion,na.strings=c('-99.9','-88.8'),stringsAsFactors=FALSE,header=TRUE)
  myframe<-qcpressure(myframe,output,nyu,stname)
  write.table(myframe,vivalanumerasion,sep='\t',col.names=TRUE,row.names=FALSE,quote=FALSE,na='-99.9')
  myframe<-read.table(vivalanumerasion,na.strings=c('-99.9','-88.8'),stringsAsFactors=FALSE,header=TRUE)
  myframe<-outliers(myframe,output,tag="SLP",i,stname)
  write.table(myframe,vivalanumerasion,sep='\t',col.names=TRUE,row.names=FALSE,quote=FALSE,na='-99.9')
  myframe<-read.table(vivalanumerasion,na.strings=c('-99.9','-88.8'),stringsAsFactors=FALSE,header=TRUE)
  myframe<-bivora(myframe,tag='SLP',filedet=stname,output=output,stname=stname)
  xlcFreeMemory()
  }

dev.off()
}

mmhgtohpa<-function(myframe,output,tag='SLP',mimo=700,mamo=800,frac=1.3332239){
x<-names(myframe)
z<-which(grepl(tag,x)==TRUE)
if(length(z)!=0){	for(i in 1:length(z)){
		columna<-z[i]
		mayday<-which(myframe[,columna]>=mimo & myframe[,columna]<=mamo)
		if(length(mayday)>0){myframe[mayday,columna]<-myframe[mayday,columna]*frac
		ganso<-max(myframe[mayday,columna]);patito<-min(myframe[mayday,columna])
		fred<-paste('mmHg_hPa',x[columna],'with range',patito,'-',ganso,sep='\t')
		 barny<-paste('mmHg_hPa',x[columna],'Number of changed values:',length(mayday),sep='\t')
	dino<-paste('mmHg_hPa',x[columna],'between:',min(myframe[mayday,1]),'-',max(myframe[mayday,1]),sep='\t')
	vilma<-rbind(fred,barny,dino)	
		write.table(vilma,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)}

}}
return(myframe)
}

outliers<-function(myframe,output,tag,serie,stname){

x<-names(myframe)
z<-which(grepl(tag,x)==TRUE)
if(length(z)!=0){	for(i in 1:length(z)){
		columna<-z[i]
		if(length(which(!is.na(myframe[,columna])))>10){
		nyu<-boxplot(as.numeric(myframe[,columna])~as.numeric(myframe[,2]),range=3.5,plot=FALSE)
		if(length(nyu$out)!=0){
			for(j in 1:length(nyu$out)){
#				if(serie==41 & j==7){browser()}
				target<-which(myframe[,2]==nyu$group[j] & myframe[,columna]==nyu$out[j])
				# Notice this length statement is necessary as Murphy can bring the same outlier in the same month
				# of different years, thus it is corrected the first time and does not exist in the second instance!
				if(length(target!=0)){
				badboy<-cbind('Outlier',stname,myframe[target,c(1:3)], x[columna], myframe[target,columna])
				write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)}
				myframe[target,columna]<- -88.8
			}}
		}
	}	
}
return(myframe)
}


cleanframe<-function(myframe){
  names(myframe)[1:3]<-c("Year","Month","Day")
  buenas<-which(apply(myframe,2,function(x) length(which(!is.na(x))))>10)
  myframe<-myframe[,buenas]
  buenas<-which(apply(myframe,1,function(x) length(which(!is.na(x))))>0)
  myframe<-myframe[buenas,]
  return(myframe)
}


notanumber<-function(myframe,tag,output,stname){
x<-names(myframe)
#z<-which(grepl(tag,x)==TRUE)
z<-3:ncol(myframe)
numero<-length(z)
	if(numero!=0){for(i in 1:numero){
	columna<-z[i]
	y<-myframe[,columna]
	nas<-which(is.na(y));if(length(nas)!=0){y[nas]<- -99.9}
	target<-which(is.na(as.numeric(y)))
	if(length(target)!=0){
	badboy<-cbind("Not a number",stname,myframe[target,c(1:3)],x[columna], myframe[target,columna]) 
	myframe[target,columna]<- -88.8
	write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)}
	}}

return(myframe)
}


qcpressure<-function(myframe,output,nyu,stname){
### Comprobar si funciona. 
  x<-names(myframe)
  z<-which(grepl('SLP',x)==TRUE)
	  numero<-length(z)
  if(numero!=0){
    for(i in 1:numero){
	    columna<-z[i]
	    limits<-c(950,1100)
	    target<-which((as.numeric(myframe[,columna]) < limits[1] | as.numeric(myframe[,columna]) > limits[2]) 
	    & as.numeric(myframe[,columna])!=-99.9 & as.numeric(myframe[,columna])!=-99.9) 
	
	if(length(target!=0)){
	badboy<-cbind("UnrealisticValue",stname,myframe[target,c(1:3)],x[columna],myframe[target,columna])
	write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
	myframe[target,columna]<- -88.8
	}
	}}
return(myframe)
}


duplidates<-function(myframe,output){ # could build a safe interpreter to remove those which are strictly equal. 
  target<-which(duplicated(myframe[,c(1:3)]))
  if(length(target!=0)){
    target<-sort(c(target-1,target))
    badboy<-cbind('DuplicateDate',myframe[target,]) # Make appear both instances. 
    write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
    }
  return(myframe)
}



variables<-function(myframe){
  print(names(myframe))
  return(names(myframe))
}



bivora<-function(myframe,tag,lowquant=0.00001,highquant=0.99999,plotty=TRUE,filedet,name='bivariate_outliers.pdf',output,stname){
# Which are pressure? Original quad values were 0.0001 and 0.9999
  x<-names(myframe)
  z<-which(grepl(tag,x)==TRUE)
  if(length(z)>1){	for(i in 1:length(z)){
    
    peta<-z[-i]
    zeta<-array(dim=c(1,length(peta)))
    for(j in 1:length(zeta)){
      zeta[j]<-calcoverlap(myframe[,z[i]],myframe[,peta[j]])
      }
    if(max(zeta)>0.7 & max(zeta)<1.0){ # added upper limit because an overlap of 1 means only one value 
      target<-peta[which(zeta == max(zeta))]
      shouldi<-cor(myframe[,z[i]],myframe[,target],use='pair')

## Murphy is everywhere: two variables have the same overlap. Then I take the better correlated. 
## This makes me wonder if I should select instead of overlap by a more elaborated system, taking into account
## how far in time observations are (i.e., I prefer to use 8-14 rather than 8-20). BUT: correlations are usually, larger than 0.9 so may not matter!
      if(length(shouldi)>1){
        recalifica<-which(shouldi==max(shouldi))
		  	if(length(recalifica)>1){recalifica<-1}
			  target<-target[recalifica]
		    shouldi<-shouldi[recalifica]
		    }
      
      espinete=0
      if(exists('don')){espinete<-length(which(don[1]==target & don[2]==z[i]))}
      if(!exists('don')){don<-c(target,z[i])}else{pimpom<-c(target,z[i]);don<-rbind(don,pimpom)}
      if(espinete==0){
        if(shouldi > 0.8){
          x1<-mean(myframe[,z[i]],na.rm=TRUE)	
          x2<-mean(myframe[,target],na.rm=TRUE)
          sd1<-sd(myframe[,z[i]],na.rm=TRUE)	
          sd2<-sd(myframe[,target],na.rm=TRUE)
          somewhere<-conditioned(myframe[,z[i]],myframe[,target],x1,x2,sd1,sd2,shouldi)
          
          ups<-which(somewhere$prob > highquant);downs<-which(somewhere$prob < lowquant) 	
          
          if(plotty==TRUE){
            
            plot(myframe[,z[i]],myframe[,target],xlab=x[z[i]],ylab=x[target],main=filedet,cex=0.75)
            points(myframe[ups,z[i]],myframe[ups,target],col=2,pch=19)
            points(myframe[downs,z[i]],myframe[downs,target],col=3,pch=19)
	}
          if(length(union(ups,downs))>0){ #added in case there are no outliers, LA 22-12-2015
            overtherainbow<-(cbind('BivOutlier',stname,myframe[union(ups,downs),c(1:3)],x[z[i]], x[target], myframe[union(ups,downs),c(z[i],target)],
                                   round(abs(myframe[union(ups,downs),z[i]]-myframe[union(ups,downs),target]), digits = 2)))
            turutu<-order(overtherainbow[,10],decreasing = TRUE )
            overtherainbow<-overtherainbow[turutu,]
            write.table(overtherainbow,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
          }


	

	}}}}	
	
}	

}

calcoverlap<-function(a,b){
total<-length(which(!is.na(a)))
haya<-which(!is.na(a))
hayb<-which(!is.na(b))
hayaba<-intersect(haya,hayb)
cuantos<-length(hayaba)
ratio<-cuantos/total
if(is.na(ratio)){ratio <- 0.0}
return(ratio)
}



conditioned<-function(x,condition,mx,my,sx,sy,ro){
# OBJECTIVE: conditional probabilities x|y based on the bivariate normal distribution. 
# the distribution conditioned parameters are given by formulas 4.37a & 4.37b from Wilks	
# having as an input the 5 parameters of the bivariate normal distro. 
# INPUT: 
# $x: vector of quantiles of suspect values in series x
# $condition: conditioning value in the series y
# $mx: mean of x
# $my: mean of y
# $sx: sd of x
# $sy: sd of y
# $ro: correlation between x and y
# OUTPUT:
# $px: cummulative conditioned probabilities associated to the input quantiles of x
	media<-mx+ro*sx/sy*(condition-my)
	desvi<-sx*sqrt(1-ro**2)
	px<-pnorm(x,media,desvi)
	return(list(prob=px,media=media,desvi=desvi))	

}

