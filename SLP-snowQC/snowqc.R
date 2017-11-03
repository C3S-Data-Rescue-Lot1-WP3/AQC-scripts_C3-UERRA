snowcheck <- function(dbdata) {
  
  
  stations <- unique(dbdata$stationid)
  outinfo <- paste("snow_errors_",Sys.Date(),".txt",sep="")
  
  if(file.exists(outinfo)){
    file.remove(outinfo)
  }
  
  write(paste("stationid","Test","year","month","day","FS","SD",sep="\t"),outinfo)
  
  
  for(i in 1:length(stations)){
    statoi <- stations[i]
    print("--------", quote = FALSE)
    print(paste("Working with",statoi), quote=FALSE)
    doi <- dbdata[which(dbdata$stationid == statoi),]
    require(reshape2)
    
    #  It is assumed that the data are in database format
    
    snowdata <- dcast(doi,rawyear+rawmonth+rawday~elementid)

    
    nan <- which(snowdata =="-99.9", arr.ind = TRUE)
    snowdata[nan] <- NA
    
    #Step 1: date errors
    baddates(doi,c(2,5:7),outinfo,statoi)
    
    #Step 2: Snow in the summer
    summersnow(snowdata,outinfo,statoi)
    
    #Step 3: Remove non-numeric values (to be updated to include Fl, Fr and DBR)
    snowdata <- nonumbs(snowdata,outinfo,statoi)
    
    #Step 4: An increase in TSD must mean an increase in FS
    notrealfs(snowdata,outinfo,statoi)
    
    #Step 5: Look for outliers
    outliers(snowdata,outinfo,statoi)
    
    #Step 6: Look for big jumps
    bigjumps(snowdata,outinfo,statoi)
    
  }
  
}



baddates<-function(myframe,secttocheck,outinfo,statname){ # Adapted from Enric's hiphipuerra
  
  #secttocheck is a vector of the columns to look at
  datos <- as.Date(paste(myframe$rawyear,myframe$rawmonth,myframe$rawday),format="%Y %m %d")

  if(length(which(is.na(datos) == TRUE)) > 0){ 
    jerk <- which(is.na(datos))
    print(paste("WEIRD DATES",myframe$rawyear[jerk],myframe$rawmonth[jerk],myframe$rawday[jerk]))
    write(paste(statname,"WEIRD DATES",myframe$rawyear[jerk],myframe$rawmonth[jerk],myframe$rawday[jerk]),outinfo,append=TRUE)
  }
  
  target<-which(duplicated(myframe[,secttocheck]))
  if(length(target!=0)){
    
    target<-sort(target)
    badboy <-cbind(statname,'Duplicate Date',myframe[target,])
    print(head(badboy))
    stop("Some duplicated dates here. Better go check that out")
    
  }
}  


summersnow <- function(snowdata,outinfo,statname){
  #This program checks for snow in the summer
  ss <- which(findInterval(snowdata$rawmonth,c(5,10))==1 & (snowdata[,c(4)]!=0 | snowdata[,c(5)]!=0))
  if(length(ss)>0){
    print("Summer snow?!", quote=F)
#    ss<-sort(c(ss-1,ss)) #Not sure why I've done this 04032016
    badboy<-cbind(statname,'Summer snow',snowdata[ss,]) # Make appear both instances? 
    write.table(badboy,outinfo,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
    
    }
    
}

notrealfs <- function(snowdata,outinfo,statname){
  #This tests looks for increases in snow depth that are not matched by increases in fresh snow
  
  inc <- c(NA,diff(as.numeric(snowdata$sd9999), 1)) #an increase in snow depth, missing value for the first time step
  notreal <- which(inc > 0 & as.numeric(snowdata$fs9999) == 0) #find dates with an increase in snowdepth, but no fresh snowfall
  
  if(length(notreal) > 0){
    print("Some not real snowfall going on", quote=F)
    likele <- cbind(statname,"Erroneous FS", snowdata[notreal,])
    write.table(likele,outinfo,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)

  }
  
  
}

nonumbs <- function(snowdata,outinfo,statname){
  snowdata[] <- lapply(snowdata,as.character)
  oc <- snowdata
  snowdata[] <- suppressWarnings(lapply(snowdata,as.numeric))
#  a <- oc[rowSums(is.na(snowdata)) > 0, ]
  a <- as.matrix(oc)[which(is.na(snowdata))]
  ng <- a[which(!is.na(a))]
  if(length(ng) > 0){ 
    print(paste(length(ng),"date values rejected as non-numeric"),quote=F)
    look <- which(is.na(snowdata) & !is.na(oc), arr.ind = TRUE) #need to change this to output FS and SD, not just the non numeric value?
    ocmelt <- cbind(statname,"Non-numeric",oc)
    write.table(ocmelt[look[,1],],outinfo,sep="\t",row.names = FALSE,col.names=FALSE,append = TRUE, quote = FALSE)
  }
  
  return(snowdata)
}


outliers<-function(snowdata,output,statname){
  #adapted from hiphipuerra
  
  x<-names(snowdata)
  z<-which(grepl("fs|sd",x)==TRUE)
  if(length(z)!=0){	for(i in 1:length(z)){
    columna<-z[i]
    if(length(which(!is.na(snowdata[,columna])))>10){
      actvals <- which(as.numeric(snowdata[,columna]) > 0)
      doi <- snowdata[actvals,]
      nyu<-boxplot(as.numeric(doi[,columna])~as.numeric(doi[,2]),range=3.5,plot=FALSE)
      if(length(nyu$out)!=0){
        print("Here be outliers", quote=F)
        for(j in 1:length(nyu$out)){
          target<-which(as.numeric(doi[,2])==as.numeric(nyu$names[nyu$group[j]]) & as.numeric(doi[,columna])==nyu$out[j])
          # Notice this length statement is necessary as Murphy can bring the same outlier in the same month
          # of different years, thus it is corrected the first time and does not exist in the second instance!
          if(length(target!=0)){
            badboy<-cbind(statname,paste('Outlier',x[columna]),doi[target,])
            write.table(badboy,output,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)}
        }}
    }
  }	
  }
}



bigjumps <- function(snowdata,outinfo,statname){
  #This tests looks for increases in snow depth that are not matched by increases in fresh snow
  
  inc <- c(NA,diff(as.numeric(snowdata$sd9999), 1)) #an increase in snow depth, missing value for the first time step
  notreal <- which(inc < 0 & as.numeric(snowdata$fs9999) > (sd(snowdata$fs9999, na.rm = TRUE)*2) ) #find dates with a decrease in snow depth and a big fresh snow dump
  
  if(length(notreal) > 0){
    print("A big snowdump that doesn't match the snowdepth", quote=F)
    likele <- cbind(statname,"SD mismatch FS", snowdata[notreal,])
    write.table(likele,outinfo,append=TRUE,sep='\t',col.names=FALSE,row.names=FALSE,quote=FALSE)
    
  }
  
  
}
