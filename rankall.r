rankall=function(outcome=string(),num)
{b<-read.csv("outcome-of-care-measures.csv")
 d<-c("heart attack","heart failure","pneumonia")
 e<-grep(outcome,d)
 if(length(e)==0)
 {stop("invalid outcome")} 
 if(outcome==d[1])
 {f<-data.frame(b[,2],b$State,b[,11])}
 if(outcome==d[2])
 {f<-data.frame(b[,2],b$State,b[,17])}
 if(outcome==d[3])
 {f<-data.frame(b[,2],b$State,b[,23])}
 f[,3]<-suppressWarnings(as.numeric(levels(f[,3])[f[,3]]))
 g<-f[complete.cases(f),]  
 group<-split(g,g[,2])
 group_sorted<-lapply(group,function(df){df[order(df[,3],df[,1]),]})
 if(num=="worst")
 {specific<-lapply(group_sorted,tail,1)}
 if(num=="best")
 {num<-1
  specific<-lapply(group_sorted,"[",num,,drop=FALSE)}
 if(num!="worst")
 {specific<-lapply(group_sorted,"[",num,,drop=FALSE)}
 frame<-do.call(rbind.data.frame, specific)
 names(frame)<-c("hospital name","state") 
 answer<-data.frame(frame$hospital,frame$state)
 names(answer)<-c("hospital","state")
 answer} 
