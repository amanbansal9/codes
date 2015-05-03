rankhospital=function(state=string(),outcome=string(),num)
{b<-read.csv("outcome-of-care-measures.csv")
 c<-grep(state,b$State)
 d<-c("heart attack","heart failure","pneumonia")
 e<-grep(outcome,d)
 if(length(c)==0)
 {stop("invalid state")}
 if(length(e)==0)
 {stop("invalid outcome")}
 i<-b[grep(state,b$State),] 
 if(outcome==d[1])
 {f<-data.frame(i[,2],i[,11])}
 if(outcome==d[2])
 {f<-data.frame(i[,2],i[,17])}
 if(outcome==d[3])
 {f<-data.frame(i[,2],i[,23])}
 f[,2]<-suppressWarnings(as.numeric(levels(f[,2])[f[,2]]))
 g<-f[complete.cases(f),]  
 x<-order(g[,2],g[,1])
 if(num=="best")
 {num<-1} 
 if(num=="worst")
 {num<-tail(x,n=1)} 
 as.character(g[tail(x,n=1),1])}