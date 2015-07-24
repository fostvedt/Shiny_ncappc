
read.PKPDdata<-function(file.name)
{ 
  temp.data<-read.csv(file.name,na.strings=".")
  colnames(temp.data)<-toupper(colnames(temp.data))
  return(temp.data)
}


XYplot.orig<-function(orig.data,X.name,Y.name,ID.name)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t)
  ggplot( data=new.data, aes(x=X.t, y=Y.t))+
     geom_point()+geom_line()+labs(x=X.name,y=Y.name) + scale_y_log10()
}   


PK.ID.orig<-function(orig.data,X.name,Y.name,ID.name)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t)
  ggplot( data=new.data, aes(x=X.t,y=Y.t))+
    geom_point()+geom_line()+labs(x=X.name,y=Y.name)+ scale_y_log10()+
    facet_wrap(~ID.t)
}   

PK.TRT.orig<-function(orig.data,X.name,Y.name,ID.name,TRT)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  trt <- orig.data[,TRT]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t,trt=trt)
  ggplot( data=new.data, aes(x=X.t,y=Y.t,group=trt))+
    geom_point()+geom_line()+labs(x=X.name,y=Y.name)+ scale_y_log10()+
    facet_wrap(~ID.t)
}   



