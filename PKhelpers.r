
read.PKPDdata<-function(file.name)
{ 
  temp.data<-read.csv(file.name,na.strings=".")
  colnames(temp.data)<-toupper(colnames(temp.data))
  return(temp.data)
}


XYplot.orig<-function(orig.data,X.name,Y.name,ID.name,x.lim,y.lim)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t)
  ggplot( data=new.data, aes(x=X.t, y=Y.t))+
    geom_point()+geom_line()+labs(x=X.name,y=Y.name) + scale_y_log10()
}   

