library(ggplot2)
library(shiny)


comp1.oral <- function(ka,ke,v,f=1,dose,time){
  (ka * dose * f)/ (v*(ka-ke)) * (exp(-ke * time) - exp(-ka*time))
}

dose.prof <- function(dose, time, schedule){
  DS = data.frame(    
    D = dose,
    Time = schedule*time
  )
  return(DS)
}



PKsim.data <- function(ka=5,v=600,ke=.05,dose=100,f=1,DS, t.half=24){
  DS$D <- DS$D*1000
  TT <- DS$Time 
  if(min(TT) <0)  DS$Time <- DS$Time - min(DS$Time)
  
  
  Cp <- 0
  if(dim(DS)[1]==1){
    Tm <- max(DS$Time)+5*t.half
    t <- seq(0,Tm,by=0.05)
    Cp <- comp1.oral(ka,ke,v,f,DS$D,t)
  }
  
  else{
    Tm <- max(DS$Time)+2*t.half
    for(i in 1:(dim(DS)[1]-1)){
      t <- seq(0,Tm-DS$Time[i], by=.05)
      C1 <- comp1.oral(ka,ke,v,f,DS$D[i],t)
      t1l <- seq(0,DS$Time[i],by=.05)
      t1 <- rep(0,length(t1l))
      C2 <- c(t1[-length(t1)],C1)
      Cp <- Cp+ C2
    }
  }
  tt <- seq(0,max(Tm),by=0.05)/24 + min(TT)/24
  #if(min(TT)<0) tt <- tt + min(TT)/24
  dat1 <- data.frame(
    Time = tt,
    Conc = Cp
  )
  return(dat1)
}  

plot.PK <- function(PK, samp.time=NULL){
  if(!is.null(samp.time)){
    samp.Conc = PK$Conc[match(samp.time,PK$Time)]
    samp <- data.frame(
      x = samp.time,
      y = samp.Conc
    )
    
    
    if(is.null(PK$type)) {
      return(PK %>% ggvis(~Time, ~Conc) %>% layer_paths() %>%
               layer_points(~x,~y, data=samp,size.hover:=200))
    }
    if(!is.null(PK$type)){
      samp1 <- rbind(samp,samp)
      samp1$type <- factor(rep(levels(PK$type),each=dim(samp)[1]))
      
      return(PK  %>% ggvis(~Time, ~Conc, stroke = ~type) %>%
               layer_lines()  %>% 
               layer_points(~x,~y, data=samp1, stroke = ~type,size.hover:=200) )
    }
  }
  else { return(PK  %>% ggvis(~Time, ~Conc) %>%
                  layer_lines() ) }
}

overlay.PK <- function(PKgraph, PK, samp.time=NULL,color="red"){
  
  if(is.null(samp.time)){
    PKnew <- PKgraph %>% layer_paths(~Time,~Conc,data=PK,stroke = color)
  }
  else{
    samp.Conc = PK$Conc[match(samp.time,PK$Time)]
    samp <- data.frame(
      x = samp.time,
      y = samp.Conc
    )
    PKnew <- PKgraph %>% layer_paths(~Time,~Conc,data=PK,stroke := color) %>%
      layer_points(~x,~y, data=samp,size.hover:=200)  
  }
  PKnew
}
