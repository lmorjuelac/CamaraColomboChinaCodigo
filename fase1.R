#Funciones


#Pruebas de liquidez

#pruebas


pacida<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  acida<-data[5,1]/data[16,1]
  return(acida)
}

pdisponibilidad<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  dips<-data[1,1]/data[16,1]
  return(dips)
}

pKTN<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  KTN<-data[8,1]- data[16,1]
  return(KTN)
}

psolvencia<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  solvencia<-data[8,1]/data[16,1]
  return(solvencia)
}


pendeudamiento<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  endeu<- data[9,1]/data[8,1]
  return(endeu)
}

papalancamiento<-function(datosExcel){
  data <- data.frame(datosExcel[,2:3]) 
  apalan<-data[8,1]/data[20,1]
  return(apalan)
}



gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="coral2")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="darkorange")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="darkolivegreen1")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}
gg.gauge1 <- function(pos,breaks=c(0,30,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/breaks[4])
    th.end   <- pi*(1-b/breaks[4])
    th       <- seq(th.start,th.end,length=5)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="coral2")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="darkorange")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="darkolivegreen1")+
    geom_polygon(data=get.poly(pos-0.05,pos+0.05,0.02),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/5)),y=1.1*sin(pi*(1-breaks/5)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    #coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}


#Ratios P&G

Cre_cuotas<-function(datosExcel){
  data <- data.frame(datosExcel)
  crecimiento_cuotas<-log(data[1,2]/data[1,3])
  return(crecimiento_cuotas)
  
}

Cre_patrocinios<-function(datosExcel){
  data <- data.frame(datosExcel) 
  crecimiento_patrocinios<-log(data[2,2]/data[2,3])
  return(crecimiento_patrocinios)
  
}

Cre_eventos<-function(datosExcel){

  data <- data.frame(datosExcel)  
  crecimiento_eventos<-log(data[3,2]/data[3,3])
  return(crecimiento_eventos)
}

#eventos+cuota+sosteniemiento / ingresos 
Peso1<-function(datosExcel){

  data <- data.frame(datosExcel)  
  pp<-data[1,2]+data[2,2]+data[3,2]
  peso11<-pp/data[9,2]
  return(peso11)
  
}

#eventos-devoluciones/ ingresos 
Peso2<-function(datosExcel){

  data <- data.frame(datosExcel) 
  pp<-data[3,2]+data[8,2]
  peso22<-pp/data[9,2]
  return(peso22)
}

Peso3<-function(datosExcel){

data <- data.frame(datosExcel) 
pp<-data[2,2]
peso33<-pp/data[9,2]
return(peso33)

}


#KPIS




#Proyectos

Proyecto1<-function(datosExcel){
  
  data <- data.frame(datosExcel) 
  Proyecto<-data[1,2]-data[9,2]
  return(Proyecto)
}

Proyecto2<-function(datosExcel){
  
  data <- data.frame(datosExcel) 
  p2<-data[9,2]/data[1,2]
  return(p2)
}




