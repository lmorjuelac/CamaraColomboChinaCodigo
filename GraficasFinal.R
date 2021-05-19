#Graficas camara

#Laura Manuela Orjuela 
#201714113

#borrar todo
rm(list = ls())

#borrar cache
gc()

#choose dir
setwd(choose.dir())

#Librerias
library(readxl)
library(ggplot2)
library(plotly)
library(scales)

#Cargar Base de datos


BG <- read_excel("Plantilla2.xlsx", sheet = "BG")
EPG<- read_excel("Plantilla2.xlsx", sheet = "ER")
KPIS<-read_excel("Plantilla2.xlsx", sheet = "KPIS")
proy<-read_excel("Plantilla2.xlsx", sheet = "Proyectos")


#Graficas

####################################################################
#                       Balance general                            #
####################################################################


########################Analisis Vertical########################### 

#Activos

Activos<-BG[1:3,]

Ann<-BG$Año2019[5]

ggplot(Activos,aes(x="",y=Año2019, fill=BG))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Año2019/Ann)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","darkolivegreen1"))+
  theme_void()+
  labs(title="Análisis vertical Activos años 2019")

#Pasivos 


Pasivos<-data.frame(BG[10:15,]) 
Pasivos<-Pasivos[-c(4,6),]

Ann<-BG$Año2019[16]

ggplot(Pasivos,aes(x="",y=Año2019, fill=BG))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Año2019/Ann)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","darkolivegreen1"))+
  theme_void()+
  labs(title="Análisis vertical Pasivos años 2019")

#Patrimonio+ Pasivo
PasPat<-data.frame(BG[13:21,]) 
PasPat<-PasPat[-c(2,4,7,9),]

Ann<-BG$Año2019[21]

ggplot(PasPat,aes(x="",y=Año2019, fill=BG))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Año2019/Ann)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","darkolivegreen1",
                             "darkorchid1","khaki1","hotpink","lightblue"))+
  theme_void()+
  labs(title="Análisis vertical Pasivos años 2019")



#############################Analisis Horizontal########################

#######Porcentual#####

h1<-((BG[,5]/BG[,4])-1)*100
h2<-((BG[,4]/BG[,3])-1)*100
h3<-((BG[,3]/BG[,2])-1)*100
cuentas<-BG[,1]
data<-data.frame(cuentas,h3,h2,h1)


#Activos
Efectivo<-c(as.numeric(data[1,2:4]))
OtrosActivos<-c(as.numeric(data[2,2:4]))
CuentasxCobrar<-c(as.numeric(data[3,2:4]))
ActivosTotales<-c(as.numeric(data[9,2:4]))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno, Efectivo,OtrosActivos,CuentasxCobrar,ActivosTotales)

fig <- plot_ly(datas, x = ~Anno, y = ~Efectivo, name = 'Efectivo', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~OtrosActivos, name = 'Otros Activos', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~CuentasxCobrar, name = 'Cuentas por cobrar', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~ActivosTotales, name = 'Total activos', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal activos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig


#Pasivos

TotalPasivos<-c(as.numeric(data[13,2:4]))
OtrosPasivos<-c(as.numeric(data[12,2:4]))
PasivosImp<-c(as.numeric(data[11,2:4]))
CuentasxPagar<-c(as.numeric(data[10,2:4]))
datas <- data.frame(Anno, TotalPasivos,OtrosPasivos, PasivosImp,CuentasxPagar)

fig <- plot_ly(datas, x = ~Anno, y = ~TotalPasivos, name = 'Total Pasivos', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~OtrosPasivos, name = 'Otros Pasivos no financieros', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~PasivosImp, name = 'Pasivos Impuestos', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~CuentasxPagar, name = 'Cuentas por pagar', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal pasivos",
                      xaxis = list(title = "Añosos"),
                      yaxis = list (title = "Porcentaje"))
fig

#Patrimonio

Excedentes.Acumulados<-c(as.numeric(data[19,2:4]))
Excedentes.Ejercicio<-c(as.numeric(data[18,2:4]))
Patrimonio<-c(as.numeric(data[20,2:4]))
datas <- data.frame(Anno, Excedentes.Ejercicio, Excedentes.Acumulados, Patrimonio)

fig <- plot_ly(datas, x = ~Anno, y = ~Excedentes.Ejercicio, name = 'Excedentes del ejercicio', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~Excedentes.Acumulados, name = 'Excedentes acumulados', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~Patrimonio, name = 'Patrimonio', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal patrimonio",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig

###########Absoluto#################

h1<- BG[,5]- BG[,4]
h2<-BG[,4]-BG[,3]
h3<-BG[,3]-BG[,2]
cuentas<-BG[,1]
data<-data.frame(cuentas,h3,h2,h1)

#Activos

Efectivo<-c(as.numeric(data[1,2:4]))
OtrosActivos<-c(as.numeric(data[2,2:4]))
CuentasxCobrar<-c(as.numeric(data[3,2:4]))
ActivosTotales<-c(as.numeric(data[9,2:4]))
datas <- data.frame(Anno, Efectivo,OtrosActivos,CuentasxCobrar,ActivosTotales)

fig <- plot_ly(datas, x = ~Anno, y = ~Efectivo, name = 'Efectivo', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~OtrosActivos, name = 'Otros Activos', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~CuentasxCobrar, name = 'Cuentas por cobrar', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~ActivosTotales, name = 'Total activos', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal activos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Miles de millones"))
fig


#Pasicvos

TotalPasivos<-c(as.numeric(data[13,2:4]))
OtrosPasivos<-c(as.numeric(data[12,2:4]))
PasivosImp<-c(as.numeric(data[11,2:4]))
CuentasxPagar<-c(as.numeric(data[10,2:4]))
datas <- data.frame(Anno, TotalPasivos,OtrosPasivos, PasivosImp,CuentasxPagar)

fig <- plot_ly(datas, x = ~Anno, y = ~TotalPasivos, name = 'Total Pasivos', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~OtrosPasivos, name = 'Otros Pasivos no financieros', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~PasivosImp, name = 'Pasivos Impuestos', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~CuentasxPagar, name = 'Cuentas por pagar', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal pasivos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Miles de millones"))
fig

#Patrimonio

Excedentes.Acumulados<-c(as.numeric(data[19,2:4]))
Excedentes.Ejercicio<-c(as.numeric(data[18,2:4]))
Patrimonio<-c(as.numeric(data[20,2:4]))
datas <- data.frame(Anno, Excedentes.Ejercicio, Excedentes.Acumulados, Patrimonio)

fig <- plot_ly(datas, x = ~Anno, y = ~Excedentes.Ejercicio, name = 'Excedentes del ejercicio', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~Excedentes.Acumulados, name = 'Excedentes acumulados', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~Patrimonio, name = 'Patrimonio', mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal patrimonio",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Miles de millones"))
fig

####################################################################
#                  Perdidas y ganancia                             #
####################################################################


########################Analisis Vertical###########################


#Ingresos

Ingreso<-EPG[2:6,]

Ann<-EPG$Año2019[1]

ggplot(Ingreso, aes(x="", y=Año2019 , fill=ER))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent( Año2019 /Ann)),
            position=position_stack(vjust=0.5),color="white",size=8)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","darkolivegreen1",
                             "darkorchid1","khaki1","hotpink","lightblue"))+
  theme_void()+
  labs(title="Análisis vertical Ingresos años 2019")



#Gastos

Gasto<-EPG[12:23,]

Ann<-EPG$Año2019[11]

fig <- plot_ly(Gasto, labels = ~ER, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
fig <- fig %>% layout(title = 'Análisis vertical Gastos administrativos años 2019',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

#Completos

completo<-EPG[-c(1:7,12:23),]

d<-t(completo[9,2:5])
Anno <- c('2016','2017','2018','2019')
datas <- data.frame(Anno,d )


fig <- plot_ly(datas, x = ~Anno, y = ~d, name = 'Excedentes del Año', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% layout(title = "Excedente del año",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Miles de millones"))
fig



#############################Analisis Horizontal####################

###########Porcentual#################


h1<-((EPG[,5]/EPG[,4])-1)*100
h2<-((EPG[,4]/EPG[,3])-1)*100
h3<-((EPG[,3]/EPG[,2])-1)*100
cuentas<-EPG[,1]
data<-data.frame(cuentas,h3,h2,h1)
Anno <- c('2017','2018','2019')

#Ingresos
d1<-as.numeric(data[2,2:4])
d2<-as.numeric(data[3,2:4])
d3<-as.numeric(data[4,2:4])
d4<-as.numeric(data[5,2:4])
d5<-as.numeric(data[6,2:4])

datas <- data.frame(Anno,d1,d2,d3,d4,d5)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = "Cuota de sostenimiento", type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d2, name = "Patrocinios ", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d3, name = "Eventos", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d4, name = "Traducciones", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d5, name = "otros", mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal Ingresos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig

#Sin cuenta otros
fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = "Cuota de sostenimiento", type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d2, name = "Patrocinios ", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d3, name = "Eventos", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d4, name = "Traducciones", mode = 'lines+markers') 
fig <- fig %>% layout(title = "Análisis horizontal Ingresos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig

#Gastos
d1<-as.numeric(data[12,2:4])
d2<-as.numeric(data[13,2:4])
d3<-as.numeric(data[14,2:4])
d4<-as.numeric(data[15,2:4])
d5<-as.numeric(data[16,2:4])
d6<-as.numeric(data[17,2:4])
d7<-as.numeric(data[18,2:4])
d8<-as.numeric(data[19,2:4])
datas <- data.frame(Anno,d1,d2,d3,d4,d5,d6, d7,d8)

fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = "Gastos del personal", type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d2, name = "Honorarios", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d3, name = "Impuestos", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d4, name = "Arrendamientos", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d5, name = "Servicios", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d6, name = "Gastos Legales", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d7, name = "Mantenimiento y reparaciones", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d8, name = "Gastos de Viaje", mode = 'lines+markers') 
fig <- fig %>% layout(title = "Análisis horizontal Gastos administrativos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig


###########Absoluto#################


h1<-((EPG[,5]-EPG[,4]))
h2<-((EPG[,4]-EPG[,3]))
h3<-((EPG[,3]-EPG[,2]))
cuentas<-EPG[,1]
data<-data.frame(cuentas,h3,h2,h1)
Anno <- c('2017','2018','2019')

#Ingresos
d1<-as.numeric(data[2,2:4])
d2<-as.numeric(data[3,2:4])
d3<-as.numeric(data[4,2:4])
d4<-as.numeric(data[5,2:4])
d5<-as.numeric(data[6,2:4])

datas <- data.frame(Anno,d1,d2,d3,d4,d5)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = "Cuota de sostenimiento", type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d2, name = "Patrocinios ", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d3, name = "Eventos", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d4, name = "Traducciones", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d5, name = "otros", mode = 'lines+markers')
fig <- fig %>% layout(title = "Análisis horizontal Ingresos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Miles de millones"))
fig

#Gastos
d1<-as.numeric(data[12,2:4])
d2<-as.numeric(data[13,2:4])
d3<-as.numeric(data[14,2:4])
d4<-as.numeric(data[15,2:4])
d5<-as.numeric(data[16,2:4])
d6<-as.numeric(data[17,2:4])
d7<-as.numeric(data[18,2:4])
d8<-as.numeric(data[19,2:4])
datas <- data.frame(Anno,d1,d2,d3,d4,d5,d6, d7,d8)

fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = "Gastos del personal", type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d2, name = "Honorarios", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d3, name = "Impuestos", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d4, name = "Arrendamientos", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d5, name = "Servicios", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d6, name = "Gastos Legales", mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~d7, name = "Mantenimiento y reparaciones", mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~d8, name = "Gastos de Viaje", mode = 'lines+markers') 
fig <- fig %>% layout(title = "Análisis horizontal Gastos administrativos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig

####################################################################
#                                KPIS                              #
####################################################################


#"Razón corriente"

d1<-as.numeric(round(KPIS[1,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Razón corriente', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Razón corriente",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Ratio"))
fig

#"Capital neto "
d1<-as.numeric(round(KPIS[2,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Capital neto', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Capital neto",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig

#"Rentabilidad de los activos"

d1<-as.numeric(round(KPIS[3,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Rentabilidad de los activos', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Rentabilidad de los activos",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Porcentaje"))
fig
#"Ratio de endeudamiento"
d1<-as.numeric(round(KPIS[4,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)


fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de endeudamiento', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Ratio de endeudamiento",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Ratio"))
fig

#"Ratio de solvencia"

d1<-as.numeric(round(KPIS[5,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)

fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de solvencia', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Ratio de solvencia",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Ratio"))
fig


d1<-as.numeric(round(KPIS[6,2:4],3))
Anno <- c('2017','2018','2019')
datas <- data.frame(Anno,d1)

fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de disponibilidad', type = 'bar') 
fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
fig <- fig %>% layout(title = "Ratio de disponibilidad",
                      xaxis = list(title = "Años"),
                      yaxis = list (title = "Ratio"))
fig
####################################################################
#                             Proyectado                           #
####################################################################



#Ingresos 

#Presupuesto 1



d1<-proy[1:7,1:4]
d11<-round((d1[,4]/d1[,2]),3)
d22<-1-d11
datas <- data.frame(d1,d11,d22)

Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
  add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~Realizado.2 ,textposition = "auto") %>% 
  layout(title = " Ingreos proyectados vs ejecutados",
         xaxis = list(title = "Ingresos"),
         yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart

#Presupuesto 2
  d1<-proy[1:7,1:4]
  d11<-round((d1[,4]/d1[,3]),3)
  d22<-1-d11
  datas <- data.frame(d1,d11,d22)
  
  Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
    add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~Realizado.2 ,textposition = "auto") %>% 
    layout(title = " Ingreos proyectados vs ejecutados",
           xaxis = list(title = "Ingresos"),
           yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart
  
  
#Gastos fijos
  
#Presupuesto

  d1<-proy[9:16,1:4]
  d11<-round((d1[,4]/d1[,2]),3)
  d22<-1-d11
  datas <- data.frame(d1,d11,d22)
  
  Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
    add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~Realizado.2 ,textposition = "auto") %>% 
    layout(title = " Gastos fijos proyectados vs ejecutados",
           xaxis = list(title = "Ingresos"),
           yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart  

#Presupuesto 2
  
  #Presupuesto
  
  d1<-proy[9:16,1:4]
  d11<-round((d1[,4]/d1[,3]),3)
  d22<-1-d11
  datas <- data.frame(d1,d11,d22)
  
  Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
    add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~Realizado.2 ,textposition = "auto") %>% 
    layout(title = " Gastos fijos proyectados vs ejecutados",
           xaxis = list(title = "Ingresos"),
           yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart  
  
# Gastos variables 

  d1<-proy[18:19,1:4]
  d11<-round((d1[,4]/d1[,2]),3)
  d22<-1-d11
  datas <- data.frame(d1,d11,d22)
  
  Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
    add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~Realizado.2 ,textposition = "auto") %>% 
    layout(title = " Gastos variables proyectados vs ejecutados",
           xaxis = list(title = "Ingresos"),
           yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart   

  
#Presupuesto  2

  d1<-proy[18:19,1:4]
  d11<-round((d1[,4]/d1[,3]),3)
  d22<-1-d11
  datas <- data.frame(d1,d11,d22)
  
  Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~Realizado.1 ,textposition = "auto") %>% 
    add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~Realizado.2 ,textposition = "auto") %>% 
    layout(title = " Gastos variables proyectados vs ejecutados",
           xaxis = list(title = "Ingresos"),
           yaxis = list (title = "Percentage (%)"),barmode = "stack")
  
  Chart 
  