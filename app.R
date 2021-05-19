#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Cargar librerias
library(shiny)
library('shinydashboard')
library(readxl)
library(plotly)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)

#Cargar las funciones a usar
#source("funcionesApp.R")

# Define UI for application that draws a histogram

######################################################################
#                       El encabezado de la app                      #
######################################################################

header <- dashboardHeader(title = 
                              "Cámara Colombo China",titleWidth = 200,
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Soporte",
                                           message = "El servidor está listo",
                                           icon = icon("smile-wink")
                                       )
                          ))

######################################################################
#                         Barra de navegaciÃ³n                        #
######################################################################

sidebar <- dashboardSidebar(
    sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem("Balance general", tabName = "BG", icon = icon("dashboard"),
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("Pérdidas y ganacias",tabName = "PG", icon = icon("chart-pie"), 
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("KPI's", tabName = "KPI" ,icon = icon("chart-line"),
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("Proyectos", tabName = "pp" ,icon = icon("chart-line"),
                 badgeLabel = "new", badgeColor = "yellow")
    )
)

######################################################################
#                            Primera pestaÃ±a                        #
######################################################################


Balance<-tabItem(tabName = "BG",
                 fluidRow(
                     column(width = 4,
                            box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                fileInput(inputId='ArchivoEntrada', label='Cargue el archivo ',accept=c(".xlsx"))
                                ),
                            box(
                                title = tagList(shiny::icon("chart-pie"), "Análisis vertical"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El análisis vertical permite ver el peso porcentual 
                                de cada cuenta dentro de una estructura de los 
                                estados financieros. Este análisis es importante para ver si 
                                la distribución  de las cuentas es equitativa
                                y permite identificar la relevancia de algunas cuentas. "
                            ),
                            box(
                                title = tagList(shiny::icon("chart-line"), "Análisis horizontal"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El análisis horizontal calcula la variación absoluta y relativa que ha sufrido cada una de las cuentas 
                                en dos períodos de tiempo consecutivos. Es decir, muestra que tanto ha crecido o decrecido una cuenta. 
                                Lo anterior es importante ya que nos analizar el comportamiento de las cuentas a lo largo de los años. "
                            )
                            
                            
                            
                     ),
                     column(width = 8,
####################### Analisis vertical ############################

                            fluidRow(
                                tabBox( 
                                    # Title can include an icon
                                    title = tagList(shiny::icon("project-diagram"), "Gráficos"),
                                    side = "left", height = "50px",width = NULL,
                                    tabPanel("ActivosV",
                                             box(
                                                        title = " Análisis vertical Activos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                                            plotlyOutput(outputId="graph1"),
                                                           hr()
                                                      ),
                                             box(
                                                 width = NULL, background = "light-blue", collapsible = T,
                                                 "La cuenta que más pesa de los activos corrientes son las cuentas por cobrar."
                                             )
                                             
                                             
                                    ),
                                    tabPanel("PasivosV", box(
                                        title = "Análisis vertical Pasivos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph2"),
                                        hr()
                                    ),
                                    box(
                                        width = NULL, background = "light-blue", collapsible = T,
                                        "La cuenta que más pesa de los pasivos corrientes son las cuentas por pagar."
                                    )),
                                    
                                    tabPanel("Pas+PatV", box(
                                        title = "Análisis vertical Pasivos + patrimonio",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph3"),
                                        hr()
                                    ),
                                    box(
                                        width = NULL, background = "light-blue",collapsible = T,
                                        "Los pasivos corrientes es la cuenta que tiene mayor peso."
                                    )),
                                    tabPanel("ActivosH", box(
                                        title = "Análisis horizontal activos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph4"),
                                        hr()
                                    )
                                        
                                    ),
                                    tabPanel("PasivosH", box(
                                        title = "Análisis horizontal pasivos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph5"),
                                        hr()
                                    )),
                                    tabPanel("PatH", box(
                                        title = "Análisis horizontal patrimonio",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph6"),
                                        hr()
                                    ))
                                    
                                )
                                
                            )
                            
                     ),
                     
                 )
)


######################################################################
#                            Segunda pestaÃ±a                        #
######################################################################


perdida<-tabItem(tabName = "PG",
    
    fluidRow(
        column(width = 4,
               box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                   fileInput(inputId='ArchivoEntrada1', label='Cargue el archivo ',accept=c(".xlsx")))
               ,
               box(
                   title = tagList(shiny::icon("chart-pie"), "Análisis vertical"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                   "El análisis vertical permite ver el peso porcentual 
                                de cada cuenta dentro de una estructura de los 
                                estados financieros. Este análisis es importante para ver si 
                                la distribución  de las cuentas es equitativa
                                y permite identificar la relevancia de algunas cuentas. "
               ),
               box(
                   title = tagList(shiny::icon("chart-line"), "Análisis horizontal"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                   "El análisis horizontal calcula la variación absoluta y relativa que ha sufrido cada una de las cuentas 
                                en dos períodos de tiempo consecutivos. Es decir, muestra que tanto ha crecido o decrecido una cuenta. 
                                Lo anterior es importante ya que nos analizar el comportamiento de las cuentas a lo largo de los años. "
               )
               ),
        column(width = 8,
               fluidRow(
                   tabBox( 
                       # Title can include an icon
                       title = tagList(shiny::icon("project-diagram"), "Gráficos"),
                       side = "left", height = "50px",width = NULL,
                       
                       tabPanel("IngresosV",
                                box(
                                    title = " Análisis vertical ingresos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                    plotlyOutput(outputId="graph7"),
                                    hr()
                                ),
                                box(
                                    width = NULL, background = "navy", collapsible = T,
                                    "La cuenta que genera más ingresos es la de cuota de sostenimiento."
                                )
                                
                                
                       ),
                       tabPanel("GastosV",
                                box(
                                    title = " Análisis vertical gastos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                    plotlyOutput(outputId="graph8"),
                                    hr()
                                ),
                                box(
                                    width = NULL, background = "navy", collapsible = T,
                                    "La cuenta que genera más gastos administrativos a la cámara son los gastos personales."
                                )
                                
                                
                       ),
                       
                       tabPanel("Excedente", box(
                           title = "Excedentes del año",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                           plotlyOutput(outputId="graph9"),
                           hr()
                       )),
                       tabPanel("IngresosH", box(
                           title = "Análisis horizontal ingresos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                           plotlyOutput(outputId="graph10"),
                           hr()
                       )),
                       tabPanel("GastosH", box(
                           title = "Análisis horizontal gastos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                           plotlyOutput(outputId="graph11"),
                           hr()
                       ))
                       
                       )
               ))
        
))


######################################################################
#                            Tercera pestaÃ±a                        #
######################################################################
medidas<-tabItem(tabName = "KPI",
                 fluidRow(
                     column(width = 4,
                            box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                fileInput(inputId='Entrada', label='Cargue el archivo ',accept=c(".xlsx"))) 
                            ,
                            box(
                                title = tagList(shiny::icon("funnel-dollar"), "Razón corriente"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "La razón corriente es uno de los indicadores financieros que nos permite determinar 
                                el índice de liquidez de una empresa, o su capacidad para disponer de efectivo ante una
                                eventualidad o contingencia que se lo exija. Especificamente, indica la capacidad
                                de la empresa para cumplir sus obligaciones financieras. Así, se sabe cuantos activos 
                                corrientes se tiene para cumplir con la deuda de corto plazo."
                            ),
                            box(
                                title = tagList(shiny::icon("search-dollar"), "capital de trabajo neto"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El capital de trabajo neto es la diferencia entre los activos corrientes y 
                                los pasivos corrientes. Así,el capital de trabajo neto es una medida tanto de 
                                la eficiencia operativa como de liquidez de una empresa ya que si los activos corrientes de una empresa no exceden sus pasivos 
                                corrientes, entonces puede existir problemas de salud financiera."
                            ),
                            box(
                                title = tagList(shiny::icon("credit-card"), "Endeudamiento"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El ratio de endeudamiento nos indica que por cada peso invertido en activos 
                                cuanto esta financiado en terceros y ademáss de la garantía que tiene la empresa con sus acreededores."
                            ),
                            box(
                                title = tagList(shiny::icon("money-check-alt"), "Solvencia"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El ratio de solvencia mide la capacidad de una empresa de poder pagar sus deudas. 
                                En otras palabras, si una empresa tuviese que pagar todas sus deudas en un momento dado, determina si tendría activos para hacer frente a esos pagos."
                            ),
                            box(
                                title = tagList(shiny::icon("hand-holding-usd"), "Disponibilidad"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                "El valor de este ratio, debe estar en torno al 0,3. Como en los casos anteriores, si es menor a esta cifra, 
                                la empresa correrá el riesgo de no poder hacer frente a los pagos, y si es muy superior, tendría recursos 
                                ociosos (exceso de recursos)."
                            )
                            )
                     ,
                     column(width = 8,
                            fluidRow(
                                tabBox( 
                                    # Title can include an icon
                                    title = tagList(shiny::icon("project-diagram"), "Gráficos"),
                                    side = "left", height = "50px",width = NULL,
                                    tabPanel("R. Corriente", box(
                                        title = "Razón corriente",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph12"),
                                        hr()
                                    )),
                                    tabPanel("WCR", box(
                                        title = "Capital de trabajo neto",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph13"),
                                        hr()
                                    )),
                                    tabPanel("Rent", box(
                                        title = "Rentabilidad sobre los activos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph14"),
                                        hr()
                                    )),
                                    tabPanel("Deuda", box(
                                        title = "Ratio endeudamiento",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph15"),
                                        hr()
                                    )),
                                    tabPanel("Solvencia", box(
                                        title = "Ratio de solvencia",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph16"),
                                        hr()
                                    )),
                                    tabPanel("Disponibilidad", box(
                                        title = "Ratio de disponibilidad",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                        plotlyOutput(outputId="graph17"),
                                        hr()
                                    ))
                                    
                                    ))
                            )
                     )
                 )


Proyecto<-tabItem(tabName = "pp", 
                  fluidRow(column(width = 4,
                                  box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                      fileInput(inputId='Archivos', label='Cargue el archivo ',accept=c(".xlsx"))),
                                  box(
                                      title = tagList(shiny::icon("coins"), "Proyectado"), width = NULL,collapsible = T, solidHeader = TRUE, status = "warning",
                                      "Las gráficas muestran que tanto se lleva ejecutado de lo presupuestado. 
                                      Existen dos escenarios (uno pesimista y otro optimista) en donde se evaluan como se comportan los
                                      ingresos y los egresos versus lo presupuestado y con ello poder reaccionar más rápido antes posibles inconvenientes."
                                  )                  
                  ),
                  column(width = 8,
                         fluidRow(
                             tabBox( 
                                 # Title can include an icon
                                 title = tagList(shiny::icon("project-diagram"), "Gráficos"),
                                 side = "left", height = "50px",width = NULL,
                                 tabPanel("Ing.P1", box(
                                     title = "Presupuesto 1. Ingresos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph18"),
                                     hr()
                                 )),
                                 tabPanel("Ing.P2", box(
                                     title = "Presupuesto 2. Ingresos",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph19"),
                                     hr()
                                 )),
                                 tabPanel("GastF.P1", box(
                                     title = "Presupuesto 1. Gastos fijo",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph20"),
                                     hr()
                                 )),
                                 tabPanel("GastF.P2", box(
                                     title = "Presupuesto 2. Gastos fijo",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph21"),
                                     hr()
                                 )),
                                 tabPanel("GastV.P1", box(
                                     title = "Presupuesto 1. Gastos variable",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph22"),
                                     hr()
                                 )),
                                 tabPanel("GastV.P2", box(
                                     title = "Presupuesto 1. Gastos variable",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="graph23"),
                                     hr()
                                 ))
                                 
                                 
                                 ))             
                         
                         
                  )
                  )
                  
)


######################################################################
#                            Paneles                                 #
######################################################################

body <- dashboardBody(
    tabItems(Balance,perdida,medidas,Proyecto)
)

######################################################################
#                              ui                                    #
######################################################################

ui<-dashboardPage(skin = "red",header, sidebar, body)



######################################################################
#                            Server                                  #
######################################################################

server <- function(input, output) {
    
    datoscamara <- eventReactive(input$ArchivoEntrada, {
        Archivo1 <- input$ArchivoEntrada
        if (is.null(Archivo1)) { return(NULL) }
        #se lee el archivo 
        dataFile <-read_excel(Archivo1$datapath,sheet=1, 
                              col_names = TRUE)
    })
    
    
    output$graph1 <- renderPlotly(
        {
        
            datosExcel<-as.data.frame(datoscamara())
            Activos<-datosExcel[1:3,]
            Ann<-datosExcel[5,4]
            
            fig <- plot_ly(Activos, labels = ~BG, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
            fig <- fig %>% layout(title = 'Análisis vertical de los activos',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            fig  
         
            
        })
    
    output$graph2 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datoscamara())
            
            Pasivos<-data.frame(datosExcel[10:15,]) 
            Pasivos<-Pasivos[-c(4,6),]
            
            
            fig <- plot_ly(Pasivos, labels = ~BG, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
            fig <- fig %>% layout(title = 'Análisis vertical de los pasivos',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            fig  
            
            
        })
    
    output$graph3 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datoscamara())
            
            PasPat<-data.frame(datosExcel[13:21,]) 
            PasPat<-PasPat[-c(2,4,7,9),]
            
            
            fig <- plot_ly(PasPat, labels = ~BG, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
            fig <- fig %>% layout(title = 'Análisis vertical de los pasivos + patrimonio',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            fig  
            
            
        })
    
    output$graph4 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datoscamara())
            
            h1<-((datosExcel[,5]/datosExcel[,4])-1)*100
            h2<-((datosExcel[,4]/datosExcel[,3])-1)*100
            h3<-((datosExcel[,3]/datosExcel[,2])-1)*100
            cuentas<-datosExcel[,1]
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
            fig <- fig %>% layout(title = "Análisis horizontal relativo activos",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
        })
    output$graph5 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datoscamara())
            
            h1<-((datosExcel[,5]/datosExcel[,4])-1)*100
            h2<-((datosExcel[,4]/datosExcel[,3])-1)*100
            h3<-((datosExcel[,3]/datosExcel[,2])-1)*100
            cuentas<-datosExcel[,1]
            data<-data.frame(cuentas,h3,h2,h1)
            Anno <- c('2017','2018','2019')
            
            TotalPasivos<-c(as.numeric(data[13,2:4]))
            OtrosPasivos<-c(as.numeric(data[12,2:4]))
            PasivosImp<-c(as.numeric(data[11,2:4]))
            CuentasxPagar<-c(as.numeric(data[10,2:4]))
            datas <- data.frame(Anno, TotalPasivos,OtrosPasivos, PasivosImp,CuentasxPagar)
            
            fig <- plot_ly(datas, x = ~Anno, y = ~TotalPasivos, name = 'Total Pasivos', type = 'scatter', mode = 'lines+markers') 
            fig <- fig %>% add_trace(y = ~OtrosPasivos, name = 'Otros Pasivos no financieros', mode = 'lines+markers') 
            fig <- fig %>% add_trace(y = ~PasivosImp, name = 'Pasivos Impuestos', mode = 'lines+markers')
            fig <- fig %>% add_trace(y = ~CuentasxPagar, name = 'Cuentas por pagar', mode = 'lines+markers')
            fig <- fig %>% layout(title = "Análisis horizontal relativo pasivos",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig 
           
            
            
        })
    output$graph6 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datoscamara())
            
            h1<-((datosExcel[,5]/datosExcel[,4])-1)*100
            h2<-((datosExcel[,4]/datosExcel[,3])-1)*100
            h3<-((datosExcel[,3]/datosExcel[,2])-1)*100
            cuentas<-datosExcel[,1]
            data<-data.frame(cuentas,h3,h2,h1)
            Anno <- c('2017','2018','2019')
            
            Excedentes.Acumulados<-c(as.numeric(data[19,2:4]))
            Excedentes.Ejercicio<-c(as.numeric(data[18,2:4]))
            Patrimonio<-c(as.numeric(data[20,2:4]))
            datas <- data.frame(Anno, Excedentes.Ejercicio, Excedentes.Acumulados, Patrimonio)
            
            fig <- plot_ly(datas, x = ~Anno, y = ~Excedentes.Ejercicio, name = 'Excedentes del ejercicio', type = 'scatter', mode = 'lines+markers') 
            fig <- fig %>% add_trace(y = ~Excedentes.Acumulados, name = 'Excedentes acumulados', mode = 'lines+markers') 
            fig <- fig %>% add_trace(y = ~Patrimonio, name = 'Patrimonio', mode = 'lines+markers')
            fig <- fig %>% layout(title = "Análisis horizontal relativo patrimonio",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
        })
    
################### perdidas y ganacias ##############
    
    datcamara <- eventReactive(input$ArchivoEntrada1, {
        Archivo2 <- input$ArchivoEntrada1
        if (is.null(Archivo2)) { return(NULL) }
        #se lee el archivo 
        dataFile <-read_excel(Archivo2$datapath,sheet=2, 
                              col_names = TRUE)
    })

    output$graph7 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datcamara())
            
            Ingreso<-datosExcel[2:6,]
            
         
            
            fig <- plot_ly( Ingreso, labels = ~ER, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
            fig <- fig %>% layout(title = 'Análisis vertical de los activos',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            fig  
            
            
        })
    
    output$graph8 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datcamara())
            
            Gasto<-datosExcel[12:23,]
            
           
            
            fig <- plot_ly(Gasto, labels = ~ER, values = ~Año2019, type = 'pie',textinfo='label+percent',insidetextorientation='radial')
            fig <- fig %>% layout(title = 'Análisis vertical Gastos administrativos',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            fig 
            
            
        })
    
    output$graph9 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datcamara())
            
            completo<-datosExcel[-c(1:7,12:23),]
            
            d<-t(completo[9,2:5])
            Anno <- c('2016','2017','2018','2019')
            datas <- data.frame(Anno,d )
            
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d, name = 'Excedentes del Año', type = 'scatter', mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Excedente del año",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Miles de millones"))
            fig
            
            
        })
    output$graph10 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datcamara())
            
            h1<-((datosExcel[,5]/datosExcel[,4])-1)*100
            h2<-((datosExcel[,4]/datosExcel[,3])-1)*100
            h3<-((datosExcel[,3]/datosExcel[,2])-1)*100
            cuentas<-datosExcel[,1]
            data<-data.frame(cuentas,h3,h2,h1)
            Anno <- c('2017','2018','2019')
            
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
            fig <- fig %>% layout(title = "Análisis horizontal relativo Ingresos",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
        })
    output$graph11 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datcamara())
            
            h1<-((datosExcel[,5]/datosExcel[,4])-1)*100
            h2<-((datosExcel[,4]/datosExcel[,3])-1)*100
            h3<-((datosExcel[,3]/datosExcel[,2])-1)*100
            cuentas<-datosExcel[,1]
            data<-data.frame(cuentas,h3,h2,h1)
            Anno <- c('2017','2018','2019')
            
            
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
            fig <- fig %>% layout(title = "Análisis horizontal relativo Gastos administrativos",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
        })
  
################### KPIS ##############  
    
    camara <- eventReactive(input$Entrada, {
        Archivo3 <- input$Entrada
        if (is.null(Archivo3)) { return(NULL) }
        #se lee el archivo 
        dataFile <-read_excel(Archivo3$datapath,sheet=3, 
                              col_names = TRUE)
    })
    
    output$graph12 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[1,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Razón corriente', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Razón corriente",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Ratio"))
            fig
          
            
            
        })
    
    output$graph13 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[2,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Capital neto', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Capital neto",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
            
        })
    
    output$graph14 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[3,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Rentabilidad de los activos', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Rentabilidad de los activos",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Porcentaje"))
            fig
            
            
        })
    output$graph15 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[4,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de endeudamiento', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Ratio de endeudamiento",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Ratio"))
            fig
            
            
            
        })
    
    output$graph16 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[5,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de solvencia', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Ratio de solvencia",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Ratio"))
            fig
            
            
            
            
        })
    output$graph17 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(camara())
            
            d1<-as.numeric(round(datosExcel[6,2:4],3))
            Anno <- c('2017','2018','2019')
            datas <- data.frame(Anno,d1)
            
            fig <- plot_ly(datas, x = ~Anno, y = ~d1, name = 'Ratio de disponibilidad', type = 'bar') 
            fig <- fig %>% add_trace(y = ~d1, type = 'scatter',mode = 'lines+markers') 
            fig <- fig %>% layout(title = "Ratio de disponibilidad",
                                  xaxis = list(title = "Años"),
                                  yaxis = list (title = "Ratio"))
            fig
            
            
            
            
        })
    
    
##################### Proyecto ####################
    
    datacamaraa <- eventReactive(input$Archivos, {
        Archivo4 <- input$Archivos
        if (is.null(Archivo4)) { return(NULL) }
        #se lee el archivo 
        dataFile <-read_excel(Archivo4$datapath,sheet=4, 
                              col_names = TRUE)
    }) 
    
    output$graph18 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            d1<-datosExcel[1:7,1:4]
            d11<-round((d1[,4]/d1[,2]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Ingresos proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Porcentaje (%)"),barmode = "stack")
            
            Chart
            
            
            
            
        })
    output$graph19 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            d1<-datosExcel[1:7,1:4]
            d11<-round((d1[,4]/d1[,2]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Ingresos proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Porcentaje (%)"),barmode = "stack")
            
            Chart
            
          
            
        })
    output$graph20 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            d1<-datosExcel[9:16,1:4]
            d11<-round((d1[,4]/d1[,2]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Gastos fijos proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Percentage (%)"),barmode = "stack")
            
            Chart  
            
            
            
            
            
        })
    output$graph21 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            d1<-datosExcel[9:16,1:4]
            d11<-round((d1[,4]/d1[,3]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Gastos fijos proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Percentage (%)"),barmode = "stack")
            
            Chart
            
            
            
            
            
        })
    output$graph22 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            d1<-datosExcel[18:19,1:4]
            d11<-round((d1[,4]/d1[,2]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.1, name = 'Presupuesto 1',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Gastos variables proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Percentage (%)"),barmode = "stack")
            
            Chart 
            
            
            
            
            
        })
    output$graph23 <- renderPlotly(
        {
            
            datosExcel<-as.data.frame(datacamaraa())
            
            
            d1<-datosExcel[18:19,1:4]
            d11<-round((d1[,4]/d1[,3]),3)
            d22<-1-d11
            datas <- data.frame(d1,d11,d22)
            
            Chart <- plot_ly(datas,x = ~Proyecto, y = ~Realizado, type = 'bar',name= 'Realizado',text = ~datas[,5] ,textposition = "auto") %>% 
                add_trace(y = ~Presupuesto.2, name = 'Presupuesto 2',text = ~datas[,6] ,textposition = "auto") %>% 
                layout(title = " Gastos variables proyectados vs ejecutados",
                       xaxis = list(title = "Ingresos"),
                       yaxis = list (title = "Percentage (%)"),barmode = "stack")
            
            Chart
            
            
            
            
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
