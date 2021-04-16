library(shiny)
library('shinydashboard')
if (!require("readxl")){install.packages("readxl");library(readxl)}else{library(readxl)}
if (!require("ggplot2")){install.packages("ggplot2");library(ggplot2)}else{library(ggplot2)}
if (!require("plotly")){install.packages("plotly");library(plotly)}else{library(plotly)}
#install.packages("rsconnect")
library(gridExtra)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
source("fase1.R")
header <- dashboardHeader(title = 
    "Cámara Colombo China",titleWidth = 200,
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 30, color = "yellow",
                          "Aplicación"
                 )
    ))

sidebar <- dashboardSidebar(
    #para la barra de pestañas
    sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem("Balance general", tabName = "BG", icon = icon("dashboard"),
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("Pérdidas y ganacias",tabName = "PG", icon = icon("chart-pie"), 
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("KPI´s", tabName = "KPI" ,icon = icon("chart-line"),
                 badgeLabel = "new", badgeColor = "yellow"),
        menuItem("Proyectos", tabName = "pp" ,icon = icon("chart-line"),
                 badgeLabel = "new", badgeColor = "yellow")
    )
)

Balance<-tabItem(tabName = "BG",
                 fluidRow(
                     column(width = 4,
                            box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                fileInput(inputId='ArchivoEntrada', label='Cargue el archivo ',accept=c(".xlsx")))
                        
                            #valueBoxOutput("acida", width = 20)
                            
                     ),
                     column(width = 8,
                            #tabBox(
                               # title = tagList(shiny::icon("search-dollar"), "Pruebas liquidez"),
                                # The id lets us use input$tabset1 on the server to find the current tab
                               # id = "t1",width = 12,
                               # tabPanel("Prueba 1", 
                                #         box(
                                 #            title = "Ratio de disponibilidad",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                  #           plotlyOutput(outputId="gaugechart"),
                                   #          hr()
                                    #     )),
                                #tabPanel("Prueba 2", box(
                                 #   title = "Prueba ácida",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                  #  plotlyOutput(outputId="gaugechart2"),
                                   # hr()
                               # )))
                            box(
                                title = "Ratio de disponibilidad",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                plotlyOutput(outputId="gaugechart"),
                                hr()
                            ),
                            fluidRow(
                            valueBoxOutput("acida"),
                            valueBoxOutput("disponibilidad"),
                            valueBoxOutput("KTN")
                            ),
                            fluidRow(
                                valueBoxOutput("solvencia"),
                                valueBoxOutput("endeudamiento"),
                                valueBoxOutput("apalancamiento")
                            )
            
                           
                     ),
                 
                 )
)

perdida<-tabItem(tabName = "PG",
                 
                 fluidRow(column(width = 4, 
                                 
                            box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                        fileInput(inputId='ArchivoEntrada1', label='Cargue el archivo ',accept=c(".xlsx")))
                                  ),
                          column(width = 8,
                                 box(
                                     title = "Crecimiento de patrocinios",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="g1"),
                                     hr()),
                                    fluidRow(
                                     valueBoxOutput("cuotas"),
                                     valueBoxOutput("patrocinios"),
                                     valueBoxOutput("eventos")
        
                                 ),
                                 
                                 fluidRow(
                                     valueBoxOutput("peso1"),
                                     valueBoxOutput("peso2"),
                                     valueBoxOutput("peso3")
                                 )
                                     
                                 
                                 )   
                          )
                 )

medidas<-tabItem(tabName = "KPI",
                 fluidRow(column(width = 4,
                                 box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                     fileInput(inputId='Entrada', label='Cargue el archivo ',accept=c(".xlsx")))      
                                 ),
                          column(width = 8,
                                 box(
                                     title = "Peso eventos sobre ingresos totales",  width = NULL,status = "warning",collapsible = T, solidHeader = TRUE,
                                     plotlyOutput(outputId="g2"),
                                     hr())
                                 
                                ) )
                 )

Proyecto<-tabItem(tabName = "pp", 
                  fluidRow(column(width = 4,
                                  box(title = "Entrada archivos", width = NULL, status = "warning", solidHeader = TRUE,
                                      fileInput(inputId='Archivos', label='Cargue el archivo ',accept=c(".xlsx")))
                                  ),
                           column(width = 8,
                                
                                      valueBoxOutput("P1", width = 20),
                                      valueBoxOutput("P2", width = 20)
                                  
                                  )
                           )
                  
                  )
body <- dashboardBody(
    tabItems(Balance, perdida, medidas, Proyecto)
)

ui<-dashboardPage(skin = "red",header, sidebar, body)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}
server <- function(input, output){
    
    datoscamara <- eventReactive(input$ArchivoEntrada, {
        Archivo1 <- input$ArchivoEntrada
        if (is.null(Archivo1)) { return(NULL) }
        #se lee el archivo 
        dataFile <-read_excel(Archivo1$datapath,sheet=1, 
                              col_names = TRUE)
    })
    
    output$acida <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        r<- as.numeric(pacida(datoscamara()))
        pp<-round(r,2)
        valueBox(paste(pp), "Prueba Ácida", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$disponibilidad <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        l<- as.numeric(pdisponibilidad(datoscamara()))
        ll<-round(l,2)
        valueBox(ll, "Ratio de Disponibilidad", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$KTN <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        l<- as.numeric(pKTN(datoscamara()))
        ll<-round(l,2)
        valueBox(paste("$",ll), "Capital Neto de trabajo", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$solvencia <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        o<- as.numeric(psolvencia(datoscamara()))
        oo<-round(o,2)
        valueBox(oo, "Ratio de solvencia", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$endeudamiento <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        j<- as.numeric(pendeudamiento(datoscamara()))
        jj<-round(j,2)
        valueBox(jj, "Ratio de endeudamiento", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$apalancamiento <- renderValueBox({
        if (is.null(datoscamara())) { return(NULL) }
        r<- as.numeric(papalancamiento(datoscamara()))
        rr<-round(r,2)
        valueBox(rr, "Ratio de apalancamiento", icon = icon("calculator"),
                 color = "yellow" 
        )
    })
    
    output$gaugechart <- renderPlotly(
        {
            dip<- (as.numeric(pdisponibilidad(datoscamara())))*100
            dispo<-round(dip,4)
            aci<- as.numeric(pacida(datoscamara()))
            acido<-round(aci,4)
            
           gg.gauge(dispo)
            
        })
    output$gaugechart2 <- renderPlotly(
        {
            
            aci<- as.numeric(pacida(datoscamara()))
            acido<-round(aci,4)
            
            gg.gauge1(acido,breaks=c(0,1,2.5,5))
            
        })

###P&G

datcamara <- eventReactive(input$ArchivoEntrada1, {
    Archivo2 <- input$ArchivoEntrada1
    if (is.null(Archivo2)) { return(NULL) }
    #se lee el archivo 
    dataFile <-read_excel(Archivo2$datapath,sheet=2, 
                          col_names = TRUE)
})

output$g1 <- renderPlotly(
    {
        cre<- (as.numeric(Cre_patrocinios(datcamara())))*100
        patrocinios<-round(cre,4)
        gg.gauge(patrocinios)
        
    })
output$cuotas <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    u<- as.numeric(Cre_cuotas(datcamara()))
    uu<-round(u,2)
    valueBox(uu, "Crecimiento de cuotas", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$patrocinios <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    y<- as.numeric(Cre_patrocinios(datcamara()))
    yy<-round(y,2)
    valueBox(yy, "Crecimiento de patrocinios", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$eventos <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    i<- as.numeric(Cre_eventos(datcamara()))
    ii<-round(i,2)
    valueBox(ii, "Crecimiento de eventos", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$peso1 <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    i<- as.numeric(Peso1(datcamara()))
    ii<-round(i,2)
    valueBox(ii, "KPS 1", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$peso2 <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    i<- as.numeric(Peso2(datcamara()))
    ii<-round(i,2)
    valueBox(ii, "KPS 2", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$peso3 <- renderValueBox({
    if (is.null(datcamara())) { return(NULL) }
    i<- as.numeric(Peso3(datcamara()))
    ii<-round(i,2)
    valueBox(ii, "KPS 3", icon = icon("calculator"),
             color = "yellow" 
    )
})

#KPIS

camara <- eventReactive(input$Entrada, {
    Archivo3 <- input$Entrada
    if (is.null(Archivo3)) { return(NULL) }
    #se lee el archivo 
    dataFile <-read_excel(Archivo3$datapath,sheet=3, 
                          col_names = TRUE)
})


#Proyecto


cam <- eventReactive(input$Archivos, {
    Archivo4 <- input$Archivos
    if (is.null(Archivo4)) { return(NULL) }
    #se lee el archivo 
    dataFile <-read_excel(Archivo4$datapath,sheet=4, 
                          col_names = TRUE)
})

output$P1 <- renderValueBox({
    if (is.null(cam())) { return(NULL) }
    i<- as.numeric(Proyecto1(cam()))
    ii<-round(i,2)
    valueBox(paste("$",ii), "Presupuesto restante", icon = icon("calculator"),
             color = "yellow" 
    )
})

output$P2 <- renderValueBox({
    if (is.null(cam())) { return(NULL) }
    i<- as.numeric(Proyecto2(cam()))
    ii<-round(i,2)
    valueBox(ii, "Porcentaje de cumplimiento E2", icon = icon("calculator"),
             color = "yellow" 
    )
})

}
# Run the application 
shinyApp(ui = ui, server = server)
