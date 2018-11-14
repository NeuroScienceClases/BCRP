#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library()
library(shiny)
library(forecast)
library(prophet)
library(ggplot2)
library(jsonlite)
library(curl)
library(readxl)
base_series_mensuales <- read_excel("base series mensuales.xlsx")
# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Series del BCRP"),
   sidebarLayout(position = "left",
                 sidebarPanel(h3("Presentacion"),
                           
                                 h5("Herramienta que permite graficar cualquier serie del Banco Central de Reserva del Peru y generar proyecciones de 12 meses. Esta herramienta permitira al investigador poder analizar rapidamente cualquier serie de tiempo del Peru")
                              ,  img(src = "Neuro.png", height = 70, width = 150)),
                 mainPanel(
                        tabsetPanel(
                             
                             tabPanel("Busqueda del codigo", 
                           
                           DT::dataTableOutput("table")
                             ),
                           tabPanel("Grafico",
                                  fluidRow( 
                                    textInput("codigo1","Codigo de la serie"),
                                    dateRangeInput('dateRange',label = "Periodo de Analisis: ",format = "mm/yyyy",language="sp",start = Sys.Date(), end=Sys.Date(),startview = "year",separator = " - "),
                                    #textOutput("SliderText"),
                                    #textOutput("SliderText1"),
                                    actionButton("Graficar","serie de tiempo"),
                                    actionButton("predecir","Proyeccion-4 meses desde el 2005-2018"),
                                    textOutput("url1"),
                                    plotOutput("distPlot", height = "600px"),
                                    plotOutput("distPlotpro", height = "600px")
                                    #,
                                    #textInput("codigo3","tipo")
                                            )
                                    )
                           
                           
                           
                           )
                 )
                 
                 
                 )
  
 # sidebarPanel(
    
 #   h2("Calculadora Microfinanzas")
 #   
 # ),

# mainPanel(
   
   #fluidRow(
   #tabsetPanel(
  # tabPanel("Series y Codigos",
   #         fluidRow(

 
  #)
 #  ),
 # tabPanel("graficos")

#  )
# ,position = c("left", "right"),fluid=FALSE)
#)
)


# Define server logic required to draw a histogram



server <- function(input, output) {
  library(jsonlite)
  library(curl)
  output$table <- DT::renderDataTable(DT::datatable({
    data <- base_series_mensuales }))
    
    Dates <- reactiveValues()
    observe({
      Dates$SelectedDates <- c(as.character(format(input$dateRange[1],format = "%Y-%m")),as.character(format(input$dateRange[2],format = "%Y-%m")))
      Dates$SelectedDates1 <- format(input$dateRange,format = "%d/%m/%Y")
    })
    
    output$SliderText <- renderText({Dates$SelectedDates})
    output$SliderText1 <- renderText({Dates$SelectedDates1})
 
    observe({
    
    x<-Dates$SelectedDates[1]
    y<-Dates$SelectedDates[2]   
    Dates$url <-paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",input$codigo1,"json",x,y,sep="/")
    })
    observeEvent(input$Graficar,{ 

    #x<-Dates$SelectedDates[1]
   # y<-Dates$SelectedDates[2]   
   # Dates$url <-paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",input$codigo1,"json",x,y,sep="/")
   
      if (sum(base_series_mensuales $Codigo==input$codigo1) == 0)
      {"Please select a data set"}
      else {
      
       Dates$firt_readlines <- readLines(Dates$url, warn = FALSE)

       Dates$test1<- fromJSON(Dates$firt_readlines)
   
       Dates$split <- matrix(unlist(strsplit(Dates$test1$periods$name, split = "[.]")),ncol=2,byrow=TRUE)
  
    
       Dates$cambio<-function(datos1){
      if (datos1=="Ene") {datos1="01"}
      else if (datos1=="Feb"){datos1="02"}
      else if (datos1=="Mar"){datos1="03"}
      else if (datos1=="Abr"){datos1="04"}
      else if (datos1=="May"){datos1="05"}
      else if (datos1=="Jun"){datos1="06"} 
      else if (datos1=="Jul"){datos1="07"} 
      else if (datos1=="Ago"){datos1="08"} 
      else if (datos1=="Sep"){datos1="09"} 
      else if (datos1=="Oct"){datos1="10"} 
      else if (datos1=="Nov"){datos1="11"} 
      else {datos1="12"}
      datos1
    }

    Dates$split[,1]<-unlist(lapply(Dates$split[,1],Dates$cambio))
    Dates$date<-c(paste("01 ", Dates$split[,1], Dates$split[,2]))
  
   #lct <- Sys.getlocale("LC_TIME")
   #Sys.setlocale("LC_TIME", "C")
   #Sys.setlocale("LC_TIME", "C")
   #Sys.setlocale("LC_TIME", lct)
   Dates$test1$periods$name<-as.Date(Dates$date,format="%d %m %Y")
   #names(Dates$test1$periods)<-(Dates$test1$config$series$name)
   Dates$test1$periods$values<-as.numeric(Dates$test1$periods$values)
   
   output$distPlot<-renderPlot({
     
     ggplot(Dates$test1$periods,aes(x=name,y=values))+geom_point(color='darkblue')+
       ggtitle(Dates$test1$config$series$name) +geom_line(color='red')+ylab(" ")+ 
       xlab("Fuente:Banco Central de Reserva del Peru")+
       theme(plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5)
     )
   
   })
    }
      })
    
    observeEvent(input$predecir,{ 
     
      #x<-Dates$SelectedDates[1]
      # y<-Dates$SelectedDates[2]   
       Dates$url1 <-paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",input$codigo1,"json","2005-01","2018-8",sep="/")
      
      if (sum(base_series_mensuales $Codigo==input$codigo1) == 0)
      {"Please select a data set"}
      else {
        
        Dates$firt_readlines2 <- readLines(Dates$url1, warn = FALSE)
        
        Dates$test2<- fromJSON(Dates$firt_readlines2)
        
        Dates$split2 <- matrix(unlist(strsplit(Dates$test2$periods$name, split = "[.]")),ncol=2,byrow=TRUE)
        
        
        Dates$cambio<-function(datos1){
          if (datos1=="Ene") {datos1="01"}
          else if (datos1=="Feb"){datos1="02"}
          else if (datos1=="Mar"){datos1="03"}
          else if (datos1=="Abr"){datos1="04"}
          else if (datos1=="May"){datos1="05"}
          else if (datos1=="Jun"){datos1="06"} 
          else if (datos1=="Jul"){datos1="07"} 
          else if (datos1=="Ago"){datos1="08"} 
          else if (datos1=="Sep"){datos1="09"} 
          else if (datos1=="Oct"){datos1="10"} 
          else if (datos1=="Nov"){datos1="11"} 
          else {datos1="12"}
          
        }
        
        Dates$split2[,1]<-unlist(lapply(Dates$split2[,1],Dates$cambio))
        Dates$date2<-c(paste("01 ", Dates$split2[,1], Dates$split2[,2]))
        
      
        Dates$test2$periods$name<-as.Date(Dates$date2,format="%Y-%m-%d")
        Dates$yearini<-as.numeric(format(Dates$test2$periods$name[1],"%Y"))
        Dates$yearfin<-as.numeric(format(Dates$test2$periods$name[length(Dates$test2$periods$name)],"%Y"))
        Dates$monthini<-as.numeric(format(Dates$test2$periods$name[1],"%m"))
        Dates$monthifin<-as.numeric(format(Dates$test2$periods$name[length(Dates$test2$periods$name)],"%m"))
        Dates$test2$periods$values<-as.numeric( Dates$test2$periods$values)
        Dates$datos<- Dates$test2$periods
        #Dates$dataaaa<-ts(Dates$datos$values,frequency=12,start=c(Dates$yearini, Dates$monthini),end=c(Dates$yearfin, Dates$monthifin)) 
        Dates$dataaaa<-ts(Dates$datos$values,frequency=12,start=c(2005,1),end=c(2018,8)) #,end=c(as.integer(Dates$yearfin[1]),as.integer(Dates$monthifin[1])))                                                    
        Dates$ model <- forecast::auto.arima( Dates$dataaaa)
        
        Dates$ m.forecast <- forecast::forecast(Dates$model, h = 12)
        
       Dates$df_prophet = data.frame(seq(as.Date("2005-01-01"),as.Date("2018-08-01"), "months"),  Dates$dataaaa)
    
       names(Dates$df_prophet) = c("ds", "y")
       Dates$m = prophet( Dates$df_prophet)
       Dates$future_df_prophet = make_future_dataframe( Dates$m, periods = 12, freq = 'months')
       Dates$prophet.forecast = predict ( Dates$m,  Dates$future_df_prophet)
        
        
       library(ggfortify)
       Dates$fortify.forecast <- fortify(Dates$m.forecast, ts.connect = TRUE)
      names(Dates$fortify.forecast) <- c('ds', 'data', 'fitted', 'yhat', 'Lo.80', 'Hi.80', 'Lo.90', 'Hi.90')
        
        library(ggplot2)
        output$distPlotpro<-renderPlot({
         
         # plot(Dates$dataaaa)
         # ggplot(data =  Dates$dataaaa, aes(x = as.Date(Dates$dataaaa[1]), y = Dates$dataaaa[2]))+
         ggplot(Dates$prophet.forecast, aes(x = as.Date(ds), y = yhat)) + #prophet
         geom_line(col = 'green') +
          geom_point(data =  Dates$fortify.forecast, aes(x=ds, y=data) ,col='black') +
          geom_line(data =  Dates$fortify.forecast, aes(x=ds, y= fitted), color='blue') +
          geom_line(data =  Dates$fortify.forecast, aes(x=ds, y= yhat), color='blue') +
          geom_ribbon(data =  Dates$fortify.forecast, aes(x=ds, ymin = Lo.80, ymax = Hi.80), fill = 'skyblue4', alpha=0.3)+
          ggtitle(paste(Dates$test2$config$series$name,"proyeccion a 12 meses",sep=" ")) +ylab(" ")+ 
            xlab("Fuente de datos:Banco Central de Reserva del Peru \nProyecciones: NeuroScience1.0")+theme(plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5))
        })
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
     #   lct <- Sys.getlocale("LC_TIME")
      #  Sys.setlocale("LC_TIME", "C")
       # Sys.setlocale("LC_TIME", "C")
      #  Sys.setlocale("LC_TIME", lct)
       # Dates$test1$periods$name<-as.Date(Dates$date,format="%d %m %Y")
        #names(Dates$test1$periods)<-(Dates$test1$config$series$name)
       # Dates$test1$periods$values<-as.numeric(Dates$test1$periods$values)
        
      #  output$distPlotpro<-renderPlot({
          
       #   ggplot(Dates$test1$periods,aes(x=name,y=values))+geom_point(color='darkblue')+
        #    ggtitle(Dates$test1$config$series$name) +geom_line(color='red')+ylab(" ")+ 
         #   xlab("Fuente:Banco Central de Reserva del Peru")+geom_smooth(method = "loess", se = FALSE)+
          #  theme(plot.title = element_text(color="blue", size=14, face="bold",hjust = 0.5)
           # )
          
       # })
      }
    })
    output$SliderText1 <- renderText({Dates$url})
    }
#)

 
#}


shinyApp(ui = ui, server = server)


