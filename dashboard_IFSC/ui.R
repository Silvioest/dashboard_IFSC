library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(ggplot2movies)
library(RColorBrewer)

df <- read.csv("/home/silvio/shiny-apps/sample-apps/Exemplo_Dashboard_Ingresso/BSelecao.csv", head=T,stringsAsFactors = FALSE)


ui <- dashboardPage(
  dashboardHeader(title = "Dados Ingresso"),
  dashboardSidebar(
    
    selectInput("campus", "Campus:",choices=NULL, 
                selected = NULL, 
                selectize = FALSE),
    
        selectInput("cursos", "Cursos:",choices=NULL, 
                    selected = NULL, 
                    selectize = FALSE)
    
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Percentual por Sexo", plotlyOutput("plot1", height = 250)),
      
     box(
        title = "Notas por Sexo", plotlyOutput("plot2", height = 250)),
        
      box(
       title = "Percentual por Raça", plotlyOutput("plot3", height = 250)),
          
     box(
       title = "Notas por Raça", plotlyOutput("plot4", height = 250)),
     
     box(
         title = "Percentual por Estado Civil", plotlyOutput("plot5", height = 250)),
       
       box(
         title = "Notas por Estado Civil", plotlyOutput("plot6", height = 250)),
     
         box(
           title = "Percentual por Origem", plotlyOutput("plot7", height = 250)),
         
         box(
           title = "Notas por Origem", plotlyOutput("plot8", height = 250)),
           
           box(
             title = "Percentual por Faixa de Renda", plotlyOutput("plot9", height = 250)),
           
           box(
             title = "Notas por Faixa de Renda", plotlyOutput("plot10", height = 250)
     )
    )
  )
)

server <- function(input, output,session) {
  
  observe({
    df_campus <- unique(df$Campus)
    updateSelectInput(session, 'campus', choices = df_campus, selected = NULL)
  })
  observe({
    req(input$campus)
    df_cursos <- df[df$Campus == input$campus,]
    df_cursos <- unique(df_cursos$Cursos)
    updateSelectInput(session, 'cursos', choices = df_cursos, selected = NULL)
  })
  
  dados <- read.csv("/home/silvio/shiny-apps/sample-apps/Exemplo_Dashboard_Ingresso/dados.csv", head=T, dec=",")
 
  output$plot1 <- renderPlotly({
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
       
    tabela<-data.frame(table(dados2$Sexo))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot2 <- renderPlotly({
    
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
     
   p <- plot_ly(dados2, x=PontFinal, color = Sexo, type = "box")
   p
  })
  
 output$plot3<- renderPlotly({
   if("Todos"==input$campus){
     dados1 <- dados
   }else{
     dados1 <- subset(dados, Campus==input$campus)
   }
   if("Todos"==input$cursos){
     dados2 <- dados1
   }else{
     dados2 <- subset(dados1, Curso==input$cursos)
   }
   
    tabela<-data.frame(table(dados2$Raca))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot4 <- renderPlotly({
    
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    
    p <- plot_ly(dados2, x=PontFinal, color = Raca, type = "box")
    p
  })  
 
  output$plot5<- renderPlotly({
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    
    tabela<-data.frame(table(dados2$EstadoCivil))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot6 <- renderPlotly({
    
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    
    p <- plot_ly(dados2, x=PontFinal, color = EstadoCivil, type = "box")
    p
  })
  
  output$plot7<- renderPlotly({
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    tabela<-data.frame(table(dados2$localiza))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot8 <- renderPlotly({
    
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    
    p <- plot_ly(dados2, x=PontFinal, color = localiza, type = "box")
    p
  })   

  output$plot9<- renderPlotly({
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    tabela<-data.frame(table(dados2$FaixaRenda))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie", hole = 0.6)
  })
  
  output$plot10 <- renderPlotly({
    
    if("Todos"==input$campus){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$campus)
    }
    if("Todos"==input$cursos){
      dados2 <- dados1
    }else{
      dados2 <- subset(dados1, Curso==input$cursos)
    }
    
    p <- plot_ly(dados2, x=PontFinal, color = FaixaRenda, type = "box")
    p
  })  
    
  }

shinyApp(ui, server)
