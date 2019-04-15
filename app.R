library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Three Variable Saving Investments"),
  fluidRow(
    column(3,
           sliderInput("init", h5("Initial Amount"),
                       min = 0, max = 100000, value = 1000, step = 500, pre ="$"),
           sliderInput("acont", h5("Annual Contribution"),
                       min = 0, max = 50000, value = 2000, step = 500, pre ="$")),
    
    column(3,
           sliderInput("retr", h5("Return Rate (in %)"),
                       min = 0, max = 20, value = 5, step = 0.1),
           sliderInput("gror", h5("Growth Rate (in %)"),
                       min = 0, max = 20, value = 2, step = 0.1)),
    
    column(3, 
           sliderInput("yrs", h5("Years"),
                       min = 0, max = 50, value = 20, step = 1),
           selectInput("facet", h5("Facet"),
                       choices = list("Yes" = 1, "No" = 2), selected = 2))
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput(outputId = "distPlot"),
    DT::dataTableOutput("table"))
)
  
# Define server logic ----
server <- function(input, output) {
  library(ggplot2)
    output$distPlot <- renderPlot({
      future_value <- function(amount,rate,years){
        return(amount*(1+rate)^years)}
      annuity <- function(contrib, rate, years){
        return(contrib*((1+rate)^years-1)/rate)}
      growing_annuity <- function(contrib,rate,growth,years){
        return(contrib*((1+rate)^years-(1+growth)^years)/(rate-growth))}
      modalities <- matrix(data=NA,input$yrs+1,4)
      for (i in 0:input$yrs){
        modalities[i+1,2] <- future_value(input$init, input$retr/100,i)
        modalities[i+1,3] <- annuity(input$acont, input$retr/100,i)+future_value(input$init,input$retr/100,i)
        modalities[i+1,4] <- growing_annuity(input$acont, input$retr/100,input$gror/100,i)+future_value(input$init,input$retr/100,i)
        modalities[i+1,1] <- i
      }
      modalities<-data.frame(modalities)
      names(modalities) <- c('years','no contribution','fixed contribution','growing contribution')
      if (input$facet == 2){
      library(reshape2)
      modalities2 <- melt(modalities,id="years")
      ggplot(modalities2, aes(x=years, y=value, group=variable, colour=variable))+geom_line()+ggtitle("Three Variable Modes of Investing")+ylab("Money (dollars)")+ xlab("Years")
      }
      else{
        library(ggplot2)
        library(reshape2)
        modalities2 <- melt(modalities,id="years")
        ggplot(data=modalities2, mapping = aes(x= years, y = value,colour=variable))+geom_line()+facet_wrap(~ variable)+ggtitle("Three Variable Modes of Investing")+ylab("Money (dollars)")+ xlab("Years")+geom_area(aes(fill=variable,group=variable,alpha=0.5,position='identity'))
      }
      
    })
    output$table <- DT::renderDataTable(DT::datatable({
      
      future_value <- function(amount,rate,years){
        return(amount*(1+rate)^years)}
      annuity <- function(contrib, rate, years){
        return(contrib*((1+rate)^years-1)/rate)}
      growing_annuity <- function(contrib,rate,growth,years){
        return(contrib*((1+rate)^years-(1+growth)^years)/(rate-growth))}
      modalities <- matrix(data=NA,input$yrs+1,4)
      for (i in 0:input$yrs){
        modalities[i+1,2] <- future_value(input$init, input$retr/100,i)
        modalities[i+1,3] <- annuity(input$acont, input$retr/100,i)+future_value(input$init,input$retr/100,i)
        modalities[i+1,4] <- growing_annuity(input$acont, input$retr/100,input$gror/100,i)+future_value(input$init,input$retr/100,i)
        modalities[i+1,1] <- i
      }
      modalities<-data.frame(modalities)
      names(modalities) <- c('years','no_contrib','fixed_contrib','growing_contrib')
      library(knitr)
      names(modalities) <- c("years","no contribution","fixed contribution","growing contribution")
      modalities
      }))
}
shinyApp(ui = ui, server = server)
