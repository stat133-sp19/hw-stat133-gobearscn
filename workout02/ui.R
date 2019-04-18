library(reshape)
library(shiny)
library(dplyr)
library(ggplot2)
# Define UI for slider demo app ----
ui <- fluidPage(
    
    wellPanel(
      headerPanel('MyCoolApp'),
      fluidRow(
        column(4,
               # Input: Initial Amount
               sliderInput("ia", "Initial Amount",
                           min = 0, max = 100000,
                           pre = "$", sep = ",",
                           value = 1000,step = 500),
               # Input: Annual Contribution
               sliderInput("ac", "Annual Contribution",
                           min = 0, max = 50000,
                           pre = "$", sep = ",",
                           value = 2000, step = 500)),
        column(4,
               
               # Input: Return Rate in percentage
               sliderInput("rr", "Return Rate",
                           min = 0, max = 20,
                           step = 0.1,post  = "%",
                           value = 5),
               
               # Input:Growth Rate in percentage
               sliderInput("gr", "Growth Rate",
                           min = 0, max = 20,post  = "%",
                           value = 2, step = 0.1
               )),
        
        column(4,
               # Input: Years
               sliderInput("yr", "Years",
                           min = 0, max = 50,
                           value = 20, step = 1),
               
               # Input: Facet or not
               selectInput("facet", "Facet?",
                           choices = c("Yes", "No"))
               
        )
        
        ),
      h4("Timelines"),
      plotOutput('plot'),
      
      # Output: Header + table of distribution ----
      h4("Balances"),
      verbatimTextOutput("table")
      )

    
  
)

# Define server logic for slider examples ----
server <- function(input, output) {
  modalities=reactive({
  future_value=function(amount,rate,years){
    amount*(1+rate)**years
  }
  annuity=function(contrib,rate,years){
    contrib*(((1+rate)**years-1)/rate)
  }
  growing_annuity=function(contrib,rate,growth,years){
    contrib*((((1+rate)**years)-((1+growth)**years))/(rate-growth))
  }
  year=c();no_contrib=c();fixed_contrib=c();growing_contrib=c()
  for (i in (0:input$yr)){
    year[i+1]=i
    no_contrib[i+1]=future_value(amount=input$ia,rate=input$rr/100,years=i)
    fixed_contrib[i+1]=future_value(amount=input$ia,rate=input$rr/100,i)+annuity(contrib=input$ac,rate=input$rr/100,years=i)
    growing_contrib[i+1]=future_value(amount=input$ia,rate=input$rr/100,i)+growing_annuity(contrib=input$ac,rate=input$rr/100,growth=input$gr/100,years=i)
  }
  
  modalities=data.frame(year,no_contrib,fixed_contrib,growing_contrib)
  
  
  
  modalities
  })
  output$plot <- renderPlot({
  
  
  if (input$facet == "No") {
    ggplot(data=modalities(),aes(x=year))+
      geom_line(aes(y=no_contrib,color='no_contrib'))+
      geom_line(aes(y=fixed_contrib,color='fixed_contrib'))+
      geom_line(aes(y=growing_contrib,color='growing_contrib'))+
      labs(title="investment modalities",x='years',y='balance')+
      scale_x_continuous(breaks=seq(0, 30,1))
  }
  else{
    tmp = melt(modalities(), id.vars = "year")
    ggplot(data=tmp,aes(x=year, y=value,color=variable))+
      geom_point()+
      geom_area(aes(fill=variable),alpha=0.4)+
      scale_x_continuous(breaks=seq(0, 30,1))+
      facet_wrap(.~variable)
  
  }
  
  
  })
  
  output$table <- renderPrint({
    
    modalities()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)