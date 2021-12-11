
library(shiny)
library(shinythemes)
library(rgdal)
library(DT)
library(corrplot)
library(RCurl)
library(rsconnect)

data <- read.csv("https://raw.githubusercontent.com/aeryali/ADS_project/main/marketing_data1.csv", header = TRUE)


x1<- -0.0000531264776288846
x2<- -0.0000209013787027434

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",
                  "Data Science Project",
                  tabPanel("About",
                           mainPanel(
                             h1("Marketing Analytics Dashboard", align = "left"),
                             h2("Alisha Aery - Exploratory Data Analysis and Visualization", align = "left"),
                             h2("Pranaykumar Pagdhare - Correlation Analysis", align = "left"),
                             h2("Saymon Mameza - Predicted Web Page Visits ", align = "left"),
                             h3("Purpose - understand data and predict web page visits", align = "left"),
                             h3("Data - Customer profiles, preferences, and shopping channel performance", align = "left"),
                             h4("Data Link - https://www.kaggle.com/jackdaoud/marketing-data", align = "left")
                           )),
                  tabPanel("Exploratory Data Analysis and Visualization",
                           sidebarPanel(
                             selectInput("var",label="Choose a Product",choice=c("MntWines"=10,
                                                                                 "MntFruits"=11,
                                                                                 "MntMeatProducts"=12,
                                                                                 "MntFishProducts"=13,
                                                                                 "MntSweetProducts"=14,
                                                                                 "MntGoldProds"=15), selectize=FALSE)),
                           mainPanel(
                             h2("Summary of the Product"),
                             verbatimTextOutput("sum"),
                             plotOutput("box")
                           ),
                           
                  ),
                  tabPanel("Correlation Analysis",icon = icon("chart-line"), 
                           fluidPage(theme = shinytheme("cerulean"), 
                                     
                                     #headerPanel("header for title 1"),
                                     titlePanel(h3("Correlation Analysis for different products")),
                                     
                                     wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
                                               id = "leftPanel",
                                               conditionalPanel(condition="input.tb1=='1'",
                                                                selectInput("corr_text_1", "Select product",
                                                                            choices = c("MntWines"=10,
                                                                                        "MntFruits"=11,
                                                                                        "MntMeatProducts"=12,
                                                                                        "MntFishProducts"=13,
                                                                                        "MntSweetProducts"=14,
                                                                                        "MntGoldProds"=15), selectize=FALSE),
                                                                
                                                                selectInput("corr_text_2", "Select Group",
                                                                            choices = c("Income"=5),
                                                                            selectize=FALSE))),
                                     mainPanel(                          
                                       tabsetPanel(
                                         tabPanel(value="1", "Correlation plots", plotOutput("distPlot")),
                                         tabPanel(            "Summary Data",
                                                              br(),
                                                              h3("Summary Data"),DTOutput(outputId = "table"),
                                                              h3("Correlation plots"), plotOutput("corrPlot")
                                         ),
                                         id = "tb1")
                                     )
                           )
                  ),
                  tabPanel("Regression",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("Income", "Annual Income:", ""),
                             numericInput("AMSPW", "Amount of Money Spent On Wine:", ""),
                             
                           ), 
                           mainPanel(
                             h1("Predicted Web Page Visits"),
                             
                             h4(""),
                             verbatimTextOutput("txtout"),
                             
                           ) 
                           
                  )
                  
                ) 
) 


# Define server function  
server <- function(input, output) {
  
  output$table <- renderDT(head(data))
  #Tab Correlation Plot
  output$distPlot <- renderPlot({
    corrx<-as.numeric(data[,as.numeric(input$corr_text_1)])
    corry<-as.numeric(data[,as.numeric(input$corr_text_2)])
    plot(corrx, corry,pch = 19,col = "blue")
    abline(lm(corry ~ corrx), col = "red", lwd = 3)
    # Pearson correlation
    #text(paste("Correlation:", round(cor(corrx, corry), 2)), x = 95, y = 95)
  })
  
  output$corrPlot <- renderPlot({
    num_data = data[, sapply(data, is.numeric)]
    M <- cor(num_data)
    corrplot(M, method="circle",type="lower")
  })
  
  
  output$txtout <- renderText({
    paste(round((input$Income*x1)+(input$AMSPW*x2)+8.1,digit = 0))
  })
  
  
  output$sum <- renderPrint({
    
    summary(data[,as.numeric(input$var)])
  })
  
  output$box <- renderPlot({
    
    x<-summary(data[,as.numeric(input$var)])
    hist(x,col="blue",border="green",main=names(data[as.numeric(input$var)]))
  })
  
  
} 


# Create Shiny object
shinyApp(ui = ui, server = server)
