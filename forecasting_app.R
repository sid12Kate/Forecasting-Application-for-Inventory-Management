library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(TTR)
library(forecast)
library(tsfknn)
library(zoo)
library(xts)


#Reading datasets
items.dataset<-read.table(file = 'C:/Users/siddh/Downloads/Lokad_Items.tsv' , sep = '\t' , header = TRUE )
po.dataset<-read.table(file = 'C:/Users/siddh/Downloads/Lokad_PurchaseOrders.tsv' , sep = '\t' , header = TRUE )
View(items.dataset)
View(po.dataset)
# Merging datasets depending on the foreign key
final_dataset <- merge(items.dataset , po.dataset, by ="Id")
View(final_dataset)
# Calculating difference between order date and delivery dates so as to obtain lead time.
final_dataset$diff <- (as.Date(final_dataset$DeliveryDate))- (as.Date(final_dataset$Date))
final_dataset$diff <- as.numeric(final_dataset$diff)
#View(final_dataset)
# As, this is time series forecasting we just select the required columns from the entire dataset.
time_series_data <- subset(final_dataset, select = c(Date,Supplier.x,SubCategory,diff))
#Ordering the data as per the oldest to newest date.
time_series_data<-time_series_data[order(as.Date(time_series_data$Date, format="%Y-%m-%d")),]
#View(time_series_data)

# Selecting columns to track buy price
profit_data <- subset(final_dataset, select = c(Date,Supplier.x,SubCategory,BuyPrice))
#Ordering data as per time
profit_data<-profit_data[order(as.Date(profit_data$Date, format="%Y-%m-%d")),]
#View(profit_data)
#Selecting columns to track demand
demand_data <- subset(final_dataset, select = c(Date, Supplier.x, SubCategory, Quantity))
#Ordering data as per time
demand_data<-demand_data[order(as.Date(demand_data$Date, format="%Y-%m-%d")),]
#View(demand_data)

ui <- dashboardPage(
                    dashboardHeader( title = "Forecasting Application for Inventory Management"),
                    dashboardSidebar(
                      selectInput("productInput_1", "Select a product ",
                                  choices = c("Speaker", "Cover", "Adapter","Keyboard","Gaming","Educational","Desktop","Connector","Headset", "Office Software")
                                  ),
                      actionButton(inputId = "submit_1" , label = "Submit")
                      ),
                    dashboardBody(
                      fluidRow(
                        box(title = "Supplier",  width = 12, htmlOutput("supplier")),
                        box(title = "Lead time forecast",  plotOutput("lead_time",  click = "plot_click_1") ,
                        verbatimTextOutput("leadtime_info")  ),
                        box(title = "Price forecast",   plotOutput("price", click = "plot_click_2" ) ,
                            verbatimTextOutput("price_info")    ),
                        box(title = "Demand forecast",  plotOutput("demand",   click = "plot_click_3"),
                            verbatimTextOutput("demand_info")    ),
                        box(title = "Results", htmlOutput("results") )
                        
                        
                        
                      )
                    )
                  )

server <- function( input, output ){
  predictions <- reactiveValues()
  
  output$lead_time <- renderPlot({
    product_value <- eventReactive(input$submit_1,{input$productInput_1})
    product_value <- as.character(product_value())
    #print(product_value)
    #Subsetting the data as per product selected
    newdata <- subset(time_series_data,  SubCategory == product_value)
    #View(newdata)
    #just selecting the time and diff attributes
    newdata <- subset(newdata, select=c(Date,diff))
    #View(newdata)
    # Converting the data to time series format
    newdata.xts <- xts(x = newdata$diff, order.by = as.Date(newdata$Date)) 
    newdata.start <- c(year(start(newdata.xts)), month(start(newdata.xts)))
    newdata.end <- c(year(end(newdata.xts)), month(end(newdata.xts)))
    newdata.train <- ts(as.numeric(newdata.xts), start = newdata.start, end = newdata.end, frequency = 12)
    #View(newdata.train)
    pred_1 <- knn_forecasting(newdata.train, h = 12, lags = 1:12, k = 4, msas = "MIMO")
    ro <- rolling_origin(pred_1, h = 3, rolling = FALSE)
    x <- ceiling(ro$predictions[1])
    predictions$lead_time <- x
    autoplot(pred_1,  highlight = "neighbors", faceting = FALSE)
  })
  output$leadtime_info <- renderText({
    paste0( "Lead time=", as.numeric(input$plot_click_1$y))
  })
  output$price <- renderPlot({
    product_value_1 <- eventReactive(input$submit_1,{input$productInput_1})
    
    product_value_1 <- as.character(product_value_1())
    #print(product_value_1)
    
    #Subsetting the data as per product selected
    
    newdata_1 <- subset(profit_data,  SubCategory == product_value_1)
     View(newdata_1) 
    #just selecting the time and diff attributes
    newdata_1 <- subset(newdata_1, select=c(Date,BuyPrice))
    #View(newdata_1)
    
    # Converting the data to time series format
    newdata_1.xts <- xts(x = newdata_1$BuyPrice, order.by = as.Date(newdata_1$Date)) 
    newdata_1.start <- c(year(start(newdata_1.xts)), month(start(newdata_1.xts)))
    newdata_1.end <- c(year(end(newdata_1.xts)), month(end(newdata_1.xts)))
    newdata_1.train <- ts(as.numeric(newdata_1.xts), start = newdata_1.start, end = newdata_1.end, frequency = 12)
    #View(newdata_1.train)
    pred_3 <- knn_forecasting(newdata_1.train, h = 12, lags = 1:12, k = 4, msas = "MIMO")
    
    ro <- rolling_origin(pred_3, h = 3, rolling = FALSE)
    x <- ro$predictions[1]
    predictions$price <- x
    autoplot(pred_3,  highlight = "neighbors", faceting = FALSE)
  })
  output$price_info <- renderText({
    paste0( "Price =", as.numeric(input$plot_click_2$y))
  })
  output$demand <- renderPlot({
  product_value_1 <- eventReactive(input$submit_1,{input$productInput_1})
  
  product_value_1 <- as.character(product_value_1())
  #print(product_value_1)
  
  #Subsetting the data as per product selected
  newdata_2 <- subset(demand_data,  SubCategory == product_value_1)
  #just selecting the time and diff attributes
  newdata_2 <- subset(newdata_2, select=c(Date,Quantity))
  #View(newdata)
  
  # Converting the data to time series format
  newdata_2.xts <- xts(x = newdata_2$Quantity, order.by = as.Date(newdata_2$Date)) 
  newdata_2.start <- c(year(start(newdata_2.xts)), month(start(newdata_2.xts)))
  newdata_2.end <- c(year(end(newdata_2.xts)), month(end(newdata_2.xts)))
  newdata_2.train <- ts(as.numeric(newdata_2.xts), start = newdata_2.start, end = newdata_2.end, frequency = 12)
  #View(newdata.train)
  pred_2 <- knn_forecasting(newdata_2.train, h = 12, lags = 1:12, k = 4, msas = "MIMO")
  ro <- rolling_origin(pred_2, h = 3, rolling = FALSE)
  x <- ceiling(ro$predictions[1])
  predictions$demand <- x
  autoplot(pred_2,  highlight = "neighbors", faceting = FALSE)
  
  })
  output$demand_info <- renderText({
    paste0( "Demand =", ceiling(as.numeric(input$plot_click_3$y)))
  })
  output$results <- renderUI({
    str_0 <-tags$h2(tags$b(paste("For your next order in January,2019 :")))
    str_1 <- tags$h3(paste("Required Number of products: ",predictions$demand))
    str_2 <- tags$h3(paste("Buy Price of Individual Product[USD]: ",predictions$price))
    str_3 <- tags$h3(paste("No. of days required to deliver the product: ",predictions$lead_time))
    str_4 <- tags$h3(paste("Net Amount to be paid to the supplier[USD]: ",(predictions$demand*predictions$price)))
    HTML(paste(str_0,str_1, str_2,str_4, str_3, sep = '<br/>'))
  })
  output$supplier <- renderUI({
    product_value <- eventReactive(input$submit_1,{input$productInput_1})
    product_value <- as.character(product_value())
    supplier_dataset <- subset(time_series_data, SubCategory == product_value)
    supplier_dataset <- subset(supplier_dataset, select = Supplier.x)
    supplier <- supplier_dataset[1,]
    str_0 <- tags$h3(tags$b(paste("This product is delivered by : ",supplier)))
    HTML(paste(str_0))
    
  })
  
}

shinyApp(ui, server)


