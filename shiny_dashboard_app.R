# Load required libraries
#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(ggplot2)


# Read database credentials
# source("./03_shiny_HW1/credentials_v3.R")
source("./credentials_v4.R")

tabs <- tabsetPanel(type = "pills",
                    tabPanel("Reservation", plotOutput("ggplot")),
                    tabPanel("Customer Number", plotOutput("lattice")))

ui <- dashboardPage(skin = "red",
      dashboardHeader(title = "Foodie"),
      #Sidebar content
      dashboardSidebar(
      #Add sidebar menus here
      sidebarMenu(
      menuItem("Restaurant in Your City", tabName = "top", icon = icon("dashboard")),
      menuItem("Add New Customer", tabName = "add", icon = icon("user")),
      menuItem("Find Your Favor", tabName = "find", icon = icon("heart")),
      menuItem("Update Price", tabName = "update", icon = icon("usd")),
      menuItem("Delete Customer", tabName = "delete", icon = icon("warning")),
      menuItem("Analytics", tabName = "analytics", icon = icon("line-chart")))
      ),
      dashboardBody(
      tabItems(
       # Add contents for first tab
       tabItem(tabName = "top", fluidPage(
         theme= shinytheme('journal'), 
         div(tags$img(src= "City_1.png", height= "100px", width= "800px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: center'),
         fluidRow(
         column(width= 6,textInput("text", label = h1("Type Your City"), value = "Enter city name")),
         column(width= 6,
                h1(),
                div(tags$img(src= "City_2.png", height= "120px", width= "240px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: left'))
                ),
         actionButton("Go", label = "Get results"),
         DT::dataTableOutput("mytable")
       )
       ),
       # Add contents for second tab
       tabItem(tabName = "add", fluidPage(
         fluidRow(
           column(width= 6,
                  numericInput("num", label = h5("Customer ID"), value = 301),
                  selectInput("select", label = h5("Select Gender"), 
                  choices = list("Male" = 'Male', "Female" = 'Female', "Prefer Not to Say" = 'NA'), 
                  selected = 'Male'),
                  textInput("age", label = h5("Your Age"), value = "Enter Age"),
                  textInput("name", label = h5("Your Name"), value = "Enter Name"),
                  textInput("phone", label = h5("Your Phone Number"), value = "123-456-7890"),
                  actionButton("Go1", label = "Enter"),
                  DT::dataTableOutput("new")
           ),
         column(
           h1(),
           width= 5, div(tags$img(src= "Cust_1.png", height= "200px", width= "300px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: right'),
           h1(),
          div(tags$img(src= "Cust_2.png", height= "180px", width= "260px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: left'))
                
         )
               )),
       # Add contents for third tab
       tabItem(tabName = "find", fluidPage(
              h2('Must Try Texas Food'),
              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/apbqDnRNhP4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>;'),
              sliderInput("slider1", label = h2("Restaurant Overall Rating"), min = 0, 
              max = 10, value = 5),
              actionButton("Go2", label = "Find the Restaurant"),
              
              DT::dataTableOutput("mytable1")
               )), 
       # Add contents for fourth tab
       tabItem(tabName = "update", 
               fluidPage(
                 fluidRow(
                column(width= 6,textInput("id", label = h2("Restuarant ID"), value = "Your ID Number"),
                sliderInput("price", label = h2("Price Per Person"), min = 0, max = 100, value = 50),
                actionButton("update", label = "Update"),
                DT::dataTableOutput("change")),
                column(width=5, 
                       h1(),
                       div(tags$img(src= "pic1.png", height= "200px", width= "300px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: right'))
                 )
               )),
       # Add contents for fifth tab
       tabItem(tabName = "delete",fluidPage(
         numericInput("a", label = h2("Customer ID Number"), value = 301),
         actionButton("delete", label = "Delete Info")
               )),
       # Add contents for sixth tab
       tabItem(tabName = "analytics",fluidPage(
         fluidRow(
         column(width= 4, tags$h2("App Performance"),
         ),
         column(width= 1,
                h1(),
                div(tags$img(src= "Analytics_2.png", height= "60px", width= "110px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: right')
                ),
         column(width= 5,
                div(tags$img(src= "Analytics_1.png", height= "90px", width= "190px", alt= "Something went wrong", deleteFile= FALSE), style= 'text-align: right')
         )
         )
         ),
         tabs
               ))
       )
       )



server <- function(input, output, session) {
  
  #Develop your server side code (Model) here
  observeEvent(input$Go, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <- paste("select rest_name, rating_overall, food_category, nation_category, price from restaurant where address_city = '",trimws(input$text),"';", sep = "")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    output$mytable = DT::renderDataTable({
      data
  })    
  })
  
  observeEvent(input$Go1, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <- paste("insert into customer (customer_ID, gender, age, customer_name, customer_phone) values (",(input$num),",'",(input$select),"',",(input$age),",'",trimws(input$name),"','",trimws(input$phone),"');",sep = "")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
    query1 <- paste("select * from customer where customer_ID = ",(input$num),";")
    print(query1)
    # Submit the fetch query and disconnect
    data1 <- dbGetQuery(db, query1)
    output$new = DT::renderDataTable({
      data1
    })    
    })
  
  observeEvent(input$Go2, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <- paste("select rest_name, address, address_city, hotline from restaurant where rating_overall = ",(input$slider1),";")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    output$mytable1 = DT::renderDataTable({
      data
    })    
  })
  
  observeEvent(input$update, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <- paste("UPDATE restaurant SET price =",(input$price),"WHERE rest_ID =",(input$id),";")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
    query2 <- paste("select rest_name, address_city, price from restaurant where rest_ID =",(input$id),";")
    print(query2)
    # Submit the fetch query and disconnect
    data2 <- dbGetQuery(db, query2)
    output$change = DT::renderDataTable({
      data2
   
    })
    })    
  
  observeEvent(input$delete, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <- paste("DELETE FROM customer WHERE customer_ID = ",(input$a),";")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
  })
  
  output$ggplot <- renderPlot({
    Month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    Reservation <- c(65,86,89,80,79,81,73,84,85,93,86,99)
    df <- data.frame(Month,Reservation)
    
    ggplot(data = df, aes(x = Month, y = Reservation))+ geom_line(color="black", size=1.5)
  })
  
  output$lattice <- renderPlot({
    pax <- c(1,2,3,4,5,6,7,8,9,10)
    Total_Reservation <- c(86,103,91,110,108,117,110,82,94,99)
    df <- data.frame(pax, Total_Reservation)
    
    ggplot(data = df, aes(x = pax, y = Total_Reservation))+ geom_bar(stat='identity', width=0.6, fill="steelblue")
  })
  
  
}

shinyApp(ui, server)