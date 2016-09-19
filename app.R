####This app outputs total ET factor for given 'date range' and 'account numbers'/'order numbers'####
####Also outputs ET factor for the same date range in previous 2 years for comparision####

#Loading Libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(RODBC)
library(lubridate)
library(DT)

#setting global parameters for R session
options(stringsAsFactors = FALSE)

setwd("C:/Users/AVanjavakam/Desktop/R_Home/ET Extended finished") #set working directory

fnDateFromServer <- function(server_time){
  ## fn takes the date variable as a string then parses and converts to a char string
  datestr <- as.character(server_time)
  if (server_time > 100000) {
    cent <- as.numeric(substr(datestr,1,1))
    yr <- as.numeric(substr(datestr,2,3))
    days <- as.numeric(substr(datestr,4,6)) - 1
  } else {
    cent <- 0
    yr <- as.numeric(substr(datestr,1,2))
    days <- as.numeric(substr(datestr,3,5)) - 1
  }
  
  year <- 1900 + cent*100 + yr
  o_date <- paste0(year,"-01-01")
  d_return <- as.Date(days, origin = o_date, format = "%Y-%m-%d")
  as.character(d_return)
}

#build col selection for query
lists.colnames_F1940 <- read.csv("F1940.csv", stringsAsFactors = FALSE) %>%  #cheater version - has 'x' in the used column for rows to be retained.
  filter(used == "x") %>%
  select(Field, Description)

lists.colnames_F1903 <- read.csv("F1903.csv", stringsAsFactors = FALSE) %>%  #cheater version - has 'x' in the used column for rows to be retained.
  filter(used == "x") %>%
  select(Field, Description)

lists.colnames_F1901 <- read.csv("F1901.csv", stringsAsFactors = FALSE) %>%  #cheater version - has 'x' in the used column for rows to be retained.
  filter(used == "x") %>%
  select(Field, Description)

sel.col_F1940 <- paste0(lists.colnames_F1940$Field,collapse = ", ") #build text string of SQL col names
sel.col_F1903 <- paste0(lists.colnames_F1903$Field,collapse = ", ") #build text string of SQL col names
sel.col_F1901 <- paste0(lists.colnames_F1901$Field,collapse = ", ") #build text string of SQL col names


#build query string
qry.strF1940 <- paste("SELECT",sel.col_F1940,"FROM PRODDTA.F1940")
qry.strF1903 <- paste("SELECT",sel.col_F1903,"FROM PRODDTA.F1903")
qry.strF1901 <- paste("SELECT",sel.col_F1901,"FROM PRODDTA.F1901")

#connection to server and query table
ch <- odbcDriverConnect("Server=lynxRO.mnwd.local;Driver=SQL Server;Catalog=JDE_PRODUCTION_COPY") #open channel
F1940Table <- sqlQuery(ch, qry.strF1940,stringsAsFactors = FALSE) #return query as F1940 table
F1903Table <- sqlQuery(ch, qry.strF1903,stringsAsFactors = FALSE) #return query as F1903 table
F1901Table <- sqlQuery(ch, qry.strF1901,stringsAsFactors = FALSE) #return query as F1901 table

odbcCloseAll() #close channel


#swapping sql names to readable names
colnames(F1903Table) <- lists.colnames_F1903$Description
colnames(F1901Table) <- lists.colnames_F1901$Description
colnames(F1940Table) <- lists.colnames_F1940$Description

#joining F1903 and F1901
JoinedTable <- left_join(F1903Table,F1901Table)
names(F1940Table) <- make.names(names(F1940Table))       #converting to R language friendly names
names(JoinedTable) <- make.names(names(JoinedTable))     #converting to R language friendly names

F1940Table[3] <- F1940Table[3]/100    #Dividing ET by 100


F1940Table[2] <- sapply(F1940Table[2],fnDateFromServer) #using helper function to convert dates from server
F1940Table[2] <- as.Date(F1940Table[[2]]) #converting into date format
F1940Table<- arrange(F1940Table,Effective.Date) #sorting by date


uniqueAccounts <- unique(JoinedTable$Order.Number) #Passing accounts into a vector

uniqueRegions <- unique(JoinedTable$Weather.Area) #Passing Regions into a vector



ui <- fluidPage(
  
  
  # Give the page a title
  titlePanel("Evapotranspiration by Account"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("order_number", "Account Number", choices = NULL,  multiple = TRUE, selectize = TRUE), #creating an empty instance for choices so that UI will load quickly initially
      dateRangeInput(inputId="date", label = "Date:",min = "2013-07-04", max = Sys.Date()),
      submitButton("Get Total ET factor"),
      
      hr(),
      helpText("Please enter your account number and a date range to get the total ET factor for this year and earlier years")
    ),
    
    # Create a spot for the output
    mainPanel(
      dataTableOutput("ETtable")
      
    )
    
  )
)




server <- function(input, output,session){
  
  updateSelectizeInput(session, 'order_number', choices = uniqueAccounts, server = TRUE) #loading choices on server side
  
  #input the order number
  Joined_final<- reactive({
     filter(JoinedTable,  Order.Number %in% input$order_number)
      
  })
  # get the required fields for that order number
  F1940_matched <- reactive({
    left_join(Joined_final(),F1940Table)
  })
  
  #input the date ranges and get the results for current year
  F1940_final<- reactive({
    filter(F1940_matched(), Effective.Date >= input$date[1] & Effective.Date <= input$date[2]-1) %>%
      group_by(Order.Number, Service.Address, Weather.Area) %>%
      summarise(Total_ETfactor = sum(ET.Factor))
  })
  #input the date ranges and get the results an year ago
  F1940_final2<- reactive({
    filter(F1940_matched(), Effective.Date >= input$date[1]-365 & Effective.Date <= input$date[2]-366) %>%
      group_by(Order.Number, Service.Address, Weather.Area) %>%
      summarise(Total_ETfactor.an.year.ago = sum(ET.Factor))
  })
  #input the date ranges and get the results 2 years ago
  F1940_final3<- reactive({
    filter(F1940_matched(), Effective.Date >= input$date[1]-730 & Effective.Date <= input$date[2]-731) %>%
      group_by(Order.Number, Service.Address, Weather.Area) %>%
      summarise(Total_ETfactor.2.years.ago = sum(ET.Factor))
  })

  #Consolidate all the results in one table
  F1940_final4<- reactive({
    
    
    F1940_final() %>% left_join(F1940_final2()) %>% left_join(F1940_final3())
      
    
     
  })
  
  
  
  # F1940_final5()<-reactive({
  #   d <- F1940_final4()
  #   colnames(d) <- c("Account Number", "Service Address", "Weather Region", "Total ET factor now", "Total ET factor an year ago", "Total ET factor 2 years ago")
  #   d
  # })
  
  
  # F1940_final5<- reactive({
  # 
  #   names(F1940_final4()) = c("Account Number", "Service Address", "Weather Region", "Total ET factor now", "Total ET factor an year ago", "Total ET factor 2 years ago")
  # })
  
  #names(F1940_final4()) <- c("Account Number", "Service Address", "Weather Region", "Total ET factor now", "Total ET factor an year ago", "Total ET factor 2 years ago")
  
  
  #print the table output
  # F1940_final5 <- reactive({
  #   names(F1940_final4())<-list("Account Number", "Service Address", "Weather Region", "Total ET factor now", "Total ET factor an year ago", "Total ET factor 2 years ago")
  # })
  output$ETtable<- renderDataTable({
    
     
     datatable(F1940_final4(),caption = 'ET factors displayed here are aggregated sums for the choosen date range',
               colnames = c("Account Number", "Service Address", "Weather Region", 
                                           "Total ET factor now", "Total ET factor an year ago", 
                                           "Total ET factor 2 years ago"),
               options = list(
                  columnDefs = list(list(className = 'dt-center',
                  targets = c(1,2,3,4,5,6)))))
    
  })
  
  session$onSessionEnded(stopApp) #to automatically stop the app when browser is closed
  
  
}

#Run below code to start the app
shinyApp(ui = ui, server = server)
 
