library(rgdal)
library(leaflet)
library(shiny)
library(htmltools)
library(shinydashboard)
library(RPostgreSQL)
library(RPostgres)
library(DT)
library(plotly)
library(rjson)
library(pool)
library(DBI)
library(ggplot2)
library(googlesheets4)
library(shinyjs)
library(plyr)
library(dplyr)
library(googledrive)

# major panels: (1) form, (2) map, tables, plots AKA dashboard, (3) about

# Set up db variables
db <- ""
host_db <- ""
db_port <- ""
db_user <- ""
db_password <- ""
table <- ""

SHEET_ID = ''

gs4_auth(email="", path="client_secret.json", scopes = "https://www.googleapis.com/auth/spreadsheets")

useShinyjs()

# Google Sheets save data function
saveData <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(SHEET_ID, data)
}

# Google Sheets load data function
loadData <- function() {
  # Read the data
  read_sheet(SHEET_ID)
}

# Create save data function
saveData1 <- function(data) {
  # Connect to the database
  con <- dbConnect(RPostgres::Postgres(), 
                   dbname = db, host=host_db, port=db_port, 
                   user=db_user, password=db_password)  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(con, query)
  dbDisconnect(con)
}

# Create load data function
loadData2 <- function() {
  # Connect to the database
  con <- dbConnect(RPostgres::Postgres(), 
                   dbname = db, host=host_db, port=db_port, 
                   user=db_user, password=db_password)  
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  data
}

# Connect to db and create a dataframe
# con <- dbConnect(RPostgres::Postgres(), 
                 #dbname = db, host=host_db, port=db_port, 
                 #user=db_user, password=db_password)  
# Construct the fetching query
# query <- sprintf("SELECT * FROM %s", table)
# Submit the fetch query and disconnect
# data <- dbGetQuery(con, query)
# dbDisconnect(con)


# Define the fields we want to save from the form
fields <- c("name", "contained", "acres", "date", "i_num", "i_name", "i_kind", "start_date", "cause", "i_commander", "imt_type",
            "state_unit", "county", "latlong", "location", "size", "percentcontained", "expectedcontainment", "line", "costs",
            "controlled", "injuries", "injuriestodate", "fatalities", "structure", "threats", "communities_threatened", "needs",
            "problems", "resources_threatened", "weather", "benefits", "fuels", "behavior", "sig_events", "est_control",
            "proj_final_size", "etd_final_cost", "forecast_weather", "next_actions", "proj_spread", "resistance", "likelihood",
            "demob", "remarks", "commit_resources", "other_agencies", "preparer", "final")

hawaii <- readOGR("2019_1999_Hawaii_Fire_Perimeters.shp", GDAL1_integer64_policy = TRUE)
hawaii[is.na(hawaii$Sat_sz_ac)] <- 0

popup <- paste("<b>Source: </b>", 
               hawaii$Source,
               "<br><b>Size: </b>", 
               hawaii$Sat_sz_ac,
               "<br><b>Incident Name: </b>",
               hawaii$Inc_name,
               "<br><b>Date: </b>",
               hawaii$YYYYMMDD) %>% 
  lapply(htmltools::HTML)

paste(unique(hawaii$Source))







# test area
testdf <- loadData()
sum(hawaii$Sat_sz_ac)
n_distinct(testdf$name, na.rm = TRUE)








ui <- dashboardPage(
  dashboardHeader(title="Hawaii Wildfire DB"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tasks")),
    menuItem("Map", tabName = "map", icon = icon("globe")),
    menuItem("Form", tabName = "form", icon=icon("table")),
    menuItem("Data", tabName = "plot", icon=icon("bar-chart-o")),
    menuItem("About", tabName = "about", icon=icon("cog"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Updates:"),
              fluidRow(
                infoBoxOutput("acresbox"),
                infoBox("Total Acres Burned (historic)", value = sum(hawaii$Sat_sz_ac), icon = icon("satellite"),
                        fill = TRUE, color = "yellow")
              ),
              fluidRow(
                infoBoxOutput("incidentsbox"),
                infoBox("Total Incidents (historic)", value = nrow(hawaii), icon = icon("history"))
              ),
              fluidRow(
                infoBoxOutput("usersbox")
              )
              ),
      tabItem(tabName = "map", 
              box(width = 4,
                  sliderInput("range", "Size (ha)",  min = min(hawaii$Sat_sz_ac, na.rm=TRUE),
                                                 max = max(hawaii$Sat_sz_ac, na.rm=TRUE),
                                                 value = range(hawaii$Sat_sz_ac),
                                                 step = 100),
                  sliderInput("year", "Year", min = 1999,
                          max = max(hawaii$Year, na.rm = TRUE),
                          value = max(hawaii$Year), sep=""),
                  checkboxGroupInput("agency", "Source: ", 
                                 choices = c("HWMO" = "Hawaii Wildfire Management Organization", 
                                             "NPS" = "National Park Service", 
                                             "USGS MTBS" = "USGS_MTBS_perimeters_2002_2011", 
                                             "Army Nat Res" = "Oahu Army Natural Resource Program", 
                                             "UH NREM" = "UH_NREM_Fire_Satellite", "0"), 
                                 selected = c("Hawaii Wildfire Management Organization", 
                                              "National Park Service", "USGS_MTBS_perimeters_2002_2011", 
                                              "Oahu Army Natural Resource Program", "UH_NREM_Fire_Satellite", "0"),
                                 inline = TRUE)),
              box(leafletOutput("mymap")
              ),
              downloadButton('downloadGIS', 'Download Selected Map Features')
              ),
      tabItem(tabName = "form", 
              tabsetPanel(type = "tabs", 
                          tabPanel("Part 1", 
                                   textInput("name", "Name", ""),
                                   checkboxInput("contained", "Is the fire contained?", FALSE),
                                   sliderInput("acres", "Size of fire (acres)", 0, 25, 0, ticks = FALSE)
                                   ),
                          tabPanel("Part 2",
                                   dateInput("date", "Date:"),
                                   textInput("i_num", "Incident Number:", ""),
                                   textInput("i_name", "Incident Name:", ""),
                                   textInput("i_kind", "Incident Kind:", ""),
                                   dateInput("start_date", "Start Date and Time:"),
                                   textInput("cause", "Cause", ""),
                                   textInput("i_commander", "Incident Commander:", ""),
                                   textInput("imt_type", "IMT Type:", ""),
                                   textInput("state_unit", "State-Unit:", "Hawaii"),
                                   selectInput("county", "County:", choices=c("Honolulu", "Hawaii", "Kalawao", "Kauai", "Maui"))
                                   ),
                          tabPanel("Part 3",
                                   textInput("latlong", "Latitude and Longitude and Ownership:", ""),
                                   textInput("location", "Short location Description (in reference to nearest town:", ""),
                                   textInput("size", "Size/Area Involved:", ""),
                                   sliderInput("percentcontained", "% Contained or MMA", 0, 100, 0),
                                   dateInput("expectedcontainment", "Expected Containment Date and Time:", value = Sys.Date() + 1),
                                   textInput("line", "Line to Build:", ""),
                                   textInput("costs", "Costs to Date:", ""),
                                   dateInput("controlled", "Declared Controlled Date and Time:", value = Sys.Date() + 1),
                                   textInput("injuries", "Injuries this Reporting Period:", ""),
                                   textInput("injuriestodate", "Injuries to Date:", ""),
                                   textInput("fatalities", "Fatalities:", ""),
                                   textInput("structure", "Structure Information:", "")
                                   ),
                          tabPanel("Part 4",
                                   textInput("threats", "Threat to Human Life/Safety:", ""),
                                   textInput("communities_threatened", "Communities/Critical Infrastructure Threatend:", ""),
                                   textInput("needs", "Critical Resources Needs:", ""),
                                   textInput("problems", "Major Problems and Concerns:", ""),
                                   textInput("resources_threatened", "Resources Threatened:", ""),
                                   textInput("weather", "Current Weather Conditions:", ""),
                                   textInput("benefits", "Resource Benefits/Objectives:", ""),
                                   textInput("fuels", "Fuels/Materials Involved:", ""),
                                   textInput("behavior", "Today's Observed Fire Behavior:", ""),
                                   textInput("sig_events", "Significant Events Today:", ""),
                                   dateInput("est_control", "Estimated Control Date and Time:", value = Sys.Date() + 1),
                                   textInput("proj_final_size", "Projected Final Size:", "")
                                   ),
                          tabPanel("Part 5", 
                                   textInput("etd_final_cost", "Estimated Final Cost:", ""),
                                   textInput("forecast_weather", "Tomorrow's Forecasted Weather:", ""),
                                   textInput("next_actions", "Actions Planned for Next Operational Period:", ""),
                                   textInput("proj_spread", "Projected Incident Movement/Spread During Next Operational Period:", ""),
                                   textInput("resistance", "Resistances to Fire Control:", ""),
                                   textInput("likelihood", "Likelihood Containment Goals Will Be Met:", ""),
                                   dateInput("demob", "Projected Demobilization Start Date:", value = Sys.Date() + 1),
                                   textInput("remarks", "Remarks:", ""),
                                   textInput("commit_resources", "Committed Resources:", ""),
                                   textInput("other_agencies", "Cooperating and Assisting Agencies Not Listed Above:", ""),
                                   textInput("preparer", "Prepared By:", ""),
                                   checkboxInput("final", label = "Final entry for incident", value = FALSE),
                                   useShinyjs(),
                                   actionButton("submit", "Submit"),
                                   hidden(
                                     div(
                                       id = "thankyou_msg",
                                       h3("Thanks, your response was submitted successfully!"),
                                       actionLink("submit_another", "Submit another response")
                                     )
                                   )  
                                   ),
                          tabPanel("Upload File",
                                   fileInput(inputId = "file", 
                                             label = "Choose file to upload",
                                             accept = NULL)
                                   )),
              ),
      tabItem(tabName = "plot", tabsetPanel(type = "tabs",
                                            tabPanel("Data", DT::dataTableOutput("responses", width = 300),
                                                     downloadButton("downloadData", "Download Data")
                                                     ),
                                            tabPanel("Plot", plotOutput("plot")))),
      tabItem(tabName = "about", 
              mainPanel(
                h1("About this dashboard"),
                h4("Feature roadmap:"),
                h5("Mobile-friendly interface"),
                h5("Consolidating forms"),
                h5("Combining historic and current form data"),
                h5("Expanding dashboard and visualizations"),
                h5("Uploading KML features"),
                h5("User log-ins"),
                img(src = "clippy.gif")
              ))
)))






server <- function(input, output, session) {
  
  filteredData <- reactive({
    hawaii[hawaii$Sat_sz_ac >= input$range[1] & hawaii$Sat_sz_ac <= input$range[2] & hawaii$Source %in% input$agency
           & hawaii$Year == input$year,]
  })
  
  output$mymap <- renderLeaflet({
    leaflet(hawaii) %>%
      addPolygons(color = "#800000", weight = 1, smoothFactor = 0.5, opacity = .3,
                  highlight = highlightOptions(color='white',weight=1,
                                               bringToFront = TRUE), label= popup) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observe({    
    leafletProxy("mymap", data=filteredData()) %>%
      clearShapes() %>%
      addPolygons(color = "#800000", weight = 1, smoothFactor = 0.5, opacity = .3,
                  highlight = highlightOptions(color='white',weight=1,
                                               bringToFront = TRUE), label= popup)
  })
  
  observeEvent(input$file, {
    drive_upload(media = input$file$datapath,
                 name = input$file$name)
  })
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::show("thankyou_msg")
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    data2 <- loadData()
    data3 <- subset(data2, select=c("name", "acres", "date", "state_unit", "county"))
  })
  
  output$acresbox <- renderInfoBox({
    input$submit
    data2 <- loadData()
    data2$acres <- unlist(data2$acres)
    data2$acres <- as.numeric(data2$acres)
    infoBox("Total Acres Burned (from form)", value = sum(data2$acres, na.rm=TRUE), icon = icon("fire"), fill = TRUE, color = "yellow")
  })
  
  output$incidentsbox <- renderInfoBox({
    input$submit
    data2 <- loadData()
    infoBox("Total Incidents (from form)", value = nrow(data2), icon = icon("free-code-camp"))
  })
  
  output$usersbox <- renderInfoBox({
    input$submit
    data2 <- loadData()
    infoBox("Total Users", value = n_distinct(data2$name), icon = icon("wrench"), color = "red")
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data.csv", sep = "")
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )
  
  # download GIS data
  output$downloadGIS <- downloadHandler(
    filename = function(){
      paste("crown_shp", "zip", sep = ".")
    },
    content = function(fname) {
      print(fname)
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      writeOGR(filteredData(), tmpdir, "crown", overwrite_layer = TRUE, driver="ESRI Shapefile")
      #zip_file <- file.path(tmpdir, "crown_shp.zip")
      shp_files <- list.files(tmpdir,
                              "crown",
                              full.names = TRUE)
      zip::zipr(zipfile=fname, files=shp_files)
    },
    contentType = "application/zip"
  )
  
  
  output$plot <- renderPlot({
    input$submit
    df <- loadData()
    # Render a barplot
    ggplot(df, aes(x=contained, y=acres)) +
      labs(x = "Containment Status", y = "Acres") +
      geom_bar(stat="identity", size = 1.0,
               width = 0.6) +
      theme_classic()
    
  })
  
}





shinyApp(ui, server)

