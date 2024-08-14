library(shiny)
library(DT)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title####
  titlePanel("Running pace calculator  "),
  titlePanel("Use Distance/ Time to workout splits for sessions"),
  #h4(tags$a(href = "https://antoinesoetewey.com/", "Antoine Soetewey")),
  
  ### Sidebar  ####
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "conversion",
        label = "What conversion do you need?",
        choices = c("Distance & time --> pace" , "Pace & time --> distance", "Pace & distance --> time"),
        multiple = FALSE,
        selected = "Distance & time --> pace"
      ),
      
      ## TEXT
      conditionalPanel(
        condition = "input.conversion == 'Distance & time --> pace'",
        tags$div(
          tags$p("Type in the distance and time you plan to run to find your necessary pace and the splits.")
        )
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & time --> distance'",
        tags$div(
          tags$p("Type in the pace and time you plan to run to find the expected running distance.")
        )
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time'",
        tags$div(
          tags$p("Type in the pace and distance you plan to run to find the expected running time.")
        )
      ),
      hr(),
      ## END OF TEXT
      
      ## UNITS
      radioButtons(
        inputId = "units",
        label = "Units:",
        choices = c(
          "Km" = "km",
          "Miles" = "mile"
        )
      ),
      hr(),
      ## END OF UNITS
      
      ## DISTANCE####
      conditionalPanel(
        condition = "input.conversion == 'Distance & time --> pace' && input.units == 'km'",
        numericInput(
          "distance_km_conv1",
          "Distance (km):",
          5,
          min = 1,
          max = 50
        ),
        tags$em("A marathon is 42.195 km and a half-marathon is 21.0975 km."),
        
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Distance & time --> pace' && input.units == 'mile'",
        numericInput(
          "distance_mile_conv1",
          "Distance (miles):",
          3.2,
          min = 1,
          max = 1000
        ),
        tags$em("A marathon is 26.2188 miles and a half-marathon is 13.1094 miles."),
        
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time' && input.units == 'km'",
        numericInput(
          "distance_km_conv3",
          "Distance (km):",
          5,
          min = 1,
          max = 1000
        ),
        tags$em("A marathon is 42.195 km and a half-marathon is 21.0975 km."),
        
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time' && input.units == 'mile'",
        numericInput(
          "distance_mile_conv3",
          "Distance (miles):",
          3.2,
          min = 1,
          max = 1000
        ),
        tags$em("A marathon is 26.2188 miles and a half-marathon is 13.1094 miles."),
        hr()
      ),
      ## END OF DISTANCE
      
      ## PACE per distance####
      
      conditionalPanel(
        condition = "input.conversion == 'Pace & time --> distance' && input.units == 'km'",
        tags$b("Pace (minutes per km)"),
        numericInput(
          "minutes_km_conv2",
          "Minutes:",
          4,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_km_conv2",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & time --> distance' && input.units == 'mile'",
        tags$b("Pace (minutes per mile)"),
        numericInput(
          "minutes_mile_conv2",
          "Minutes:",
          6,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_mile_conv2",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time' && input.units == 'km'",
        tags$b("Pace (minutes per km)"),
        numericInput(
          "minutes_km_conv3",
          "Minutes:",
          3,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_km_conv3",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time' && input.units == 'mile'",
        tags$b("Pace (minutes per mile)"),
        numericInput(
          "minutes_mile_conv3",
          "Minutes:",
          6,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_mile_conv3",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      ## END OF PACE
      
      ## TIME####
      conditionalPanel(
        condition = "input.conversion == 'Distance & time --> pace'",
        tags$b("Running time"),
        numericInput(
          "hours_conv1",
          "Hours:",
          0,
          min = 0,
          max = 10
        ),
        numericInput(
          "minutes_conv1",
          "Minutes:",
          20,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_conv1",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      conditionalPanel(
        condition = "input.conversion == 'Pace & time --> distance'",
        tags$b("Running time"),
        numericInput(
          "hours_conv2",
          "Hours:",
          0,
          min = 0,
          max = 10
        ),
        numericInput(
          "minutes_conv2",
          "Minutes:",
          16,
          min = 0,
          max = 59
        ),
        numericInput(
          "seconds_conv2",
          "Seconds:",
          0,
          min = 0,
          max = 59
        ),
        hr()
      ),
      ## END OF TIME
      
      ## FOOTNOTE####
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/running-pace-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/running-pace-calculator">code</a>. Back to <a href="https://antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>'),h6(tags$a(href = "https://antoinesoetewey.com/", "most code taken from Antoine Soetewey's example"))
    ),
    
    
    
    # mainPanel####
    mainPanel(
      
      ##pace## using this section####
      
      #tabsetPanel(
      # tabPanel("PAce", 
      #tabPanel("Summary", verbatimTextOutput("summary")),
      #tabPanel("Table", tableOutput("table"))
      
      conditionalPanel(
        condition = "input.conversion == 'Distance & time --> pace'",
        tags$h3("Pace"),
        tags$h4("this table calulates splits for various distances"),
        tags$text(uiOutput("data_conv1")),
        tags$text(uiOutput("results33_conv1")),
        br(),
        tags$ul(
          #MIN/KM
          tags$li(tags$b(uiOutput("results_conv1"))),
          #KPH
          tags$li(tags$b(uiOutput("results2_conv1"))),
          
          #MIN/MILE
          tags$li(tags$b(uiOutput("results22_conv1"))),
          tags$li(tags$b(uiOutput("results33_conv1"))),
          hr(),
        ),
        br(),
        tags$h4("Splits per distance"),
        DT::dataTableOutput("tbl_conv1")
      ),
      ##distance#### 
      conditionalPanel(
        condition = "input.conversion == 'Pace & time --> distance'",
        tags$h4("Distance"),
        tags$text(uiOutput("data1_conv2")),
        br(),
        tags$ul(
          tags$li(tags$b(uiOutput("data2_conv2"))),
        )
      ),
      
      ##time####
      conditionalPanel(
        condition = "input.conversion == 'Pace & distance --> time'",
        tags$h4("Time"),
        tags$text(uiOutput("data1_conv3")),
        br(),
        tags$ul(
          tags$li(tags$b(uiOutput("data2_conv3"))),
        )
      )
    )
  ))


# Define server logic required to draw a histogram####
server <- function(input, output) {
  output$data_conv1 <- renderUI({
    # display results
    paste0("If you want to run ",
           ifelse(input$units == "km", paste0(input$distance_km_conv1, " km"), paste0(input$distance_mile_conv1, " miles")),
           " in ",
           ifelse(input$hours_conv1 < 10, paste0(0, input$hours_conv1), input$hours_conv1),
           ":",
           ifelse(input$minutes_conv1 < 10, paste0(0, input$minutes_conv1), input$minutes_conv1),
           ":",
           ifelse(input$seconds_conv1 < 10, paste0(0, input$seconds_conv1), input$seconds_conv1),
           ", you will need to average:")
  })
  
  output$data1_conv2 <- renderUI({
    # display data
    paste0("If you run at ",
           ifelse(input$units == "km",
                  paste0(input$minutes_km_conv2, ":", ifelse(input$seconds_km_conv2 < 10, paste0(0, input$seconds_km_conv2), input$seconds_km_conv2), "/km"),
                  paste0(input$minutes_mile_conv2, ":", ifelse(input$seconds_mile_conv2 < 10, paste0(0, input$seconds_mile_conv2), input$seconds_mile_conv2), "/mile")),
           " during ",
           ifelse(input$hours_conv2 < 10, paste0(0, input$hours_conv2), input$hours_conv2),
           ":",
           ifelse(input$minutes_conv2 < 10, paste0(0, input$minutes_conv2), input$minutes_conv2),
           ":",
           ifelse(input$seconds_conv2 < 10, paste0(0, input$seconds_conv2), input$seconds_conv2),
           ", you will run"
    )
  })
  
  output$data2_conv2 <- renderUI({
    # compute results
    total_time_seconds <- ifelse(input$units == "km",
                                 sum(input$minutes_km_conv2 * 60, input$seconds_km_conv2),
                                 sum(input$minutes_mile_conv2 * 60, input$seconds_mile_conv2))
    running_time_seconds <- sum(input$hours_conv2 * 60 * 60, input$minutes_conv2 * 60, input$seconds_conv2)
    total_distance <- running_time_seconds / total_time_seconds
    paste0(round(total_distance, 2), ifelse(input$units == "km", " km", " miles"))
  })
  
  output$data1_conv3 <- renderUI({
    # display data
    paste0("If your goal is to run ",
           ifelse(input$units == "km", paste0(input$distance_km_conv3, " km"), paste0(input$distance_mile_conv3, " miles")),
           " at ",
           ifelse(input$units == "km",
                  paste0(input$minutes_km_conv3, ":", ifelse(input$seconds_km_conv3 < 10, paste0(0, input$seconds_km_conv3), input$seconds_km_conv3), "/km"),
                  paste0(input$minutes_mile_conv3, ":", ifelse(input$seconds_mile_conv3 < 10, paste0(0, input$seconds_mile_conv3), input$seconds_mile_conv3), "/mile")),
           ", you will need to run during "
    )
  })
  
  output$data2_conv3 <- renderUI({
    # compute results
    total_time_seconds_conv3 <- ifelse(input$units == "km",
                                       sum(input$minutes_km_conv3 * 60, input$seconds_km_conv3),
                                       sum(input$minutes_mile_conv3 * 60, input$seconds_mile_conv3))
    seconds_conv3 <- ifelse(input$units == "km",
                            total_time_seconds_conv3 * input$distance_km_conv3,
                            total_time_seconds_conv3 * input$distance_mile_conv3)
    time_conv3 <- seconds_to_period(seconds_conv3)
    ifelse(day(time_conv3) == 0,
           sprintf('%02.0f:%02.0f:%02.0f', time_conv3@hour, minute(time_conv3), second(time_conv3)),
           sprintf('%02.0f %02.0f:%02.0f:%02.0f', day(time_conv3), time_conv3@hour, minute(time_conv3), second(time_conv3))
    )
  })
  
  output$results_conv1 <- renderUI({
    # compute pace
    #total time in sec
    time_total <- sum(c(input$hours_conv1 * 3600, input$minutes_conv1 * 60, input$seconds_conv1), na.rm = TRUE)
    #seconds per km or mile
    time <- time_total / ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1)
    #pace per km or mile
    pace <- paste0(time %/% 60, ":", sprintf("%02.0f", time %% 60))
    
    # display results with /km or /mile at end
    paste0(pace, ifelse(input$units == "km", "/km", "/mile"))
  })
  
  
  
  
  output$results2_conv1 <- renderUI({
    # compute pace
    time_total <- sum(c(input$hours_conv1 * 3600, input$minutes_conv1 * 60, input$seconds_conv1), na.rm = TRUE)
    time_h <- time_total / 3600
    pace <- ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1) / time_h
    
    # display results
    paste0(round(pace, 2), ifelse(input$units == "km", " km/hour", " miles/hour"))
  })
  
  output$results22_conv1 <- renderUI({
    # compute pace min/mile
    distance <- as.integer(c(100, 200, 300, 400, 500,600,800,1000,1200,1500,1600,1609,2000,3000,5000,10000))
    distance2 <- (c(.1, .2, .300, .400, .500, .6, .8, 1, 1.2, 1.5, 1.6, 1.609, 2, 3, 5, 10))
    
    
    #distance <- 1:ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1)
    time_total <- sum(c(input$hours_conv1 * 3600, input$minutes_conv1 * 60, input$seconds_conv1), na.rm = TRUE)
    time <- time_total / ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1) * distance2
    td <- seconds_to_period(time)
    Time_sec <- time
    
    dat <- data.frame(
      Distance = distance,
      Time = ifelse(
        day(td) > 0,
        sprintf("%02.0f %02.0f:%02.0f:%02.0f", day(td), td@hour, minute(td), second(td)),
        sprintf("%02.0f:%02.0f:%02.0f", td@hour, minute(td), second(td))
      ),
      Time_sec =sprintf(Time_sec, fmt = '%#.1f') 
    )
    
    time3 <- as.numeric(dat[12,3])
    
    pace3 <- paste0(time3 %/% 60, ":", sprintf("%02.0f", time3 %% 60))
    
    
    
    
    # display results
    paste0( pace3, ifelse(input$units == "km", " min/mile", " na"))
  })
  
  output$results33_conv1 <- renderUI({
    # compute pace min/mile
    
    minutes_conv1 <- 16
    seconds_conv1 <- 30
    hours_conv1 <- 0
    distance_km_conv1 <- 5000
    
    time1 <- sum(c(input$hours_conv1 / 24, (input$minutes_conv1 /24 / 60), (input$seconds_conv1/ 24/60/60)), na.rm = TRUE)
    
    secondss<- (input$seconds_conv1/ 24/60/60)
    minss <- (input$minutes_conv1 /24 / 60)
    hrss <- (input$hours_conv1 / 24)
    
    vo2max <- 0.8+0.1894393 * exp(-0.012778*time1*1440)+0.2989558* exp(-0.1932605*time1*1440)
    
    vdot <- (-4.6 + (0.182258 * (input$distance_km_conv1*1000 /time1/1440)) + 0.000104 *(input$distance_km_conv1*1000/time1/1440)^2)/vo2max
    
    vdot =sprintf(vdot, fmt = '%#.1f') 
    
    vo2max =sprintf(vo2max, fmt = '%#.2f') 
    
    
    # display results
    paste0( vo2max, "   Vo2max %   " , vdot, "   vdot   ")
  })
  
  
  
  
  
  # Data output
  output$tbl_conv1 <- DT::renderDataTable({
    distance <- as.integer(c(100, 200, 300, 400, 500,600,800,1000,1200,1500,1600,1609,2000,3000,5000,10000))
    distance2 <- (c(.1, .2, .300, .400, .500, .6, .8, 1, 1.2, 1.5, 1.6, 1.609, 2, 3, 5, 10))
    
    
    #distance <- 1:ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1)
    time_total <- sum(c(input$hours_conv1 * 3600, input$minutes_conv1 * 60, input$seconds_conv1), na.rm = TRUE)
    time <- time_total / ifelse(input$units == "km", input$distance_km_conv1, input$distance_mile_conv1) * distance2
    td <- seconds_to_period(time)
    Time_sec <- time
    
    dat <- data.frame(
      Distance = distance,
      Time = ifelse(
        day(td) > 0,
        sprintf("%02.0f %02.0f:%02.0f:%02.0f", day(td), td@hour, minute(td), second(td)),
        sprintf("%02.0f:%02.0f:%02.0f", td@hour, minute(td), second(td))
      ),
      Time_sec =sprintf(Time_sec, fmt = '%#.1f') 
    )
    
    DT::datatable(dat,
                  extensions = "Buttons",
                  options = list(
                    lengthChange = TRUE,
                    dom = "frtipB",
                    buttons = c("copy", "csv", "excel", "pdf", "print"),
                    lengthMenu = list(c(-1, 10, 20, 50, 100), c("All", "10", "20", "50", "100"))
                  ),
                  rownames = FALSE,
                  colnames = c(paste0("Distance ", ifelse(input$units == "km", "(m)", "(miles)")), "Time (hh:mm:ss)", "Time (seconds)")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

