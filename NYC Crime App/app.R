
## app.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(naivebayes)
library(rgdal)
library(leaflet)
library(tidygeocoder)
library(knitr)
library(kableExtra)
library(shinyWidgets)

#--------------------------------------------------------------------------------------------------------
##                                                UI COMPONENTS
#--------------------------------------------------------------------------------------------------------

# load .RData files from NB Machine Learning file
# datamodels <- readRDS(file = "datamodels.rds")
dataset <- readRDS(file = "nbdata.rds")

# load spatial map data
my_spdf <- readRDS("spdf.rds")



# create the page
ui <-  navbarPage("NYC Crime App", theme = shinytheme("flatly"),
                  
                  tabPanel("Crime Breakdown",
                           
                           sidebarPanel(width = 2,
                                        
                                        strong(span("Current Selection", style = "color:blue")),
                                        p(),
                                        # select the year, month, time of day, and precinct
                                        # selectInput(inputId = 'chosenYear', label = 'Choose Year', choices = as.factor(c(2016:2020)),
                                                    # selected = c("2020")),
                                        pickerInput(inputId = "chosenYear",
                                                    label = "Select Year(s)",
                                                    choices = c(2016:2020),
                                                    selected = c(2018:2020),
                                                    options = list(`actions-box` = TRUE), multiple = T),
                                        # br(),
                                        # selectInput(inputId = 'chosenMonth', 'Choose Month', choices = levels(dataset$Month)),
                                        pickerInput(inputId = "chosenMonth",
                                                    label = "Select Month(s)",
                                                    choices = month.abb,
                                                    selected =  month.abb[7:9],
                                                    options = list(`actions-box` = TRUE), multiple = T),
                                        # br(),
                                        # selectInput(inputId = 'chosenTime', 'Choose Time of Day', choices = levels(dataset$Time)),
                                        pickerInput(inputId = "chosenTime",
                                                    label = "Select Time(s)",
                                                    choices = levels(dataset$Time),
                                                    selected =  levels(dataset$Time)[2],
                                                    options = list(`actions-box` = TRUE), multiple = T),
                                        h6('Morning: 5am-11am'),
                                        h6('Afternoon: 11am-5pm'),
                                        h6('Evening: 5pm-11pm'),
                                        h6('Late Night: 11pm-5am'),
                                        #br(),
                                        # selectInput(inputId = 'chosenPrecinct', 'Choose Precinct', choices = levels(dataset$Precinct)),
                                        pickerInput(inputId = "chosenPrecinct",
                                                    label = "Select Precincts(s)",
                                                    choices = levels(dataset$Precinct),
                                                    selected =  levels(dataset$Precinct)[5],
                                                    options = list(`actions-box` = TRUE), multiple = T),
                                        strong(span("Note:", style = "color:blue")),
                                        p('Use the ', span("Information & Precinct Map", style = "color:blue"), 
                                          " tab to identify the relevant precinct based on address")
                                        
                                        
                           ), # sidebar panel end
                           
                           mainPanel(width = 10,
                                     
                                     # view prediction output
                                     box(title = c("Most Frequent & Top 10"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 4,
                                         
                                         # prediction output
                                         span(tableOutput('mostfrequent'), style="color:red")
                                         
                                     ),
                                     
                                     # current selection
                                     box(title = str_to_title("citywide summary: most frequent offense"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 8,
                                         
                                         # view cctable output
                                         tableOutput('cctable')
                                         
                                     ),
                                     
                                     # view top 10 output
                                     box(status = "primary",
                                         solidHeader = TRUE,
                                         width = 4,
                                         
                                         # top 10 prob w/ crime
                                         tableOutput('mostfrequentt10')
                                         
                                     ),
                                     
                                     # view monthly plot
                                     box(title = "",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 8,
                                         
                                         # output
                                         plotOutput('mplot')
                                         
                                     )
                                     
                           ) # main panel end
                  ), # tabPanel end
                  
                  tabPanel("Information & Precinct Map",
                           
                           sidebarPanel( width = 4,
                                         
                                         # enter offense
                                         pickerInput(inputId = "ChosenOffensePg2", 
                                                     label = "Select Offense Type(s) to View",
                                                     choices = sort(unique(dataset$`Offense Description`)),
                                                     selected = "GRAND LARCENY",
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         pickerInput(inputId = "ChosenMonthPg2",
                                                     label = "Select Month(s) of interest",
                                                     choices= month.abb,
                                                     selected =  month.abb,
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         pickerInput(inputId = "ChosenYearPg2",
                                                     label = "Select Year(s) of interest",
                                                     choices=sort(as.character(unique(dataset$Year))),
                                                     selected =sort(as.character(unique(dataset$Year))),
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         pickerInput(inputId = "ChosenTimePg2",
                                                     label = "Select Time(s) of interest",
                                                     choices=   sort(as.character(unique(dataset$Time))),
                                                     selected = sort(as.character(unique(dataset$Time))),
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         
                                         # add notes
                                         strong(span("Note:", style = "color:blue")),
                                         p("The map may take a few seconds to load when options are selected."),
                                         br(),
                                         
                                         # add options
                                         p(span("Additional Options: ", style = "color:blue"), "To pin a location, enter the address and check the box below"),
                                         
                                         # enter address to pin location on map
                                         textInput(inputId = "enteredAddress", label = "Enter Address",
                                                   placeholder = "Ex. 20 W 34th St, New York, NY 10001"),
                                         
                                         # check to proceed and pin the location
                                         checkboxInput(inputId = "addMarker", label = "Add Marker to Map",
                                                       value = FALSE)
                                        
                                         
                           ), # sidebar panel end
                           
                           mainPanel( width = 8,
                                      
                                      # view leaflet plot
                                      box(title = str_to_title("Precinct Map"),
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 8,
                                          
                                          # top 10 prob w/ crime
                                          leafletOutput('cpleth', width = 900, height = 550)
                                      )
                                      
                           ) # main panel end
                  ) # tabPanel end
                  
) # navbarpage end

#--------------------------------------------------------------------------------------------------------
##                                             SERVER COMPONENTS
#--------------------------------------------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  # information tabs
  output$info <- renderText({
    
    # empty
    
  })
  
  # view current selection
  output$cctable <- function(){
    
    # catch errors
    validate(
      
      need(length(input$chosenMonth) != 0, "A month is required for citywide comparison."),
      need(length(input$chosenTime) != 0, "A time of day is required for citywide comparison."),
      need(length(input$chosenYear) != 0, "A year is required for citywide comparison."),
      need(length(input$chosenPrecinct) != 0, "A precinct is required for citywide comparison.")
      
    )
    
    ydata <- dataset %>% filter(Year %in% input$chosenYear) %>% 
      filter(Month %in% input$chosenMonth) %>% 
      filter(Time %in% input$chosenTime) %>% 
      filter(Precinct %in% input$chosenPrecinct)
    
    displaydata <- ydata %>% group_by(`Offense Description`) %>%
      summarize(Frequency = n(),Percentage = Frequency/nrow(ydata)) %>% 
      arrange(desc(Percentage))
    
    offense_to_compare <- displaydata$`Offense Description`[1]
    
    displaydata2 <- dataset %>% filter(Year %in% input$chosenYear) %>% 
      filter(Month %in% input$chosenMonth) %>% 
      filter(Time %in% input$chosenTime) %>% 
      filter(`Offense Description` %in% offense_to_compare) %>%
      group_by(Precinct) %>% 
      summarize(Frequency = n(), Percentage = Frequency/nrow(ydata))
    
    summary_vec <- summary(displaydata2$Frequency)
    names(summary_vec) <- NULL
    
    cctable <- data.frame(rbind(c(1:6)))
    names(cctable) <- names(summary(displaydata2$Frequency))
    cctable[1,] <-  summary_vec
    
    cctable %>% 
      knitr::kable("html") %>%
      kable_styling("striped")
    
  }
  
  # set reactive output to identify most frequent
  output$mostfrequent <- renderTable({
    
    if (sum(input$chosenMonth %in% c("Oct", "Nov", "Dec")) == length(input$chosenMonth)  &  input$chosenYear == "2020"){

      xname <- c("Warning:")
      xvalue1 <- c("Data is complete through Sepetember 30th, 2020.")
      error_df <- as.data.frame(xvalue1)
      names(error_df) <- xname

      print(error_df)
    }

    else{ # start of main else
      
      # catch errors
      validate(

        need(length(input$chosenMonth) != 0, "A month is required to calculate frequency."),
        need(length(input$chosenTime) != 0, "A time of day is required to calculate frequency."),
        need(length(input$chosenYear) != 0, "A year is required to calculate frequency."),
        need(length(input$chosenPrecinct) != 0, "A precinct is required to calculate frequency.")

      )
      
      ydata <- dataset %>% filter(Year %in% input$chosenYear) %>% 
        filter(Month %in% input$chosenMonth) %>% 
        filter(Time %in% input$chosenTime) %>% 
        filter(Precinct %in% input$chosenPrecinct)
      
      displaydata <- ydata %>% group_by(`Offense Description`) %>%
        summarize(Frequency = n(),Percentage = Frequency/nrow(ydata)) %>% 
        arrange(desc(Percentage))
      
      print(displaydata[1,1])
      
    } # end of main else
    
  })
  
  # set reactive output of top 10 offenses
  output$mostfrequentt10 <- function(){
    
    if (sum(input$chosenMonth %in% c("Oct", "Nov", "Dec")) == length(input$chosenMonth)  &  input$chosenYear == "2020"){

      xname <- c(" ")
      xvalue1 <- c("Any selection that inclused data beyond 9/30/2020 will not contirbute to totals and percentages.")
      error_df <- as.data.frame(xvalue1)
      names(error_df) <- xname

      head(error_df, n = 10) %>%
        knitr::kable("html") %>%
        kable_styling("striped")
    }

    else{ # start of main else
      
      # catch errors
      validate(

        need(length(input$chosenMonth)!= 0, "Select month(s) of interest."),
        need(length(input$chosenTime) != 0, "Select time(s) of interest."),
        need(length(input$chosenYear) != 0, "Select year(s) of interest."),
        need(length(input$chosenPrecinct) != 0, "Select precinct(s) of interest.")

      )
      
      ydata <- dataset %>% filter(Year %in% input$chosenYear) %>% 
        filter(Month %in% input$chosenMonth) %>% 
        filter(Time %in% input$chosenTime) %>% 
        filter(Precinct %in% input$chosenPrecinct)
      
      displaydata <- ydata %>% group_by(`Offense Description`) %>%
        summarize(Frequency = n(),Percentage = Frequency/nrow(ydata)) %>% 
        arrange(desc(Percentage)) %>% 
        mutate(Percentage = round(Percentage*100, digits = 1)) %>% 
        mutate(Percentage = paste0(Percentage,"%"))
      
      head(displaydata, n = 10) %>% 
        knitr::kable("html") %>%
        kable_styling("striped")
      
    } # end of else statement
    
  }
  
  # monthly totals plot
  output$mplot <- renderPlot({
    
    # catch errors
    validate(
      
      need(length(input$chosenMonth) != 0, "A month is required to generate a plot."),
      need(length(input$chosenTime) != 0, "A time of day is required to generate a plot."),
      need(length(input$chosenYear) != 0, "A year is required to generate a plot."),
      need(length(input$chosenPrecinct) != 0, "A precinct is required to generate a plot.")
      
    )
    
    ydata <- dataset %>% 
      filter(Year %in% input$chosenYear) %>% 
      filter(Time %in% input$chosenTime) %>% 
      filter(Precinct %in% input$chosenPrecinct) %>%
      filter(Month %in% input$chosenMonth) %>% 
      group_by(Year, Month) %>% 
      summarize(Total = n())
    
    p <- ggplot(data = ydata, mapping = aes(x= Year, y = Total, color = Month))+
      geom_line(mapping = aes(group = Month), alpha = .5)+
      geom_text(mapping = aes(x = Year, y = Total + 5, label = Total))+
      ylab("Monthly Incident Total")+
      ylim(c(min(ydata$Total - 5), max(ydata$Total+5)))+
      labs(title = paste0("Year-Over-Year Comparison"), 
                          # "     Time: ", 
                          # input$chosenTime,
                          # "     Precinct: ",
                          # input$chosenPrecinct),
           caption = "Data: 1. NYPD Complaints YTD | 2. NYPD Complaints Historic")+
      theme_classic()+
      theme(plot.title = element_text(hjust = .5),
            panel.background = element_rect(fill = "white",
                                            color = "white"))
    
    print(p)
    
  })
  
  # leaflet precinct map
  output$cpleth <- renderLeaflet({
    
    # catch error thrown by domain issues in my_spdf$Offensefreq
    validate(
      
      need(!is.null(input$ChosenMonthPg2), "A month is required to generate a frequency map."),
      need(!is.null(input$ChosenTimePg2), "A time of day is required to generate a frequency map."),
      need(!is.null(input$ChosenYearPg2), "A year is required to generate a frequency map."),
      need(!is.null(input$ChosenOffensePg2), "An offense is required to generate a frequency map.")
      
    )
    
    # create custom dataset based on inputs
    datasetsub<-
      dataset %>%  
      filter(Time %in% input$ChosenTimePg2) %>% 
      filter(Month %in% input$ChosenMonthPg2) %>% 
      filter(Year %in% input$ChosenYearPg2) %>% 
      select(c(1,2)) %>% 
      table %>% as.data.frame() %>% 
      filter(Offense.Description %in% input$ChosenOffensePg2) %>%
      group_by(Precinct) %>% 
      summarize(Freq = sum(Freq)) %>% 
      mutate(Precinct = as.character(Precinct))
    
    # match freq values to precincts in spatial dataframe
    my_spdf$Offensefreq <- datasetsub$Freq[match(my_spdf$precinct,datasetsub$Precinct)]
    
    # set color palette according to custom dataset domain
    colorpalette <- colorNumeric(palette = 'YlOrRd',domain = my_spdf@data$Offensefreq, na.color = 'transparent')
    
    # catch error thrown by selection yielding no results
    validate(
      
      need(length(datasetsub) != 0, "No data exists for the current selection. Please update the selection to render a new map.")
      
    )
    
    # generate map given the 'pin location' check box is not selected
    if(!input$addMarker){
    
      # map construction excluding pinned location
      leaflet(my_spdf) %>% 
        addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = .6, 
                  fillColor = ~colorpalette(Offensefreq), 
                  highlight = highlightOptions(weight = 5,
                                               fillOpacity = 0.4,
                                               bringToFront = TRUE),
                  label = paste0("Precinct: ", my_spdf$precinct, "  |  ",
                                         "Incidents: ", my_spdf$Offensefreq)) %>%
        addPolylines(stroke = TRUE, weight = 1, color = "darkblue") %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addLegend(pal = colorpalette,values =~Offensefreq, title = "Offense Totals", position = "topleft")
    }

    # generate map given the check box is selected
    else{
      
      # find lat and lon of entered address
      georesult <- geo(input$enteredAddress)
      
      # catch error thrown by user entering incorrect address format
      validate(
        
        need(!is.na(georesult$lat), "Please enter a valid address")
        
      )

      # map construction including pinned location
      leaflet(my_spdf) %>%
        addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = .6,
                    fillColor = ~colorpalette(Offensefreq),
                    highlight = highlightOptions(weight = 5,
                                                 fillOpacity = 0.4,
                                                 bringToFront = TRUE),
                    label = paste0("Precinct: ", my_spdf$precinct, "  |  ",
                                   "Incidents: ", my_spdf$Offensefreq)) %>%
        addPolylines(stroke = TRUE, weight = 1, color = "darkblue") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addLegend(pal = colorpalette,values =~Offensefreq, title = "Offense Totals", position = "topleft") %>%
        addAwesomeMarkers(lat = georesult$lat, lng = georesult$long, label = georesult$address)

    }
    
  })
  
})

shinyApp(ui, server)



