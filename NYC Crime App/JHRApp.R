
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
datamodels <- readRDS(file = "datamodels.rds")
dataset <- readRDS(file = "nbdata.rds")

# load spatial map data
my_spdf <- readRDS("spdf.rds")



# create the page
ui <-  navbarPage("NYC Crime App", theme = shinytheme("flatly"),
                  
                  tabPanel("Crime Breakdown",
                           
                           sidebarPanel(width = 2,
                                        
                                        # select the year, month, time of day, and precinct
                                        selectInput(inputId = 'chosenYear', label = 'Choose Year', choices = as.factor(c(2015:2020)),
                                                    selected = c("2020")),
                                        #br(),
                                        selectInput(inputId = 'chosenMonth', 'Choose Month', choices = levels(dataset$Month)),
                                        #br(),
                                        selectInput(inputId = 'chosenTime', 'Choose Time of Day', choices = levels(dataset$Time)),
                                        h6('Morning: 5am-11am'),
                                        h6('Afternoon: 11am-5pm'),
                                        h6('Evening: 5pm-11pm'),
                                        h6('Late Night: 11pm-5am'),
                                        br(),
                                        selectInput(inputId = 'chosenPrecinct', 'Choose Precinct', choices = levels(dataset$Precinct)),
                                        strong(span("Note:", style = "color:blue")),
                                        p('Use the ', span("Information & Precinct Map", style = "color:blue"), 
                                          " tab to identify the relevant precinct based on address")
                                        
                                        
                           ), # sidebar panel end
                           
                           mainPanel(width = 10,
                                     
                                     # view prediction output
                                     box(title = c("Most Frequent  &  Top 10"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 5,
                                         
                                         # prediction output
                                         span(tableOutput('mostfrequent'), style="color:red")
                                         
                                     ),
                                     
                                     # current selection
                                     box(title = str_to_title("current selection"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 7,
                                         
                                         # view pqtable output
                                         tableOutput('pqtable')
                                         
                                     ),
                                     
                                     # view top 10 output
                                     box(status = "primary",
                                         solidHeader = TRUE,
                                         width = 5,
                                         
                                         # top 10 prob w/ crime
                                         tableOutput('mostfrequentt10')
                                         
                                     ),
                                     
                                     # view monthly plot
                                     box(title = "",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 7,
                                         
                                         # output
                                         plotOutput('mplot')
                                         
                                     )
                                     
                           ) # main panel end
                  ), # tabPanel end
                  
                  tabPanel("Information & Precinct Map",
                           
                           sidebarPanel( width = 4,
                                         
                                         # enter offense
                                         selectInput(inputId = "ChosenOffensePg2", 
                                                     label = "Select Offense Type to View",
                                                     choices = sort(unique(dataset$`Offense Description`)),
                                                     selected = "GRAND LARCENY"),
                                         pickerInput(inputId = "ChosenMonthPg2",
                                                     label = "select Month(s) of interest",
                                                     choices= month.abb,
                                                     selected =  month.abb,
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         pickerInput(inputId = "ChosenYearPg2",
                                                     label = "select Year(s) of interest",
                                                     choices=sort(as.character(unique(dataset$Year))),
                                                     selected =sort(as.character(unique(dataset$Year))),
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         pickerInput(inputId = "ChosenTimePg2",
                                                     label = "select Time(s) of interest",
                                                     choices=   sort(as.character(unique(dataset$Time))),
                                                     selected = sort(as.character(unique(dataset$Time))),
                                                     options = list(`actions-box` = TRUE), multiple = T),
                                         strong(span("Note:", style = "color:blue")),
                                         p("The map may take a few seconds to load when option is selected.")
                                         
                           ), # sidebar panel end
                           
                           mainPanel( width = 8,
                                      
                                      # view leaflet plot
                                      box(title = str_to_title("Precinct Map"),
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 8,
                                          p("If no data exists for filter conditions selected, the map will not render"),
                                          
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
  
  # set reactive output of prediction & probabilities
  output$pqtable <- function(){
    
    pqtable <- dataset[1,c(7,8,9,1)]
    pqtable[1,1] <- input$chosenYear
    pqtable[1,2] <- input$chosenMonth
    pqtable[1,3] <- input$chosenTime
    pqtable[1,4] <- input$chosenPrecinct
    
    pqtable %>% 
      knitr::kable("html") %>%
      kable_styling("striped")
    
  }
  
  # set reactive output of predictions 
  output$mostfrequent <- renderTable({
    
    if (!input$chosenMonth %in% c("Jan","Feb","Mar","Apr","May","Jun") &  input$chosenYear == "2020"){
      
      xname <- c("Note:")
      xvalue1 <- c("Data does not extend beyond June 30th, 2020.")
      error_df <- as.data.frame(xvalue1)
      names(error_df) <- xname
      
      print(error_df)
    }
    
    else{ # start of main else
      
      ydata <- dataset %>% filter(Year == input$chosenYear) %>% 
        filter(Month == input$chosenMonth) %>% 
        filter(Time == input$chosenTime) %>% 
        filter(Precinct == input$chosenPrecinct)
      
      displaydata <- ydata %>% group_by(`Offense Description`) %>%
        summarise(Frequency = n(),Percentage = Frequency/nrow(ydata)) %>% 
        arrange(desc(Percentage))
      
      print(displaydata[1,1])
      
    } # end of main else
    
  })
  
  # set reactive output of probabilities 
  output$mostfrequentt10 <- function(){
    
    if (!(input$chosenMonth %in% c("Jan","Feb","Mar","Apr","May","Jun")) &  input$chosenYear == "2020"){
      
      xname <- c(" ")
      xvalue1 <- c("Please select a different month.")
      error_df <- as.data.frame(xvalue1)
      names(error_df) <- xname
      
      head(error_df, n = 10) %>% 
        knitr::kable("html") %>%
        kable_styling("striped")
    }
    
    else{ # start of main else
      
      ydata <- dataset %>% filter(Year == input$chosenYear) %>% 
        filter(Month == input$chosenMonth) %>% 
        filter(Time == input$chosenTime) %>% 
        filter(Precinct == input$chosenPrecinct)
      
      displaydata <- ydata %>% group_by(`Offense Description`) %>%
        summarise(Frequency = n(),Percentage = Frequency/nrow(ydata)) %>% 
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
    
    ydata <- dataset %>% group_by(Year, Month, Time, Precinct)%>% 
      summarise(Total = n()) %>% 
      filter(Month == input$chosenMonth) %>% 
      filter(Time == input$chosenTime) %>% 
      filter(Precinct == input$chosenPrecinct)
    
    p <- ggplot(data = ydata, mapping = aes(x= Year, y = Total, fill = Month))+
      geom_col(fill = "navyblue", show.legend = TRUE)+
      geom_text(mapping = aes(x = Year, y = Total + 5, label = Total))+
      labs(title = paste0("Year-Over-Year Comparison", 
                          "     Time = ", 
                          input$chosenTime,
                          "     Precinct = ",
                          input$chosenPrecinct),
           caption = "Data: 1. NYPD Complaints YTD | 2. NYPD Complaints Historic | 3. NYPD Precincts GeoJSON")+
      theme_classic()+
      theme(plot.title = element_text(hjust = .5))
    
    print(p)
    
  })
  
  # leaflet precinct map
  output$cpleth <- renderLeaflet({
    
    datasetsub<-
      dataset %>%  
      filter(Time %in% input$ChosenTimePg2) %>% 
      filter(Month %in% input$ChosenMonthPg2) %>% 
      filter(Year %in% input$ChosenYearPg2) %>% 
      select(c(1,2)) %>% 
      table %>% as.data.frame() %>% 
      filter(Offense.Description%in%input$ChosenOffensePg2) %>% 
      mutate(Precinct = as.character(Precinct))
    
    
    my_spdf$Offensefreq<-datasetsub$Freq[match(my_spdf$precinct,datasetsub$Precinct)]
    
    colorpalette<- colorNumeric(palette = 'YlOrRd',domain = my_spdf@data$Offensefreq,na.color = 'transparent')
    leaflet(my_spdf) %>% 
      addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = .6, 
                  fillColor = ~colorpalette(Offensefreq), 
                  highlight = highlightOptions(weight = 5,
                                               fillOpacity = 0.4,
                                               bringToFront = TRUE),
                  label = paste0("Precinct: ", my_spdf$precinct,"\n",
                                 "Number of Incidents: ", my_spdf$Offensefreq))%>% 
      addPolylines(stroke = TRUE, weight = 1, color = "darkblue") %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLegend(pal = colorpalette,values =~Offensefreq, title = "Frequency of Offenses", position = "topleft")
    
  })
  
})

shinyApp(ui, server)



