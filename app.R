
rm(list = ls())

### INITIALISE -----------------------------------------------------------------


# App version
app_v <- "0013 (24.11.2020)"

# Import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(leaflet)
library(rgdal)
library(ggiraph)
library(DT)
library(reactable)
library(stringr)
library(tippy)
library(shinyBS)
library(crosstalk)
library(htmlwidgets)

#wd <- setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")


# css path
csspath <- "app_style.css"

## NOTE ##
# If the input data changes, change in this code the names of the locations 
# in global and server to correspond with the changed names!


### Load & wrangle data --------------------------------------------------------

# Data for plots and tables
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")
chg_dfs <- readRDS("data/chg_dfs.rds")

# Flood data
flood <- read.table("data/flood_coord_proj.txt", dec = ",", sep = "\t", 
                    header=TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
flood <- flood[,c(1,2,3,6,4,5,9,7,8,11,10)] 
flood[,c(3:9)] <- round(flood[,c(3:9)], 0)
names(flood) <- c("ID", "Nimi", "Alue", "Keskiarvo","Maksimi","Minimi", 
                  "Keskiarvo", "Maksimi","Minimi", "lat", "long")

# Create separate dataframes and append to list, use in Flood-tab to create table and map
# Selection depends on the table created when reprojecting he coordinates. 

# With name
flood_1_nimi <- flood[,c(2,4:6)]
flood_2_nimi <- flood[,c(2,7:9)]
# 2010-39
flood_1 <- flood[,c(2,4:6, 10,11)]
# 2040-69
flood_2 <- flood[,c(2,7:9,10,11)]

# Append to list
flood_list <- list(flood_1_nimi = flood_1_nimi, flood_2_nimi = flood_2_nimi, 
                   flood_1 = flood_1, flood_2 = flood_2)


#### ---------------------------------------------------------------------------

# Parameters
locations <- c("Vuoksi", "Kymijoki", "Naarajärvi, Saarijärven reitti", "Konnevesi","Vantaanjoki",
               "Aurajoki","Kokemäenjoki, Pori","Valkeakoski, Mallasvesi",
               "Loimijoki","Lapväärtinjoki", "Laihianjoki",
               "Kyrönjoki", "Lapuanjoki","Perhonjoki",
               "Kalajoki", "Pyhäjoki", "Siikajoki","Oulujoki, Merikoski","Niemelänjärvi", "Iijoki", "Simojoki",
               "Kemijoki, Isohaara","Ounasjoki, Hossa", "Kitinen", "Tornionjoki, Tornio","Teno", "Paatsjoki, Kaitakoski") %>%
  sort()

timeframe_names <- c("2010-2039", "2040-2069") # 1, 2
scenario_names <- c("Average of several scenarios", "Warm and wet", "Cold") # 1, 2, 3
floodmap_names <- c("Average (%)", "Maximum (%)", "Minimum (%)")

#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output, session){
  

  
### First tab with discharges  ------------
  
  # Should come up with some other solution than "title" as it is not supported with mobile phones etc.
  # Should use for example tippy (--> had a problem in changing the background color, see the .css)
  with_tooltip <- function(value, tooltip) {
    tags$abbr(style = "cursor: help",
              title = tooltip, value)
  }
  
  # Table with changes in mean made with reactable https://glin.github.io/reactable/ v. 0.2.3
  output$table1 <- renderReactable({
    thisName <- paste(input$location, input$timeframe,
                      input$scenario, "%", sep = "_")
    
    chg_dfs[[thisName]] <- chg_dfs[[thisName]][, c("Virtaama_ref", "Virtaama_ilm", "Muutos")]
    reactable(chg_dfs[[thisName]],
              pagination = FALSE,
              highlight = FALSE,
              sortable = FALSE,
              
              columns = list(
                Virtaama_ref = colDef(
                  minWidth = 110,
                  name = "Flow (\ u33a5 / s) reference period",
                  header = with_tooltip("Flow (\ u33a5 / s) reference period "," Reference period flow value for the selected time period.")
                  
                ),
                
                Virtaama_ilm = colDef(
                  minWidth = 110,
                  header = with_tooltip("Flow (\ u33a5 / s) climate change "," Simulated flow value of the selected climate change scenario for the selected time period."),
                  style = function(value) {
                  list(fontWeight = "bold")
                    }),
                
                Muutos = colDef(
                  header = with_tooltip("Change "," Change compared to the reference period flow. Red indicates an increase in flow, blue a decrease."),
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 0) {
                      color <- "#b2182b"
                    } else {
                      color <- "#3275B8"
                    } 
                    list(color = color, fontWeight = "bold")
                  }) 

              ),        
    )
 
  }) 
  
  # Simple table for downloading CSV
  dataInput <- reactive({
    thisName <- paste(input$location, input$timeframe,
                      input$scenario, "%", sep = "_")
    chg_dfs[[thisName]] <- chg_dfs[[thisName]][, c("Virtaama_ref", "Virtaama_ilm", "Muutos")]
    # colnames(chg_dfs[[thisName]]) <- c("VirtaamaRef_1981-2010", "VirtaamaIlmastonmuutos", "Muutosprosentti")
  })
  
  # Download link for table
  output$taulukko1_lataus <- downloadHandler(

    filename = function() {
      times <- c("1" = "2010-2039",
                 "2" = "2040-2069")
      scens <- c("1" = "Average Scenerio",
                 "2" = "Warm scenerio",
                 "3" = "Cold Scenerio")
      # Names without commas and spaces (could be done more smoothly too)
      locs <- c("Vuoksi" = "Vuoksi", "Kymijoki" = "Kymijoki","Naarajärvi, Saarijärven reitti" = "NaarajärviSaarijärvenReitti", 
                "Konnevesi" = "Konnevesi","Vantaanjoki" = "Vantaanjoki",
                "Aurajoki" = "Aurajoki", "Kokemäenjoki, Pori" = "KokemäenjokiPori","Valkeakoski, Mallasvesi" = "ValkeakoskiMallasvesi",
                "Loimijoki" = "Loimijoki","Lapväärtinjoki" = "Lapväärtinjoki", "Laihianjoki" = "Laihianjoki",
                "Kyrönjoki" = "Kyrönjoki", "Lapuanjoki" = "Lapuanjoki","Perhonjoki" = "Perhonjoki",
                "Kalajoki" = "Kalajoki", "Pyhäjoki" = "Pyhäjoki", "Siikajoki" = "Siikajoki", "Oulujoki, Merikoski" = "OulujokiMerikoski",
                "Niemelänjärvi" = "Niemelänjärvi", "Iijoki" = "Iijoki", "Simojoki" = "Simonjoki",
                "Kemijoki, Isohaara" = "KemijokiIsohaara", "Ounasjoki, Hossa" = "OunasjokiHossa", "Kitinen" = "Kitinen", 
                "Tornionjoki, Tornio" = "TornionjokiTornio","Teno" = "Teno","Paatsjoki, Kaitakoski" = "PaatsjokiKaitakoski")
      
      paste("Virtaama_",locs[input$location], "_1981-2010_", times[input$timeframe], "_",
            scens[input$scenario], ".csv",
            sep = "")
    },

    content = function(file) {
      write.csv(dataInput(), file, row.names = TRUE)
    }
  )
  
  
  # Plot
  output$plo <- renderggiraph({
    
    thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
    thisPlot <- scen_list[[thisName]]
    
    nameRef <- paste(input$location, "ref", sep ="_")
    thisRefPlot <- ref_list[[nameRef]]
    
    cols <- c("ref1" = "grey40",
              "ref2" = "grey75",
              "1" = "indianred2", 
              "2" = "tan1", 
              "3" = "turquoise3")
    
    scens <- c("1" = "Mean of several scenarios (RCP4.5 with emission scenario)",
               "2" = "Warm and wet (MIROC-ESM-CHEM global climate model with RCP4.5 emission scenario)",
               "3" = "Cold (CESM1-CAM5 global climate model with RCP2.6 emission scenario)")
    
    times <- c("1" = "2010-2039",
               "2" = "2040-2069")
    
    m_labels <- c("January "," Pearl "," March "," April "," May "," Summer "," July "," August "," September "," October "," November "," Christmas")
    m_breaks <- c("2020-01-01", "2020-02-01","2020-03-01","2020-04-01","2020-05-01",
                  "2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01",
                  "2020-11-01","2020-12-01")

    plo <- ggplot(
      data = thisRefPlot,aes(x = D_M, y = mean,  group = "group")) +
      labs(title= paste("Simulated flows, ", input$location,
                       "\nPeriod:", times[input$timeframe],
                      "\nScenario: ", scens[input$scenario]),
        y = expression(paste("Discharge (", m^3,"/s)", sep=""))) +
      
      # Control period ribbom + geom line in all of the plots
      geom_ribbon(aes(ymin=min, ymax=max, fill = "ref2"), 
                  colour = NA, alpha = 0.5) +
      geom_line(aes(y = mean, colour = "ref1"), size = 1.6, alpha = 0.8) +
      
      # Changes when input changes
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.6, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.4, alpha = 0.8) +
      
      
      
      # x-axis
      scale_x_date(expand = c(0,0),labels = m_labels, breaks = as.Date(m_breaks))+
      
      # colour & legend settings
 
      
      
      scale_fill_manual(name = " ", values = cols,
                       breaks = c("ref2"),
                      labels = c("1981-2010 range (max-min)")) +
     scale_colour_manual(name = " ", values = cols,
                        breaks = c("ref1", as.character(input$scenario)),
                       labels = c("1981-2010  Average",
                                 paste(times[input$timeframe], "mean and range", sep = " "))) +
      
      
      guides(colour = guide_legend(order = 1, reverse = T, nrow= 2,override.aes = list(linetype=c(1,1),
                                                       shape = c(16, 16))),
             fill = guide_legend(order = 2))+

      # Style settings
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=25, face = "bold"),
            axis.text.y = element_text(size=25),
            axis.title.y = element_text(size = 25),
            panel.background = element_blank(),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.justification ="left",
            legend.margin = margin(),
            legend.background = element_blank(),
            legend.text = element_text(size=25),
            legend.spacing.y = unit(0.2, "cm"),
            legend.box = "vertical",
            legend.box.just = 'left',
            legend.key.height = unit(1.2, "cm"),
            legend.key.size = unit(1, "cm"),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=25))
 
    
    # copy to global environment for saving
    plo_out <<-
      plo +
      theme(legend.text = element_text(size = 22),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
    
    # display plot
    ggiraph(code = print(plo),
            width_svg = 16.7,
            height_svg = 11.3)
    
  })
  
  # Download button for plot
  output$kuvaaja_lataus <- downloadHandler(
    filename = function() {
      # Create short names for plot output
      times <- c("1" = "2010-2039",
                 "2" = "2040-2069")
      
      scens <- c("1" = "Average scenario",
                "2" = "Warm Scenario",
               "3" = "Cold Scenario")
     # Names without commas and spaces (could be done more smoothly too)
      locs <- c("Vuoksi" = "Vuoksi", "Kymijoki" = "Kymijoki","Naarajärvi, Saarijärven reitti" = "NaarajärviSaarijärvenReitti", 
                "Konnevesi" = "Konnevesi","Vantaanjoki" = "Vantaanjoki",
                "Aurajoki" = "Aurajoki", "Kokemäenjoki, Pori" = "KokemäenjokiPori","Valkeakoski, Mallasvesi" = "ValkeakoskiMallasvesi",
                "Loimijoki" = "Loimijoki","Lapväärtinjoki" = "Lapväärtinjoki", "Laihianjoki" = "Laihianjoki",
                "Kyrönjoki" = "Kyrönjoki", "Lapuanjoki" = "Lapuanjoki","Perhonjoki" = "Perhonjoki",
                "Kalajoki" = "Kalajoki", "Pyhäjoki" = "Pyhäjoki", "Siikajoki" = "Siikajoki", "Oulujoki, Merikoski" = "OulujokiMerikoski",
                "Niemelänjärvi" = "Niemelänjärvi", "Iijoki" = "Iijoki", "Simojoki" = "Simonjoki",
                "Kemijoki, Isohaara" = "KemijokiIsohaara", "Ounasjoki, Hossa" = "OunasjokiHossa", "Kitinen" = "Kitinen", 
                "Tornionjoki, Tornio" = "TornionjokiTornio","Teno" = "Teno","Paatsjoki, Kaitakoski" = "PaatsjokiKaitakoski")
      
      paste("Flow Graph_",locs[input$location], "_1981-2010_", times[input$timeframe], "_", 
            scens[input$scenario], ".png",
            sep = "")
    },
    
    content = function(file) {
      ggsave(plot = plo_out, file, height = 10, width = 16, dpi = 150)
    }
  )
  
  
  # Map for page 1: locations
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      addCircleMarkers(data = flood_1, lng = ~lat, lat = ~long,
                       weight = 1,
                       radius = 5,
                       color = "#3275B8",
                       fillOpacity = 0.7,
                       stroke = FALSE,
                       label = ~htmlEscape(Nimi),
                       labelOptions = labelOptions(textsize = "14px"),
                       layerId = locations) 

    
  })
  
  # Highlight the input location on map
  observe({
    thisPoint <- subset(flood_1, flood_1$Nimi == input$location)
    
    leafletProxy(mapId = "map1") %>%
      clearGroup("highlighted_point") %>%
      addCircleMarkers(data = thisPoint, lng=~lat, lat=~long,
                       color = "#275A90", group = "highlighted_point",
                       label = ~htmlEscape(Nimi),
                       labelOptions = labelOptions(textsize = "14px")
      )
  })
  # Update the location selectInput on map click (https://www.r-bloggers.com/2016/03/r-shiny-leaflet-using-observers/)
  observeEvent(input$map1_marker_click, { 
    p <- input$map1_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  
  
### Second tab with floods  ---------------------
  
  
  # Flood table made with reactable https://glin.github.io/reactable/ v. 0.2.3
  # To do: change colours to just positive/negative like in the map
  output$table2 <- renderReactable({
    tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
    
    reactable(SharedData$new(flood_list[[tableData]], group ="floods"),
              height = 600,
              pagination = FALSE,
              highlight = TRUE,
              defaultSortOrder = "desc",
              
              columns = list(
                Nimi = colDef(
                  name = "Waters",
                  header = with_tooltip("Water body "," Location of the modeled flow point. Press to sort alphabetically.")
                ),
                
                Keskiarvo = colDef(
                  name = "Average",
                  header = with_tooltip("Average", "25 Average change in scenarios. Press to sort."),
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 0) {
                      color <- "#b2182b"
                    } else if (value < 0) {
                      color <- "#3275B8"
                    } 
                  list(color = color, fontWeight = "bold")
                  
                }),
                Maksimi = colDef(
                  name = "Maximum",
                  header = with_tooltip("Maximum", "The biggest change in 25 scenarios. Press to sort."),
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 0) {
                      color <- "#b2182b"
                    } else if (value < 0) {
                      color <- "#3275B8"
                    } 
                  list(color = color)
                }) ,
                
                Minimi = colDef(
                  name = "Minimum",
                  header = with_tooltip("Minimum", "The smallest change in 25 scenarios. Press to sort."),
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 0) {
                      color <- "#b2182b"
                    } else if (value < 0) {
                      color <- "#3275B8"
                    } 
                  list(color = color)
                })
              ),
              )
  })
  
  
  # Simple table for downloading CSV, tab 2
  dataInput_flood <- reactive({
    tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
    flood_list[[tableData]]
  })
  
  # Download link for table
  output$taulukko2_lataus <- downloadHandler(
    filename = function() {
      times <- c("1" = "2010-2039",
                 "2" = "2040-2069")
      
      paste("Floods_change% _25 scenarios_1981-2010_", times[input$timeframe], ".csv",
            sep = "")
    },
    
    content = function(file) {
      write.csv(dataInput_flood(), file, row.names = FALSE)
    }
  )
  
  # Markermap displaying 3 columns, % change in 100-y flood, input changes with user selection
  # To do: how to change the input data without redrawing the map. Not working all the time. Something with leafletProxy...
  output$map2 <- renderLeaflet({
    
    mapData <- reactive({
      paste("flood", input$timeframe2, sep="_")
    })
    

    
    # Define colours and bins for colour palette
    bins <- c(50, 0, -50)
    cols <- c("#3275B8", "#b2182b")
    pal <- colorBin(cols, bins = bins, pretty = FALSE)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      addCircleMarkers(data = flood_list[[mapData()]], lng = ~lat, lat = ~long,
                       weight = 10,
                       radius = ~sqrt(abs(Keskiarvo))*5,
                       stroke = FALSE,
                       fillOpacity = 0.4,
                       color = ~pal(Keskiarvo),
                       label = ~paste(Nimi, ", change: ", Keskiarvo, "%", sep =""),
                       labelOptions = labelOptions(textsize = "14px"),
                       group = "Average (%)") %>%
      addCircleMarkers(data = flood_list[[mapData()]], lng = ~lat, lat = ~long,
                       weight = 10,
                       radius = ~sqrt(abs(Maksimi))*5,
                       stroke = FALSE,
                       fillOpacity = 0.4,
                       color = ~pal(Maksimi),
                       label = ~paste(Nimi, ", change: ", Maksimi, "%", sep =""),
                       labelOptions = labelOptions(textsize = "14px"),
                       group = "Maximum (%)") %>%
      addCircleMarkers(data = flood_list[[mapData()]], lng = ~lat, lat = ~long,
                       weight = 10,
                       radius = ~sqrt(abs(Minimi))*5,
                       stroke = FALSE,
                       fillOpacity = 0.4,
                       color = ~pal(Minimi),
                       label = ~paste(Nimi, ", change: ", Minimi, "%", sep =""),
                       labelOptions = labelOptions(textsize = "14px", ),
                       group = "Minimum (%)") %>%
      # Radiobuttons for each column
      addLayersControl(
        baseGroups = c("Average (%)", "Maximum (%)", "Minimum (%)"),
        options = layersControlOptions(collapsed = F)) 
   
    
  })
  
  # Observe input timeframe and change the dataframe smoothly ( NOT WORKING )
  
   observeEvent(input$timeframe2, {
     mapData <- paste("flood", input$timeframe2, sep="_")
     
     bins <- c(50, 0, -50)
     cols <- c("#3275B8", "#b2182b")
     pal <- colorBin(cols, bins = bins, pretty = FALSE)
     
     leafletProxy("map2") %>%
       clearMarkers() %>%
       
       addCircleMarkers(data = flood_list[[mapData]], lng = ~lat, lat = ~long,
                        weight = 10,
                        radius = ~sqrt(abs(Keskiarvo))*5,
                        stroke = FALSE,
                        fillOpacity = 0.4,
                        color = ~pal(Keskiarvo),
                        label = ~paste(Nimi, ":", Keskiarvo, "%"),
                        labelOptions = labelOptions(textsize = "14px"),
                        group = "Average (%)") %>%
       addCircleMarkers(data = flood_list[[mapData]], lng = ~lat, lat = ~long,
                        weight = 10,
                        radius = ~sqrt(abs(Maksimi))*5,
                        stroke = FALSE,
                        fillOpacity = 0.4,
                        color = ~pal(Maksimi),
                        label = ~paste(Nimi, ":", Maksimi, "%"),
                        labelOptions = labelOptions(textsize = "14px"),
                        group = "Maximum (%)") %>%
       addCircleMarkers(data = flood_list[[mapData]], lng = ~lat, lat = ~long,
                        weight = 10,
                        radius = ~sqrt(abs(Minimi))*5,
                        stroke = FALSE,
                        fillOpacity = 0.4,
                        color = ~pal(Minimi),
                        label = ~paste(Nimi, ":", Minimi, "%"),
                        labelOptions = labelOptions(textsize = "14px"),
                        group = "Minimum (%)") %>%
       
       addLayersControl(
         baseGroups = c("Average (%)", "Maximum (%)", "Minimum (%)"),
         options = layersControlOptions(collapsed = F)
         
       )
   })
  
}




#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  
  
  useShinyjs(),
  # Style from css file
  theme = "app_style.css",
  
  # Set fonts and style
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css",
                      href="//fonts.googleapis.com/css?family=Raleway"),
            htmltools::includeCSS(csspath)),
  
  
  headerPanel(
    title=tags$a(href='https://www.carbonbrief.org/analysis-climate-papers-most-featured-in-media-2018', target="_blank"),
    windowTitle = "Impact of climate change on water bodies"),
  
  titlePanel(h3("The impact of climate change on water bodies visualization tool")),
  
  
  # First tab #########
  tabsetPanel(
    tabPanel("Climate change and flows", fluid = TRUE,
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 id = "sidebar",
                 
                 helpText("Visualize the effects of climate change on watercourses over different time periods and scenarios."),
                 
                 selectInput(inputId = "location",
                             label = HTML("Select a body of water"),
                             choices = locations,selected = ""),
                 bsTooltip("location", "Select the water body on which the results of the flow point are visualized.", "bottom"),
                 
                 radioButtons(
                            inputId = "timeframe",
                            label = "Select a date range",
                            choiceNames = timeframe_names,
                            choiceValues = seq(1:length(timeframe_names))),
                 bsTooltip("timeframe", "Choose one of the two future time periods.", "bottom"),
                 
                 radioButtons(inputId = "scenario",
                              label = "Select a scenario",
                              choiceNames = scenario_names,
                              choiceValues = seq(1:length(scenario_names))),
                 
                 bsTooltip("scenario", "Choose one of the three scenario options. More information about the scenarios can be found on the More Information tab.", "bottom"),
                 
                 # Download plot & table
                 br(),
                 strong("Download links"),
                 div(),
                 downloadLink("graph_load", label = "Download the graph (png)"),
                 bsTooltip("graph_load", "Download the graph on the screen to the workstation in png format.", "bottom"),
                 div(),
                 downloadLink("table1_load", label = "Download the spreadsheet (csv)"),
                 bsTooltip("table1_load", "Download the on-screen table to your workstation in csv format.", "bottom"),
                
                 div(),
                 br(),
                 HTML(paste("<p id='version-info' style='color: grey; font-size: small;'>Versio<br>", 
                            app_v, "</p>")),
               ),
               # Main panel
               mainPanel(
                 
                          fluidRow(
                            column(7,
                                   br(),
                                   # Graph
                                   ggiraphOutput("plo", 
                                                 width = "100%",
                                                 height = "100%"),
                                   # Tooltip over the plot
                                   bsPopover("plo", "Graph "," The graph shows the daily simulated Keskiarvo flows and the range (daily Maksimi and Minimi) for the reference period 1981-2010 and for the selected climate change scenario and period. More information about the scenarios can be found on the More Information tab. You can download the graph from the sidebar link.", 
                                             "right", trigger = "click"),
                                   
                                   # Table
                                   reactableOutput("table1", width = "100%"),
                                   # Tooltip over the table
                                   bsPopover("table1", "Table "," The table shows the Keskiarvo of the daily flows, the seasonal variation and the Keskiarvo and underflows. The columns show the simulated discharges for the reference period 1981-2010 and the selected climate change scenario and period in the selected water body, as well as the percentage change between them. You can download the table from the sidebar link.",
                                             "right", trigger = "click")),

                          
                            column(5,
                                 br(),
                                 p("Location of modeled flow points on the map"),
                                 # Map
                                 leafletOutput("map1", height = 750, width = "100%"))))
                 ,
             )
    ),
    
    # Second tab ############
    tabPanel("Climate change and floods", fluid = TRUE,
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 helpText("Visualize the effects of climate change on floods occurring once a hundred years (1 / 100a) in different periods."),
                
                 radioButtons(
                   inputId = "timeframe2",
                   label = "Select a date range",
                   choiceNames = timeframe_names,
                   choiceValues = seq(1:length(timeframe_names))),
                 bsTooltip("timeframe2", "Choose one of the two future time periods.", "bottom"),
                 
                 # Download table
                 strong("Download link"),
                 div(),
                 downloadLink("table2_load", label = "Download the spreadsheet (csv)"),
                 bsTooltip("table2_load", "Download the on-screen table to your workstation in csv format.", "bottom"),
                 br(),
                 
                 helpText("* the map is still under development. If changing the date range doesn't change the map variables, try reloading the page.")
               ),
               
               # Main panel
               mainPanel(
                 # fluidRow(
                 #   column(12,
                          fluidRow(
                          column(6,
                                 br(),
                                 strong("How much is an Keskiarvo flood every 100 years (1 / 100a) estimated to be affected by climate change?"),
                                 p(" "),
                                 p("The table and map have been used to estimate, using 25 different climate change scenarios, how much the 100-year open water flood will change over the selected period relative to the reference period (1981-2010). The mean multiplies the Keskiarvo change in 25 scenarios, with the Maksimi being the largest change in the scenarios and the Minimi being the smallest change. Note that there is a great deal of uncertainty in the estimates."),
                                 
                                 # Table
                                 reactableOutput("table2", width ="100%")),
                          
                          column(6,
                                 br(),
                                 strong("To visualize changes in floods, select a level from the map."),
                                 p(span(strong("Red", style = "color:#b2182b")), "color indicates the growth of floods and ",
                                 span(strong("Blue", style ="color:#3275B8")), "reduction."),
                                 
                                 # Map
                                 leafletOutput("map2", height=750, width = "100%")))),
                 # )),
             )
    ),
    
    # Third tab ##############
    tabPanel("More information",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 strong("On this Page:"),
                 em("background, more information, contacts and feedback."),
                 helpText("About the project: ", 
                          tags$a(href= "https://www.carbonbrief.org/analysis-climate-papers-most-featured-in-media-2018",
                                 "Rajesh Singh Kadyan", target="_blank")),
                 helpText("Give Feedback: ", 
                          tags$a(href="https://docs.google.com/document/d/16YuErfrdtHGcA77F8fnEqWmLyRyz0OtiZJEcfgoBIFI/edit?usp=sharing", target="_blank")),
                 
               ),
             # Add user guide as R Markdown document
             mainPanel(
               fluidRow(
                 column(8,
                        includeMarkdown('userguide/user_guide.rmd'))
                 ))
             )
    
    
  ))
))




### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)