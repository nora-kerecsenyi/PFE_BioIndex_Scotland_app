# Load libraries ----

library(fresh)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
library(leaflet)
library(DT)
library(htmltools)
library(htmlwidgets)
library(scales)
library(gapminder)
library(forcats)
library(ggplot2)
library(gridExtra)
library(fmsb)
library(plotly)
library(tidyr)
library(tidyverse)
library(rstatix)




# Create custom theme for the shiny app ----
mytheme <- create_theme(
  adminlte_color(purple = "#612271"),
  adminlte_sidebar(width = "500px",
                   dark_bg = "#612271",
                   dark_hover_bg = "#612271",
                   dark_color = "#612271"),
  adminlte_global(content_bg = "#EFE8F0",
                  box_bg = "EFE8F0", 
                  info_box_bg = "EFE8F0"))



# Load data ----

data_2011 <- read.csv("./Inputs/Scotland_allregions_2011.csv")
data_2019 <- read.csv("./Inputs/Scotland_allregions_2019.csv")

data_2011 <- data_2011 %>% dplyr::select(-"X")
data_2019 <- data_2019 %>% dplyr::select(-"X")

# add column for year to both data sets
data_2011$Year <- 2011
data_2019$Year <- 2019



# combine into one data frame
combined <- rbind(data_2011, data_2019)
combined$Year <- as.factor(combined$Year)


combined$Region[combined$Region  == 1] <- "Central Region"
combined$Region[combined$Region  == 2] <- "West Region"
combined$Region[combined$Region  == 3] <- "North Region"
combined$Region[combined$Region  == 4] <- "East Region"
combined$Region[combined$Region  == 5] <- "South Region"

combined$Region <- as.factor(combined$Region)

names(combined) <- c("SCOBJECTID", "OBJECTID", "Region", "Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
               "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
               "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
               "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
               "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover", 
               "Connectivity Indicator", "Condition Indicator", "Diversity Indicator", "Extent Indicator", 
               "Local Biodiversity Indicator", "Landscape Biodiversity Indicator", "Combined Biodiversity Index", "Year")


combined.long <- combined %>%
  select(-c(SCOBJECTID, OBJECTID)) %>%
  pivot_longer(-c(Year, Region), names_to = "variable", values_to = "value")

# run t-tests for Scotland
stat.test <- combined.long %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()


# * T-tests and summary stats ----
# run t-tests for regions
stat.test.Central <- combined.long %>%
  filter(Region == "Central Region" & 
           !(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator"))) %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  mutate(Region = "Central Region")


stat.test.West <- combined.long %>%
  filter(Region == "West Region" & 
           !(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator"))) %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  mutate(Region = "West Region")


stat.test.North <- combined.long %>%
  filter(Region == "North Region" & 
           !(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator"))) %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  mutate(Region = "North Region")


stat.test.East <- combined.long %>%
  filter(Region == "East Region" & 
           !(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator"))) %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  mutate(Region = "East Region")


stat.test.South <- combined.long %>%
  filter(Region == "South Region" & 
           !(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator"))) %>%
  group_by(variable) %>%
  t_test(value ~ Year) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  mutate(Region = "South Region")


stat.test.regions <- rbind(stat.test.Central, stat.test.West, stat.test.North, stat.test.East, stat.test.South)




# summary statistics
sum_stats_2011 <- combined %>%
  filter(Year == "2011") %>%
  get_summary_stats(-c(SCOBJECTID, OBJECTID), type = "mean_sd") %>%
  rename("n_2011" = "n", "mean_2011" = "mean", "sd_2011" = "sd")


sum_stats_2019 <- combined %>%
  filter(Year == "2019") %>%
  get_summary_stats(-c(SCOBJECTID, OBJECTID), type = "mean_sd") %>%
  rename("n_2019" = "n", "mean_2019" = "mean", "sd_2019" = "sd")


sum_stats <- left_join(sum_stats_2011, sum_stats_2019, by = "variable")



sum_stats_regions_2011 <- combined %>%
  filter(Year == "2011") %>%
  group_by(Region) %>%
  get_summary_stats(-c(SCOBJECTID, OBJECTID), type = "mean_sd") %>%
  rename("n_2011" = "n", "mean_2011" = "mean", "sd_2011" = "sd")


sum_stats_regions_2019 <- combined %>%
  filter(Year == "2019") %>%
  group_by(Region) %>%
  get_summary_stats(-c(SCOBJECTID, OBJECTID), type = "mean_sd") %>%
  rename("n_2019" = "n", "mean_2019" = "mean", "sd_2019" = "sd")

sum_stats_regions <- left_join(sum_stats_regions_2011, sum_stats_regions_2019, by = c("variable", "Region"))

sum_stats_landscape_regions <- sum_stats_regions %>%
  filter(variable %in% c("Landcover", "Woodland cover", "Woodland size diversity", "Landscape Biodiversity Indicator")) %>%
  mutate(p.adj = "NA", p.adj.signif = "NA") %>%
  mutate(change = ifelse(mean_2011 < mean_2019, "up","down"))



# join t-test and summary stats results
results.Scotland <- left_join(stat.test, sum_stats, by = c("variable")) %>%
  select("variable", "n_2011", "mean_2011", "sd_2011", "n_2019","mean_2019", "sd_2019", "p.adj", "p.adj.signif") %>%
  mutate(change = ifelse(mean_2011 < mean_2019 & p.adj < 0.05, "up", 
                         ifelse(mean_2011 > mean_2019 & p.adj < 0.05, "down", "no change")))


results.regions <- left_join(stat.test.regions, sum_stats_regions, by = c("variable", "Region")) %>%
  select("Region", "variable", "n_2011", "mean_2011", "sd_2011", "n_2019","mean_2019", "sd_2019", "p.adj", "p.adj.signif") %>%
  mutate(change = ifelse(mean_2011 < mean_2019 & p.adj < 0.05, "up", 
                         ifelse(mean_2011 > mean_2019 & p.adj < 0.05, "down", "no change"))) %>%
  rbind(sum_stats_landscape_regions) %>%
  mutate(difference = mean_2019 - mean_2011) %>%
  as.data.frame()


# * Shapefiles ----

# regions shapefile
FLS <- readOGR("./Inputs/regions.shp")

# re-name regions to match the polygons
FLS@data$Region_nam[FLS@data$Region_nam  == "Central_lowlands"] <- "Central Region"
FLS@data$Region_nam[FLS@data$Region_nam  == "West"]  <- "West Region"
FLS@data$Region_nam[FLS@data$Region_nam  == "North_Scotland"]  <- "North Region"
FLS@data$Region_nam[FLS@data$Region_nam  == "Moray, Aberdeenshire, and Tay"]  <- "East Region"
FLS@data$Region_nam[FLS@data$Region_nam  == "Dumfries_borders_Galloway"]  <- "South Region"


# change coordinate reference system to be compatible with leaflet mapping
FLS <- spTransform(FLS, CRS("+proj=longlat +datum=WGS84 +no_defs"))




#  Define UI ----

# *header ----

header <- dashboardHeader(title = strong("Biodiversity Index change over time - Scotland"),
                          tags$li(class = "dropdown",
                                  tags$style(".main-header {max-height: 70px}"),
                                  tags$style(".main-header .logo {height: 59px}"),
                                  tags$li(a(href = 'https://www.forestresearch.gov.uk/',
                                            img(src = 'frlogo.png'),
                                            style = "padding-top:0px; padding-bottom:0px;"),
                                          class = "dropdown")))


# *sidebar ----

sidebar <- dashboardSidebar(width = 200,
                            shinyjs::useShinyjs(),
                            tags$style(type = 'text/css', 
                                       HTML('.sidebar, section.sidebar .shiny-input-container, 
                                             .skin-purple .main-sidebar {font-size: 16px; font-weight: bold; color:#612271 ; 
                                             line-height: 20px; margin-right: 0px; background-color: white; margin-bottom: 12px;}')),
                            sidebarMenu(menuItem("Introduction", tabName = "intro"), 
                                        menuItem("Combined Biodiversity Index", tabName = "bioindex"),
                                        menuItem("Indicators", tabName = "indicators"),
                                        menuItem("Metrics", tabName = "metrics")),
                            
                            
                            tags$img(src = "tree_Scotland.png", height="90%", width="90%",
                                     style = "display: block; margin-left: auto; margin-right: auto; margin-top: 400px;"))


# *body ----

body <- dashboardBody(
  use_theme(mytheme),
  tabItems(
    
    # Tab 1
    tabItem(tabName = "intro",
            
            # Row 1
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         p("National forestry bodies have a statutory duty to take steps to further the conservation of biodiversity. To help evidence the impact of management and policies, we have co-developed an evidence based, transparent and repeatable approach for assessing the biodiversity potential of the NationalForest Estate with Forestry & Land Scotland and Forestry England, using primarily the public forestry bodies' forest inventory data."),
                         p("A number of metrics capturing different aspects of woodland biodiversity were generated and grouped into the following four biodiversity indicator types: i) diversity, ii) extent, iii) condition, and iv) connectivity (see Theoretical framework on the figure below."),
                         p("Using survey data on forest and habitat structure, these metrics are assigned to individual forest stands to enable reporting at regional and national scales. The resulting index can then be used to inform locally targeted action, national long-term monitoring and objective reporting."),
                         tags$img(src = "framework_Scotland.png"),
                         style = "font-size: 16px")))),
    
    
    
    # Tab 2
    tabItem(tabName = "bioindex",
            # Row 1, Column 1
            fluidRow(
              column(width = 4,
                     box(width = NULL,
                         height = '450px',
                         h4(strong("Combined biodiversity index")),
                         p("A single score resulting from the combination of the Local Biodiversity Index and the Landscape Biodiversity Index.
                           Each subcompartment receives a single score. Values can range from 0 to 1 (low biodiversity score - high score) for
                           the baseline year (2019). Results from data for other years or scenarios can result in scores above or below this 
                           0-1 range, as these are calculated relative to the baseline year to highlight the direction of change."),
                         checkboxGroupInput("BioIndex_year", 
                                            h4(strong("Select years to compare")), 
                                            choices = c("2011", "2019", "2020", "2021"),
                                            selected = c("2011", "2019"),
                                            inline = TRUE),
                         selectInput("Bioindex_region", 
                                     h4(strong("Select region for plotting")), 
                                     choices = unique(combined$Region), 
                                     selected = "Central Region")),
                     
                     box(width = NULL,
                         leafletOutput("bioindex_map"))),
              
              
              # Row 1, Column 3 
              column(width = 8,
                     box(width = NULL,
                         tabsetPanel(type = "tabs",
                                     tabPanel("Scotland",  DT::DTOutput("bioindex_table_Scotland"), 
                                              style = "overflow-x: scroll;"),
                                     tabPanel("Regions",  DT::DTOutput("bioindex_table_regions"), 
                                              style = "overflow-x: scroll;"))),
                         
                         box(width = NULL,    
                         tabsetPanel(type = "tabs",
                                     tabPanel("Indicator Violin plot",
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Scotland",  plotlyOutput("violin_bioindex_Scotland")),
                                                          tabPanel("Regions",  plotlyOutput("violin_bioindex_regions")))),
                                     tabPanel("Metrics radar plot",
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Scotland",  plotlyOutput("roseplot_indicators_Scotland")),
                                                          tabPanel("Regions",  plotlyOutput("roseplot_indicators_regions"))))))))),
                     
                     
            
    
    # Tab 3
    tabItem(tabName = "indicators",
            # Row 1, Column 1
            fluidRow(
              column(width = 4,
                     box(width = NULL,
                         height = '450px',
                         selectInput("select_indicator", 
                                     h4(strong("Select indicator group")), 
                                     choices = names(combined[,22:27]),
                                     selected = "Landscape Biodiversity Indicator"),
                         checkboxGroupInput("indicator_year", 
                                            h4(strong("Select years to compare")), 
                                            choices = c("2011", "2019", "2020", "2021"),
                                            selected = c("2011", "2019"),
                                            inline = TRUE),
                         selectInput("indicator_region", 
                                     h4(strong("Select region for plotting")), 
                                     choices = unique(combined$Region), 
                                     selected = "Central Region")),
                     
                     box(width = NULL,
                         leafletOutput("indicator_map"))),
              
              
              # Row 1, Column 3 
              column(width = 8,
                     box(width = NULL,
                         tabsetPanel(type = "tabs",
                                     tabPanel("Scotland",  DT::DTOutput("indicator_table_Scotland"), 
                                              style = "overflow-x: scroll;"),
                                     tabPanel("Regions",  DT::DTOutput("indicator_table_regions"), 
                                              style = "overflow-x: scroll;"))),
                     
                     box(width = NULL,    
                         tabsetPanel(type = "tabs",
                                     tabPanel("Indicator Violin plot",
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Scotland",  plotlyOutput("violin_indicator_Scotland")),
                                                          tabPanel("Regions",  plotlyOutput("violin_indicator_regions")))),
                                     tabPanel("Metrics radar plot",
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Scotland",  plotlyOutput("roseplot_metrics_Scotland")),
                                                          tabPanel("Regions",  plotlyOutput("roseplot_metrics_regions"))))))))),
    
    
    
    # Tab 4
    tabItem(tabName = "metrics")))





## app.R ##
ui <- dashboardPage(header, sidebar, body, skin = "purple")


# Define server logic ----

server <- function(input, output) {
  
  # *Bioindex map ----
  output$bioindex_map <- renderLeaflet({
  
    # bioindex differences
    bioindex_diff <- subset(results.regions, variable == "Combined Biodiversity Index")
    
    # combine spatial polygons and regional change data frames
    FLS_map <- FLS
    FLS_map@data <- data.frame(FLS@data, bioindex_diff[match(FLS@data[,"Region_nam"], bioindex_diff[,"Region"]),])
    
    
    
    # Create a palette function
    pal <- colorNumeric(palette = "RdYlGn",
                        domain = FLS_map$difference)
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -3 , lat = 57, zoom = 6) %>%
      addPolygons(data = FLS_map,
                  weight = 1,
                  color = "black",
                  fillOpacity = 0.6,
                  fillColor = ~pal(difference),
                  label = ~paste0(Region, ", change: ", difference),
                  highlight = highlightOptions(
                    weight = 3, 
                    color = "#612271",
                    fillColor = "#612271", 
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal, 
                values = FLS_map$difference,
                title = paste("Change in", "<br/>", "Combined", "<br/>",  "Biodiversity", "<br/>", "Index"),
                opacity = 0.6)
    
     })
  
  
  # *Table of stats for Scotland BioIndex ----
  output$bioindex_table_Scotland <- DT::renderDT({
    
    DT::datatable(results.Scotland %>%
                    rename("N 2011" = "n_2011", "N 2019" = "n_2019", 
                           "mean 2011" = "mean_2011", "mean 2019" = "mean_2019",
                           "standard deviation 2011" = "sd_2011", "standard deviation 2019" = "sd_2019",
                           " p value" = "p.adj", "significance" = "p.adj.signif") %>%
                    mutate_if(is.numeric, ~round(., 3)) %>%
                    filter(variable == "Combined Biodiversity Index") %>%
                    relocate(change, .after = variable) ,
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bftsp',
                    buttons = c('copy', 'csv', 'excel'))) %>%
      formatStyle('change', 
                  backgroundColor = styleEqual(unique(as.factor(results.Scotland$change)), 
                                               c('lightgreen', 'lightpink', 'lightgray')))
    
  
  })
  
  
  
  # *Table of stats for regional BioIndex ----
  output$bioindex_table_regions<- DT::renderDT({
    
    DT::datatable(results.regions %>%
                    rename("N 2011" = "n_2011", "N 2019" = "n_2019", 
                           "mean 2011" = "mean_2011", "mean 2019" = "mean_2019",
                           "standard deviation 2011" = "sd_2011", "standard deviation 2019" = "sd_2019",
                           " p value" = "p.adj", "significance" = "p.adj.signif") %>%
                    mutate_if(is.numeric, ~round(., 3)) %>%
                    filter(variable == "Combined Biodiversity Index") %>%
                    relocate(change, .after = variable) %>%
                    select(-c(difference)),
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bftsp',
                    buttons = c('copy', 'csv', 'excel'))) %>%
      formatStyle('change', 
                  backgroundColor = styleEqual(unique(as.factor(results.regions$change)), 
                                               c('lightgreen', 'lightpink', 'lightgray')))
  })
  
  # *Violin plot for Scotland BioIndex ----
  output$violin_bioindex_Scotland <- renderPlotly({

    
    fig <- combined %>%
      plot_ly(
        x = ~Year,
        y = ~`Combined Biodiversity Index`,
        split = ~Year,
        color = ~Year,
        colors = c( "#999999", "#008000"),
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)) 
    
    fig <- fig %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Combined Biodiversity Index",
          zeroline = F))
    
    fig
  })
  
  
  # *Violin plot for regions BioIndex ----
  output$violin_bioindex_regions <- renderPlotly({
    
    # Filter data based on user selection above
    region <- shiny::reactive ({
      req(input$Bioindex_region)
      combined %>% 
        filter(Region %in% input$Bioindex_region)
    })
    
    fig <- region() %>%
      plot_ly(
        x = ~Year,
        y = ~`Combined Biodiversity Index`,
        split = ~Year,
        color = ~Year,
        colors = c( "#999999", "#008000"),
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)) 
    
    fig <- fig %>%
      layout(
        title = as.character(input$indicator_region),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Combined Biodiversity Index",
                     zeroline = F))
    
    fig
    
  })
  
  
  
  
  # *Radar plot for Scotland indicators ----
  output$roseplot_indicators_Scotland <- renderPlotly({

    
    mean_2011 <- combined %>%
      filter(Year == 2011) %>%
      select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
               "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
               "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
               "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
               "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover")) %>%
      summarise(across(everything(), mean)) %>%
      mutate(Year = 2011)
    
    
    
    mean_2019 <- combined %>%
      filter(Year == 2019) %>%
      select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
               "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
               "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
               "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
               "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover")) %>%
      summarise(across(everything(), mean)) %>%
      mutate(Year = 2019)
    
    
    
    df <-  rbind(mean_2011, mean_2019)
    
    
    df <- df %>%
      select("Woodland cover", "Woodland size diversity", "Landcover",
             "Woodland connectivity", "Matrix permeability",
             "Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
             "Core area",
             "Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
             "Tree species diversity", "Vertical complexity", "Tree age diversity",
             "Year")
   
    
    labs <- c(names(df %>% select(-c(Year))), "Woodland cover")

    
    plot_ly(type = 'scatterpolar',
            fill = 'toself', 
            mode = 'lines+markers') %>%
      add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                      as.numeric(df %>% filter(Year == "2011") %>% select("Woodland cover"))),
                theta = labs,
                name = '2011',
                fillcolor = "rgba(153,153,153,0)",
                marker = list(color = "#999999"),
                line = list(color = "#999999", dash = 'dash')) %>%
      add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                      as.numeric(df %>% filter(Year == "2019") %>% select("Woodland cover"))),
                theta = labs,
                name = '2019',
                fillcolor = "rgba(0,128,0,0)",
                marker = list(color = "#008000"),
                line = list(color = "#008000"))

  })
  
  
  # *Radar plot for regional indicators ----
  output$roseplot_indicators_regions <- renderPlotly({
    
    
    mean_2011 <- combined %>%
      filter(Year == 2011) %>%
      select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
               "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
               "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
               "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
               "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover", "Region")) %>%
      group_by(Region) %>%
      summarise(across(everything(), mean)) %>%
      mutate(Year = 2011)
      
    
    
    mean_2019 <- combined %>%
      filter(Year == 2019) %>%
      select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
               "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
               "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
               "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
               "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover", "Region")) %>%
      group_by(Region) %>%
      summarise(across(everything(), mean)) %>%
      mutate(Year = 2019)
    
    
  
    df <-  rbind(mean_2011, mean_2019)
    
   
    df <- df %>%
      select("Woodland cover", "Woodland size diversity", "Landcover",
             "Woodland connectivity", "Matrix permeability",
             "Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
             "Core area",
             "Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
             "Tree species diversity", "Vertical complexity", "Tree age diversity",
             "Year", "Region")
    
    # Filter data based on user selection above
    region_df_2011 <- shiny::reactive ({
      req(input$Bioindex_region)
      df %>% 
        filter(Region %in% input$Bioindex_region) %>%
        filter(Year == "2011") %>%
        select(-c(Region, Year))
    })
    
    region_df_2019 <- shiny::reactive ({
      req(input$Bioindex_region)
      df %>% 
        filter(Region %in% input$Bioindex_region) %>%
        filter(Year == "2019") %>%
        select(-c(Region, Year))
    })
    
    
    
    labs <- c(names(region_df_2019()), "Woodland cover")
    
   
    
    plot_ly(type = 'scatterpolar',
                   fill = 'toself', 
                   mode = 'lines+markers') %>%
      add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Woodland cover"))),
                theta = labs,
                name = '2011',
                fillcolor = "rgba(153,153,153,0)",
                marker = list(color = "#999999"),
                line = list(color = "#999999", dash = 'dash')) %>%
      add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Woodland cover"))),
                theta = labs,
                name = '2019',
                fillcolor = "rgba(0,128,0,0)",
                marker = list(color = "#008000"),
                line = list(color = "#008000")) %>%
      layout(title = list(text = as.character(input$Bioindex_region), x = 0, y = 0.9))
   
    
  })  
  
  
  
  # *Indicators map----
  output$indicator_map <- renderLeaflet({
    
    # bioindex differences
    bioindex_diff <- subset(results.regions, variable == input$select_indicator)
    
    # combine spatial polygons and regional change data frames
    FLS_map <- FLS
    FLS_map@data <- data.frame(FLS@data, bioindex_diff[match(FLS@data[,"Region_nam"], bioindex_diff[,"Region"]),])
    
    
    
    # Create a palette function
    pal <- colorNumeric(palette = "RdYlGn",
                        domain = FLS_map$difference)
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -3 , lat = 57, zoom = 6) %>%
      addPolygons(data = FLS_map,
                  weight = 1,
                  color = "black",
                  fillOpacity = 0.6,
                  fillColor = ~pal(difference),
                  label = ~paste0(Region, ", change: ", round(difference, 3)),
                  highlight = highlightOptions(
                    weight = 3, 
                    color = "#612271",
                    fillColor = "#612271", 
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal, 
                values = FLS_map$difference,
                title = paste("Change in", "<br/>", "Indicator"),
                opacity = 0.6)
    
  })
  # *Table of stats for Scotland indicators ----
  output$indicator_table_Scotland <- DT::renderDT({
    
    DT::datatable(results.Scotland %>%
                    rename("N 2011" = "n_2011", "N 2019" = "n_2019", 
                           "mean 2011" = "mean_2011", "mean 2019" = "mean_2019",
                           "standard deviation 2011" = "sd_2011", "standard deviation 2019" = "sd_2019",
                           " p value" = "p.adj", "significance" = "p.adj.signif") %>%
                    mutate_if(is.numeric, ~round(., 3)) %>%
                    filter(variable %in% c("Connectivity Indicator", "Condition Indicator", "Diversity Indicator", "Extent Indicator", 
                                           "Local Biodiversity Indicator", "Landscape Biodiversity Indicator")) %>%
                    relocate(change, .after = variable) ,
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bftsp',
                    buttons = c('copy', 'csv', 'excel'))) %>%
      formatStyle('change', 
                  backgroundColor = styleEqual(unique(as.factor(results.Scotland$change)), 
                                               c('lightgreen', 'lightpink', 'lightgray')))
    
    
  })
  
  # *Table of stats for regional indicators ----
  output$indicator_table_regions<- DT::renderDT({
    
    DT::datatable(results.regions %>%
                    mutate(p.adj = as.numeric(p.adj)) %>%
                    rename("N 2011" = "n_2011", "N 2019" = "n_2019", 
                           "mean 2011" = "mean_2011", "mean 2019" = "mean_2019",
                           "standard deviation 2011" = "sd_2011", "standard deviation 2019" = "sd_2019",
                           " p value" = "p.adj", "significance" = "p.adj.signif") %>%
                    mutate_if(is.numeric, ~round(., 3)) %>%
                    filter(variable %in% c("Connectivity Indicator", "Condition Indicator", "Diversity Indicator", "Extent Indicator", 
                                           "Local Biodiversity Indicator", "Landscape Biodiversity Indicator")) %>%
                    relocate(change, .after = variable) %>%
                    select(-c(difference)),
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(
                    pageLength = 6,
                    searching = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bftsp',
                    buttons = c('copy', 'csv', 'excel'))) %>%
      formatStyle('change', 
                  backgroundColor = styleEqual(unique(as.factor(results.regions$change)), 
                                               c('lightgreen', 'lightpink', 'lightgray')))
  })
  # *Violin plot for Scotland indicators ----
    output$violin_indicator_Scotland <- renderPlotly({
      
      violin_indicator_Scotland <- shiny::reactive ({
        req(input$select_indicator)
        combined %>% 
          select(input$select_indicator, Year)
      })
      
      y_axis <- violin_indicator_Scotland()[[input$select_indicator]]
      
      fig <- violin_indicator_Scotland() %>%
        plot_ly(
          x = ~Year,
          y = ~y_axis,
          split = ~Year,
          color = ~Year,
          colors = c( "#999999", "#008000"),
          type = 'violin',
          box = list(visible = T),
          meanline = list(visible = T)) 
      
      fig <- fig %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = as.character(input$select_indicator),
                       zeroline = F))
      
      fig
      
    })
    
  # *Violin plot for regional indicators ----
    output$violin_indicator_regions <- renderPlotly({
      
      violin_indicator_region <- shiny::reactive ({
        req(input$select_indicator)
        req(input$indicator_region)
        combined %>% 
          filter(Region %in% input$indicator_region) %>%
          select(input$select_indicator, Year)  
          
      })
      
      y_axis <- violin_indicator_region()[[input$select_indicator]]
      
      fig <- violin_indicator_region() %>%
        plot_ly(
          x = ~Year,
          y = ~y_axis,
          split = ~Year,
          color = ~Year,
          colors = c( "#999999", "#008000"),
          type = 'violin',
          box = list(visible = T),
          meanline = list(visible = T)) 
      
      fig <- fig %>%
        layout(
          title = as.character(input$indicator_region),
          xaxis = list(title = "Year"),
          yaxis = list(title = as.character(input$select_indicator),
                       zeroline = F))
      
      fig
      
    })
    
    
  # *Radar plot for Scotland metrics ---- 
    output$roseplot_metrics_Scotland <- renderPlotly({
      
      
      mean_2011 <- combined %>%
        filter(Year == 2011) %>%
        select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
                 "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
                 "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
                 "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
                 "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover")) %>%
        summarise(across(everything(), mean)) %>%
        mutate(Year = 2011)
      
      
      
      mean_2019 <- combined %>%
        filter(Year == 2019) %>%
        select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
                 "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
                 "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
                 "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
                 "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover")) %>%
        summarise(across(everything(), mean)) %>%
        mutate(Year = 2019)
      
      
      
      df <-  rbind(mean_2011, mean_2019)
    
    
      if(input$select_indicator == "Landscape Biodiversity Indicator") {
        
        df <- df %>%
          select("Woodland cover", "Woodland size diversity", "Landcover",
                 "Year") 
        
        labs <- c(names(df %>% select(-c(Year))), "Woodland cover")
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                          as.numeric(df %>% filter(Year == "2011") %>% select("Woodland cover"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                          as.numeric(df %>% filter(Year == "2019") %>% select("Woodland cover"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000"))
        
        
        
        } else if(input$select_indicator == "Local Biodiversity Indicator") {
        
        df <- df %>%
          select("Woodland connectivity", "Matrix permeability",
                 "Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
                 "Core area",
                 "Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
                 "Tree species diversity", "Vertical complexity", "Tree age diversity",
                 "Year") 
        
        labs <- c(names(df %>% select(-c(Year))), "Woodland connectivity")
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                          as.numeric(df %>% filter(Year == "2011") %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                          as.numeric(df %>% filter(Year == "2019") %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000"))
        
        } else if (input$select_indicator == "Connectivity Indicator"){
          
          df <- df %>%
            select("Woodland connectivity", "Matrix permeability",
                   "Year") 
          
          labs <- c(names(df %>% select(-c(Year))), "Woodland connectivity")
          
          plot_ly(type = 'scatterpolar',
                  fill = 'toself', 
                  mode = 'lines+markers') %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2011") %>% select("Woodland connectivity"))),
                      theta = labs,
                      name = '2011',
                      fillcolor = "rgba(153,153,153,0)",
                      marker = list(color = "#999999"),
                      line = list(color = "#999999", dash = 'dash')) %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2019") %>% select("Woodland connectivity"))),
                      theta = labs,
                      name = '2019',
                      fillcolor = "rgba(0,128,0,0)",
                      marker = list(color = "#008000"),
                      line = list(color = "#008000"))
          
        } else if (input$select_indicator == "Condition Indicator"){
          
          df <- df %>%
            select("Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
                   "Year") 
          
          labs <- c(names(df %>% select(-c(Year))), "Niche condition")
          
          plot_ly(type = 'scatterpolar',
                  fill = 'toself', 
                  mode = 'lines+markers') %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2011") %>% select("Niche condition"))),
                      theta = labs,
                      name = '2011',
                      fillcolor = "rgba(153,153,153,0)",
                      marker = list(color = "#999999"),
                      line = list(color = "#999999", dash = 'dash')) %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2019") %>% select("Niche condition"))),
                      theta = labs,
                      name = '2019',
                      fillcolor = "rgba(0,128,0,0)",
                      marker = list(color = "#008000"),
                      line = list(color = "#008000"))
          
        } else if (input$select_indicator == "Extent Indicator"){
          
          df <- df %>%
            select("Core area",
                   "Year") 
          
          labs <- c(names(df %>% select(-c(Year))), "Core area")
          
          plot_ly(type = 'scatterpolar',
                  fill = 'toself', 
                  mode = 'lines+markers') %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2011") %>% select("Core area"))),
                      theta = labs,
                      name = '2011',
                      fillcolor = "rgba(153,153,153,0)",
                      marker = list(color = "#999999"),
                      line = list(color = "#999999", dash = 'dash')) %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2019") %>% select("Core area"))),
                      theta = labs,
                      name = '2019',
                      fillcolor = "rgba(0,128,0,0)",
                      marker = list(color = "#008000"),
                      line = list(color = "#008000"))
          
        } else if (input$select_indicator == "Diversity Indicator"){
          
          df <- df %>%
            select("Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
                   "Tree species diversity", "Vertical complexity", "Tree age diversity",
                   "Year") 
          
          labs <- c(names(df %>% select(-c(Year))), "Niche diversity")
          
          plot_ly(type = 'scatterpolar',
                  fill = 'toself', 
                  mode = 'lines+markers') %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2011") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2011") %>% select("Niche diversity"))),
                      theta = labs,
                      name = '2011',
                      fillcolor = "rgba(153,153,153,0)",
                      marker = list(color = "#999999"),
                      line = list(color = "#999999", dash = 'dash')) %>%
            add_trace(r = c(as.numeric(df %>% filter(Year == "2019") %>% select(-c(Year))), 
                            as.numeric(df %>% filter(Year == "2019") %>% select("Niche diversity"))),
                      theta = labs,
                      name = '2019',
                      fillcolor = "rgba(0,128,0,0)",
                      marker = list(color = "#008000"),
                      line = list(color = "#008000"))}
      
     
      
    })
  # *Radar plot for regional metrics ----
    output$roseplot_metrics_regions <- renderPlotly({
      
      
      mean_2011 <- combined %>%
        filter(Year == 2011) %>%
        select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
                 "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
                 "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
                 "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
                 "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover", "Region")) %>%
        group_by(Region) %>%
        summarise(across(everything(), mean)) %>%
        mutate(Year = 2011)
      
      
      
      mean_2019 <- combined %>%
        filter(Year == 2019) %>%
        select(c("Native woodland cover", "Core area", "Oldest tree", "Woodland connectivity", 
                 "Matrix permeability", "Subcompartment type diversity", "Tree age diversity",
                 "Tree species diversity", "Tree size (dbh) diversity", "Vertical complexity",
                 "Open space cover", "Topographic roughness", "Deadwood volume", "Niche condition",
                 "Niche diversity", "Landcover", "Woodland size diversity", "Woodland cover", "Region")) %>%
        group_by(Region) %>%
        summarise(across(everything(), mean)) %>%
        mutate(Year = 2019)
      
      
      
      df <-  rbind(mean_2011, mean_2019)
      
      
      if(input$select_indicator == "Landscape Biodiversity Indicator") {
        
        df <- df %>%
          select("Woodland cover", "Woodland size diversity", "Landcover",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Woodland cover")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Woodland cover"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Woodland cover"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))
        
        
        
      } else if(input$select_indicator == "Local Biodiversity Indicator") {
        
        df <- df %>%
          select("Woodland connectivity", "Matrix permeability",
                 "Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
                 "Core area",
                 "Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
                 "Tree species diversity", "Vertical complexity", "Tree age diversity",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Woodland connectivity")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))
        
        
      } else if (input$select_indicator == "Connectivity Indicator"){
        
        df <- df %>%
          select("Woodland connectivity", "Matrix permeability",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Woodland connectivity")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Woodland connectivity"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))
        
        
      } else if (input$select_indicator == "Condition Indicator"){
        
        df <- df %>%
          select("Niche condition", "Deadwood volume", "Native woodland cover", "Open space cover", "Oldest tree",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Niche condition")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Niche condition"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Niche condition"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))
        
        
      } else if (input$select_indicator == "Extent Indicator"){
        
        df <- df %>%
          select("Core area",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Core area")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Extent Indicator"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Extent Indicator"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))
        
        
      } else if (input$select_indicator == "Diversity Indicator"){
        
        df <- df %>%
          select("Niche diversity", "Subcompartment type diversity", "Topographic roughness", "Tree size (dbh) diversity", 
                 "Tree species diversity", "Vertical complexity", "Tree age diversity",
                 "Year", "Region") 
        
        labs <- c(names(df %>% select(-c(Year, Region))), "Niche diversity")
        
        # Filter data based on user selection above
        region_df_2011 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2011") %>%
            select(-c(Region, Year))
        })
        
        region_df_2019 <- shiny::reactive ({
          req(input$indicator_region)
          df %>% 
            filter(Region %in% input$indicator_region) %>%
            filter(Year == "2019") %>%
            select(-c(Region, Year))
        })
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself', 
                mode = 'lines+markers') %>%
          add_trace(r = c(as.numeric(region_df_2011()), as.numeric(region_df_2011() %>% select("Niche diversity"))),
                    theta = labs,
                    name = '2011',
                    fillcolor = "rgba(153,153,153,0)",
                    marker = list(color = "#999999"),
                    line = list(color = "#999999", dash = 'dash')) %>%
          add_trace(r = c(as.numeric(region_df_2019()), as.numeric(region_df_2019() %>% select("Niche diversity"))),
                    theta = labs,
                    name = '2019',
                    fillcolor = "rgba(0,128,0,0)",
                    marker = list(color = "#008000"),
                    line = list(color = "#008000")) %>%
          layout(title = list(text = as.character(input$indicator_region), x = 0, y = 0.9))}
      
      
    })  
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

