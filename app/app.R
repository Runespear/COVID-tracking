# Packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(r2d3maps)
library(shiny)
library(shinyBS)
library(DT)
library(rsconnect)
library(shinyWidgets)
library(shinydisconnect)
library(shinycssloaders)
library(bootstraplib)
library(waiter)
library(sparkline)
library(htmlwidgets)
library(leaflet)
library(RColorBrewer)
library(sf)
library(tigris)
library(maps)
library(maptools)
library(sp)
library(rgeos)


# Bootstrap theme ---------------------------------------------------------


bs_theme_new(version = "4+3", bootswatch = NULL)

bs_theme_base_colors(bg = "white", fg = "black")
bs_theme_accent_colors(primary = "#BE1E2D", secondary = "#BE1E2D")
bs_theme_fonts(base = "Raleway")
bs_theme_add_variables("font-size-base" = "1.2rem")


# Data preprocessing ------------------------------------------------------


cty_sf_thre <- readRDS("cty_sf_thre.rds") %>% as.data.frame() %>% select(-geometry)

cty_sf_thre <- cty_sf_thre %>% mutate(names = str)

cty <- counties(cb = TRUE, resolution = "20m") %>% rename(fips = GEOID)

new_york <- c("36061", "36047", "36081", "36005", "36085")

nyc_counties <- cty %>% 
  st_drop_geometry() %>% 
  filter(STATEFP == "36") %>% 
  filter(fips %in% new_york) %>% 
  select(fips, NAME, STATEFP)

nyc_counties_combined <- cty %>% 
  semi_join(nyc_counties) %>% 
  summarize(census_area = sum(AWATER)) %>% 
  mutate(STATEFP = "36",
         NAME = "New York City", 
         fips = "99999")

cty <- cty %>% 
  anti_join(nyc_counties) %>% 
  bind_rows(nyc_counties_combined)

cty <- geo_join(cty, cty_sf_thre, by_sp = "fips", by_df = "fips")

new_counties <- read_csv("new_counties.csv") %>% clean_names()

pal <- colorFactor(
  palette = rev(brewer.pal(10, "YlOrRd")),
  domain = cty$predicted_double_days_f,
  na.color = rgb(0.8,0.8,0.8,0.5) #or use show()
)


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  tags$head(includeHTML(("google-analytics.html"))),
  
  use_waiter(),
  
  bootstrap(),
  
  titlePanel(windowTitle = "COVID-19 Outbreak Detection Tool", column(12,includeHTML("ma-header8.html"))),
  
  br(), br(),
  
  h2("COVID-19 Outbreak Detection Tool", align = "center", style = "color:#BE1E2D"), br(),
  tags$p("The COVID-19 Outbreak Detection Tool detects recent COVID-19 outbreaks 
  in U.S. counties. The tool leverages machine learning to predict how fast an outbreak could spread at the county level by estimating the 
                        doubling time of COVID-19 cases.
                        It accounts for reported COVID-19 cases and deaths, 
                        face mask mandates, social distancing policies, the CDC’s 
                        Social Vulnerability Index, changes in tests performed and rate of positive tests. 
                        The tool offers an interactive map and a data explorer allowing users to filter 
                        and rearrange counties based on predicted trends, which get 
         updated at least once per week.", style = "margin-left:10%; margin-right:10%; font-size:20px"), br(), 
  h6("Latest data:", max(cty_sf_thre$date_x, na.rm = T), align = "center"), br(),
  
  hr(style = "border-color:#BE1E2D"),
  
  h3("Interactive Map", align = "center", style = "color:#BE1E2D"), 
  
  hr(style = "border-color:#BE1E2D"),
  
  br(),
  
  
  fluidRow(
    column(12, leafletOutput("map", width = "1400px", height = "710px"))), br(),
           tags$p("Note: the counties colored in grey have less than 20 new cases 
                  (7-day moving average) in the past 22 days or a drop of more than 20 cumulative
                  cases in a single day during the past 30 days. Mobile, Alabama is also colored grey due to a data reporting issue. Results will be released shortly.", style = "margin-left:20%; margin-right:10%;"),
           
           br(),
           
  fluidRow(
    column(12,
           align = "center", 
           bsCollapse(id = "collapseExample", bsCollapsePanel("View new counties in 0 to 1 week category",
                                                              DT::dataTableOutput("new_counties")
           )
           ))),
           
           
           disconnectMessage(
             text = "Your session timed out, please reload the application.",
             refresh = "Reload now",
             background = "#3d3b3b",
             colour = "white",
             top = "center",
             refreshColour = "#BE1E2D"
           ),
           
           br(),
           br(),
           
           hr(style = "border-color:#BE1E2D"),
           
           h3("Data Explorer", align = "center",  style = "color:#BE1E2D"),
           
           
           hr(style = "border-color:#BE1E2D"),
           
           use_waiter(),
           
           DT::dataTableOutput("table"),
  
  
  includeHTML("footer.html")

  
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # map
  
  labels <- sprintf(
    "<strong>%s, %s</strong><br/> Doubling time: %s<br/> Avg. daily cases (past week): %s",
    cty$NAME, cty$state.y, cty$predicted_double_days_f, cty$weekly_new_cases_avg
  ) %>% lapply(htmltools::HTML)
  
  w <- Waiter$new("plot",
                  html = spin_whirly(),
                  color =" #DCDCDC")
  
  observeEvent(input$p1Button, ({
    updateCollapse(session, "collapseExample", open = "Notes")
  }))
  

  output$new_counties <- DT::renderDataTable({
    DT::datatable(new_counties, rownames = F, selection = 'none',
                  colnames = c(" " = "county_state", " " = "county_state_1", " " = "county_state_2"), 
                  options = list(pageLength = 14, 
                                 dom = 'tr', 
                                 ordering = FALSE))
  })

  
  output$map <- renderLeaflet({
    
    w$show()
    
    leaflet(cty) %>%
      setView(-96, 37.8, 5) %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "maplabels", zIndex = 420) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addPolygons(
        fillColor = ~pal(predicted_double_days_f),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal,
                values = cty$predicted_double_days_f,
                position = "bottomright",
                title = "Predicted <br> Doubling Time",
                opacity = 0.8,
                na.label = "Insufficient data")
    
    
    
  })
  
    
  # data
  
  output$table <- DT::renderDataTable({
    
    cty_sf_thre <- cty_sf_thre %>%
      as.data.frame() %>%
      select(
        County = name,
        State = state.x,
        `Population` = popestimate2019,
        `Total new cases in the past week` = weekly_new_cases,
        `Average daily cases in the past week` = weekly_new_cases_avg,
        `Average daily cases in the past week (per 100K)` = weekly_new_cases_capita,
        `14-day incident case trend` = TrendSparkline,
        `COVID-19 Doubling Weeks` = predicted_double_days_f)
    
    
    DT::datatable(cty_sf_thre, rownames = F,  filter = 'bottom', escape = F, selection = 'none', 
                  class = 'cell-border stripe',
                  extensions = c('FixedHeader', 'Scroller'),
                  options = list(pageLength = 10,
                                 fnDrawCallback = htmlwidgets::JS(
                                   '
function(){
  HTMLWidgets.staticRender();
}
'
                                 ),
                                 order = list(list(5, 'desc')),
                                 fixedHeader = TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#97979A', 'color': '#fff'});",
                                   "}")
                  )) %>%
      formatStyle(
        'Total new cases in the past week',
        background = styleColorBar(cty_sf_thre$`Total new cases in the past week`, 'lightyellow'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Average daily cases in the past week',
        background = styleColorBar(cty_sf_thre$`Average daily cases in the past week`, '#FFE4B5'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Average daily cases in the past week (per 100K)',
        background = styleColorBar(cty_sf_thre$`Average daily cases in the past week (per 100K)`, '#FFA07A'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>% spk_add_deps()
    
  })
  
}

shinyApp(ui = ui, server = server)


