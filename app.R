
library(tidyverse)
library(tmap)
library(sf)
library(leaflet)
library(shiny)
library(tmaptools)

epsg_NY_long_island <- 2263
epsg_wgs84 <- 4326

#First we read in all of the columns that we need, and then perform a series of steps from
#the main Rmd file so that the three census records are all comparable

tree_1995 <- read_csv("data/new_york_tree_census_1995.csv",
                          col_types = cols_only("borough" = col_character(),
                                                "spc_latin" = col_character(),
                                                "longitude" = col_double(),
                                                "latitude" = col_double()))


tree_2015 <- read_csv("data/new_york_tree_census_2015.csv",
                          col_types = cols_only("boroname" = col_character(),
                                                "spc_latin" = col_character(),
                                                "longitude" = col_double(),
                                                "latitude" = col_double()))


tree_2005 <- read_csv("data/Cleaned_new_york_tree_census_2005.csv",
                          col_types = cols_only("borocode" = col_integer(),
                                                "boroname" = col_character(),
                                                "spc_latin" = col_character(),
                                                "longitude" = col_double(),
                                                "latitude" = col_double()))



tree_2005 <- tree_2005 %>% filter(!is.na(boroname))
tree_2005$boroname[tree_2005$boroname == 5] <- "Staten Island"

tree_1995$year <- "1995"
tree_2005$year <- "2005"
tree_2015$year <- "2015"

#Make all spc_latin names lowercase
tree_1995$spc_latin <- tolower(tree_1995$spc_latin)
tree_2005$spc_latin <- tolower(tree_2005$spc_latin)
tree_2015$spc_latin <- tolower(tree_2015$spc_latin)


#Rename boroname to borough
names(tree_2005)[names(tree_2005)=="boroname"] <- "borough"
names(tree_2015)[names(tree_2015)=="boroname"] <- "borough"

#Read in the five borough boundaries
nyc_borough <- st_read("data/maps/geo_export_0fee68cc-9bb2-4196-ae41-9ca93a0f4690.shp") %>%
  st_transform(epsg_NY_long_island)

year_choices <- list("2015", "2005", "1995")

borough_choices <- unique(tree_2015$borough)

tree_2005 <- tree_2005[c("spc_latin", "borough", "year", "latitude", "longitude")]

tree_2005 <- tree_2005 %>% filter(!is.na(borough))

#Fix 2015 species names
tree_2015$spc_latin[tree_2015$spc_latin == "platanus x acerifolia"] <- "platanus acerifolia"
tree_2015$spc_latin[tree_2015$spc_latin == "gleditsia triacanthos var. inermis"] <- "gleditsia triacanthos"

all_map_data <- rbind(tree_1995, tree_2005, tree_2015) %>%
  filter(!is.na(spc_latin))

bar_map_data <- all_map_data %>% group_by(spc_latin, year, borough) %>% mutate(count = n()) %>%
  filter(count > 4000) %>% ungroup() %>% filter(!is.na(latitude), !is.na(longitude))


ui <- fluidPage(
   titlePanel("New York City Tree Census Exploration"),
   hr(),
   fluidRow(
     column(width = 3, offset = 3,
            selectInput(inputId = "borough", label = "Select a borough", choices = borough_choices)
     ),
     column(width = 3,
            selectInput(inputId = "year", label = "Select a year", choices = year_choices)
     )
   ),
   tabsetPanel(
     tabPanel("Map",
              leafletOutput("map", height = "75vh")),
     tabPanel("Bar Plots",
              h1("Most common species from selected borough and year"),
              plotOutput("barplot"))
   )
)

server <- function(input, output) {
  
   output$map <- renderLeaflet({
     #Use the two user inputs to subset the data
     borough <- nyc_borough %>% filter(boro_name == input$borough)
     selected_trees <- all_map_data %>% filter(borough == input$borough, year == input$year) %>% st_as_sf(coords = c("longitude", "latitude")) %>%
       st_set_crs(epsg_wgs84) %>% st_transform(epsg_NY_long_island)
     
     #Create a heatmap based on the subsetted data and then plot on the map and return for displaying
     heatmap <- smooth_map(selected_trees, bandwidth = 4, unit.size = 150, cover = borough)
     tm <- tm_shape(heatmap$polygons) +
       tm_polygons(col = "level", alpha = 0.75, palette = "Greens", title = "Tree Density")
     tmap_leaflet(tm)
   })
  
   output$barplot <- renderPlot({
     #Use the two user inputs to also create a bar plot of the most common species
     #in each Borough based on whether each species count is over 20,000 when grouped by
     #year and borough
     bar_map_data %>% filter(borough == input$borough, year == input$year) %>%
       ggplot() + geom_bar(aes(x = spc_latin)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
   })
}

shinyApp(ui = ui, server = server)

