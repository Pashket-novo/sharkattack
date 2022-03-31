#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

data <- read.csv("data.csv", sep = ";", header = T)


# Define server logic required to draw a map
shinyServer(function(input, output) {
    
    
    country_count <- data %>% count(Country, Latitude_country, Longitude_country, sort = TRUE)
    area_count <- data %>% count(Country, Area, Latitude_area,Longitude_area,sort = TRUE)
    
    
    area_count$Latitude_area <- as.numeric(area_count$Latitude_area  )
    area_count$Longitude_area <- as.numeric(area_count$Longitude_area  )
    
    area_formap <- area_count %>% drop_na()
    
    world_map <- readOGR("ne_50m_admin_0_countries.shp")
    
    w_attacks <- country_count$n[match(toupper(world_map$admin),toupper(country_count$Country))]
    
    mybins <- c(0,30,60,100,300,600,1000,1600,2000,2500)
    
    # Prepare the text for tooltips:
    mytext <- paste(
        "Country: ", world_map$admin,"<br/>", 
        "Population: ", round(world_map$pop_est/1000000, 2), " mln", "<br/>",
        "Number of Attacks: ", w_attacks,"<br/>",
        "Attacks per 100k population: ", round(w_attacks/world_map$pop_est*100000,2),
        sep="") %>%
        lapply(htmltools::HTML)
    
    mytext2 <- paste(
        "Area: ", area_formap$Area,"<br/>",
        "Number of Attacks: ", area_formap$n,"<br/>",
        "Country: ", area_formap$Country,
        sep="") %>%
        lapply(htmltools::HTML)
    
    # Shark attacks title
    # https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))
    
    title <- tags$div(
        tag.map.title, HTML("Shark Attacks")
    )  
    
    mypalette <- colorBin( palette="PuBu", domain=w_attacks, na.color="transparent", bins=mybins)
    p = colorFactor(palette = c("yellow"),domain = c("Size of symbol proportional to number of attacks"))
    
    output$mymap <- renderLeaflet({
        leaflet(world_map) %>% # create a blank canvas
            addTiles() %>% # add tile
            setView( lat=10, lng=0 , zoom=1) %>%
            addPolygons( # draw polygons on top of the base map (tile)
                fillColor = ~mypalette(w_attacks),
                stroke = TRUE,
                fillOpacity = 0.9, 
                color = "black", 
                weight = 0.3,
                label = mytext,
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto",
                ),
                group = "Country"
            ) %>%
            addLegend( pal=mypalette, values=~w_attacks, opacity=0.9,
                       title = "Shark attacks, total", position = "bottomleft" )%>%
            addCircles(data = area_formap,
                       lng= ~Longitude_area,
                       lat= ~Latitude_area,
                       color = "yellow",
                       fillColor = "yellow",
                       fillOpacity = 0.8,
                       radius = ~n * 300, 
                       label = mytext2,
                       labelOptions = labelOptions( 
                           style = list("font-weight" = "normal", padding = "3px 8px"), 
                           textsize = "13px", 
                           direction = "auto",
                       ),
                       
                       group = "Area"
            ) %>%
            addLegend(position = "bottomright",pal = p, values = c("Size of symbol proportional to number of attacks"),title = "Area/region")%>%
            addLayersControl(
                overlayGroups = c("Country", "Area"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Layer control</label>');
        }
        ")%>%
            
            addControl(title, position = "topleft", className="map-title")
    })
    
    
    # rendering barchart
    output$plot1 <- renderPlot({
      
      if(is.null(input$activity1)== TRUE) {
        actInp1 <- unique(data$Activity)
      }
      else {
        actInp1 <- input$activity1
      }
      
      if(is.null(input$activity2)== TRUE) {
        top <- 15
      }
      else {
        top <- as.numeric(input$activity2[1])
      }
      
      if(length(actInp1) > 20) {
        activities_sort<- data %>% count(Activity, sort = TRUE)%>% slice_max(n,n = top)
        activity_chart <- filter(data, Activity %in% activities_sort$Activity)
        act_length <- length(unique(activity_chart$Activity))
      } else{
        activity <- subset(data, data$Activity%in%actInp1)
        activity_chart <- filter(activity, Activity %in% actInp1)
        act_length <- length(unique(activity_chart$Activity))
      }
      
     
      nb.cols <- act_length + 5
      mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
    
      
        ggplot(activity_chart, aes(fct_infreq(Activity))) + 
            geom_bar(aes(fill = Activity)) + scale_fill_manual(values = mycolors) +
            geom_text(aes(label=..count..),stat = "count", vjust=1.6, color="black", size=3.5)+
            theme_minimal() + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
            labs(y= "Number of attacks", x = "Water acitivity") + 
            ggtitle("Water actitivies involved in shark attacks")
        
    })
    
    
    
})


