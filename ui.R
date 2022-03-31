# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Developed with R version 3.3.2 (64-bit)

# Checking installed packages and install missing ones from the list
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list.of.packages <- c("dplyr", "shiny", "rgdal","leaflet", "RColorBrewer", "tidyr", "ggplot2", "forcats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(dplyr)
library(rgdal)
library(leaflet)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(forcats)


# UI inspiration
# https://shiny.rstudio.com/gallery/career-pathfinder.html

data <- read.csv("data.csv", sep = ";", header = T)

source("panel.R")

# Panel div for visualization
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = img(src="shark_logo.png", height = "40px"), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "Shark attacks",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 75px;}"),
                   
                   tabPanel("HOME", value = "home",
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var butClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("
                                 <section class='banner'>
                                <h2 class ='parallax', style='color: white'><strong>SHARK ATTACKS</strong></h2>
                                <p class='parallax_description'>Shark attacks in our life</p>
                                </section>
                                     ")
                            ),
                            
                            
                            
                            # WHAT
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
                                       shiny::HTML("<h5>An interactive data visualisations of the shark attacks
                                                worldwide for the period of 1700 - 2018 years.  </h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # HOW
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>How it can help you</h1> </center><br>"),
                                       shiny::HTML("<h5>Information from the visualisations
                                                   will help you to understand geographic regions where shark
                                                   attack happen more often and water activities involved in 
                                                   such attacks, which can help you to be more cautious planning
                                                   your next trip related to the ocean/sea, or just increase
                                                   your knowledge about shark attacks. </h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # WHERE
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Where it came from</h1> </center><br>"),
                                       shiny::HTML("<h5> Dataset with shark attacks for more than 300 years
                                       was analyzed and represented with various data visualisations.</h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # AFTERWARD
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>How does it fit in the big picture?</h1> </center><br>"),
                                       shiny::HTML("<h5>Looking for the visualisation of shark attacks across the world
                                                   can make you think about the causes of it in the particular regions and
                                                   bring awareness of such events. After looking into data visualisations,
                                                   probably you will want to continue your exploration of shark attacks,
                                                   for this purpose link to Global Shark Attack File's website provided in 'About'
                                                   section. </h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(shiny::HTML("<br><br><center> <h1>Ready to explore?</h1> </center>
                                                 <br>")
                            ),
                            fluidRow(
                                column(3),
                                column(6,
                                       tags$div(align = "center", 
                                                tags$a("LOOK FOR VISUALISATION", 
                                                       onclick="butClick('visual')", 
                                                       class="btn btn-primary btn-lg")
                                       )
                                ),
                                column(3)
                            ),
                            fluidRow(style = "height:25px;"
                            )
                            
                   ), # Closes the first tabPanel called "Home"
                   
                   tabPanel("VISUALISATION", value = "visual",
                            
                            fluidRow(
                                div(align = "center",
                                    tags$span(h4("Visualisation of shark attacks 1700-2018"), 
                                              style = "font-weight:bold"
                                    ))
                            ),
                            fluidRow(
                                
                                leafletOutput("mymap", height = "470px")
                            ),
                            tags$hr(),
                            fluidRow(
                                tags$ul(
                                    tags$li(h6("Data visualisations shown on this page can help you to understand, 
                                           where attacks happen more often and what is the most common water activities")),
                                    tags$li(h6("The map is interactive, you can hover over the map and see the information for
                                           the country or area of attack")),
                                    tags$li(h6("Country layer and area layers can be turned on and off for a better user experience")),
                                    tags$li(h6("The size of the area marker represents the number of attacks in that region")),
                                    tags$li(h6("Some data for the regions are missing in the dataset, but the main shark attacks areas
                                           are shown on the map.")),
                                )
                            ),
                            
                            fluidRow(
                                plotOutput("plot1")
                                
                            ),
                            fluidRow(
                              column(1),
                              column(4,
                                     
                              selectInput("activity1", "Activity:",
                                          unique(data$Activity),
                                          multiple = TRUE,
                                          selected = NULL
                              )),
                              column(4,
                                     
                                     selectInput("activity2", "TOP:",
                                                 c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                 multiple = TRUE,
                                                 selected = NULL
                                     )),
                              
                              ),
                            fluidRow(
                                tags$ul(
                                    tags$li(h6("By default TOP15 activities is shown")),
                                    tags$li(h6("Each bar chart represent water activity involved in the shark attack")),
                                    tags$li(h6("The legend shows the colour codes for the activities")),
                                    tags$li(h6("You can select any activity from the drop-down menu or by search")),
                                    tags$li(h6("If more than 20 activities selected, the system will show default TOP15,
                                               due to the limited space on a webpage. It is hard to read the data if it is
                                               more than 20 bars")),
                                    tags$li(h6("You can select TOP 1-20 from dropdown menu, the system will show you TOP attacks
                                               in the selected range. IMPORTANT: before selection of TOP activities, single activities
                                               from the 'Activity' menu should be deselected, otherwise the system will not show TOP")),
                                    tags$li(h6("To change the number of TOP, the previous value should be deselected"))
                                    
                                    
                                    
                                    
                                )
                                
                            )
                            
                            
                   ),  # Closes the second tabPanel called "VISUALISATION"
                   
                   tabPanel("ABOUT", value = "about",
                            
                            fluidRow(
                                shiny::HTML("<br><br><center> 
                                            <h1>About Shark Attacks</h1> 
                                            <h4>What's behind the data.</h4>
                                            </center>
                                            <br>
                                            <br>"),
                                style = "height:250px;"),
                            fluidRow(
                                div(align = "center",
                                    tags$span(h4("Introduction to the investigation of shark accidents"), 
                                              style = "font-weight:bold"
                                    ))
                            ),
                            fluidRow(
                                column(3),
                                column(6,
                                       tags$ul(
                                           tags$li(h6("Global Shark Attack file and Shark Research Institute, Inc. started to collect and investigate
                                                      data about sharks attacks to show humans that sharks are no threat to people
                                                      in the big picture: 'each year, for every human killed by a shark, our species slaughters millions of sharks - about 73 million sharks'")), 
                                           tags$li(h6("Provide an overview of the shark attacks")), 
                                           tags$li(h6("Provide more information related to sharks and their behaviour")),
                                           tags$li(h6("Attempt to understand why sharks attacks. Usually, they do not attack people, we are not interesting to them,
                                                      but sharks somehow provoked")),
                                           tags$li(h6("Help to avoid potentially dangerous situations")),
                                           tags$li(h6("Global Shark Attack file and Shark Research Institute, Inc. tried to investigate each case individually,
                                                      to find the causes of the attacks and provide information to people"))
                                       )
                                ),
                                column(3)
                            ),
                            fluidRow(
                                column(2),
                                column(8,
                                       # Panel for Background on Data
                                       div(class="panel panel-default",
                                           div(class="panel-body",  
                                               tags$div( align = "center",
                                                         icon("bar-chart", class = "fa-4x"),
                                                         div( align = "center", 
                                                              h5("About the Data")
                                                         )
                                               ),
                                               tags$p(h6("Dataset for the data visualisations downloaded from Kaggle:")),
                                               tags$a(href="https://www.kaggle.com/teajay/global-shark-attacks", "Global Shark Attacks"),
                                               tags$p(h6("Dataset prepared by Toby Jolly with permission from the Global Shark Attack File's website.
                                                         Dataset contains more attributes than available publicly on Global Shark Attack File's website.")),
                                               tags$p(h6("Original public dataset, updated regularly can be accessed on Global Shark Attack File's website:")),
                                               tags$a(href="http://www.sharkattackfile.net/incidentlog.htm", "Global Shark Attack File by Shark Research Institute"),
                                               tags$p(h6("Also Global Shark Attack File's website contains useful information for the people about shark species,
                                                         recommendations, and how to become a member or supporter of the project")),
                                               tags$p(h6("Over 300 years of data were collected, which resulted in nearly 6000 records of shark attacks")),
                                               tags$p(h6("Some steps of data preparation:")),
                                               tags$ul(
                                                   tags$li(h6("Dataset was wrangled and cleaned")),
                                                   tags$li(h6("Activities were grouped together where possible")),
                                                   tags$li(h6("The 1700 year was chosen as a starting year, due to the insufficient details for older years")),
                                                   tags$li(h6("Latitude and longitude were autogenerated for countries and regions in Tableau")),
                                                   tags$li(h6("Spatial data used up to region level, due to the absence of geographical coordinates for each accident")),
                                               )
                                           )
                                       ) # Closes div panel
                                ), # Closes column
                                column(2)
                            ),
                            
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h5>About the project</h5> </center><br>"),
                                       shiny::HTML("<h6>The project is part of the assessment for FIT5147</h6>"),
                                       shiny::HTML("<h6>FIT5147 Narrative Visualisation Project</h6>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            fluidRow(
                                column(5),
                                
                                
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                   tags$img(src = "man_beard_1.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Pavel Zemnukhov"),
                                                   tags$h6( tags$i("Student, Monash University"))
                                               ),
                                               div(
                                                   "As a surfer myself, shark attacks is an interesting topic to me"
                                               )
                                           )
                                       )
                                ),
                                
                                column(5)
                                
                            ),
                            fluidRow(style = "height:150px;")
                   )  # Closes About tab
                   
)

)