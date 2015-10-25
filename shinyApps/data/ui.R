# ui.R for green gov shinyApp
library(shiny)
library(shinydashboard)
library(rCharts)
library(graphics)
library(plotly)
library(leaflet)



dashboardPage(
  
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName="welcome", icon=icon("database")),
      menuItem("CO2e", tabName="co2e", icon=icon("database")),
      menuItem("State Buildings Energy Use", tabName="building", icon=icon("database")),
      menuItem("Fleet Program", tabName="fleet", icon=icon("database")),
      menuItem("Waste Management", tabName="waste", icon=icon("database")),
      menuItem("Sustainability", tabName="sustainbility", icon=icon("database")),
      menuItem("Interactive Map", tabName="map", icon=icon("database")),
      menuItem("Predictive Analytics", tabName="predictive", icon=icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      {tabItem(
        "welcome",
        img(src='logo.jpg', width=500, align = "right"),
        h4("Insert introduction here"),
        HTML("<font size='4'>Contact: <br>Member1 <br>Member2 <br>Member3 <br>Member4<br></font>")
      )}, # this is the welcome tab. 
      
      {tabItem(
        "co2e",
        tabsetPanel(
          tabPanel(
            title = strong("Emission by type"),
            column(
              width=3,
              uiOutput("DepartmentSelector"),
              helpText("You can select a department for CO2 emission."),
              actionButton("DepartmentButton","See Visualization!")
            ),
            column(
              width=9,
              showOutput("CO2Plot1", "nvd3")
            )
          )
        )
      )}, # co2e
      
      {tabItem(
        "building",
        tabsetPanel(
          tabPanel(
            title = strong("building1"),
            column(
              width=3,
              radioButtons("buildingBasicLevel", "Select Interested Type: ", 
                           choices = c("Entire Departmen"="dept", "Individual Building"="build"),
                           selected = "dept"),
              #                   checkboxGroupInput("buildingBasicIndex", "Select Interested Variable: ",
              #                                      choices = c(x))
              uiOutput("buildingBasicUnitSelector"),
              helpText("You can select an individual building or an entire department."),
              actionButton("buildingBasicUnitButton","See Visualization!")
            ),
            column(
              width=9,
              showOutput("buildingPlot1", "nvd3")
            )
          ), 
          tabPanel(
            "building 2"
          ), 
          tabPanel(
            "building 3"
          )
        )
      )}, # building
      
      {tabItem(
        "fleet",
        tabsetPanel(
          tabPanel(
            title = strong("Number of Assets by Agencies"),
            column(
              width=10,
              uiOutput("FleetDepartmentSelector"),
              helpText("You can select a department for the assets information."),
              actionButton("FleetDepartmentButton","See Visualization!")
            ),
            fluidRow(
              #height = 725, 
              plotlyOutput("FleetBarplot1")
            )
          ),
          tabPanel(
            title = strong("Number of Assets by Year"),
            fluidRow(
              #height = 725, 
              plotlyOutput("FleetLineChart")
            )
          ),
          tabPanel(
            title = strong("Number of Active Assets by Fuel Type"),
            fluidRow(
              height = 425, 
              plotlyOutput("FleetBarFuelType")
            )
          ),
          tabPanel(
            title = strong("Number of EVs in active vehicles"),
            fluidRow(
              height = 425, 
              plotlyOutput("FleetBarEVType")
            )
          ),
          tabPanel(
            title = strong("Number of Assets by Acquisitions"),
            fluidRow(
              height = 425, 
              plotlyOutput("FleetBarAcqType")
            )
          )
        )
      )}, # fleet
      
      {tabItem(
        "waste",
        tabsetPanel(
          tabPanel(
            title = strong("waste 1.1"),
            column(
              width=3
            ),
            column(
              width=9
            )
          )
        )
      )}, # waste
      
      {tabItem(
        "sustainbility",
        tabsetPanel(
          tabPanel(
            title = strong("sustainbility 1.1"),
            column(
              width=3
            ),
            column(
              width=9
            )
          ),
          tabPanel(
            title = strong("How to reduce Disposition Assets"),
            column(
              width=3
            ),
            column(
              width=9
            )
          )
        )
      )}, # sustainbility
      
      {tabItem(
        "map",
        div(class="outer",
            tags$head(
              # Include our custom CSS
               includeCSS("data/styles.css")
              #includeScript("gomap.js")
             ),
        
            leafletOutput("map", width="100%", height="100%")
            
            )
      )}, # map
      
      {tabItem(
        "predictive",
        tabsetPanel(
          tabPanel(
            title = strong("predictive 1.1"),
            column(
              width=3
            ),
            column(
              width=9
            )
          )
        )
      )} # predictive
    )
    

  )
)