# ui.R for green gov shinyApp
library(shiny)
library(shinydashboard)
library(rCharts)
library(graphics)

dashboardPage(
  
  dashboardHeader(),
  
  dashboardSidebar(
    h1("Version 1.0"),
    sidebarMenu(
      menuItem("Welcome", tabName="welcome", icon=icon("child")),
      menuItem("CO2e", tabName="co2e", icon=icon("cloud")),
      menuItem("State Buildings Energy Use", tabName="building", icon=icon("building")),
      menuItem("Fleet Program", tabName="fleet", icon=icon("car")),
      menuItem("Waste Management", tabName="waste", icon=icon("database")),
      menuItem("Sustainability", tabName="sustainbility", icon=icon("question")),
      menuItem("Interactive Map", tabName="map", icon=icon("flag")),
      menuItem("Predictive Analytics", tabName="predictive", icon=icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      {tabItem(
        "welcome",
        column(
          width=6,
          h4("Insert introduction here"),
          HTML("<font size='4'>Contact: <br>Member1 <br>Member2 <br>Member3 <br>Member4<br></font>")
        ),
        column(
          width=6,
          img(src='logo.jpg', width=500, align = "right"),
          img(src="pic.png", width=500, align = "right")
        )
      )}, # this is the welcome tab. 
      
      {tabItem(
        "co2e",
        tabsetPanel(
          tabPanel(
            title = "Emission General Information",
            column(
              width=3,
              radioButtons("DepartmentIndex", "Select Interested Index", 
                           choices=c("Facility Name"="Facility.Name",
                                     "Source Name"="Source.Name",
                                     "Fuel Type"="Fuel.Type"), selected = "Fuel.Type"),
              sliderInput("DepartmentMax", "Select range of ranks of top contributing types", min=1, max=50, value=c(1,10)),
              helpText("The dashboard will track these top contributing types based on 2010 data through time"),
              actionButton("DepartmentButton","See Visualization!")
              
            ),
            column(
              width=9,
              h1("Top Contribution to CO2 Emission", align="center"),
              showOutput("CO2Plot1", "nvd3")
            )
          ),
          tabPanel(
            title = "Explore A Specific Department",
            column(
              width=3,
              uiOutput("DepartmentSelector2"),
              uiOutput("DepartmentSelector3"),
              radioButtons("DepartmentIndex2", "Select Interested Index", 
                           choices=c("Facility Name"="Facility.Name",
                                     "Source Name"="Source.Name",
                                     "Fuel Type"="Fuel.Type"), selected = "Fuel.Type"),
              actionButton("co2Button","See Visualization!")
            ),
            column(
              width=9,
              h1("CO2 Emission Within the Department"),
              showOutput("CO2Plot2", "nvd3"),
              showOutput("CO2Plot3", "nvd3")
            )
          )
        )
      )}, # co2e
      
      {tabItem(
        "building",
        fluidPage(
          column(
            width=3,
            radioButtons("buildingBasicLevel", "Select Interested Type: ", 
                         choices = c("Entire Department"="dept", "Individual Building"="build"),
                         selected = "dept"),
            helpText("You can compare between individual buildings or between departments."),
            uiOutput("buildingBasicUnitSelector"),
            helpText("Multiply Selection is allowed"),
            actionButton("buildingBasicUnitButton","See Visualization!"),
            helpText("Below is an interactive option!"),
            radioButtons("buildingBasicIndex", "Select Interested Variable: ", 
                         choices = c("Green Electricity Percentage (%)"="GreenPercent",
                                     "Electricity Used (kWh)"="Electricity",
                                     "Natural Gas Used (therms)"="NaturalGas",
                                     "Propane Used (therms)"="Propane",
                                     "Water Used (kgal)"="Water",
                                     "Total Energy Used (kBtu)"="Total"),
                         selected = c("Total"))
            
          ),
          column(
            width=9,
            h1("Energy Usage Visualization", align = "center"),
            showOutput("buildingPlot1", "nvd3")
          )
        )
      )}, # building
      
      {tabItem(
        "fleet",
        tabsetPanel(
          tabPanel(
            title = strong("fleet 1.1"),
            column(
              width=3
            ),
            column(
              width=9
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
        fluidPage(
          #tabPanel(
          # title = strong("map 1.1"),
          column(
            width=3,
            uiOutput("yearBasicSelector"),
            helpText("You can select the available year that has both CO2 and energy consumption.")
          ),
          column(
            width=9,
            fluidRow( box(title="CO2 against energy bubble plot",status="primary", 
                          solidHeader = TRUE, htmlOutput("testing"),width=850,height=725))
          )
          #)
        )
        
      )}, # sustainbility
      
      {tabItem(
        "map",
        tabsetPanel(
          tabPanel(
            title = strong("map 1.1"),
            column(
              width=3
            ),
            column(
              width=9
            )
          )
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