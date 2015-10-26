# ui.R for green gov shinyApp
library(shiny)
library(shinydashboard)
library(rCharts)
library(graphics)
library(plotly)
library(leaflet)

dashboardPage(
  
  dashboardHeader(
    title="California State Government Sustainability Information Dashboard",
    titleWidth=600
  ),
  
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
          h1('Introduction'),
          p('In the face of environmental change and the drought,
            California government seeks to find innovative ways to reduce waste in resources and increase sustainable practices. Here we leverage the pilot 
            Statewide Open Data Portal and visulize publicly available data for derived insights and help
            state government make informed decisions.'),
          p('Specifically, in each tab you will find the most informative interactive graph/maps
            to help you understand each dataset and connections among datasets.'),
          h1('Dashboard creators:'),
          p(strong('Qiwei Li:'), 'qwli at ucdavis dot edu'),
          p(strong('Haomiao Meng:'), 'hmmeng at ucdavis dot edu'),
          p(strong('Yu Pei:'), 'whpei at ucdavis dot edu'),
          p(strong('Jiaping Zhang:'), 'jpzhang at ucdavis dot edu'),
          br(),
          br(),
          h4(strong('Acknowledgement:')),
          p('Thanks to California Department of general service for organizing this event.'),
          p('Technical supports from all the staff/technicians. And last but not the least, the great food! wink emoticon')
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
            title = strong("SABRC Results"),
            column(
              width=3,
              uiOutput("AgencySelector"),
              radioButtons("Year", "Select One Year",
                           choices=c("2013", "2013/2014"),
                           selected = "2013/2014"),
              actionButton("wasteButton", "See Visualization!")
            ),
            column(
              width=9,
              h1("Reportable Value and Compliant Value Within the Agency"),
              showOutput("wastePlot", "nvd3")
            )
          )
        )
      )}, # waste
      
      {tabItem(
        "sustainbility",
        fluidPage(
          column(
            width=3,
            uiOutput("yearBasicSelector"),
            helpText("You can select the available year that has both CO2 and energy consumption."),
            actionButton("yearBasicUnitButton","See Visualization!")
          ),
          column(
            width=9,
            box(title="CO2 against energy bubble plot",status="primary",
                solidHeader = TRUE, htmlOutput("testing"),width=850,height=725)
          )  
        )
        
      )}, # sustainbility
      
      {tabItem(
        "map",
        div(class="outer",
            tags$head(
              # Include our custom CSS
              includeCSS("www/styles.css")
              #includeScript("gomap.js")
            ),
            
            leafletOutput("map", width="100%", height="100%")
            
        )
      )}, # map
      
      {tabItem(
        "predictive",
        tabsetPanel(
          tabPanel(
            title = "View Data",
            dataTableOutput("table1")
          ),
          tabPanel(
            title = "Regression",
            helpText("On normalized data:"),
            fluidPage(
              column(
                width=3,
                uiOutput("regressionSelector")
              ),
              column(
                width=9,
                verbatimTextOutput("regTab")
              )
            )
          ),
          tabPanel(
            title = "4 Way Table Link",
            tableOutput("tablekey")
          )
        )
      )} # predictive
    )
    

  )
)