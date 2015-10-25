# server.R for the green gov shinyApps
library(shiny)
library(ggplot2)
library(plyr)
library(stringr)
library(rCharts)
library(rjson)
library(lattice)

load("data/building.rda")
load('data/co2e.rda') #load data frame co2 

shinyServer(function(input, output) {
  
  output$buildingBasicUnitSelector <- renderUI ({
    if(input$buildingBasicLevel=="dept"){
      selectizeInput("buildingBasicUnit", "Select the Property: ",
                     choices = unique(as.character(building$Department.Name)), 
                     selected = "Department of General Services",
                     multiple = TRUE)
    } else {
      selectizeInput("buildingBasicUnit", "Select the Property: ",
                     choices = unique(as.character(building$Property.Name)), 
                     selected = "Haagen-Smit Laboratory",
                     multiple = TRUE)
    }
  })
  
  buildingBasicUnitCore <- eventReactive(input$buildingBasicUnitButton, {
    department_names = unique(as.character(building$Department.Name))
    if(input$buildingBasicLevel=="dept"){
      mySubset = building[building$Department.Name %in% input$buildingBasicUnit, ]
      return(list(mySubset=mySubset, 
                  buildingBasicLevel=input$buildingBasicLevel,
                  buildingBasicUnit=input$buildingBasicUnit))
    }else{
      mySubset = building[building$Property.Name %in% input$buildingBasicUnit, ]
      return(list(mySubset=mySubset, 
                  buildingBasicLevel=input$buildingBasicLevel,
                  buildingBasicUnit=input$buildingBasicUnit))
    }
  })
  
  output$buildingPlot1 <- renderChart({
    data = buildingBasicUnitCore()
    mySubset = data$mySubset
    buildingBasicLevel = data$buildingBasicLevel
    buildingBasicUnit = data$buildingBasicUnit
    if(buildingBasicLevel=="dept"){
      Year = rep(2012:2015, each=length(buildingBasicUnit))
      Name = rep(buildingBasicUnit, times=4)
      Total_Energy_Use = rep(NA, length(Name))
      df = data.frame(Year,Name,Total_Energy_Use)
      for(i in 1:nrow(df)){
        df$Total_Energy_Use[i] = sum(mySubset[mySubset$Department.Name==Name[i] & mySubset$Year.Ending==Year[i], "Site.Energy.Use..kBtu."], na.rm=TRUE)
      }
      df$Total_Energy_Use[df$Total_Energy_Use==0]=NA
      df$Year = as.character(df$Year)
      p1 <- nPlot(Total_Energy_Use ~ Year, group = 'Name', data=df, type = 'multiBarChart', dom="buildingPlot1")
      p1$chart(forceY = c(0))
      p1$yAxis(tickFormat = "#!  function(y) { return (y/1000000).toFixed(2) + 'M'} !#")
      #p1$params$width <- 400
      #p1$params$height <- 800
      return(p1)
    } else {
      
    }
  })

    #CO2e
    output$DepartmentSelector <- renderUI ({
      selectizeInput("DepartmentSelectorUnit", "Select the Department: ",
                     choices = unique(co2$Organization.Name), selected = "Department of General Services")
    })
    
    DepartmentCore <- eventReactive(input$DepartmentButton, {
      department_names = unique(as.character(co2$Organization.Name))
      if(input$DepartmentSelectorUnit %in% department_names){
        mySubset = co2[co2$Organization.Name == input$DepartmentSelectorUnit, ]
        return(mySubset)
      }
    })
    
  output$CO2Plot1 <- renderChart({
    mySubset = DepartmentCore()
    year = c(2010:2015)
    #value = as.integer(tapply(X = mySubset$Site.Energy.Use..kBtu., INDEX = mySubset$Year.Ending, FUN = sum, na.rm=TRUE))
    #value = c(NA, value, NA)
    tmp = data.frame(year, value = 1:6)
    p1 <- nPlot(CO2e ~ Emission.Year, data=mySubset, group = "Fuel.Type",
                type = "multiBarChart", dom="CO2Plot1")
    #p1$chart(forceY = c(0))
    p1$params$width = 500
    p1$params$height = 400
    return(p1)
  })
})