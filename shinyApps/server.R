# server.R for the green gov shinyApps
library(shiny)
library(ggplot2)
library(plyr)
library(stringr)
library(rCharts)
library(rjson)
library(lattice)
library(googleVis)
library(plotly)
library(dplyr)
library(leaflet)

load("data/building.rda")
load('data/co2e.rda') #load data frame co2 
load("data/fleet.rda")
load('data/four_way_key.rda')
load('data/bubble.rda')
load('data/waste.rda')
load("data/regression.rda")
load('data/VehicleEngergyDistribution.rda')

shinyServer(function(input, output) {
  
  output$buildingBasicUnitSelector <- renderUI ({
    if(input$buildingBasicLevel=="dept"){
      selectizeInput("buildingBasicUnit", "Select the Property: ",
                     choices = unique(as.character(building$Department.Name)), 
                     selected = c("California Department of Transportation",
                                  "California Department of Education",
                                  "Department of Motor Vehicles",
                                  "Department of State Hospitals"),
                     multiple = TRUE)
    } else {
      selectizeInput("buildingBasicUnit", "Select the Property: ",
                     choices = unique(as.character(building$Property.Name)), 
                     selected = c("Haagen-Smit Laboratory","NAPA STATE HOSPITAL"),
                     multiple = TRUE)
    }
  })
  buildingBasicUnitCore <- eventReactive(input$buildingBasicUnitButton, {
    names(building)[which(names(building)=="Percent.of.Electricity.that.is.Green.Power")]<-"GreenPercent"
    names(building)[which(names(building)=="Electricity.Use...Grid.Purchase..kWh.")]<-"Electricity"
    names(building)[which(names(building)=="Natural.Gas.Use..therms.")]<-"NaturalGas"
    names(building)[which(names(building)=="Propane.Use..kBtu.")]<-"Propane"
    names(building)[which(names(building)=="Water.Use..All.Water.Sources...kgal.")]<-"Water"
    names(building)[which(names(building)=="Site.Energy.Use..kBtu.")]<-"Total"
    building$GreenPercent = as.numeric(gsub("%","",building$GreenPercent))
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
    mySubset[is.na(mySubset)]=0
    buildingBasicLevel = data$buildingBasicLevel
    buildingBasicUnit = data$buildingBasicUnit
    if(buildingBasicLevel=="dept"){
      Year = rep(2012:2015, times=length(buildingBasicUnit))
      Name = rep(buildingBasicUnit, each=4)
      Value = rep(NA, length(Name))
      df = data.frame(Year,Name,Value)
      if(input$buildingBasicIndex=="GreenPercent"){
        for(i in 1:nrow(df)){
          df$Value[i] = mean(mySubset[mySubset$Department.Name==Name[i] & mySubset$Year.Ending==Year[i], input$buildingBasicIndex], na.rm=TRUE)
        }
      } else {
        for(i in 1:nrow(df)){
          df$Value[i] = sum(mySubset[mySubset$Department.Name==Name[i] & mySubset$Year.Ending==Year[i], input$buildingBasicIndex], na.rm=TRUE)
        }
      }
      df$Value[df$Value==0]=NA
      df$tag = rep(NA, nrow(df))
      df$tag2 = rep(NA, nrow(df))
      df$tag[df$Year==2013] = paste0(df$Name[df$Year==2013], " on ", df$Year[df$Year==2013], " is ", df$Value[df$Year==2013])
      df$tag2[df$Year==2013] = paste0("<br> the 10% reducation target is ", df$Value[df$Year==2013]*0.9,
                                      "<br> the 20% reducation target is ", df$Value[df$Year==2013]*0.8)
      df$tag[df$Year==2014] = paste0(df$Name[df$Year==2014], " on ", df$Year[df$Year==2014], " is ", df$Value[df$Year==2014])
      df$tag2[which(df$Year==2014)] = df$tag2[which(df$Year==2013)]                               
      p1 <- nPlot(Value ~ Year, group = 'Name', data=df, type = 'multiBarChart', dom="buildingPlot1")
      if(!input$buildingBasicIndex=="GreenPercent"){
        p1$chart(tooltipContent = "#! function(key, x, y, e){ return e.point.tag + e.point.tag2} !#")
      }
      p1$chart(forceY = c(0))
      if(max(df$Value, na.rm=TRUE)>=1000000){
        p1$yAxis(tickFormat = "#!  function(y) { return (y/1000000).toFixed(2) + 'M'} !#")
      }
      return(p1)
    } else {
      Year = rep(2012:2015, times=length(buildingBasicUnit))
      Name = rep(buildingBasicUnit, each=4)
      Value = rep(NA, length(Name))
      df = data.frame(Year,Name,Value)
      for(i in 1:nrow(df)){
        df$Value[i] = mean(mySubset[mySubset$Property.Name==Name[i] & mySubset$Year.Ending==Year[i], input$buildingBasicIndex], na.rm=TRUE)
      }
      df$Value[df$Value==0]=NA
      df$tag = rep(NA, nrow(df))
      df$tag2 = rep(NA, nrow(df))
      df$tag[df$Year==2013] = paste0(df$Name[df$Year==2013], " on ", df$Year[df$Year==2013], " is ", df$Value[df$Year==2013])
      df$tag2[df$Year==2013] = paste0("<br> the 10% reducation target is ", df$Value[df$Year==2013]*0.9,
                                      "<br> the 20% reducation target is ", df$Value[df$Year==2013]*0.8)
      df$tag[df$Year==2014] = paste0(df$Name[df$Year==2014], " on ", df$Year[df$Year==2014], " is ", df$Value[df$Year==2014])
      df$tag2[which(df$Year==2014)] = df$tag2[which(df$Year==2013)]                               
      p1 <- nPlot(Value ~ Year, group = 'Name', data=df, type = 'multiBarChart', dom="buildingPlot1")
      if(!input$buildingBasicIndex=="GreenPercent"){
        p1$chart(tooltipContent = "#! function(key, x, y, e){ return e.point.tag + e.point.tag2} !#")
      }
      p1$chart(forceY = c(0))
      if(max(df$Value, na.rm=TRUE)>=1000000){
        p1$yAxis(tickFormat = "#!  function(y) { return (y/1000000).toFixed(2) + 'M'} !#")
      }
      return(p1)
    }
  })
  
  DepartmentCore <- eventReactive(input$DepartmentButton, {
    co2$tag = paste0(co2$Emission.Year, ".", co2[ ,input$DepartmentIndex])
    tmp = tapply(co2[, "CO2e"], co2[, "tag"], sum, na.rm=TRUE)
    return(tmp)
  })
  output$CO2Plot1 <- renderChart({
    mySubset = DepartmentCore()
    name = names(mySubset)
    value = as.numeric(mySubset)
    year = as.numeric(gsub("([0-9]{4}).*", "\\1", name))
    type = gsub("[0-9]{4}.(.*)", "\\1", name)
    Value = sort(value, decreasing=TRUE)
    year = year[order(value, decreasing=TRUE)]
    type = type[order(value, decreasing=TRUE)]
    df = data.frame(year, type, Value)
    
    df2 = df[df$year==2010, ]
    df2 = unique(df$type)[input$DepartmentMax[1]:input$DepartmentMax[2]]
    df = df[df$type %in% df2, ]
    year = rep(c(2010:2014), times=length(df2))
    type = rep(df2, each=5)
    tmp = data.frame(year, type, rep(NA, length(year)))
    names(tmp)=c("year","type","value")
    for(i in 1:nrow(tmp)){
      tmp$value[i] = mean(df[df$year==tmp$year[i] & df$type==tmp$type[i], "Value"], na.rm=TRUE)
    }
    tmp$value[tmp$value==0]<-NA
    qiwei = list(df2, tmp)
    p1 <- nPlot(value ~ year, group='type', data = tmp, type = 'lineChart', dom="CO2Plot1")
    p1$chart(margin = list(left = 150))
    p1$chart(forceY = c(0))
    return(p1)
  })
  output$DepartmentSelector2 <- renderUI ({
    selectizeInput("DepartmentSelectorUnit2", "Select the Department: ",
                   choices = unique(co2$Organization.Name), selected = "California Department of Conservation")
  })
  output$DepartmentSelector3 <- renderUI ({
    selectizeInput("DepartmentSelectorUnit3", "Select the Department: ",
                   choices = unique(co2$Organization.Name), selected = "California Department of Motor Vehicles")
  })
  DepartmentCore2 <- eventReactive(input$co2Button, {
    mySubset = co2[co2$Organization.Name == input$DepartmentSelectorUnit2, ]
    return(mySubset)
  })
  DepartmentCore3 <- eventReactive(input$co2Button, {
    mySubset = co2[co2$Organization.Name == input$DepartmentSelectorUnit3, ]
    return(mySubset)
  })
  output$CO2Plot2 <- renderChart({
    mySubset = DepartmentCore2()
    index = unique(mySubset[ , input$DepartmentIndex2])
    year = rep(c(2010:2014), times=length(index))
    name = rep(index, each=5)
    value = rep(NA, length(year))
    df = data.frame(year, name, value)
    for(i in 1:nrow(df)){
      df$value[i] = mean(mySubset[mySubset$Emission.Year==df$year[i] & mySubset[, input$DepartmentIndex2]==df$name[i], "CO2e"], na.rm=TRUE)
    }
    #mySubset$Emission.Year = as.character(mySubset$Emission.Year)
    p1 <- nPlot(value ~ year, data=df, group = "name",
                type = "multiBarChart", dom="CO2Plot2")
    p1$chart(forceY = c(0))
    #p1$params$width = 500
    p1$params$height = 300
    return(p1)
  })
  output$CO2Plot3 <- renderChart({
    mySubset = DepartmentCore3()
    index = unique(mySubset[ , input$DepartmentIndex2])
    year = rep(c(2010:2014), times=length(index))
    name = rep(index, each=5)
    value = rep(NA, length(year))
    df = data.frame(year, name, value)
    for(i in 1:nrow(df)){
      df$value[i] = mean(mySubset[mySubset$Emission.Year==df$year[i] & mySubset[, input$DepartmentIndex2]==df$name[i], "CO2e"], na.rm=TRUE)
    }
    #mySubset$Emission.Year = as.character(mySubset$Emission.Year)
    p1 <- nPlot(value ~ year, data=df, group = "name",
                type = "multiBarChart", dom="CO2Plot3")
    p1$chart(forceY = c(0))
    #p1$params$width = 500
    p1$params$height = 300
    return(p1)
  })
  
  ## Sustainability Bubble plot
  output$yearBasicSelector <- renderUI ({
    selectizeInput("yearSelectorUnit", "Select year: ",
                   choices = unique(res$year),
                   selected = 2013)
  })
  
  sustainabilityCore <- eventReactive(input$yearBasicUnitButton, {
    return(res[input$yearSelectorUnit == res$year, ])
  })
  
  
  output$testing <- renderGvis({
    dat = sustainabilityCore()
    #dat$x = log(dat$energy)
    #dat$y = log(dat$emission)
    dat = dat[dat$energy < 4e9 & dat$emission < 6e5,]
    co2_Bubble <- gvisBubbleChart(dat, xvar = 'energy', yvar = 'emission', colorvar = 'building',
                                  sizevar = 'N',
                                  options=list(
                                    legend = 'none',
                                    explorer="{actions: ['dragToZoom', 
                                    'rightClickToReset'],
                                    maxZoomIn:0.05}",
                                    vAxis="{title:'CO2 Emission',minValue:-2e4}",
                                    hAxis="{title:'Energy Consumption',textPosition:'out',minValue:-5e7}",
                                    title="Correlations between energy consumption, CO2 emission and Departments",
                                    width=850, height=725, bubble.opacity=0.5,
                                    bubble = "{textStyle:{color:'none'}}"))
    
    co2_Bubble
})
  
  
  #Fleet Program
  output$FleetDepartmentSelector <- renderUI({
    selectizeInput("FleetDepartmentSelectorUnit", "Select the Department: ",
                   choices = unique(fleet$Agency), selected = "Department of General Services",multiple = TRUE)
  })
  
  FleetDepartmentCore <- eventReactive(input$FleetDepartmentButton, {
    fleetdepartment_names = unique(as.character(fleet$Agency))
    if(input$FleetDepartmentSelectorUnit %in% fleetdepartment_names){
      mySubset = input$FleetDepartmentSelectorUnit
      return(mySubset)
    }
  })
  
  output$FleetBarplot1 <- renderPlotly({
    #detach("package:plyr", unload=TRUE)
    mySubset = FleetDepartmentCore()
    bar_Active_df <- fleet %>% 
      select(Agency,Equipment.Number,Disposed) %>% 
      group_by(Agency,Disposed) %>% 
      summarise(CountOfVeh = n()) %>% 
      filter(Disposed == 'No')
    
    bar_Disposed_df  <- fleet %>% 
      select(Agency,Equipment.Number,Disposed) %>% 
      group_by(Agency,Disposed) %>% 
      summarise(CountOfVeh = n()) %>% 
      filter(Disposed == 'Yes')
    
    bar_Active_df %>% filter(Agency %in% mySubset) %>% 
      plot_ly(x = Agency, y = CountOfVeh, name = "Active Assests", type = "bar",filename='newInventory-bar') %>%
      add_trace(x = Agency, y = CountOfVeh, name = "Disposed Assets", data = filter(bar_Disposed_df,Agency %in% mySubset)) %>%
      layout(xaxis = list(title="Agency Name"), yaxis = list(title="Number of Assets"), barmode = "group", 
             title='Number of Assets by Agencies')
  })
  
  output$FleetLineChart <- renderPlotly({
    linedata <- fleet %>% select(Equipment.Number, Report.Year, Disposed) %>% 
      group_by(Report.Year,Disposed) %>% 
      summarise(CountOfVeh = n()) 
    
    active_line <- linedata %>% filter(Disposed == 'Yes')
    disposed_line <- linedata %>% filter(Disposed == 'No')
    
    active_line %>% plot_ly(x=Report.Year,y=CountOfVeh,name = "Active Assests", type = 'line')%>%
      add_trace(x = Report.Year, y = CountOfVeh, name = "Disposed Assets", data = disposed_line ) %>%
      layout(xaxis = list(title="Agency Name"), yaxis = list(title="Number of Assets"), 
             title='Number of Assets across years')
  })
  
  output$FleetBarFuelType <- renderPlotly({
    bardata <- fleet %>% select(Equipment.Number,Fuel.Type, Disposed) %>%
      filter(Disposed=="No") %>% group_by(Fuel.Type) %>% 
      summarise(CountOfVeh = n())
    
    bardata %>% plot_ly(x=Fuel.Type,y=CountOfVeh, type = 'bar')%>%
      layout(xaxis = list(title="Fuel Type"), yaxis = list(title="Number of Assets"), 
             title='Number of Active Assets by Fuel Types')
  })
  
  output$FleetBarEVType <- renderPlotly({
    bardata <- fleet %>% select(Equipment.Number,Fuel.Type, Disposed,Report.Year) %>%
      filter(Disposed=="No") %>% group_by(Fuel.Type,Report.Year) %>% 
      summarise(CountOfVeh = n_distinct(Equipment.Number)) %>% filter(Fuel.Type!='N/A') %>% 
      mutate(isEV = Fuel.Type %in% c("EVC")) %>% 
      group_by(Report.Year,isEV) %>% 
      summarise(Count = sum(CountOfVeh))
    bardata$fuel = rep(c('Traditional Vehicle','Electric Vehicle'),4)
    
    bardata %>% plot_ly(x=as.factor(Report.Year),y=Count, type = 'bar', color=fuel)%>%
      layout(xaxis = list(title="Year"), yaxis = list(title="Count"),
             title='Number of EVs in active vehicles')
  })
  
  output$FleetBarAcqType <- renderPlotly({
    bardata <- fleet %>% select(Equipment.Number, Acquisition.Method.Reason, Report.Year,Disposed) %>%
      filter(Disposed=="No") %>%
      group_by(Report.Year,Acquisition.Method.Reason) %>% 
      summarise(CountOfVeh = n_distinct(Equipment.Number)) %>% 
      filter(Acquisition.Method.Reason != '')
    
    bardata %>% plot_ly(x=as.factor(Report.Year),y=CountOfVeh, type = 'bar', color=Acquisition.Method.Reason)%>%
      layout(xaxis = list(title="Year"), yaxis = list(title="Count"), barmode = "stack",
             title='Distribution of New Assets through Acquisition ')
  })
  
  output$table1 <- renderDataTable({
    return(regression)
  })
  
  output$tablekey <- renderTable({
    return(four_way_key)
  })
  
  
  output$AgencySelector <- renderUI ({
    selectizeInput("AgencySelectorUnit", "Select an Agency: ",
                   choices = unique(sabrc$AgencyName), selected="Department of General Services")
  })
  getAgency <- eventReactive(input$wasteButton, {
    mySubset = sabrc[sabrc$AgencyName == input$AgencySelectorUnit & as.character(sabrc$ReportYear) == input$Year, ]
    return(mySubset)
  })
  output$wastePlot <- renderChart({
    mySubset = getAgency()
    p <- nPlot(Value~SABRCCategory, data=mySubset, group='ValueType', type='multiBarChart', dom="wastePlot")
    p$chart(forceY = c(0))
    p$params$height = 300
    return(p)
  })
  
  output$regressionSelector <- renderUI({
    checkboxGroupInput("Xs", "Regress total CO2 emission on: ",
                       choices=names(regression)[-c(1:3)],
                       selected = names(regression)[-c(1:3)])
  })
  
  runRegression <- reactive({
    lm(as.formula(paste0("CO2e ~ ", paste(input$Xs,collapse="+"))),data=regression)
  })
    
  output$regTab <- renderPrint({
    #summary(runRegression())$coefficients
    summary(runRegression())
    
  })
  
  output$map <- renderLeaflet({
    #leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17) %>% 
    # addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')
    test <- na.omit(energyEVDisMap) %>% mutate(identifier = paste0(GeoLat,GeoLon))
    test2 <- test[!duplicated(test$identifier),]
    
    leaflet(test2,padding = c(0,0,0,100)) %>%
      addTiles() %>%
      addMarkers(lng = ~GeoLon, lat = ~GeoLat, #weight = 2,
                 #radius = test2$SumofFuel+1000, #~sqrt(SumofFuel) * 100000
                 popup = ~paste(sep = "<br/>", paste0("<strong>", building,"</strong>"),
                                paste0("Zipcode: ", Postal.Code),
                                paste0("Vehicles: ", CountOfVeh), paste0("EVs: ", CountofEV),
                                paste0("Sum of Fuel: ", SumofFuel), paste0("Average MPG: ", AverageMPG),
                                paste0("Water Used: ",Water.Use..All.Water.Sources...kgal.),
                                paste0("Electricity Used: ",Electricity.Use...Grid.Purchase..kWh.),
                                paste0("Energy star score: ",ENERGY.STAR.Score)))
  })
})