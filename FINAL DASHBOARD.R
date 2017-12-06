library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggmap)
library(rgdal)
library(tidyverse)
library(leaflet)
library(sp)
library(shinydashboard)
library(graphics)
library(knitr)
library(readxl)

count_tract1=read.csv("Dashboard-16+17CT_LACity.csv")

#### read data #### 
SD_LA = read.csv("SDnew.csv")

count_tract = read.csv("homelesstract.csv")

call = read.csv("311_calls_w_CTs20171102134828.csv")
names(call)<-tolower(names(call))

crime = read.csv("crime_w_CTs20171102134814.csv")
names(crime)<-tolower(names(crime))

shelter = read.csv("shelters_w_CTs20171102134808.csv")
names(shelter)<-tolower(names(shelter))

ctmap_shp = readOGR(dsn = "raw_data/CENSUS_TRACTS_2010.zip_unzipped",
                    layer = "CENSUS_TRACTS_2010")


#### Clean shapefile ####

# convert UTM to LongLat
ctmap_shp <- spTransform(ctmap_shp, CRS("+init=epsg:4326"))

# 'ctmap_shp_data' contains census tract id
ctmap_shp_data = ctmap_shp@data
ctmap_shp_data$id = rownames(ctmap_shp_data)
ctmap_shp_data$CT10 = as.numeric(as.character(ctmap_shp_data$CT10))


#### merge shp + PIT count data ####
ctmap_LA = fortify(ctmap_shp) %>%
  left_join(ctmap_shp_data, by = "id") %>%
  left_join(count_tract, by = c("CT10" = "tract")) %>%
  filter(LACity ==1) %>%
  filter(CT10 != "930401")


###filter shelter to LA###
shelter_LA = shelter %>%
  filter(city == "Los Angeles") %>%
  filter(ct10 != "930401")


#LA only shp file
ctmap_LA_shp = subset(ctmap_shp, CT10 %in% ctmap_LA$CT10)


###Prepare for the final shp file###

tract_num= count_tract %>%
  filter(LACity == 1) %>%
  filter(tract != "930401")

crime_num = crime %>%
  filter(ct10 != "930401") %>%
  group_by(ct10) %>%
  summarise(crimenum = n())

shelter_num = shelter_LA %>%
  filter(ct10 != "930401") %>%
  group_by(ct10) %>%
  summarise(shelternum = n())

call_num = call %>%
  filter(ct10 != "930401") %>%
  group_by(ct10) %>%
  summarise(callnum = n())

LA_ct_all = tract_num %>%
  left_join(crime_num, by = c("tract" = "ct10")) %>%
  left_join(shelter_num, by = c("tract" = "ct10")) %>%
  left_join(call_num, by = c("tract" = "ct10"))

ctmap_LA_all_shp = merge(ctmap_LA_shp, LA_ct_all, by.x = "CT10", by.y = "tract")


###preparation for the code ###

#color 
#shelter icon
ShelterIcon <- makeIcon(
  iconUrl = "https://www.shareicon.net/data/2016/07/23/800460_house_512x512.png",
  iconWidth = 15, iconHeight = 20)



bins <- c(0,100,200,300,400,500,600, Inf)
pal <- colorBin(palette = c("#1a9850","#91cf60","#d9ef8b","#ffffbf","#fee08b","#fc8d59","#990000"),domain = NULL, bins = bins)

###Prepare for the final shp file###

ctmap_LA_sd= merge(ctmap_LA_shp, SD_LA, by.x = "CT10", by.y = "tract")


#color 

bins2 <- c(-600,-400,-200,0,200,400,600, Inf)
pal2 <- colorBin(palette = c("#990000","#fc8d59","#fdd49e","#a8ddb5","#7bccc4","#0570b0","#034e7b"),domain = NULL, bins = bins2)


Crimetime = read.csv("Dashboard-crime.csv")

newcalls=read.csv("Dashboard-311.csv")
names(newcalls)<-tolower(names(newcalls))

crime2 = Crimetime %>%
  mutate(mth_yr = paste(sep = "/",as.character(Crimetime$Month),as.character(Crimetime$Year)),
         yr_mth = paste(sep = "",as.character(Crimetime$Year),as.character(Crimetime$Month))) %>%
  filter(yr_mth != 20168)

crime2$mth_yr = as.factor(crime2$mth_yr)
levels(crime2$mth_yr) = unique(crime2$mth_yr)

ui <- navbarPage(
  "Homelessness Dashboard for City of Los Angeles", id="nav",
  
  tabPanel("Overview Map",
           
           leafletOutput("map")#, width = "100%", height = "100%")
             
           ),
  tabPanel("Shelter Availability",
           leafletOutput("map2"))
  
  ,tabPanel("Timeline Charts",
           titlePanel("Timeline View"), 
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput(inputId = "cc", 
                           label = "Choose your focus",
                           choices = list ("Crime","311 Calls"
                           ), 
                           selected = FALSE),
               uiOutput("crimect"),
               uiOutput("callct")
               
               
               )
             ,
             mainPanel(
               conditionalPanel("input.cc==='Crime'",plotOutput("crimeplot1")),
               conditionalPanel("input.cc==='Crime'",plotOutput("crimeplot2")),
               conditionalPanel("input.cc==='311 Calls'",plotOutput("callplot1")),
               conditionalPanel("input.cc==='311 Calls'",plotOutput("callplot2"))

             )
            ))
  ,

  tabPanel("Details",
           titlePanel("Sheltered and Unsheltered People Distribution"
                      ),
           sidebarLayout(

             sidebarPanel(

           selectInput(inputId = "us",
                       label = "Choose the type of homeless people",
                       choices = list("Sheltered people","Unsheltered people"),
                       selected = FALSE),
           selectInput(inputId = "usct",
                       label = "Choose the tract of homeless people",
                       choices = unique(count_tract1$Tract),
                       selected = "ALL"),
           selectInput(inputId = "year",label = "Choose the Year ",
                       choices = unique(count_tract1$Year)),
                       selected="2017"),

           mainPanel(
             plotOutput(outputId = "usplot")
           ))

  )
           
)



server <- function(input, output, session) {
  
  output$map2=renderLeaflet(
    
    leaflet(ctmap_LA_sd,width = "100%",height = "100%")%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(popup = paste("CT:", ctmap_LA_sd$CT10, "<br>",
                                "Community:", ctmap_LA_sd$Community_Name, "<br>",
                                "Beds:", ctmap_LA_sd$Beds.Available, "<br>",
                                "Unsheltered People:", ctmap_LA_sd$Unsheltered.Homeless.People, "<br>",
                                "Beds Availability:", ctmap_LA_sd$SOD, "<br>"),
                  stroke = TRUE,weight = 0.5,
                  color = ~pal2(SOD),
                  fillOpacity =0.5) %>%
      addLegend("bottomright", 
                pal = pal2, 
                values = ~ SOD,
                title = "Shelter Availability",
                opacity =1) 
  )
  
  output$map = renderLeaflet(
    
    leaflet(ctmap_LA_all_shp,height = "100%",width = "100%")%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        popup = paste("CT:", ctmap_LA_all_shp$CT10, "<br>",
                      "Community:", ctmap_LA_all_shp$Community_Name, "<br>",
                      "Homelss:", ctmap_LA_all_shp$totPeople, "<br>",
                      "Crimes:", ctmap_LA_all_shp$crimenum, "<br>",
                      "Calls:",ctmap_LA_all_shp$callnum, "<br>",
                      "Shelters:", ctmap_LA_all_shp$shelternum, "<br>"),
        stroke = TRUE,weight = 0.5,
        color = pal(ctmap_LA_all_shp$totPeople),
        fillOpacity = 0.4,
        group = "Heat Map: Homeless People") %>%
      addPolygons(popup = paste("CT:", ctmap_LA_all_shp$CT10, "<br>",
                                "Community:", ctmap_LA_all_shp$Community_Name, "<br>",
                                "Homelss:", ctmap_LA_all_shp$totPeople, "<br>",
                                "Crimes:", ctmap_LA_all_shp$crimenum, "<br>",
                                "Calls:",ctmap_LA_all_shp$callnum, "<br>",
                                "Shelters:", ctmap_LA_all_shp$shelternum, "<br>"),
                  stroke = TRUE,weight = 0.5,
                  color = ~pal(ctmap_LA_all_shp$crimenum),
                  fillOpacity = 0.4,
                  group = "Heat Map: Crimes") %>%
      addPolygons(popup = paste("CT:", ctmap_LA_all_shp$CT10, "<br>",
                                "Community:", ctmap_LA_all_shp$Community_Name, "<br>",
                                "Homelss:", ctmap_LA_all_shp$totPeople, "<br>",
                                "Crimes:", ctmap_LA_all_shp$crimenum, "<br>",
                                "Calls:",ctmap_LA_all_shp$callnum, "<br>",
                                "Shelters:", ctmap_LA_all_shp$shelternum, "<br>"),
                  stroke = TRUE,weight = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  color = ~pal(ctmap_LA_all_shp$callnum),
                  fillOpacity = 0.4,
                  group = "Heat Map: Calls") %>%
      addMarkers(data = shelter_LA,
                 lng = ~longitude, lat = ~latitude,
                 popup = paste(shelter_LA$name, "<br>",
                               shelter_LA$url, "<br>",
                               shelter_LA$addrln1, "<br>",
                               "Hours:", shelter_LA$hours),
                 icon = ShelterIcon,
                 group = "shelters") %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ ctmap_LA_all_shp$totPeople,
                title = "Number",
                opacity =1) %>%
      addCircleMarkers(data = crime,
                       lng = ~longitude, lat = ~latitude,
                       popup = crime$ct10,
                       radius = 0.5,
                       color = c("red"),
                       clusterOptions = markerClusterOptions(),
                       opacity = 1,
                       group = "crimes") %>%
      addCircleMarkers(data = call,
                       lng = ~longitude, lat = ~latitude,
                       radius = 0.2,
                       clusterOptions = markerClusterOptions(),
                       group = "calls") %>%
      addLayersControl(
        baseGroups = c("Heat Map: Homeless People", "Heat Map: Crimes", "Heat Map: Calls"),
        overlayGroups = c("shelters", "crimes", "calls"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("crimes", "calls"))
    
  )  
  
  

  output$crimect=renderUI({
    if(input$cc=="Crime")
    {selectInput("ct","Choose the tract", choices=unique(Crimetime$CT10),selected = "ALL")}
    else{selectInput("cct","Choose the tract", choices=unique(newcalls$ct10),selected = "ALL")}
  })
  
  

  output$crimeplot1=renderPlot(
  
    crime2 %>%
      filter(CT10 == input$ct) %>%
      ggplot(aes(x = mth_yr)) +
      geom_bar(fill = "indianred", 
               alpha = 0.7,
               width = 0.5) +
      scale_y_continuous(expand = c(0,0)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "grey"),
            axis.ticks = element_line(color = "grey"),
            aspect.ratio = 0.5,
            axis.text = element_text(size = 8,
                                     color = "grey20"),
            axis.title = element_text(size = 11,
                                      color = "grey20")) +
      labs(x = "Month", y = "Number of Crime",
           title = "Number of Crime by Months")
    
   )
  
  
output$crimeplot2=renderPlot(
    # day of week; filter by CT
    
  
  Crimetime %>%
    filter(CT10 == input$ct) %>%
    ggplot(aes(x = as.factor(Day), y = ..density.., 
               group = as.factor(Year), 
               color = as.factor(Year))) +
    geom_line(stat = "density") +
    scale_color_discrete(name = "Year") +
    theme(panel.background = element_rect(fill = "transparent", 
                                          color = "lightgrey")) +
    scale_x_discrete(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Mon","Tue","Wed",
                                "Thur","Fri","Sat","Sun")) +
    labs(x = "Day of Week", y = "Percent of Crime",
         title = "Number of Crime by Day of Week"))


output$callplot1=renderPlot(

  #### timeline of calls
  # month (filt by tract)
  newcalls %>%
    filter(ct10==input$cct)%>%
    group_by(month)%>%
    summarize(Monthcount=n())%>%
    ggplot(aes(x=month,y= Monthcount)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
    geom_bar(fill = "indianred", 
             alpha = 0.7,
             width = 0.5,
             stat = "identity") +
    ylab("Number of Calls")+
    xlab("Month")+
    ggtitle("Number of Calls by Month")+
    geom_text(aes(label =Monthcount),
              vjust = -0.5, size = 3)+
    theme(panel.background = element_rect(fill = "white",color = "lightgrey"))
)

output$callplot2=renderPlot(
  # day of week (filt by ct)
  newcalls %>%
    filter(ct10==input$cct)%>%
    group_by(day.of.week)%>%
    summarize(Dowcount=n())%>%
    ggplot(aes(x =day.of.week, y =Dowcount)) +
    geom_bar(fill = "indianred", 
             alpha = 0.7,
             width = 0.5,
             stat = "identity") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                       labels = c("Mon","Tue","Wed",
                                  "Thur","Fri","Sat","Sun"))+
    ylab("Number of Calls")+
    xlab("Day of Week")+
    ggtitle("Number of Calls by Day of Week")+
    geom_text(aes(label =Dowcount),
              vjust = -0.5, size = 3)+
    theme(panel.background = element_rect(fill = "white",color = "lightgrey"))
)



output$usplot=renderPlot(

if(input$us=="Sheltered people"){
  count_tract1 %>%
    filter(Year==input$year)%>%
    filter(Tract==input$usct)%>%
    filter(If_Shelter=="Shelter")%>%
    group_by(Resident)%>%
    summarize(Count=sum(value))%>%
    ggplot(aes(x=Resident,y=Count,fill=Resident))+
    geom_bar(stat="identity")+
    xlab("Location Type")+
    ylab("Number of People")+
    scale_fill_manual(values=c("#66c2a5","#d96f02","#a6d854"))+
    theme(axis.text = element_text(size=8),
          axis.title = element_text(size=10),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7),
          plot.title = element_text(size=10)) +
    geom_text(aes(label =Count),    
              vjust = -0.5, size = 3)+
    theme(panel.background = element_rect(fill = "white",color = "lightgrey"))}

else{
  count_tract1 %>%
    filter(Tract==input$usct)%>%
    filter(If_Shelter=="Unshelter")%>%
    group_by(Resident)%>%
    summarize(Count=sum(value))%>%
    ggplot(aes(x=Resident,y=Count,fill=Resident))+
    geom_bar(stat="identity")+
    xlab("Type")+
    ylab("Number of People")+
    scale_fill_manual(values=c("#e66101","#fdb863","#b8e186","#b2abd2","#5e3c99"))+
    theme(axis.text = element_text(size=8),
          axis.title = element_text(size=10),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7),
          plot.title = element_text(size=10)) +
    geom_text(aes(label =Count),    
              vjust = -0.5, size = 3)+
    theme(panel.background = element_rect(fill = "white",color = "lightgrey"))}
)

}

shinyApp(ui, server)

