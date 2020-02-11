# sample R + Shiny example for CS 424 Spring 2020 UIC - Andy Johnson
# Project by Syed Hadi, Shadi2@UIC.EDU
#Done
#
#libraries to include/ Not all the Libraries are being used

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(stringr)

################################################################################################### Data Wrangling

#importing dataset
dataset<-read.csv("litterati challenge-65.csv")

#total number of trash thrown 
totalLitter<-nrow(dataset)

#Tagging all items that do not have a tag with "Untagged"
dataset$tags[dataset$tags==""] <- NA
dataset$tags <- as.character(dataset$tags)
dataset$tags[is.na(dataset$tags)] <- "Untagged"

#Getting the time in the right format (Chicago time)
dataset$Time <- format(as.POSIXct(dataset$litterTimestamp),format = "%H", tz =  "America/Chicago")
dataset$Date <- as.Date(dataset$litterTimestamp)
dataset$DoW <- wday(dataset$Date, label=TRUE)

#Removing all the outlier coordinates
dataset<-dataset[dataset$lat > 41 & dataset$lat < 42,]
dataset<-dataset[dataset$lon > -88 & dataset$lon < -86,]
longitude <- subset(dataset, select ="lon")
latitude <- subset(dataset, select ="lat")

#Cleaning up the tags
dataset$Newtags <- strsplit(dataset$tags,",")

#Cleaning up user names
literrati <- "(litterati-)"
dataset$NewUsername <- str_replace(dataset$username, literrati, "")

#Getting the top ten pickers and dates and hours
NamesCol<-subset(dataset, select = "NewUsername")
tempName <- as.data.frame(sort(table(NamesCol),decreasing=T))
topTen <- head(tempName,10)
DateGraph<- as.data.frame(table(subset(dataset, select ="Date")))



Time <- as.data.frame(table(subset(dataset, select ="Time")))
DayOfTheWeek <-  as.data.frame(table(subset(dataset, select ="DoW")))
HourGraph<- as.data.frame(table(subset(dataset, select ="Time")))


tempTime1 <-subset(dataset, select = "Date")
tempTime <- as.data.frame(table(tempTime1))

tempDayOfTheWeek1 <-subset(dataset, select = "DoW")
tempDayOfTheWeek <- as.data.frame(tempDayOfTheWeek1)

tempHourGraph <- subset(dataset, select = "Time")
#Getting the top ten tags 

tempTag <- subset(dataset, select = "tags")
splitTag <- strsplit(tempTag$tags,",")
unlist <- unlist(splitTag)
tempTag2 <- as.data.frame(sort(table(unlist),decreasing=T))
topTenTag <- head(tempTag2, 10)



################################################################################################### UI

ui <- dashboardPage(
    dashboardHeader(title = "Project 1 - No Time to Waste by Syed Hadi"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,

                     sidebarMenu(
                       menuItem("Project by Syed Hadi", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("Data Source: litterati.org", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("Libraries used: ggplot,lubridate,", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("shiny,leaflet,stringr", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     
                     selectInput("Year", "Choose a name from the top 10", topTen$NamesCol, selected = "julieta"),
                     selectInput("Room", "Choose a tag from the top 10", topTenTag$unlist, selected = "Plastic")
                     ),
    dashboardBody(
  fluidRow(
    column(3,
        fluidRow(
          box( status = "primary", width = 12,
              plotOutput("jpeg", height = 300)
          )
    ),
        fluidRow(
          valueBoxOutput("count", width = 12)
          ),       
    fluidRow(
            box(title = "Litter Picked Up by Tag", solidHeader = TRUE, status = "primary", width = 12,
                tableOutput("tabTag2")
            )
          )
    ),
    
    column(3,
       fluidRow(
            box( title = "Pieces Picked up by Tag", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("barTag", height = 216)
            )
        ), 

       fluidRow(
         box(title = "Location of Litter Picked", solidHeader = TRUE, status = "primary", width = 12,
             leafletOutput("leaf", height = 216)
         )
       ),
       fluidRow(
         box(title = "List of Top Ten Pickers", solidHeader = TRUE, status = "primary", width = 12,
             dataTableOutput("tab1", height = 216)
         )
       )
   ),
   
   column(3,
          fluidRow(
            box( title = "Total Amount of Litter Picked Up", solidHeader = TRUE, status = "primary", width = 12,
                 plotOutput("barTotal", height = 216)
            )
          )
       ,
        fluidRow(
             box(title = "Litter Picked Up by Week", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("barDOW", height = 216)
                )
            ),
        fluidRow(
             box(title = "Litter Picked Up by Hour", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("barHour", height = 216)
                )
             )
        ),
   column(3,
          fluidRow(
            box(title = "Litter Picked Up by Week", solidHeader = TRUE, status = "primary", width = 12,
               tableOutput("tabWeek")
            )
          ),
          fluidRow(
            box(title = "Litter Picked Up by Hour", solidHeader = TRUE, status = "primary", width = 12,
               tableOutput("tabHour")
            )
        )
   )
    )
))

################################################################################################### Server

server <- function(input, output) {

#calculating values once to use elsewhere in the code   
  #userReactive <-  reactive({subset(dataset, (topTen$NamesCol) == input$user)})
  
  
# increase the default font size
theme_set(theme_dark(base_size = 18)) 

# calculate the values one time and re-use them in multiple charts to speed things up
justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})

# this is the count of total litter 
output$count <- renderValueBox({
  valueBox(
    value = totalLitter,
    subtitle = "Total Litter Collected",
    icon = icon("users")
  )
})

#This is the bar chart for days of the week
output$barTag <- renderPlot({
  #user <- userReactive()
  ggplot(data=topTenTag, aes(x=topTenTag$unlist , y=Freq)) +
    xlab("Type of litter") + ylab("Litter Picked")+
    geom_bar(stat="identity", fill= "steelblue", color = "Black")+
    theme_minimal()
})

#This is the bar chart for days of the week
output$barDOW <- renderPlot({
  #user <- userReactive()
  ggplot(data=DayOfTheWeek, aes(x=Var1 , y=Freq)) +
    xlab("Days of the week") + ylab("Litter Picked")+
    geom_bar(stat="identity", fill= "steelblue", color = "Black")+
    theme_minimal()
})

output$barHour <- renderPlot({
  ggplot(data=HourGraph, aes(x=Var1 , y=Freq)) +
    xlab("Hour") + ylab("Litter Picked")+
    geom_bar(stat="identity", fill= "steelblue", color = "Black")+
    theme_minimal()
})

output$barTotal <- renderPlot({
  ggplot(data=DateGraph, aes(x=Var1 , y=Freq)) +
    xlab("Days") + ylab("Litter Picked")+
    geom_bar(stat="identity", fill= "steelblue", color = "Black")+
    theme_minimal()
})

# use DT to help out with the tables - https://datatables.net/reference/option/
output$tab1 <- DT::renderDataTable(
    DT::datatable( head(tempName,10), 
  options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    )
)


output$tabTag2<- renderTable(topTenTag)
output$tabHour<- renderTable(HourGraph)
output$tabWeek<- renderTable(DayOfTheWeek)

# read in a jpeg map of the lab to show the room layout and plot some text on it
output$jpeg <- renderPlot({
    # read in a jpg image
    jp <- jpeg::readJPEG('unnamed.jpg')
    df <- data.frame(x = 1:10, y = 1:10) # set the range to be 1 to 10 in x and y for the image
    markerX = 0
    markerY = 0
    ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") +
        annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf)  +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
})

# add a leaflet map and put a marker on it at the location of the lab
output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.81282	, lat = 41.86555, zoom = 8)
    map <- addMarkers(map, lng = longitude$lon, lat = latitude$lat, popup = "Litter", clusterOptions = markerClusterOptions())
    map
})

}

shinyApp(ui = ui, server = server)
