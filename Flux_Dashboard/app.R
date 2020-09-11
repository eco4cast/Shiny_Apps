#Call Packages
library(shiny)
library(shinydashboard)
library(gganimate)
library(plotly)
library(tidyverse)
library(rpart)
library(rattle)
library(leaflet)
library(htmltools)

#Create Data
df <- read.csv(textConnection(
  "Name,Lat,Long, Abv
  Willow Creek (WCr),45.8059,-90.07961, WCr
  Potato,44.1394,-89.5727, Potato
  Park Falls (WLEF),45.9459,-90.2723, WLEF
  Sylvania Wilderness Area, 46.242017, -89.347567, Syv
  Lost Creek (Los), 46.0765, -89.9742, Los
  Harvard Forest, 42.5378, -72.1715, Harvard"))
#load in data for first run
load("/srv/shiny-server/Flux_Dashboard/data/WCr.RData")



frame_end = Sys.Date() + lubridate::days(16)
frame_start = Sys.Date() - lubridate::days(10)

ftime = seq(as.Date(frame_start), as.Date(frame_end), by="days")
ctime = seq(as.Date(Sys.Date() - lubridate::days(10)), Sys.Date(), by = "days")


# Define UI for application that draws a histogram
#"Willow Creek" = "WCr", 
ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Flux Dashboard"),
                     
                     dashboardSidebar(
                       selectInput("site", h3("Select Site:"), 
                                   choices = c("Willow Creek" = "WCr", "Potato" = "Potato", "Park Falls" = "WLEF", "Sylvania" = "Syv", "Lost Creek" = "Los", "Harvard Forest" = "Harvard")), 
                       sidebarMenu(
                         menuItem("Site Map", tabName = "sites", icon = icon("map")),
                         menuItem("Forecast", tabName = "ft", icon = icon("tree")), 
                         menuItem("Model Performance", tabName = "modelperformance", icon = icon("chart-area")),
                         menuItem("Model by Start Date", tabName = "start", icon = icon("calendar")), 
                         menuItem("NOAA GEFS", tabName = "GEFS", icon = icon("cloud"))
                       )
                     ),
                     
                     dashboardBody(
                       tabItems(
                         #Map Tab
                         tabItem(tabName = "sites",
                                 leafletOutput("m")
                         ), 
                         #Forecast Tab 
                         tabItem(tabName = "ft", 
                                 fluidRow(
                                   plotlyOutput('neeplot')
                                 ),
                                 fluidRow(
                                   plotlyOutput("leplot")
                                 )
                                 #, 
                                 #fluidRow(
                                 #if(s != "NA"){plotlyOutput('soilplot')}else{h5("Soil Moisture not available for this site")}
                                 #)
                         ),
                         #Model Performance
                         tabItem(tabName = "modelperformance", 
                                 fluidRow(
                                   selectInput("var", "MET Variable:", 
                                               choices = c("Air Temp" = 'Tair', "Relative Humidity" = 'rH'), 
                                               multiple = TRUE),
                                   
                                   selectInput(inputId = "fdate", label="Forecast Horizon Date:", choices = c(rev(ctime[5:10])))
                                 ),
                                 fluidRow(plotOutput("reg")), 
                                 fluidRow(plotOutput("horiz"))
                                 
                         ),
                         #Start Date Plots
                         tabItem(tabName = "start", 
                                 fluidRow(
                                   selectInput(inputId = "date", label="Forecast Date:", choices = c(ctime[-c((length(ctime)-1),length(ctime))]))
                                 ),
                                 h4("Plot of 16-day NEE Forecast"),
                                 fluidRow(plotOutput("nee_start")), 
                                 h4("Plot of Predictions vs Observed for NEE"), 
                                 fluidRow(plotOutput('nee_scat')), 
                                 h4("Plot of Error by Hour for NEE"), 
                                 fluidRow(plotOutput('nee_hour')), 
                                 h4("Plot of 16-day LE Forecast"),
                                 fluidRow(plotOutput("le_start")), 
                                 h4("Plot of Predictions vs Observed for LE"), 
                                 fluidRow(plotOutput('le_scat')), 
                                 h4("Plot of Error by Hour for LE"),
                                 fluidRow(plotOutput('le_hour'))
                         ), 
                         #NOAA GEFS
                         tabItem(tabName = "GEFS", 
                                 fluidRow(plotlyOutput("tair")), 
                                 fluidRow(plotlyOutput("rh")), 
                                 fluidRow(plotlyOutput("rain")),
                                 fluidRow(plotlyOutput("WS")),
                                 fluidRow(plotlyOutput("SR")),
                                 fluidRow(plotlyOutput("LR"))
                         )
                       )
                     )
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Create leaflet map of sites
  output$m <- renderLeaflet({ 
    leaflet(df) %>%
      addTiles() %>%
      addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))
  })
  
  
  #Observe a click for input site - changes data download, creates new graphs  
  observeEvent(input$site,{
    rm(list = ls())
    load(paste0('/srv/shiny-server/Flux_Dashboard/data/', input$site, ".RData"))
    nee <<- p
    le <<- q 
    #soil <<- s 
    nee.met <<- nee.met
    nee.data <<- nee.data
    le.data <<- le.data
    #soil.data <<- soil.data
    forecasted_data <<- forecasted_data
    
    
    
    output$neeplot <- renderPlotly({
      
      ggplot.nee<-ggplotly(nee, tooltip = 'all') %>% 
        animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
        animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
        animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
        layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
        layout(showlegend = T, margin = c(30,50,30,50)) 
      
      ggplot.nee$x$data[[1]]$name <-"95% Confidence Interval"
      ggplot.nee$x$data[[2]]$name <- "Observed Data"
      ggplot.nee$x$data[[3]]$name <- "Predicted Mean"
      
      ggplot.nee
    })  
    
    
    output$leplot <- renderPlotly({
      
      ggplot.le<-ggplotly(le, tooltip = 'all', layerData = 2) %>% 
        animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
        animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
        animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
        layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
        layout(showlegend = T, margin = c(30,50,30,50)) 
      
      ggplot.le$x$data[[1]]$name <-"95% Confidence Interval"
      ggplot.le$x$data[[2]]$name <- "Observed Data"
      ggplot.le$x$data[[3]]$name <- "Predicted Mean"
      
      ggplot.le
    })
    
    #    output$soilplot <- renderPlotly({
    #        
    #        
    #       ggplot.soil<-ggplotly(soil, tooltip = 'all', layerData = 2) %>% 
    #            animation_opts(frame = 1200, easing = 'linear-in', transition = 0, redraw = F, mode = "next") %>% 
    #            animation_slider(x = 0, y = -0.1, visible = T, currentvalue = list(prefix = "Forecast Date:", font = list(color = 'black'))) %>%
    #            animation_button(x = 0, xanchor = "left", y = 1.5, yanchor= "top") %>% 
    #            layout(legend = list(orientation = "h", x = 0.25, y = 1.1)) %>% 
    #            layout(showlegend = T, margin = c(30,50,30,50)) 
    #       ggplot.soil$x$data[[1]]$name <-"95% Confidence Interval"
    #       ggplot.soil$x$data[[2]]$name <- "Observed Data"
    #       ggplot.soil$x$data[[3]]$name <- "Predicted Mean"
    #       
    #       ggplot.soil
    #        
    #    })
  })
  
  
  #Create Regression Tree of Error for Model Performance Tab 
  output$reg <- renderPlot({
    
    if(length(input$var) >= 1){
      
      tree.df <- nee.met %>% dplyr::select(input$var, error)
      mod <- rpart(error ~ ., data = tree.df, method = 'anova')
      fancyRpartPlot(mod, sub = "")}
  })
  #Forecast Horizon for Model Performance Tab
  output$horiz <- renderPlot({
    date <- lubridate::force_tz(as.POSIXct(paste(input$fdate, "12", sep = " "), format = "%Y-%m-%d %H"), "UTC")
    sub.nee <- nee.data[which(nee.data$Time == date),] 
    
    x.breaks = sub.nee$start_date
    labels = rev(seq(from = 1, to = length(x.breaks), by = 1))
    
    
    ggplot(sub.nee, aes(group = 1)) + 
      geom_ribbon(aes(x = start_date, ymin = Lower, ymax = Upper, fill="95% Confidence Interval"), alpha = 0.4) + 
      geom_line(aes(x = start_date, y = Predicted, color = "Predicted")) + 
      geom_line(aes(x = start_date, y = NEE, color = "Observed Data"), size = 1) +  
      ggtitle(paste0("Forecast Horizon for ", input$fdate, ' for ', input$site)) +
      scale_color_manual(name = "Legend", labels = c("Observed Data", "Predicted"), values=c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")) +
      scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) +
      scale_y_continuous(name="NEE (kg C m-2 s-1)") + 
      scale_x_discrete(name = "Days from Observed Date", breaks = x.breaks, labels = labels) + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size = 12), legend.title = element_blank(), legend.text = element_text(size = 10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 12)) 
    
  })
  
  #Model By Start Date Plots 
  #16 day forecast
  output$nee_start<- renderPlot({
    
    sub.nee <- nee.data[which(as.character(nee.data$start_date) == input$date),]
    sub.nee <- sub.nee %>% filter(date < Sys.Date()) #%>% mutate(Time = as.factor(Time))
    
    ggplot(sub.nee, aes(group = 1)) +
      geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
      geom_line(aes(x = Time, y = Predicted, color = "Predicted"), size = 1) +
      geom_line(aes(x = Time, y = NEE, color = "Observed")) + 
      ggtitle(paste0("NEE from ", input$date, " forecast")) +
      scale_color_manual(labels = c("Observed", "Predicted"), values=c("Observed" = "firebrick4", "Predicted" = "skyblue1")) +
      scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) + 
      scale_y_continuous(name="NEE (kg C m-2 s-1)") + 
      #scale_x_discrete(name = "Time", breaks = x.breaks, labels = format(x.breaks, "%m-%d")) + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 
    
  }) 
  
  #Scatted
  output$nee_scat <- renderPlot({
    
    #subset data by start date    
    sub.nee <- subset(nee.data, nee.data$start_date == input$date)
    sub.nee <- sub.nee %>% filter(date < Sys.Date()) %>% mutate(Time = as.factor(Time))
    
    E = sub.nee$Predicted
    O = sub.nee$NEE
    all = c(E, O)
    RMSE = sqrt(mean((E-O)^2, na.rm = T))
    Bias = mean(E-O, na.rm = T)
    
    #Predicted vs Observed Scatter + 1:1 line + regression
    par(pty="s") 
    plot(E, O, pch = 19, cex = 0.7,  ylab = "Observed", xlab = "Predicted", 
         xlim = c(min(all, na.rm = T), max(all, na.rm = T)), 
         ylim = c(min(all, na.rm = T), max(all, na.rm = T)),
         main = paste0(unique(sub.nee$start_date), " forecast"), asp = 1)
    abline(0,1, col = 'darkgrey', lwd = 3)
    #abline(NEE.fit, col = "lightgrey", lwd = 2, lty = 2)
    legend("bottomleft",legend=c('obs','1:1'),col= c('black', "darkgrey"),lwd=3, cex = 0.8)
    mtext(text = paste0("RMSE = ", formatC(RMSE, format = "e", digits = 2)), 
          side = 3, line = 0, adj = -0.01, cex = 0.8) 
    
    
  })
  
  #NEE Error by Hour
  output$nee_hour <- renderPlot({
    sub.nee <- nee.data[which(as.character(nee.data$start_date) == input$date),]
    sub.nee <- sub.nee %>% filter(date < Sys.Date()) %>% mutate(Time = as.factor(Time))
    sub.nee$Hour <- as.factor(lubridate::hour(sub.nee$Time))
    sub.nee$Time = lubridate::force_tz(as.POSIXct(sub.nee$Time), tz = "UTC")
    
    
    ggplot(sub.nee, aes(x = Time, y = error, group = 1)) + 
      geom_point(aes(color = Hour), size = 3) + 
      geom_hline(yintercept = 0, color = "black") + 
      xlab("Date") + 
      scale_y_continuous(name = "NEE Error (kg C m-2 s-1)") + 
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14), legend.title = element_text(size = 14), legend.text = element_text(size = 12))
    
    
  })  
  
  #LE Forecast
  
  output$le_start <- renderPlot({
    
    sub.le <- le.data[which(as.character(le.data$start_date) == input$date),]
    sub.le <- sub.le %>% filter(date < Sys.Date()) #%>% mutate(Time = as.factor(Time))
    
    ggplot(sub.le, aes(group = 1)) +
      geom_ribbon(aes(x = Time, ymin=Lower, ymax=Upper, fill="95% Confidence Interval"), alpha = 0.4)  + 
      geom_line(aes(x = Time, y = Predicted, color = "Predicted"), size = 1) +
      geom_line(aes(x = Time, y = LE, color = "Observed")) + 
      ggtitle(paste0("LE from ", input$date, " forecast")) +
      scale_color_manual(labels = c("Observed", "Predicted"), values=c("Observed" = "firebrick4", "Predicted" = "skyblue1")) +
      scale_fill_manual(labels = c("95% Confidence Interval"), values=c("95% Confidence Interval" = "blue1")) + 
      scale_y_continuous(name="LE (W m-2 s-1)") + 
      #scale_x_discrete(name = "Time", breaks = x.breaks, labels = format(lab[-length(lab)], "%m-%d")) + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size = 16), legend.title = element_blank(), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 16)) 
    
  }) 
  
  #Pred vs Observed LE
  
  output$le_scat <- renderPlot({
    
    #subset data by start date    
    sub.le <- le.data[which(as.character(le.data$start_date) == input$date),]
    sub.le <- sub.le %>% filter(date < Sys.Date()) %>% mutate(Time = as.factor(Time))
    sub.le$Hour <- as.factor(lubridate::hour(sub.le$Time))
    sub.le$Time = lubridate::force_tz(as.POSIXct(sub.le$Time), tz = "UTC")
    
    
    E = sub.le$Predicted
    O = sub.le$LE
    all = c(E, O)
    RMSE = sqrt(mean((E-O)^2, na.rm = T))
    Bias = mean(E-O, na.rm = T)
    
    #Predicted vs Observed Scatter + 1:1 line + regression
    par(pty="s") 
    plot(E, O, pch = 19, cex = 0.7,  ylab = "Observed", xlab = "Predicted", 
         xlim = c(min(all, na.rm = T),30), 
         ylim = c(min(all, na.rm = T), 30),
         main = paste0(unique(sub.le$start_date), " forecast"), asp = 1)
    abline(0,1, col = 'darkgrey', lwd = 3)
    #abline(NEE.fit, col = "lightgrey", lwd = 2, lty = 2)
    legend("topright",legend=c('obs','1:1'),col= c('black', "darkgrey"),lwd=3, cex = 0.8)
    mtext(text = paste0("RMSE = ", formatC(RMSE, format = "e", digits = 2)), 
          side = 3, line = 0, adj = -0.01, cex = 0.8) 
    
  })
  
  #LE Error by hour
  output$le_hour <- renderPlot({
    sub.le <- le.data[which(as.character(le.data$start_date) == input$date),]
    sub.le <- sub.le %>% filter(date < Sys.Date()) %>% mutate(Time = as.factor(Time))
    sub.le$Hour <- as.factor(lubridate::hour(sub.le$Time))
    sub.le$Time = lubridate::force_tz(as.POSIXct(sub.le$Time), tz = "UTC")
    
    ggplot(sub.le, aes(x = Time, y = error, group = 1)) + 
      geom_point(aes(color = Hour), size = 3) + 
      geom_hline(yintercept = 0, color = "black") + 
      xlab("Date") + 
      scale_y_continuous(name = "LE Error (kg C m-2 s-1)") + 
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14), legend.title = element_text(size = 14), legend.text = element_text(size = 12))
    
    
    
  })  
  
  #Graphs for NOAA GEFS Tab  
  
  output$tair <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = Tair_C, x = timestamp, group = ensemble)) + 
                                         geom_line(aes(y = Tair_C, x = timestamp, color = ensemble))+ 
                                         theme(legend.position = 'none') + 
                                         ggtitle(paste0("GEFS Air Temp Forecast from ", Sys.Date())))
  )
  output$rh <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = Qair, x = timestamp, group = ensemble)) + 
                                       geom_line(aes(y = Qair, x = timestamp, color = ensemble))+ 
                                       theme(legend.position = 'none') + 
                                       ggtitle(paste0("GEFS Specific Humidity Forecast from ", Sys.Date())))
  )
  output$rain <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = Rain, x = timestamp, group = ensemble)) + 
                                         geom_line(aes(y = Rain, x = timestamp, color = ensemble))+ 
                                         theme(legend.position = 'none') + 
                                         ggtitle(paste0("GEFS Rain Forecast from ", Sys.Date())))
  )
  output$WS <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = ws, x = timestamp, group = ensemble)) + 
                                       geom_line(aes(y = ws, x = timestamp, color = ensemble))+ 
                                       theme(legend.position = 'none') + 
                                       ggtitle(paste0("GEFS Wind Speed Forecast from ", Sys.Date())))
  )
  output$SR <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = SW, x = timestamp, group = ensemble)) + 
                                       geom_line(aes(y = SW, x = timestamp, color = ensemble))+ 
                                       theme(legend.position = 'none') + 
                                       ggtitle(paste0("GEFS Shortwave Radiation Forecast from ", Sys.Date())))
  )
  output$LR <- renderPlotly(ggplotly(ggplot(forecasted_data, aes(y = LW, x = timestamp, group = ensemble)) + 
                                       geom_line(aes(y = LW, x = timestamp, color = ensemble))+ 
                                       theme(legend.position = 'none') + 
                                       ggtitle(paste0("GEFS Longwave Radiation Forecast from ", Sys.Date())))
  )
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
