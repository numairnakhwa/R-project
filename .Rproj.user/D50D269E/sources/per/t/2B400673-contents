#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(devtools)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(skin = "green",
        dashboardHeader(
            title = "Welcome ",
            dropdownMenu(type = "message",
                         messageItem(from="You from Future",message = "Farming is the Future")
                         )
        ),
        dashboardSidebar(
            sidebarMenu(
            menuItem("Crops",icon = icon("crop",class = "fa-leaf"),
            menuSubItem("Pattern",tabName = "pattern",icon = icon("patt",class = "fa-table")),
            menuSubItem("Irrigation",tabName = "irrigation",icon = icon("patt",class = "fa-shower"))
            ),
            menuItem("Land",icon = icon("land",class = "fa-area-chart"),
                     menuSubItem("Use",tabName = "use",icon = icon("use",class = "fa-compass")),
                     menuSubItem("Status",tabName = "status",icon =icon("line chart",class = "fa-line-chart"))),
            menuItem("Source of Irrigation",tabName = "sourceofirrigation",icon = icon("patt",class = "fa-tint")),
            menuItem("Area Dispersal",tabName = "dooa",icon = icon("patt",class = "fa-home")),
            menuItem("Gross Cropped Area",tabName = "gca",icon = icon("patt",class = "fa-bar-chart")),
            menuItem("Crop Prices",icon = icon("patt",class = "fa-btc"),
                     menuSubItem("Mango",tabName = "mangoprice"),
                     menuSubItem("Other Crops",tabName = "other")),
            menuItem("What to Grow",tabName = "what_to",icon = icon("patt",class = " fa-question-circle"))
                     
                    
            
            
        )),
        dashboardBody(
            tabItems(
                tabPanel("Map",tags$img(src="Dapoli_map.jpg")),
                tabItem(plotOutput("pattern"),tabName = "pattern"),
                tabItem(plotOutput("irrigation"), tabName = "irrigation"),
                tabItem(plotOutput("use"),tabName = "use"),
                tabItem(fluidRow(
                    box(plotlyOutput("statusline")),
                    box(plotOutput("statusbar"))
                ) ,tabName = "status"),
                tabItem(plotOutput("sourceofirrigation"),tabName = "sourceofirrigation"),
                tabItem(plotOutput("dooa"),tabName = "dooa"),
                tabItem(plotOutput("gca"),tabName = "gca"),
                tabItem(fluidRow(
                    plotlyOutput("mangoprices1"),
                    plotOutput("mangoprices2")
                ),tabName = "mangoprice"),
                tabItem(selectInput("choose_crop","crop",c("PADDY","JOWAR","BAJRA","MAIZE","RAGI","ARHAR(Tur)","MOONG","GROUNDNUT","SUNFLOWER","SOYABEEN","SESAMUM","NIGERSEED")),plotlyOutput("other_prices"),tabName = "other"),
                tabItem(
                    selectInput("what","what",c(0.5,1.0,2.0,3.0,4.0,5.0,7.5,10.0,20.0,30.0)),
                    selectInput ("Year","future",c(2020:2035)),
                    textOutput("what"),
                    tabName = "what_to")
                    
                )
                
                
            )
            
            
                
            
        )
    )
    

















# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pattern <- renderPlot({
        ggplot(data, aes( fill=Crop_Data$CROP,x=Crop_Data$YEAR, y=Crop_Data$area_avail )) + 
            geom_bar(position="dodge", stat="identity")
    })
    
    output$irrigation <- renderPlot({
        ggplot(data, aes( fill=Crop_Data$SIZE_CLASS,x=Crop_Data$irr_ar, y=Crop_Data$unirr_ar )) + 
            geom_bar(position="dodge", stat="identity")
        #ggplotly(irr+geom_line()+stat_smooth())
        
    })
    
    output$use <- renderPlot({
        ggplot(land_use, aes(fill=land_use$SizeClass, x=land_use$YEAR, y=land_use$total_area )) + 
            geom_bar(position="dodge", stat="identity")
        
    })
    
    output$statusline <- renderPlotly({
        status <- ggplot(land_use, aes(fill=irr_status$SizeClass, x=irr_status$YEAR, y=irr_status$Wholly_Irrigated_Holdings_ar )) + 
            geom_line(position="dodge", stat="identity")
        
        ggplotly(status+geom_jitter()+stat_smooth())
        
    })
    
    output$statusbar <- renderPlot({
        ggplot(land_use, aes(fill=irr_status$SizeClass, x=irr_status$YEAR, y=irr_status$Wholly_Irrigated_Holdings_ar )) + 
            geom_bar(position="dodge", stat="identity")
    })
    
    output$sourceofirrigation <- renderPlot({
        library(dplyr)
        colours <- c("red", "orange", "blue", "yellow", "green")
        barplot(as.matrix(select(src_irr,canal_hd,tank_hd,well_hd,tubewel_hd,other_hd)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
        
    })
    
    output$dooa <- renderPlot({
        colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
        barplot(as.matrix(select(dooa,Entirely_in_the_Village_no.)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
        
    })
    
    output$gca <- renderPlot({
        colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
        barplot(as.matrix(select(gca,Gr_irr_ar,Gr_unirr_ar)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4,col=colours, beside=TRUE)
        
    })
    
    output$mangoprices1 <- renderPlotly({
        mango <- ggplot(data = mango_p, mapping = aes(x = mango_p$Arrival_Date, y = mango_p$Modal.Price,color=mango_p$Variety)) +
            geom_line()
        ggplotly(mango+geom_jitter()+stat_smooth())
        
    })
    
    
    
   
    
    output$mangoprices2 <- renderPlot({
        ggplot(mango_p, aes(x = mango_p$Variety, y = mango_p$Modal.Price)) +
            geom_boxplot() +
            grey_theme
    })
    
    output$hapus <- renderPlotly({
        p <- ggplot(hapus, aes(x = hapus$Arrival_Date, y = hapus$Modal.Price)) +
            geom_point()
        p <- p + geom_point() + stat_smooth()
        ggplotly(p)
        
    })
    
    output$what <- renderText({
        size <- Crop_Data[Crop_Data[,6]==input$what,]
        crops <- c("CASHEW","PADDY","JOWAR","MANGOES")
        result <- 0
        fruit <- NULL
        a <- data.frame(x=input$future)
        if(input$what<4.0){
        for (i in crops) {
            which_crp <- size[size[,2]==i,]
            x <- which_crp$YEAR
            y <- which_crp$total_ar
            relation <- lm(y~x)
            if(result<predict(relation,a)){
                result <- predict(relation,a)
                fruit <- i
            }
        }}
        else{
            for (i in crops) {
                #i=c("JOWAR")
                which_crp <- size[size[,2]==i,]
                if(i=="MANGOES"){
                    what_crp <- mango_p[mango_p[,5]=="Hapus(Alphaso)",]
                    what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
                    what_crp$Arrival_Date <- as.numeric(what_crp$Arrival_Date)
                }
                else if(i=="CASHEW"){
                    what_crp <- cashew_p
                    what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
                    what_crp$Arrival_Date <- as.numeric(what_crp$Arrival_Date)
                }
                else{
                    what_crp <- other_prices[other_prices[,1]==i,]
                    #what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
                    
                }
                
                
                x_p <- what_crp$Arrival_Date
                y_p <- what_crp$Modal.Price
                
                relation_p <- lm(y_p~x_p)
                x_c <- which_crp$YEAR
                y_c<- which_crp$total_ar
                relation_c <- lm(y_c~x_c)
                a <- data.frame(x_c=2022)
                b <- data.frame(x_p=2022)
                print(a)
                if(result<predict(relation_c,a)*predict(relation_p,b)){
                    result <- predict(relation,a)*predict(relation_p,b)
                    fruit <- i
                }
            }
        }
        paste("you should grow",fruit)
    })
    
    output$other_prices <- renderPlotly({
        new_graph <- other_prices[other_prices[,1]==input$choose_crop,]
        #print(input$choose_crop)
        x <- ggplot(data = new_graph, mapping = aes(x = new_graph$Arrival_Date, y = new_graph$Modal.Price)) +
            geom_point()
        x <- x+geom_point()+stat_smooth()
        ggplotly(x)
    })
        
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
