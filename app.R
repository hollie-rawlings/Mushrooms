################################################Static side################################################################

###########Packages#######
pac <- c("shiny","shinydashboard","plotly","tidyverse","inspectdf","glue","caret","funModeling","caretEnsemble","DMWR","PROC","corrr")
npac <- pac[!(pac %in% installed.packages()[,"Package"])]
if(length(npac)) install.packages(npac)

lapply(pac, require, character.only = TRUE)

##########WD#############
setwd("C:/Users/hollie.rawlings/Documents/Mushroom")
###########Data###########
data <-read.csv("mushrooms.csv")
##########Preliminary analysis####
###########Machine learning#####
#####Create predictive models#####
#####Make your own predictions#####

####Processing for bar charts####






###########Messages##########
messageData <- data.frame(
    from = c("Development", "Learning", "Downloads"),
    message = c(
        "New functions are avalible",
        "New tutorials are avaible",
        "Data is ready to download"
    ),
    stringsAsFactors = FALSE
)
###################################################Ui side#################################################################
# Define UI for application 


header <- dashboardHeader(dropdownMenuOutput("messageMenu"),title = "Mushroom dashboard")
sidebar<- dashboardSidebar(        
    sliderInput("obs","Number of observations:", min = 1,max = 200, value = 10), 
    
    sidebarMenu(
        menuItem("Homepage", tabName = "home", icon = icon("home")),
        menuItem("Idea1", tabName = "id1", icon = icon("idea"))
    ), 
    radioButtons("buttons", "Key Varaibles:",
                 list("Habitat" = "habitat",
                      "Gill colour" = "gill.color",
                      "Cap Shape" = "cap.shape",
                      "Smell" = "odor")),
    br(), 
    selectInput("dataset", "Choose a dataset to download:", 
                choices = c("mushrooms")),
    downloadButton('downloadData', 'Download')
)
body <- dashboardBody(
    tabItems(
        tabItem(tabName="home",
                        
                        box(title = "Bar Plot", solidHeader=TRUE, collapsible = TRUE, background = "maroon",
                        plotlyOutput("plot1")), 
                        
                        box(title = "Data Summary", solidHeader=TRUE, collapsible = TRUE, background = "green",
                            verbatimTextOutput("summary")),
                        
                        box(title = "Data Table", solidHeader=TRUE, collapsible = TRUE,status = "primary" ,
                        dataTableOutput("view"))), 
        
        tabItem(tabName = "id1",
                fluidRow(
                    tabBox(
                        title = "First tabBox",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Correlations", dataTableOutput("Corr")),
                        tabPanel("Tab2", plotOutput("plot2"))
                    ),
                    tabBox(
                        side = "right", height = "250px",
                        selected = "Tab3",
                        tabPanel("Tab1", plotOutput("plot3")),
                        tabPanel("Tab2", "Tab content 2"),
                        tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                    )
                )
        )))

ui <- dashboardPage(header,sidebar,body)
################################################Server side################################################################
# Define server logic 
server <- function(input, output) { 
  bardata <- reactive({  
    buttons <- switch(input$buttons,
                      habitat = data %>%  select(class,habitat),
                      gill.color = data %>%  select(class,gill.color),
                      cap.shape = data %>%  select(class,cap.shape),
                      odor = data %>%  select(class,odor), habitat)
    ##want to make it so it also uses the observation number 
    buttons <- as.data.frame(buttons)
    buttons <- buttons[1:input$obs,]
    buttons <- buttons %>% 
                        as.data.frame() %>%
                        group_by(class) %>%
                        add_count(class)
    print(buttons)
  })  
  ##Radio Buttons 
    mushdata <- reactive({  
        buttons <- switch(input$buttons,
                       habitat = data %>%  select(class,habitat),
                       gill.color = data %>%  select(class,gill.color),
                       cap.shape = data %>%  select(class,cap.shape),
                       odor = data %>%  select(class,odor), habitat)
       ##want to make it so it also uses the observation number 
        buttons <- as.data.frame(buttons)
        buttons <- buttons[1:input$obs,]
        })
    ##Message menu
    output$messageMenu <- renderMenu({
        msgs <- apply(messageData, 1, function(row) {
            messageItem(from = row[["from"]], message = row[["message"]])})
        dropdownMenu(type = "messages", .list = msgs)   
            })
    ##Plot1
    output$plot1 <- 
 renderPlotly({
        req(bardata())
        plot_ly(bardata(),x=~)
        })
    ##plot2
    output$plot2 <- renderPlot({
        plotar(data = mushdata(), target = "class", plot_type = "boxplot")   
    })
    ##plot3
    output$plot3 <- renderPlot({cross_plot(data = mushdata(), target = "class")})
    ##Table1
    output$view <- renderDataTable({
        mushdata()
    },options = list(autoWidth = TRUE,lengthMenu = c(5, 30, 50), pageLength = 10))
    ##Table2
    output$Corr <- renderDataTable({
        v_corr
    },options = list(autoWidth = TRUE,lengthMenu = c(5, 30, 50), pageLength = 10))
    ##Summary1 
    output$summary <- renderPrint({
        dataset <- mushdata()
        summary(dataset)
    })
    ##Download handler
    output$downloadData <- downloadHandler(
        filename = function() { paste(input$dataset, '.csv', sep='') },
        content = function(file) {
            write.csv(datasetInput(), file)
        }
    )
    }

# Run the application 
shinyApp(ui = ui, server = server)
