library(shiny)
library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(shinythemes)

# Define UI for application that plots features of movies
ui <- navbarPage("MAT224 Shiny Web App Project", theme = shinytheme('darkly'),
  tabPanel("Upload Dataset",
           # Input: Select a file ----
           fileInput("file1", "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Checkbox if file has header ----
           checkboxInput("header", "Header", TRUE),
           
           # Input: Select separator ----
           radioButtons("sep", "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ","),
           
           # Input: Select quotes ----
           radioButtons("quote", "Quote",
                        choices = c(None = "",
                                    "Double Quote" = '"',
                                    "Single Quote" = "'"),
                        selected = '"'),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Select number of rows to display ----
           radioButtons("disp", "Display",
                        choices = c(Head = "head",
                                    All = "all"),
                        selected = "head")
           
  

),
  
  tabPanel("View Data",
           # Main panel for displaying outputs ----
           mainPanel(
             
             # Output: Data file ----
             tableOutput("contents")
             
           )),

  tabPanel("Elbow Test",
           mainPanel(
             plotOutput("elbowPlot", click = "plot_click")
           )),

  tabPanel("KMeans",
           
           numericInput('clusters', 'Cluster count', 3,
                        min = 1, max = 9),
           
           mainPanel(
             plotOutput("Kmeans", click = "plot_click")
           )
           
           
           )

)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  data <- reactive({df <- read.csv(input$file1$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
                    df<-na.omit(df)
                    max(df)
                  return(df)})
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    if(input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
    
  })
  
  # ELBOW TEST
  output$elbowPlot <- renderPlot({

   fviz_nbclust(data(), kmeans, method = "wss")
                          

    
    
  })
  
  # K MEANS CLUSTERING
  
  
  km <- reactive({
    kmeans(data(), input$clusters)
  })
  
  output$Kmeans <- renderPlot({
  fviz_cluster(km(), data(), geom = "point",
               stand = FALSE, ellipse.type = "norm", ggtheme = theme_gray())
  
})
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)