library(shiny)
library(ggplot2)
library("ElemStatLearn")
library("class")
library("plotrix")
train <- mixture.example$x
trainclass <- mixture.example$y
test <- mixture.example$xnew
pts1 <- mixture.example$px1
pts2 <- mixture.example$px2

load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
# Define UI for application that plots features of movies
ui <- navbarPage("MAT224 Shiny Web App Project",
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

  tabPanel("KMeans")

)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)