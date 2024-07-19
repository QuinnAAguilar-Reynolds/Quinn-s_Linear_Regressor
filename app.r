#Quinn's Notes
#Its 2;54PM 7/19/24
#This assignment is due tonight at midnight so i need to focus!
#However, I want this to work the way I want it to.

#The app seems to make plots only from the particular .csv file "regrex1"
#I do not know why the program can not interprate any csv with 2 columns.
#Also, I tried to reverse the order of the varribles in the csv to see how the program would react
#I made "1xerger.csv" to test that, but the plots come out identical to regrex.csv, why?


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Quinn's Linear Regressor"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(


            # funny button
            checkboxInput("funny", "push button 4 nothing", F),

            tags$hr(),
            
            # Input: Select a file ----
            fileInput("file1", "Give me 'regrex1.csv' or suffer!",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line _________________
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
            
            # Horizontal line ______________________
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),

            tags$hr(),

            checkboxInput("linearize", "Commence Linearization!!!", F),

            
        ),

            

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           verbatimTextOutput("stats")            
        )
    )
)
##################################################################################################################


# Define server logic
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    
    output$distPlot <- renderPlot({
       df <- dataInput()  # Get the data frame from the reactive expression
        
        plot(df[[1]], df[[2]],
             xlab = names(df)[1],
             ylab = names(df)[2])
    })
    
    
    output$lmPlot <- renderPlot({
        df <- dataInput()  # Get the data frame from the reactive expression

        if(input$linearize == TRUE) {
            plot(df[[1]], df[[2]],
                xlab = names(df)[1],
                ylab = names(df)[2])
            model <- lm(df$y ~ df$x)
            abline(model, col = "red")
        }
        else {
            return()
        }
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })

    output$stats <- renderPrint({

        df <- dataInput()
        
        if (input$linearize) {
            model <- lm(df[[2]] ~ df[[1]])
            slope <- coef(model)[2]
            intercept <- coef(model)[1]
            correlation <- cor(df[[1]], df[[2]])
            
            cat("Slope:", slope, "\n")
            cat("Intercept:", intercept, "\n")
            cat("Correlation coefficient:", correlation, "\n")
        } else {
            cat("Linear regression not enabled.\n")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)