library(shiny)
library(dplyr)

# Define UI for app that explores the iris dataset
ui <- fluidPage(
  
  # App title for the 'Iris-Explorer' app
  titlePanel("The Iris-Explorer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Provide instructions for users to explore different outputs
      # of 'Iris-Explorer'
      helpText("This Shiny App can help you explore the 'Iris' dataset as 
               the way you want. The operation includes specifying the
               species that you are interested in, adjusting the bins of
               histograms, "),
      
      # Input:  Select the species of iris to explore 
      selectInput(inputId = "species_select", 
                  label = "Choose the species that you are interested in",
                  choices = list("setosa", 
                                 "versicolor",
                                 "virginica", 
                                 "All"),
                  selected = "Percent White"),
      
      # Input:  Select the number of bins for histograms
      sliderInput(inputId = "bins_number",
                  label = "Choose the number of bins",
                  min = 1,
                  max = 20,
                  value = 10)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Information", textOutput(outputId = "selected_species"),
                           textOutput(outputId = "number_of_obs")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Histograms", plotOutput(outputId = "distPlot_1"))
      )
    )
  )
)

server <- function(input, output) {
  # Output the species chosen by the user
  output$selected_species <- renderText({
    if(input$species_select == "All"){
      paste("You have selected all species")
    }
    else{
      paste("You have selected the species:", input$species_select) 
    }
  })
  
  # Output the statistical summary of the dataframe
  output$summary <- renderPrint({
    if(input$species_select == "All"){
      summary(iris)
    }
    else{
      y <- iris|> filter(Species == input$species_select)
      summary(y)
    }
  })
  
  # Output the number of observations
  output$number_of_obs <- renderText({
    if(input$species_select == "All"){
      obs <- length(iris$Sepal.Length)
    }
    else{
      y <- iris|> filter(Species == input$species_select)
      obs <- length(y$Sepal.Length) 
    }
    paste("There are", obs, "observations")
  })
  
  # Remind the user that the photo(s) of the species selected will be shown
  
  # Plot histograms for each feature under certain condition
  output$distPlot_1 <- renderPlot({
    if(input$species_select == "All"){
        x_1 <- iris$Sepal.Length
        x_2 <- iris$Sepal.Width
        x_3 <- iris$Petal.Length
        x_4 <- iris$Petal.Width
    }
    else{
      y <- iris|> filter(Species == input$species_select)
      x_1 <- y$Sepal.Length
      x_2 <- y$Sepal.Width
      x_3 <- y$Petal.Length
      x_4 <- y$Petal.Width
    }
    par(mfrow=c(2,2))
    bins_1 <- seq(min(x_1), max(x_1), length.out = input$bins_number + 1)
    bins_2 <- seq(min(x_2), max(x_2), length.out = input$bins_number + 1)
    bins_3 <- seq(min(x_3), max(x_3), length.out = input$bins_number + 1)
    bins_4 <- seq(min(x_4), max(x_4), length.out = input$bins_number + 1)
    hist(x_1, breaks = bins_1, col = "orange", border = "black",
         xlab = "Length of Sepal",
         main = "Histogram Sepal.Length")
    hist(x_2, breaks = bins_2, col = "green", border = "black",
         xlab = "Width of Sepal",
         main = "Histogram Sepal.Width")
    hist(x_3, breaks = bins_3, col = "pink", border = "black",
         xlab = "Length of Petal",
         main = "Histogram Petal.Length")
    hist(x_4, breaks = bins_4, col = "grey", border = "black",
         xlab = "Width of Petal",
         main = "Histogram Petal.Width")
    
  })
  
}

shinyApp(ui = ui, server = server)