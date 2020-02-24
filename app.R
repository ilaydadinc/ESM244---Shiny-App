#My first app example!
#Maddie 

#Attach packages
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(shinydashboard)

#Read in the data here

meat_diary<- read_csv("meat.csv")

veggies<-read_csv("veggies.csv")

restaurant<-read_csv("j_tacos_burritos.csv")


#Create 'ui' = "User Interface"

ui <- navbarPage("Maddie's nav bar",
                 theme = shinytheme("united"),
                 tabPanel("Summary",
                          h1("Some giant text"),
                          p("The word 'burrito' means 'little donkey' in Spanish. The name burrito is assumed to derive from the tendency for burritos to contain a lot of different things, similar to how donkeys can carry a lot"),
                          img(src="image.jpg", height="100%",width="100%",style = 'position: absolute; opacity: 0.4;')),
                 tabPanel("Burrito Builder!",
                          sidebarLayout(
                            sidebarPanel("Choose your ingredients",
                                         h1("Meat"),
                                         sliderInput(inputId = "chicken", #change widget here
                                                     label = "Chicken",
                                                     min = 0,
                                                     max = 50,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "beef", #change widget here
                                                     label = "Beef",
                                                     min = 0,
                                                     max = 50,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "pork", #change widget here
                                                     label = "Pork",
                                                     min = 0,
                                                     max = 50,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "vegetables", #change widget here
                                                     label = "Veggies",
                                                     min = 0,
                                                     max = 10,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE),
                                         h1("Toppings"),
                                         sliderInput(inputId = "rice", #change widget here
                                                     label = "Rice",
                                                     min = 0,
                                                     max = 50,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "cheese", #change widget here
                                                     label = "Cheese",
                                                     min = 0,
                                                     max = 10,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "salsa", #change widget here
                                                     label = "Salsa",
                                                     min = 0,
                                                     max = 10,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE)
                                         ),
                            mainPanel("Main panel text",
                                      plotOutput(outputId = "diamond_plot_2")) #output
                          )),
                 tabPanel("Offset Calculator"),
                 tabPanel("Get your burrito")
                 
                          
                          
)


#Create a 'server'

server <- function(input, output){
  #first tab
  output$diamond_plot <- renderPlot({
    
    ggplot(data = diamonds, aes(x = carat, y = price))+
      geom_point(aes(color = clarity))
    
  })
  
  #reactive df for second tab
  diamond_clarity <- reactive({
    diamonds %>% 
      filter(clarity %in% input$diamondclarity)
  })
  
  #outout for secondtab
  output$diamond_plot_2 <- renderPlot({
    ggplot(data = diamond_clarity(), aes(x = clarity, y = price))+
      geom_violin(aes(fill = clarity))
  })
  
}

# Let R know that you want to combine the ui & server into an app: 
shinyApp(ui = ui, server = server)