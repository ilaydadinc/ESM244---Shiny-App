#My first app example!
#Maddie 

#Attach packages
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(stringr)
library(png)
library(dplyr)
library(janitor)
library(leaflet)
library(shinyWidgets)
library(kableExtra)
library(extrafont)
library(paletteer)




#Create 'ui' = "User Interface"

ui <- navbarPage(
  
  header = tagList(
    useShinydashboard()
  ),
  
  "Guilt-free Burritos",
  theme = shinytheme("flatly"),
                 tabPanel("Home",
                          h1("BURRITO BUILDER", style = "font-size:40px",align="center"),
                          p("The word 'burrito' means 'little donkey', in Spanish. The name burrito is assumed to derive from the tendency for burritos to contain a lot of different things, similar to how donkeys can carry a lot.",  style = "font-size:18px",align="center"),
                          img(src="image.jpg", height="100%",width="100%",style = 'position: absolute; opacity: 0.2;'),
                          tags$hr(),
                          # WHAT
                          fluidRow(
                            column(2),
                            column(8,
                                   shiny::HTML("<center><h4>It is true. Burritos carry a lot. Have you ever thought about how it matters what it contains? This app is here to help you to build your burrito and see your environmental impact in the form of greenhouse gas emissions.</h4></center>")
                            ),
                            column(2)
                            ),
                          fluidRow(
                            style = "height:10px;"
                            ),
                          # PAGE BREAK
                          tags$hr(),
                          # WHERE
                          fluidRow(
                            column(2),
                            column(8,
                                   shiny::HTML("<br><br><center> <h1>How does it work?</h1> </center><br>"),
                                   shiny::HTML("<center><h4> In the 'Burrito Builder!' tab, you will be able to calculate your greenhouse gas emissions according to the your choices.Then, if you keep exploring our app and you will see how you can offset these emissions by making small changes to your life choices in the 'Offset Calculator'tab.</h4></center>")
                            ),
                            column(2)
                          ),
                          fluidRow(
                            style = "height:50px;"),
                          # PAGE BREAK
                          tags$hr(),
                          # HOW
                          fluidRow(
                            column(2),
                            column(8,
                                   shiny::HTML("<br><br><center> <h1>All you can think of is having a burrito now, right? Don’t worry we got you!

</h1> </center><br>")
                            ),
                            column(2)
                          ),
                          fluidRow(
                            style = "height:50px;"),
                          # PAGE BREAK
                          tags$hr()
                 ), # Closes the first tabPanel called "Home"
                 
      
                 tabPanel("Burrito Builder!",
                          sidebarLayout(
                            sidebarPanel(h1("Choose your ingredients"),
                                         h3("Start with a base"),
                                         sliderInput(inputId = "chicken", #change widget here
                                                     label = "Chicken (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "beef", #change widget here
                                                     label = "Beef (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "pork", #change widget here
                                                     label = "Pork (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         #sliderInput(inputId = "fish", #change widget here
                                          #           label = "Fish (grams)",
                                           #          min = 0,
                                            #         max = 200,
                                             #        0,
                                              #       step = 10,
                                               #      ticks = FALSE),
                                         sliderInput(inputId = "vegetables", #change widget here
                                                     label = "Veggies (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE),
                                         h3("Add some other stuff"),
                                         sliderInput(inputId = "rice", #change widget here
                                                     label = "Rice (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 10,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "cheese", #change widget here
                                                     label = "Cheese (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE),
                                         sliderInput(inputId = "salsa", #change widget here
                                                     label = "Salsa (grams)",
                                                     min = 0,
                                                     max = 200,
                                                     0,
                                                     step = 1,
                                                     ticks = FALSE)
                                         ),
                            mainPanel(h1("Greenhouse Gas Emissions by Ingredient", style = "font-size:25px",align="center"),
                                      plotOutput(outputId = "emission_contri"),
                                      htmlOutput("emission_table")) #output
                          )),
                 tabPanel("Offset Calculator",
                          sidebarLayout(
                            sidebarPanel(
                                         selectInput(inputId = "offset_select",
                                                     label = "How do you want to offset your Greenhouse gas emissions?",
                                                     choices = c("Planting Trees" = "Tree",
                                                                 "Biking instead of Driving" = "Bike",
                                                                 "Walking instead of Driving" = "Walk",
                                                                 "Recyling instead of Landfilling" = "Waste"))),
                            mainPanel(valueBoxOutput("offset_table"))
                          )),
                 tabPanel("Get your burrito",
                          sidebarLayout(
                            sidebarPanel(h2("Nearby burritos"),
                                         "Enter your zipcode to find a burrito establishment close to you",
                                         textInput("postalcode",
                                                      label = "",
                                                      value = "e.g. 93117"
                                         )),
                            mainPanel(leafletOutput("burr_map", width = 700, height = 500))
                          )),
                 tabPanel("References", style = "font-size:25px",align="left",
                            shiny::HTML("<h1><b> REFERENCES </b></h1>   <h4>[1] Wernet, G., Bauer, C., Steubing, B., Reinhard, J., Moreno-Ruiz, E., and Weidema, B., 2016. The ecoinvent database version 3 (part I): overview and methodology. The International Journal of Life Cycle Assessment, [online] 21(9), pp.1218–1230. Available at: <http://link.springer.com/10.1007/s11367-016-1087-8> [Accessed February 5, 2020]. <br>
<br>
[2]  Greenhouse Gases Equivalencies Calculator - Calculations and References. (2019, October 25). Retrieved February 5, 2020, from 
https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-reference

<br>
<br>
[3]”Just tacos and burritos”, compiled for Kaggle.com by Datafiniti. Retrieved February 5, 2020 from https://www.kaggle.com/datafiniti/restaurants-burritos-and-tacos 

 <br>
 <br>
[4] McPherson, E. G.; van D. N. S.; Peper, P. J. (2016). Urban tree database and allometric equations. Gen. Tech. Rep. PSW-GTR-253. Albany, CA: U.S. Department of Agriculture, Forest Service, Pacific Southwest Research Station. 86 p.

<br>
<br>
[5] U.S. DOE (1998). Method for Calculating Carbon Sequestration by Trees in Urban and Suburban Settings. Voluntary Reporting of Greenhouse Gases, U.S. Department of Energy, Energy Information Administration (16 pp, 111K, About PDF)

<br>
<br>
[6] EPA (2019). Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2017. Chapter 3 (Energy), Tables 3-13, 3-14, and 3-15. Environmental Protection Agency, Washington, D.C. EPA #430-R-19-001 (PDF) (113 pp, 3 MB About PDF)

<br>
<br>
[7] FHWA (2019). Highway Statistics 2017. Office of Highway Policy Information, Federal Highway Administration. Table VM-1. (1 pp, 12 KB About PDF)
          </h4>                              "))
                          
                 
                 
                          
                          
)

#######################


#Read in the data here

veggie_raw<-read_csv("veggies.csv")
meat_raw<-read_csv("meat.csv")


#Data manipulation - Burrito Builder
cheese_percent <- 0.249
cheese_cf_kg<- 0.1/cheese_percent #ef/cheese_cf_kg => ef(kg CO2e/kg) 
fish_percent <- 0.17 #cod, http://www.fao.org/3/x5916e01.htm
fish_cf_kg <- 0.1/fish_percent
bread_percent <- 3.6/28 #usda
bread_cf_kg <- 0.1/bread_percent

meat_clean <- meat_raw %>% 
  clean_names() %>% 
  select(area, item, unit, y2003) %>% 
  filter(area == "United States of America") %>% 
  filter(unit == "kg CO2eq/kg product") %>% 
  rename(emission_factor = y2003) %>% 
  rename(ingredient = item) %>% 
  select(ingredient, emission_factor)

veggie_clean <- veggie_raw %>% 
  clean_names() %>% 
  filter(product %in% c("Wheat & Rye (Bread)", "Tomatoes", "Onions & Leeks", "Other Vegetables", "Cheese", "Fish (farmed)")) %>%
  mutate(emission_factor = case_when(
    product == "Wheat & Rye (Bread)" ~ median/bread_cf_kg,
    product == "Cheese" ~ median/cheese_cf_kg,
    product == "Fish (farmed)" ~ median/fish_cf_kg,
    TRUE ~ median
  )) %>% 
  rename(ingredient = product) %>% 
  select(ingredient, emission_factor)

ingredient_clean <- rbind(meat_clean, veggie_clean)

col_names <- ingredient_clean$ingredient

#Transpose the dataframe
ingredient_value <- ingredient_clean %>% 
  select(emission_factor)

ingredient_final <- as.data.frame(t(ingredient_value))

names(ingredient_final)<-col_names

ingredient_final <- ingredient_final %>% 
  clean_names()

#########################

#Data Manipulation - Map 

#read in data and clean
burritos <- read_csv("tacos_burritos.csv") %>% 
  filter(str_detect(menus.name, pattern = "Burrito")) %>%  #get rid of the tacos
  filter(!str_detect(categories, pattern = "Fast Food")) %>% #no fast food because we are classy
  select(address, 
         categories, 
         city, 
         country, 
         cuisines, 
         latitude, 
         longitude, 
         menus.description, 
         menus.name, 
         name, 
         postalCode,
         province) %>% 
  clean_names() %>% 
  filter(latitude != "NA" | longitude != "NA") %>% 
  filter(longitude < 0)


#note: we may have to filter out the messy zipcodes too

#######################

#Create a 'server'

server <- function(input, output){
  ### TAB 2 ###
  #output for burrito builder
  output$emission_contri <- renderPlot({
    
    plot_data <- data.frame(ingredient = c("Chicken", "Beef", "Pork", "Vegetables", "Rice", "Cheese", "Salsa", "Bread"),
                       emission = c(input$chicken*ingredient_final$meat_chicken, 
                                    input$beef*ingredient_final$meat_cattle,
                                    input$pork*ingredient_final$meat_pig,
                                    input$vegetables*ingredient_final$other_vegetables,
                                    input$rice*ingredient_final$rice_paddy,
                                    input$cheese*ingredient_final$cheese,
                                    (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks),
                                    100 * ingredient_final$wheat_rye_bread
                       ))
    
    ggplot(data = plot_data, aes(x = ingredient, y = emission))+
      geom_bar(aes(fill = ingredient), stat="identity")+
      theme_classic()+
      theme(legend.position="none")+
      scale_y_continuous(expand = c(0,0))+
      labs(x ="\nIngredient",
           y = expression(paste("Greenhouse Gas Emission" , " (g-CO"[2], " equivalent) ")))+
      theme(axis.text.x = element_text(color = "grey20", size = 15),
            axis.title.x = element_text(color = "grey20", size = 16),
            axis.title.y = element_text(color = "grey20", size = 16))+
      scale_fill_paletteer_d("calecopal::figmtn")
     
  })
  
  output$emission_table <- renderText({
    
    total_emission <- input$chicken*ingredient_final$meat_chicken + 
      input$beef*ingredient_final$meat_cattle+
      input$pork*ingredient_final$meat_pig+
      input$vegetables*ingredient_final$other_vegetables+
      input$rice*ingredient_final$rice_paddy+
      input$cheese*ingredient_final$cheese+
      (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
      100 * ingredient_final$wheat_rye_bread
    
    total_name <-  "Total GHG Emission (gram carbon dioxide-eq)" 
    
    kable_data <- data.frame(c(as.character(round(total_emission,digits = 1))))
    
    kable(kable_data, col.names = c("Total Greenhouse Gas Emissions (g CO2 eq)")) %>%
      kable_styling(
        font_size = 20,
        bootstrap_options = c("striped", "condensed")
      ) 
  })
  ### TAB 3 ###
  
  #Read in the data
  
 offset<-read_csv("offset mechanisms.csv")
 
    # Create reactive object state_candy that changes based on state_select widget selection
    offset_amount <- reactive({
      offset %>%
        filter(Method == input$method_select) %>%
        select(Amount, Hours) %>% 
        mutate(Offset== Amount*emission)
      
    })
    
    # Render a reactive table that uses state_candy reactive object (and note the parentheses after state_candy -- do that if calling a reactive object!)
    output$offset_table <- renderValueBox({
      
      total_emission <- input$chicken*ingredient_final$meat_chicken + 
        input$beef*ingredient_final$meat_cattle+
        input$pork*ingredient_final$meat_pig+
        input$vegetables*ingredient_final$other_vegetables+
        input$rice*ingredient_final$rice_paddy+
        input$cheese*ingredient_final$cheese+
        (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
        100 * ingredient_final$wheat_rye_bread
      
      if (req(input$offset_select) == "Walk") {
        temp_df <- offset %>% 
          filter(Method == "Walk") %>% 
          select(Hours)
        
        temp_para <- as.numeric(temp_df$Hours)
        
        valueBox(paste0(round(1/(total_emission*temp_para/1e6), digits = 1), "B to 1Hr"), 
                 paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), "burritos, 1 hour of walking instead of driving is needed."), icon = icon("walking"), color = "yellow", width = NULL)
      } else if (req(input$offset_select) == "Tree") {
        temp_df <- offset %>% 
          filter(Method == "Tree") %>% 
          select(Amount)
        
        temp_para <- as.numeric(temp_df$Amount)
        
        valueBox(paste0(round(1/(total_emission*temp_para/1e6), digits = 1), "B to 1S"), 
                 paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), "burritos, 1 tree need to be grown."), icon = icon("tree"), color = "green", width = NULL)
        
      } else if (req(input$offset_select) == "Bike") {
        temp_df <- offset %>% 
          filter(Method == "Bike") %>% 
          select(Hours)
        
        temp_para <- as.numeric(temp_df$Hours)
        
        valueBox(paste0(round(1/(total_emission*temp_para/1e6), digits = 1), "B to 1Hr"), 
                 paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), "burritos, 1 hour of biking instead of driving is needed."), icon = icon("bicycle"), color = "blue", width = NULL)
      
      } else {
        temp_df <- offset %>% 
          filter(Method == "Waste") %>% 
          select(Amount)
        
        temp_para <- as.numeric(temp_df$Amount)
        
        valueBox(paste0(round(1/(total_emission*temp_para/1e6), digits = 1), "B to 1B"), 
                 paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), "burritos, 1 bag of trash need to be recycled instead of landfilled."), icon = icon("recycle"), color = "orange", width = NULL)
      }
    })
  
  #output for offset calculator
  output$diamond_plot <- renderPlot({
    
    ggplot(data = diamonds, aes(x = carat, y = price))+
      geom_point(aes(color = clarity))
    
  })
  ### TAB 4 ###
  #reactive df for burrito map
 local_burritos <- reactive({
   burritos %>%
   filter(postal_code == input$postalcode)
 })
  
  #output for burrito map
  
  output$burr_map <-renderLeaflet({
    leaflet(burritos) %>%
    #addCircles(lng = ~longitude, lat = ~latitude) %>%
    addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })

  observe({
    leafletProxy("burr_map", data = local_burritos()) %>%
      clearShapes() %>% 
      addCircles(data = local_burritos(),
      lat = ~latitude,
      lng= ~longitude,
      radius = 5,
      fillOpacity = 0.8,
      popup = ~name,
      color = "#D46F10FF") %>% 
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })

#table with information
  
}

# Let R know that you want to combine the ui & server into an app: 
shinyApp(ui = ui, server = server)