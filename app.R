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
  header = tagList(useShinydashboard()),
  "Guilt-free Burritos", 
  theme = shinytheme("flatly"),
  tabPanel("Home",
           icon = icon("home"),
           h1("BURRITO BUILDER", 
              style = "font-size:40px",
              align="center"
              ),
           p("The word 'burrito' means 'little donkey', in Spanish. The name burrito is assumed to derive from the tendency for burritos to contain a lot of different things, similar to how donkeys can carry a lot.",  style = "font-size:20px",align="center"
             ),
           img(src="image.jpg", height="100%",width="100%",style = 'position: absolute; opacity: 0.2;'
               ),
           tags$hr(),
           # WHAT
           fluidRow(column(12,shiny::HTML("<center><h4>It's true - burritos carry a lot. Have you ever thought about the environmental impact of what it contains? <br><br>This app is here to help you to assess the greenhouse gass emissions of different ingredients<br> while designing your dream burrito.</h4></center>")
                            ),
                    column(4
                           )
                    ),
           fluidRow(style = "height:10px;"
                    ),
           # PAGE BREAK
           tags$hr(),
           # WHERE
           fluidRow(column(2),
                    column(12,
                           shiny::HTML("<br><br><center> <h1>How does it work?</h1> </center><br>"),
                           shiny::HTML("<center><h4> In the 'Burrito Builder!' tab, you will be able to calculate your greenhouse gas emissions<br>according to the your choices.<br><br> Then, if you keep exploring our app and you can see how to offset these emissions<br>by making small changes to your life choices in the 'Offset Calculator' tab.</h4></center>")
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
                            column(12,
                                   shiny::HTML("<br><br><center> <h1>All you can think of is having a burrito now, right? <br> Don’t worry we got you!</h1><br><h4>Make sure to check out the 'Get Your Burrito' tab! </h4>
</center><br>")
                                   ),
                            column(2)
                            ),
                          fluidRow(
                            style = "height:50px;"),
                          # PAGE BREAK
                          tags$hr()
                 ), # Closes the first tabPanel called "Home"
  tabPanel("Burrito Builder!",icon = icon("utensils"),
           sidebarLayout(
             sidebarPanel(h1("Choose your ingredients"),
                          h3("Start with a base"),
                          sliderInput(inputId = "chicken", #change widget here
                                      label = "Chicken (grams)",
                                      min = 0,
                                      max = 200,
                                      0,
                                      step = 10,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "beef", #change widget here
                                      label = "Beef (grams)",
                                      min = 0,
                                      max = 200,
                                      0,
                                      step = 10,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "pork", #change widget here
                                      label = "Pork (grams)",
                                      min = 0,
                                      max = 200,
                                      0,
                                      step = 10,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "fish", #change widget here
                                      label = "Fish (grams)",
                                      min = 0,
                                      max = 200,
                                      0,
                                      step = 10,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "vegetables", #change widget here
                                      label = "Veggies (grams)",
                                      min = 0,
                                      max = 200,
                                      0,
                                      step = 1,
                                      ticks = FALSE
                                      ),
                          h3("Add some other stuff"),
                          sliderInput(inputId = "rice", #change widget here
                                      label = "Rice (grams)",
                                      min = 0,
                                      max = 60,
                                      0,
                                      step = 10,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "cheese", #change widget here
                                      label = "Cheese (grams)",
                                      min = 0,
                                      max = 60,
                                      0,
                                      step = 1,
                                      ticks = FALSE
                                      ),
                          sliderInput(inputId = "salsa", #change widget here
                                      label = "Salsa (grams)",
                                      min = 0,
                                      max = 60,
                                      0,
                                      step = 1,
                                      ticks = FALSE
                                      )
                          ),
             mainPanel(h1("Greenhouse Gas Emissions by Ingredient",
                          style = "font-size:25px",
                          align="center"
                          ),
                       plotOutput(outputId = "emission_contri"),
                       HTML("<br><br><br>"),
                       htmlOutput("emission_table"),
                       HTML("<br>"),
                       fluidRow(
                         column(1),
                     
                         column(10,
                                #             Panel for Background on Data
                                div(class="panel panel-success",
                                    div(class="panel-body",  
                                        tags$div( 
                                          div( align = "center", style="color:darkslateblue",
                                               h4("What does this mean?")
                                          )
                                        ),
                                        tags$p(h5("Greenhouse gases trap heat in the atmosphere and cause climate change. It is usually expressed in kg carbon dioxide equivalent. Electricity and heat production, industrial production, and agriculture and forestry are the top three contributors to greenhouse gas emissions [1].
The bar graph shows the breakdown of greenhouse gas emissions by ingredients for your customized burrito. As an example, the result for beef accounts for all the greenhouse gas emissions in its production lifetime, which potentially includes cattle grazing and slaughtering, animal feed growth, storage & transportation, etc.
The total greenhouse gas emission is also shown under the graph."))
                                
                                    )
                                ) # Closes div panel
                         ), # Closes column
           
                       column(1))
                      )
             )
           ),
  tabPanel("Offset Calculator",icon = icon("calculator"),
           sidebarLayout(
             sidebarPanel(h1("Choose your offset options"),
                          selectInput(inputId = "offset_select_1",
                                      label = "What is your first option to offset your Greenhouse gas emissions?",
                                      choices = c("Planting Trees" = "tree",
                                                  "Biking instead of Driving" = "bicycle",
                                                  "Walking instead of Driving" = "walking",
                                                  "Recyling instead of Landfilling" = "recycle",
                                                  "Skating instead of Driving" = "snowboarding",
                                                  "Switching from Incandescent Lamps to LED" = "lightbulb")
                                      ),
                          selectInput(inputId = "offset_select_2",
                                      label = "What is your second option to offset your Greenhouse gas emissions?",
                                      choices = c("Planting Trees" = "tree",
                                                  "Biking instead of Driving" = "bicycle",
                                                  "Walking instead of Driving" = "walking",
                                                  "Recyling instead of Landfilling" = "recycle",
                                                  "Skating instead of Driving" = "snowboarding",
                                                  "Switching from Incandescent Lamps to LED" = "lightbulb")
                                      ),
                          selectInput(inputId = "offset_select_3",
                                      label = "What is your third option to offset your Greenhouse gas emissions?",
                                      choices = c("Planting Trees" = "tree",
                                                  "Biking instead of Driving" = "bicycle",
                                                  "Walking instead of Driving" = "walking",
                                                  "Recyling instead of Landfilling" = "recycle",
                                                  "Skating instead of Driving" = "snowboarding",
                                                  "Switching from Incandescent Lamps to LED" = "lightbulb")
                                      )
                          ),
             mainPanel(uiOutput("offset_table_1"),
                       uiOutput("offset_table_2"),
                       uiOutput("offset_table_3"),
                       fluidRow(
                         column(1),
                         column(10,
                                #             Panel for Background on Data
                                div(class="panel panel-success",
                                    div(class="panel-body",  
                                        tags$div( 
                                          div( align = "center", style="color:darkslateblue",
                                               h4("Why make these changes?")
                                          )
                                        ),
                                        tags$p(h5("Even if you can't go without that beef burrito, there are many ways to reduce your carbon footprint. Trees absorb carbon dioxide - here you can see how many burritos you can offset by planting just one seedling. Biking, walking or skating help avoid emissions from gas-powered vehicles, while recycling helps avoid emissions
                            that result from material decomposing at the landfill. Lastly, switching out incandescent lightbulbs for LEED bulbs makes lighting more energy efficient, reducing demand on power plants, which produce high amounts of GHG emissions."))
                                        
                                    )
                                ) # Closes div panel
                         ), # Closes column
                        column(1) 
                       ))
             )
           ),
  tabPanel("Get Your Burrito", icon = icon("map-marked-alt"),
           sidebarLayout(
             sidebarPanel(h1("Find a burrito"),
                          "Enter the first three digits of your zipcode to find a burrito establishment close to you",
                          textInput("postalcode",
                                    label = "",
                                    value = "e.g. 931"
                                    )
                          ),
             mainPanel(leafletOutput("burr_map", width = 700, height = 500),
                       tableOutput("burrito_table")) 
             )
           ),
   tabPanel("About", icon = icon("users"), 
            img(src="image3.jpg", height="140%",width="100%",style = 'position: absolute; opacity: 0.2;'),
   
   fluidRow(
     shiny::HTML("<center> 
                                             <h1>GUILT-FREE BURRITOS</h1> 
                             
                                             </center>
                                       
                                             ")),
   fluidRow(
     div( div(
       tags$img(src = "environment.png", 
                width = "100px", height = "100px")
     ),
       align = "center",
         tags$span(h2("How did we come up with this idea?"), 
                   style = "font-weight:bold",
         ))
   ),
   fluidRow(
     column(1),
     column(10, align = "left",
            tags$p(h4("We are students at UCSB Bren School. We took Life Cycle Assessment(LCA) class together and we wanted to use it as a tool in our daily lives. Globally, agriculture accounts for almost 30% of Greenhouse gas emissions[2]. We all love burritos and we wanted to see how our food preferences impacts the environment."))
     ),
     column(1)
   ),
   fluidRow(
     column(1),
     column(10,
#             Panel for Background on Data
            div(class="panel panel-success",
                div(class="panel-body",  
                    tags$img(src = "dashboard.png", 
                             width = "90px", height = "90px"), align = "center",
                    tags$div( 
                     div( align = "center",
                                   h2("Where does the data come from?")
                              )
                    ),
                    #tags$p(h4("Over 30 years of data were collected, which resulted in nearly 500,000 records of career movement. Several business rules were developed to ensure the data reflected real opportunities in the current classification system.")),
                    tags$ul(align = "left",
                      tags$li(h4("The greenhouse gas emission dataset for the ingredients is normalized from two sources. The chicken, beef, pork data comes from the Food and Agriculture Organization of the United Nations [3], and the rest comes from a study published in Science in 2018 [4].")),
                      tags$li(h4("The offset data is taken from EPA’s Greenhouse Gas Equivalencies Calculator [5, 6, 7, 8, 9].")),
                      tags$li(h4("The restaurant location data is compiled from a Kaggle.com dataset named “Restaurants that sell Tacos and Burritos.” [10]"))
                    )
                )
            ) # Closes div panel
     ) # Closes column
   
   ),
   # TEAM BIO
   fluidRow(
     column(1),
     column(10,
            shiny::HTML("<font color='darkslateblue'><center> <h1>ABOUT THE TEAM</h1> </center></font><br>")
     ),
     column(1)
   ),
   
   fluidRow(
     column(3),
     
     # Marc
     column(2,
           #div(class="panel panel-success", align="left",
               # div(class="panel-body",  width = "400px",
                    align = "center",
                    div(
                      tags$img(src = "avatar.jpeg", 
                               width = "120px", height = "120px",class="img-circle")
                    ),
                    div(
                      tags$h4("Maddie"),
                      tags$h5( tags$i("'Veggie Burrito'"))
                    ),
                    div(
                      tags$h4("Berger")
                     
                    )
                   
               # )
           # )
     ),
     
#     # George
     column(2, 
            #div(class="panel panel-success",align="center",
                #div(class="panel-body",  width = "600px", 
                    align = "center",
                    div(
                      tags$img(src = "ilayda.jpg", 
                               width = "120px", height = "120px",class="img-circle")
                    ),
                    div(
                      tags$h4("Ilayda"),
                      tags$h5( tags$i("'Queen Burrito'"))
                    ),
                    div(
                      tags$h4("Dinc")
                    )
               # )
            #)
           ),
     # Angela
     column(2,
            #div(class="panel panel-success",align="right",
               # div(class="panel-body",  width = "600px", 
                   align = "center",
                    div(
                      tags$img(src = "ted.jpg", 
                               width = "120px", height = "120px",class="img-circle")),
                    div(
                      tags$h4("Ted"),
                      tags$h5( tags$i("'Breakfast Burrito'"))
                    ),
                    div(
                      tags$h4("Jiang")
                    )
                )
          #  ) )
      ),
   fluidRow(style = "height:200px;")
 ),  # Closes About tab
                 tabPanel("References", style = "font-size:25px",align="left", icon = icon("link"),
                            shiny::HTML("<h1><b> REFERENCES </b></h1>   <h4>[1] US EPA. (2020). Global Greenhouse Gas Emissions Data. Retrieved from https://www.epa.gov/ghgemissions/global-greenhouse-gas-emissions-data<br>
<br>
[2] Smith, P., & Gregory, P. J. (2013). Climate change and sustainable food production. Proceedings of the Nutrition Society, 72(1), 21-28.
                            <br>
<br>
                            [3] FAO STAT. (n.d.). Food and Agriculture Data. Retrieved March 11, 2020, from http://www.fao.org/faostat/en/#home<br>
<br>
[4]Poore, J., & Nemecek, T. (2018). Reducing food’s environmental impacts through producers and consumers. Science, 360(6392), 987–992. doi: 10.1126/science.aaq0216 

<br>
<br>
[5]  Greenhouse Gases Equivalencies Calculator - Calculations and References. (2019, October 25). Retrieved February 5, 2020, from 
https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-reference
 <br>
 <br>
[6] McPherson, E. G.; van D. N. S.; Peper, P. J. (2016). Urban tree database and allometric equations. Gen. Tech. Rep. PSW-GTR-253. Albany, CA: U.S. Department of Agriculture, Forest Service, Pacific Southwest Research Station. 86 p.

<br>
<br>
[7] U.S. DOE (1998). Method for Calculating Carbon Sequestration by Trees in Urban and Suburban Settings. Voluntary Reporting of Greenhouse Gases, U.S. Department of Energy, Energy Information Administration (16 pp, 111K, About PDF)

<br>
<br>
[8] EPA (2019). Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2017. Chapter 3 (Energy), Tables 3-13, 3-14, and 3-15. Environmental Protection Agency, Washington, D.C. EPA #430-R-19-001 (PDF) (113 pp, 3 MB About PDF)

<br>
<br>
[9] FHWA (2019). Highway Statistics 2017. Office of Highway Policy Information, Federal Highway Administration. Table VM-1. (1 pp, 12 KB About PDF)
<br>
<br> 
[10]”Just tacos and burritos”, compiled for Kaggle.com by Datafiniti. Retrieved February 5, 2020 from https://www.kaggle.com/datafiniti/restaurants-burritos-and-tacos
</h4>                              "))
                          
                 
                 
                          
                          
)

#######################


#Read in the data here

veggie_raw<-read_csv("veggies.csv")
meat_raw<-read_csv("meat.csv")


#Create a normalization factor using beef
beef_percent_n <- 0.26
beef_cf_kg_n<- 0.1/beef_percent_n
beef_ef_n <- 99.5/beef_cf_kg_n
n_factor <- 11.8739/beef_ef_n #normalization factor of two dataframes!

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
    product == "Wheat & Rye (Bread)" ~ n_factor * median/bread_cf_kg,
    product == "Cheese" ~ n_factor * median/cheese_cf_kg,
    product == "Fish (farmed)" ~ n_factor * median/fish_cf_kg,
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

##add zeros to the zipcode column and then make a new column with just the first three digits
burritos_clean <- burritos %>%
  mutate(
    postal_code = case_when(
      str_length(postal_code) == 4 ~ paste("0",postal_code, sep = ""), #add zeros to 4 digit zipcodes
      TRUE ~ postal_code
    )
  ) %>% 
  mutate(
    zip = paste(substr(postal_code,1,3)) #create new column with only the first three
  ) %>% 
  mutate(
    popup = paste('<strong>',name,'<strong>',
                  '<br>',address,
                  '<br>',city)
  )

##create burrito icon

burr_icon <- makeIcon("burr_icon.svg", iconWidth = 15, iconHeight = 15)

#######################

#Create a 'server'

server <- function(input, output){
  ### TAB 2 ###
  #output for burrito builder
  output$emission_contri <- renderPlot({
    
    plot_data <- data.frame(ingredient = c("Chicken", "Beef", "Pork", "Fish", "Vegetables", "Rice", "Cheese", "Salsa", "Tortilla"),
                       emission = c(input$chicken*ingredient_final$meat_chicken, 
                                    input$beef*ingredient_final$meat_cattle,
                                    input$pork*ingredient_final$meat_pig,
                                    input$fish*ingredient_final$fish_farmed,
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
      input$fish*ingredient_final$fish_farmed+
      input$rice*ingredient_final$rice_paddy+
      input$cheese*ingredient_final$cheese+
      (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
      100 * ingredient_final$wheat_rye_bread
    
    total_name <-  "Total GHG Emission (gram carbon dioxide-eq)" 
    
    kable_data <- data.frame(c(as.character(round(total_emission,digits = 1))))
      
    kable(kable_data, col.names = c("Total Greenhouse Gas Emissions (g CO2 eq)")) %>%
      kable_styling(
        font_size = 20,
        full_width = F,
        position = "center",
        bootstrap_options = c("striped", "condensed","hover")
      ) 
  })
  ### TAB 3 ###
  
  #Read in the data
  
 offset<-read_csv("offset mechanisms.csv")
 
    # Create reactive object state_candy that changes based on state_select widget selection
#    offset_amount <- reactive({
#      offset %>%
#        filter(Method == input$method_select) %>%
#        select(Amount, Hours) %>% 
#        mutate(Offset== Amount*emission)
      
#    })
    
    # Render a reactive table that uses state_candy reactive object (and note the parentheses after state_candy -- do that if calling a reactive object!)
    output$offset_table_1 <- renderUI({
      
      total_emission <- input$chicken*ingredient_final$meat_chicken + 
        input$beef*ingredient_final$meat_cattle+
        input$pork*ingredient_final$meat_pig+
        input$vegetables*ingredient_final$other_vegetables+
        input$fish*ingredient_final$fish_farmed+
        input$rice*ingredient_final$rice_paddy+
        input$cheese*ingredient_final$cheese+
        (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
        100 * ingredient_final$wheat_rye_bread
      
        temp_df <- offset %>% 
          filter(Method == input$offset_select_1)
        
        temp_para <- as.numeric(temp_df$Value)
        
        valueBox(paste(round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_1)), 
                 paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_2)), icon = icon(as.character(temp_df$Method)), color = "yellow", width = 12)
    })
    
    output$offset_table_2 <- renderUI({
      
      total_emission <- input$chicken*ingredient_final$meat_chicken + 
        input$beef*ingredient_final$meat_cattle+
        input$pork*ingredient_final$meat_pig+
        input$vegetables*ingredient_final$other_vegetables+
        input$fish*ingredient_final$fish_farmed+
        input$rice*ingredient_final$rice_paddy+
        input$cheese*ingredient_final$cheese+
        (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
        100 * ingredient_final$wheat_rye_bread
      
      temp_df <- offset %>% 
        filter(Method == input$offset_select_2)
      
      temp_para <- as.numeric(temp_df$Value)
      
      valueBox(paste(round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_1)), 
               paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_2)), icon = icon(as.character(temp_df$Method)), color = "green", width = 12)
    })
    
    output$offset_table_3 <- renderUI({
      
      total_emission <- input$chicken*ingredient_final$meat_chicken + 
        input$beef*ingredient_final$meat_cattle+
        input$pork*ingredient_final$meat_pig+
        input$vegetables*ingredient_final$other_vegetables+
        input$fish*ingredient_final$fish_farmed+
        input$rice*ingredient_final$rice_paddy+
        input$cheese*ingredient_final$cheese+
        (0.7 * input$salsa*ingredient_final$tomatoes + 0.3 * input$salsa*ingredient_final$onions_leeks)+
        100 * ingredient_final$wheat_rye_bread
      
      temp_df <- offset %>% 
        filter(Method == input$offset_select_3)
      
      temp_para <- as.numeric(temp_df$Value)
      
      valueBox(paste(round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_1)), 
               paste("After eating", round(1/(total_emission*temp_para/1e6), digits = 1), as.character(temp_df$text_2)), icon = icon(as.character(temp_df$Method)), color = "blue", width = 12)
    })
  

    

  #### TAB 4 ####
  #reactive df for burrito map
 local_burritos <- reactive({
   burritos_clean %>%
   filter(zip == input$postalcode)
 })
    

  
  #output for burrito map
  
  output$burr_map <-renderLeaflet({
    leaflet(burritos_clean) %>%
    #addCircles(lng = ~longitude, lat = ~latitude) %>%
    addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })

  observe({
    leafletProxy("burr_map", data = local_burritos()) %>%
      clearShapes() %>% 
      addMarkers(data = local_burritos(),
      lat = ~latitude,
      lng= ~longitude,
      popup = ~popup,
      icon = burr_icon) %>% 
      #radius = 8, #only need these if using circles
      #opacity = 0.8)
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })

#table with information

kable_data_map <- reactive({
    burritos_clean %>% 
      filter(zip == input$postalcode) %>% 
      select(name, menus_name, menus_description)
  }) 

output$burrito_table <- renderTable({
  
  kable(kable_data_map(), col.names = c("Burrito Details")) %>%
    kable_styling(
      font_size = 20,
      bootstrap_options = c("striped", "condensed")
    ) 
})


  
}


# Let R know that you want to combine the ui & server into an app: 
shinyApp(ui = ui, server = server)