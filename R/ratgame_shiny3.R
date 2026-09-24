install.packages("shinydashboard")
library(shiny)
library(shinydashboard)


#### load all datasets available to play the game ####
game_files <- list.files(pattern = "r_game", full.names = TRUE, recursive = TRUE) #makes list of all files with "r_game" in name, looks through all subfolders (recursive)
game_data<-lapply(game_files,read.csv) #reads in all csv files with r_game in name
for (i in game_data){ #give each name in list a unique name - need to change to be endless if required
  names(game_data)[1] <- "fish"
  names(game_data)[2] <- "rodents"
} 


#### UI v2 ####
#TO DO: 
  # - CHECK IF "" and '' are the same for JS elements
  # - button move to diff. pages (in progress)
  # - link buttons to datasets using Sam code on github

  #### 1. pick a dataset to run ####
#page 1. pick a dataset (~85% complete, just needs some aesthetic changes)
pg1<- tabItem(
    tabName = "page1", titlePanel(title = "Pick a Dataset"),
    tagList(
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Hanken+Grotesk:ital,wght@0,100..900;1,100..900&display=swap');

    * { box-sizing: border-box; }/* everything fits in this box */
    html, body {
      margin: 0;
      padding: 0;
      height: 100%;
      overflow: hidden;/* cuts off excess, doesn't scroll */
      background-color: #e0eec2;
    }
    .selectize-input {/* dropdown box */
      min-height: 20px;
      height: auto;
      padding-top: 10px;
    }
    .selectize-input input::placeholder {/* dropdown box placeholder text */
      color: #6b8a27;
      font-family: 'Hanken Grotesk', sans-serif;
      font-style: italic;
    }
    .selectize-dropdown-content {/* dropdown box content text */
      color: #3b4d15;
      font-family: 'Hanken Grotesk', sans-serif;
      font-style: normal;
    }
    .selectize-dropdown .option {/* dropdown box actual dropdown section */
      min-height: 10px;
      padding: 6px 10px;
    }
    .selectize-dropdown .option.active {/* colour when scroll over dropdown box option */
      background-color: #a8a57d;
    }

   /* to centre whole page */
   
    .start-screen {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100vh;
      text-align: center;
    }

    .hero-wrap {/* to overlay text & buttons on image, need this wrap */
      position: relative;
      width: auto;
      max-width: auto;
      margin-bottom: NA;
    }
    .hero-img {/*image */
      width: 100%;
      max-height: 100vh;
      height: auto;
      object-fit: contain;
      display: block;
    }
    .hero-overlay {/*overlay to make image transparent */
      position: absolute;
      height: 100%;
      width: 100%;
      top: 0;
      left: 0;
      background: rgba(224, 238, 194, 0.7);
      color: white;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: column;
    }
    .hero-text {/*text */
      text-align: center;
      font-family: 'Hanken Grotesk', sans-serif;
      font-weight: 800;
      font-size: clamp(2.5rem, 6vw, 5rem);
      color: #0B1307;
      margin-bottom: 30px;
    }
    .hero-dropdown {/*dropdown button */
      width: min(320px, 100%);
      margin-top: 30px;
    }
  ")),
  
  div(
    class = "start-screen",
    div(
      class = "hero-wrap",
      img(src = "https://raw.githubusercontent.com/mf-mouse/TheRatGame/main/R/food.png",
          class = "hero-img"),
      div(
        class = "hero-overlay",
        h1("CHOOSE AN ANIMAL TYPE", class = "hero-text"),
        div(
          class = "hero-dropdown",
          selectizeInput(
            'taxon_select',
            label = NULL,
            choices = names(game_data),
            options = list(
              placeholder = 'Start typing to search for animal datasets',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(pgbutton_UI("next_button"))
        )
      )
    )
    )
)

  #### 2. enter the game ####
#page 2 - pick a habitat (in progress)
pg2<- tabItem(
  tabName = "page2", titlePanel(title = "Pick a Habitat"),
  tagList(
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Hanken+Grotesk:ital,wght@0,100..900;1,100..900&display=swap');

    * { box-sizing: border-box; }/* everything fits in this box */
    html, body {
      margin: 0;
      padding: 0;
      height: 100%;
      overflow: hidden;/* cuts off excess, doesn't scroll */
      background-color: #c9e7fd;
    }
    .selectize-input {/* dropdown box */
      min-height: 20px;
      height: auto;
      padding-top: 10px;
    }
    .selectize-input input::placeholder {/* dropdown box placeholder text */
      color: #6b8a27;
      font-family: 'Hanken Grotesk', sans-serif;
      font-style: italic;
    }
    .selectize-dropdown-content {/* dropdown box content text */
      color: #3b4d15;
      font-family: 'Hanken Grotesk', sans-serif;
      font-style: normal;
    }
    .selectize-dropdown .option {/* dropdown box actual dropdown section */
      min-height: 10px;
      padding: 6px 10px;
    }
    .selectize-dropdown .option.active {/* colour when scroll over dropdown box option */
      background-color: #a8a57d;
    }

   /* to centre whole page */
   
    .start-screen {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100vh;
      text-align: center;
    }

    .hero-wrap {/* to overlay text & buttons on image, need this wrap */
      position: relative;
      width: auto;
      max-width: auto;
      margin-bottom: NA;
    }
    .hero-img {/*image */
      width: 100%;
      max-height: 100vh;
      height: auto;
      object-fit: contain;
      display: block;
    }
    .hero-overlay {/*overlay to make image transparent */
      position: absolute;
      height: 100%;
      width: 100%;
      top: 0;
      left: 0;
      background: rgba(201, 231, 253, 0.7);
      color: white;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: column;
    }
    .hero-text {/*text */
      text-align: center;
      font-family: 'Hanken Grotesk', sans-serif;
      font-weight: 800;
      font-size: clamp(2.5rem, 6vw, 5rem);
      color: #141719;
      margin-bottom: 30px;
    }
    .hero-dropdown {/*dropdown button */
      width: min(320px, 100%);
      margin-top: 30px;
    }
  ")),
  
  div(
    class = "start-screen",
    div(
      class = "hero-wrap",
      img(src = "https://raw.githubusercontent.com/mf-mouse/TheRatGame/main/R/habitats.png",
          class = "hero-img"),
      div(
        class = "hero-overlay",
        h1("PICK A HABITAT", class = "hero-text"),
        div(
          class = "hero-dropdown",
          selectizeInput(
            'taxon_select',
            label = NULL,
            choices = names(game_data),
            options = list(
              placeholder = 'Start typing to search for available habitats',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        div(pgbutton_UI("next_button")),
        div(pgbutton2_UI("prev_page"))
      )
    )
  )
))



shinyApp(pg2, server)



#### page buttons #### --> go to next/previous page
#need to indent into main UI code for each page
#add button to change to next page
#make mini ui for button so can re-use
#module button so can re-use
pgbutton_UI <- function(id) {
  tagList(
    tags$style(HTML("
        .pg-button {/*next page button */
        height: min(50px, 100%) !important;
        width: min(200px, 100%) !important;
        margin-top: 50px !important;
        background-color:#092841 !important;
        padding: 10px 50px !important;
        font-weight: bold !important;
        font-family: 'Hanken Grotesk', sans-serif !important;
        color: #e6ebdf !important;
        border-radius: 10px !important;
    }
    .pg-button:hover, .pg-button:focus, .pg-button:active {
        background-color: #e0d5b9 !important;
        color: #718b4f !important;
      }
    ")),
  actionButton(NS(id, "next_page"), #wraps input on id so can use multiple times and won't collide
               label="Continue", class = "pg-button"))
}

#mini server for page buttons 
#again, module so can re-use
pgbutton_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$pgbutton, {
      updateTabItems(session, "page1","page2")
    })
  })
}


# go back button
#ui
pgbutton2_UI <-function(id) {
  tagList(
    tags$style(HTML("
        .pg-button2 {/*previous page button */
        height: min(50px, 100%) !important;
        width: min(200px, 100%) !important;
        margin-top: 50px !important;
        background-color:#092841 !important;
        padding: 10px 50px !important;
        font-weight: bold !important;
        font-family: 'Hanken Grotesk', sans-serif !important;
        color: #e6ebdf !important;
        border-radius: 10px !important;
    }
    .pg-button2:hover, .pg-button2:focus, .pg-button2:active {
        background-color: #e0d5b9 !important;
        color: #718b4f !important;
      }
    ")),
    actionButton(NS(id, "prev_page"), #wraps input on id so can use multiple times and won't collide
                 label="Go Back", class = "pg-button2"))
}


#server
pgbutton2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$page_change, {
      updateNavbarPage(session=session,
                       inputId="pages",
                       selected="page_one")
    })
  })
}



shinyApp(ui, server)


#### server ####



shinyApp(ui, server)






######## old code to keep in case need ########
#can't exactly remember what the plan was here...but will figure it out
actionButton("remove_taxon", "Change animal dataset") #remove taxon dataset, if want to alter choice


#2. once picked dataset, need to select the column titles from that dataset and put as input labels
conditionalPanel('input.taxon_select != ""', #if taxon_select input not empty, do the following
                 selectizeInput(...)) #select from names of columns in dataset, (see server below)

conditionalPanel('input.taxon_select != ""',
                 grep('', names(input.taxon_select), value = TRUE)) #might work?

#or could do
chars<-grep('', names(input.taxon_select), value = TRUE)
#then see #3

#ideas for #2, not necessarily in code
#find column names from game_data for selected dataset
#does this go in server section?
grep('', names(game_data$taxon2), value=TRUE) #this works, but hard-coded
grep('', names('input.taxon_select'), value = TRUE) #might work?


#3. then need selection box for each characteristic (i.e. column) in taxon_select
#from above -> #2.....
#shiny can do up to 12 different characteristics (design layout)
#i.e. if habitat column, need to be able to choose from values in that column

for (name in chars) {
  conditionalPanel('input.taxon_select != ""', #if taxon selected
                   selectizeInput('char_select', 'char',
                                  choices = name['input.taxon_select'], #this line won't work - think about how to change
                                  multiple = FALSE, #does not allow selection of multiple items per characteristic
                                  options = list(
                                    placeholder = 'Start typing to search for characteristics',#in box, have this text before start typing
                                    onInitialize = I('function() { this.setValue(""); }') #when start typing, look for characters in colnames that contain letters typed
                                  )))
  }


actionButton("remove_c", "Remove characteristic") #add species characteristic

#### server v2 ####

taxon_df <- shiny::reactiveValues() #create reactive values object
# create empty dataframe to put chosen values in - may not need this
ing_df$df <- data.frame("quantity" = numeric(), "units" = character(), 
                        "ingredient_name" = character(), 
                        "FoodID" = numeric(), stringsAsFactors = F)

#1. reactive - pick taxon dataset 
taxon_ch <- eventReactive(input$food_id,{
  measure_df <- ca_food_name[ca_food_name$FoodID==input$food_id, "FoodID"] %>% 
    left_join(ca_conversion_factor) %>% 
    left_join(ca_measure_name) %>% 
    select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
  measure_df
})

#### UI ####
#header
Header = dashboardHeader(title = "The Rat Game", titleWidth = 250)

#sidebar
Sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Play the Game", tabName = "PlayGame", icon = icon("gamepad")),
    menuItem("How to Play", tabName = "HowPlay", icon = icon("circle-question")),
    menuItem("Why are rats important?", tabName = "WhyImportant", icon = icon("earth-americas")),
    menuItem("Created by", tabName = "CreatedBy", icon = icon("people-line"))
  ))

#tabs
Tab1 = tabItem(tabName = "PlayGame",
               fluidRow(
                 column(width = 12, offset = 0,
                        box(title = "What size do you want your animal to be?",
                            background = "navy",
                            solidHeader = TRUE,
                            selectInput( 
                              "select", 
                              "Select size:", 
                              list("Big" = "big", "Medium" = "med", "Small" = "small") 
                            ), ),
                        box(title = "What habitat do you want your animal to live in?",
                            background = "light-blue",
                            solidHeader = TRUE,
                            selectInput( 
                              "select", 
                              "Select habitat:", 
                              list("Woodland" = "wood", "Desert" = "des", "Water" = "water") 
                            ), ),
                        box(title = "What food do you want your animal to eat?",
                            background = "maroon",
                            solidHeader = TRUE,
                            selectInput( 
                              "select", 
                              "Select food:", 
                              list("Seeds" = "seed", "Roots" = "root", "Grass" = "grass", "Insects" = "ins") 
                            ),))
               )
)

Tab2 = tabItem(tabName = "HowPlay",
               fluidRow(
                 box(title = "What is the aim of the game?",
                     br(),
                     "You are a rodent. The question is, which one? There are over 2000 species of rodent in the world, including
                     mice, rats, squirrels, beavers, and so many more! They all exist in different habitats, are different sizes, 
                     and eat different foods. The brown rat you catch rooting through your trashcan is just the beginning. In fact,
                     most rodent species avoid people at all costs. With this game, you can choose your ideal size, habitat, and diet
                     to find out what rodent you would be, if you were one living out in the wild. Once you've chosen these things,
                     you'll be shown a picture of your rodent and given some facts about it.",
                     background = "light-blue",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("AIMimage")),
                 box(title = "How do I pick a size?",
                     br(),
                     "On the game page, there is a SIZE box, allowing you to choose if you want to be a 
                     big, medium, or small sized rodent. A small rodent weighs 0 - 100 g (0 - 0.22 lbs). A
                     medium-sized rodent weighs 101 - 500 g (0.22 - 1.10 lbs). A large rodent is anything weighing
                     over 501 g (1.10 lbs). The largest rodent is the Capybara, which can weigh up to 66 kg (174 lbs)!",                     
                     background = "light-blue",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("SIZEimage")),
                 box(title = "How do I pick a habitat?",
                     br(),
                     "A habitat is a word used to describe the place an animal lives. This includes the plants, landscape, and the amount of sunlight
                     and water in the area. Using the HABITAT box on the game page, you can choose if you want to live in a woodland, a desert, or 
                     in the water. A woodland is an area with lots of trees and plants, a good amount of water, and some sunlight. A desert has a lot of
                     sunlight, but not much water or many plants. An animal living in water spends most of its time near or in this water, and needs the water
                     to survive, breed, and find food.",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("HABimage")),
                 box(title = "How do I pick a food?",
                     br(),
                     "On the game page, there is a FOOD box, allowing you to choose what you want to eat as a rodent. What an
                     animal eats is known as its diet. You can choose a diet of seeds, roots, grass, or insects. Seeds come
                     from plants, and, if they are not eaten, can transform into new plants. Humans also eat seeds, such as pumpkin, 
                     sunflower, and sesame seeds. Rodents that eat seeds often have special teeth to break through the seed's hard
                     outer shell (husk) to reach the tasty centre. A root is also part of a plant - the part that grows underground. 
                     Rodents may have strong arms or sharp claws to help them dig and reach the roots they want to eat. Roots can be 
                     quite tasty, even for us humans. Did you know that a carrot is a root? Grass can also be a part of a rodent's diet.
                     We don't typically eat grass as people, but it can be very tasty to some animals. Grass is not always very nutritious,
                     so animals that eat it often have to get enough nutrients to stay healthy. Insects are also eaten by rodents. Bees, ants,
                     crickets, flies and bugs are all insects. Insects are very nutritious, even for humans, and some people eat insects
                     daily as part of their diet.",                     
                     background = "light-blue",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("FOODimage"))
               )
)

Tab3 = tabItem(tabName = "CreatedBy",
               fluidRow(
                 box(title = "Maya Folkes",
                     br(),
                     "TEST FROM WIKI: Rats are various medium-sized, long-tailed rodents. 
                     Species of rats are found throughout the order Rodentia, 
                     with the genus Rattus containing the most familiar rats. 
                     Other rat genera include Neotoma (pack rats), 
                     Bandicota (bandicoot rats) and Dipodomys (kangaroo rats).",                     background = "light-blue",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("MFimage")),
                 box(title = "Sam Borstein",
                     br(),
                     "TEST FROM WIKI: Rats are various medium-sized, long-tailed rodents. 
                     Species of rats are found throughout the order Rodentia, 
                     with the genus Rattus containing the most familiar rats. 
                     Other rat genera include Neotoma (pack rats), 
                     Bandicota (bandicoot rats) and Dipodomys (kangaroo rats).",                     background = "light-blue",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     imageOutput("SBimage"))
               )
)

#body
Body = dashboardBody(tabItems(Tab1, Tab2, Tab3))

#UI
ui = dashboardPage(header = Header,
                   sidebar = Sidebar,
                   body = Body,
                   skin = "green")

#### run app ####
server <- function(input, output, session) {
  # make reactive to store rat types
  rat_df <- shiny::reactiveValues()
  rat_df$df <- data.frame("quantity" = numeric(), 
                          "units" = character(), 
                          "ingredient_name" = character(), 
                          "FoodID" = numeric(), 
                          stringsAsFactors = F)}

shinyApp(ui, server)


