install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

#Valid colours are: red, yellow, aqua, blue, light-blue, green, 
#navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

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
server <- function (input, output){}

shinyApp(ui, server)
