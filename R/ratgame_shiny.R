install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

#Valid colors are: red, yellow, aqua, blue, light-blue, green, 
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
Tab = tabItem(tabName = "PlayGame",
              fluidRow(
                column(width = 12, offset = 0,
                       box(title = "What size do you want your animal to be?",
                           background = "navy",
                           solidHeader = TRUE),
                       box(title = "What habitat do you want your animal to live in?",
                           background = "light-blue",
                           solidHeader = TRUE),
                       box(title = "What food do you want your animal to eat?",
                           background = "maroon",
                           solidHeader = TRUE))
              )
)

#body
Body = dashboardBody(tabItems(Tab))

#UI
ui = dashboardPage(header = Header,
                   sidebar = Sidebar,
                   body = Body,
                   skin = "green")

#### older code ####
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "The Rat Game"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("How to Play", tabName = "How to Play", icon = icon("circle-question")),
    menuItem("Why are rats important?", tabName = "Why are rats important?", icon = icon("earth-americas"))),
    menuItem("Created by", tabName = "Created by", icon = icon("people-line"))
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, offset = 0,
             box(title = "What size do you want your animal to be?",
                 background = "navy",
                 solidHeader = TRUE),
             box(title = "What habitat do you want your animal to live in?",
                 background = "light-blue",
                 solidHeader = TRUE),
             box(title = "What food do you want your animal to eat?",
                 background = "maroon",
                 solidHeader = TRUE),
      )
  )
  ))


server <- function(input, output) { }

shinyApp(ui, server)


