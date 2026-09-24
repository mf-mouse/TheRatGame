install.packages("shinydashboard")
library(shiny)
library(shinydashboard)


#### load all datasets available to play the game ####
game_files <- list.files(pattern = "r_game", full.names = TRUE, recursive = TRUE) #makes list of all files with "r_game" in name, looks through all subfolders (recursive)
game_data<-lapply(game_files,read.csv) #reads in all csv files with r_game in name
for (i in game_data){ #give each name in list a unique name - need to change to be endless if required
  names(game_data)[1] <- "fish"
}

pg1<- tabItem(
  tabName = "page1",
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

pgbutton_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$pgbutton, {
      updateTabItems(session, "page1","page2")
    })
  })
}

server <- function(input, output, session) {}

shinyApp(pg1, server)
