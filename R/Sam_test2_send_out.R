#' Play the rodent game. Find the rodent you are based on inputs and get an image of it
#' @param Taxa A matrix or data frame. The first column must be labelled species. Remaining columns should be traits to search for.
#' @param ... Column names and inputs to identify the rodent you are. Must be present in Taxa.
#' @param verbose Logical. Print verbose messaging. Default is true.
#' @return A message containing the identified rodent that fits your criteria and optional messaging to get an image of it through iNaturalist.
#' @author Maya Folkes, Samuel Borstein
#' @examples
#' animals <- read.csv("r_game_datav2.csv")
#' play_rodent_game(animals, size = "large", habitat = "woodland", food = "insects", Verbose = TRUE)
#' @export

play_rodent_game <- function(Taxa, ..., Verbose = TRUE){
  
  #Capture the user input passed with "..." into a list. This lets a user enter whatever columns they want
  user_choices <- list(...) 
  
  #For each column passed by users
  for (col_name in names(user_choices)) {
    if (!(col_name %in% colnames(Taxa))) {    #check if the column  exists in the data
      stop(paste("Error: The column", col_name, "does not exist in your dataset!"))#error if not
    }
    Taxa <- Taxa[ Taxa[[col_name]] == user_choices[[col_name]], ]#Filter the dataframe based on user input
  }
  message("                                                         ") #print blank line message
  if(Verbose == TRUE){
    proceed <- readline(prompt = "Are you ready to find out what rodent you are?")#display this prompt, save as proceed vector (to continue)
    if (!toupper(proceed) %in% c("YES","Y","YEAH")) { #if user prints something other than yes (in any format i.e. YES/yes/Yes/yeS)
      message("Game cancelled. Re-run to play again!") #print this message
      return(NULL)#end the game
    }
  }
  if (nrow(Taxa) == 0) { #if there are no matches (if matches = NA)
    message("No rodent like this exists. You've made a new species!") #print message and terminate
  }else{
    species_match <- Taxa$species[1]
    print(paste("You are a:", species_match))
    photo <- readline(prompt = "Do you want to see a photo?")#display this prompt, save as photo vector (to continue)
    if (!toupper(photo) %in% c("YES", "Y", "YEAH")) { #if user prints something other than yes (in any format i.e. YES/yes/Yes/yeS)
      print("No worries! Game cancelled - re-run to play again!") #print this message
      return(NULL)#end the game
    } else { #check internet
      Search_Inat <- rinat::get_inat_obs(taxon_name = species_match, photo_license = "any", maxresults = 100)#search rinat - check photo license
      ImageCheck <- Search_Inat[Search_Inat$image_url != "", ]
      img_url <- ImageCheck$image_url[2]#get an image url by index - add in option to ask if want more than one image if more than three images, if so, ask multiple from inat
      picture_raw <- jpeg::readJPEG(getURLContent(img_url), native = TRUE)#read in in the jpeg
      res = dim(picture_raw)[2:1]#get the resolution [x,y]
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),
           asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
      graphics::rasterImage(picture_raw,1,1,res[1],res[2])
    }
  }
}
