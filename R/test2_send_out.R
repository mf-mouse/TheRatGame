#Written by : maybe a mouse 🐭?
#Written on : 28 April 2026
#Purpose : Play the rodent game! Based on the user's argument choices, the function will find out what rodent you are
#Description: Chose your favourite food, habitat, size (from specified lists below) and if you are/aren't aggressive. 
#the function will ask if the user wants to proceed (if answer is not "yes" (any spelling, written in console)), the game quits
#The function will find the rodent species that fits this combination - the rodent you are! 
#food : one character value from: "seeds" ; "roots" ; "grass"
#habitat: one character value from: "woodland" ; "desert" ; "water"
#size: one character value from: "small" ; "medium" ; "large"
#Value : Outputs a character (name of rodent) and opens a browser tab to image search that rodent
#Additional notes: user must first load("fun_data.RData") to play the game

#load required packages 
library(rinat);library(jpeg);library(RCurl)

#load the data
animals<-read.csv("~/The_Rat_Game/r_game_datav2.csv") #read in dataset of choice
animal_cols<-colnames(animals)

#create names for choices based off column headings in dataframe
for (i in seq_along(animal_cols)) {
  assign(paste0("col_name_", i), animal_cols[i])
  print(paste0("col_name_", i, " = ", animal_cols[i]))
}

ls(pattern = "col_name_")

animal_cols

#make the function
play_rodent_game <- function(col_2, col_3, col_4){ #arguments are cols
  if (!(col_2 %in% animals[[col_name_2]])) { #if the input in col_2 argument is not one of the options in the dataset
    stop(paste("Invalid Input: ", col_2)) #print this message, paste the input given
  }
  if (!(col_3 %in% animals[[col_name_3]])) { #if the input in col_3 argument is not an option
    stop(paste("Invalid Input: ", col_3)) #print this message, paste the input given
  }
  if (!(col_4 %in% animals[[col_name_4]])) { #if the input in col_4 argument is not an option
    stop(paste("Invalid Input: ", col_4)) #print this message, paste the input given
  }
  print("                                                         ") #print blank line message
  proceed <- readline(prompt = "Are you ready to find out what rodent you are?")#display this prompt, save as proceed vector (to continue)
  if (!toupper(proceed) %in% c("YES","Y","YEAH")) { #if user prints something other than yes (in any format i.e. YES/yes/Yes/yeS)
    print("Game cancelled. Reload to play again!") #print this message
    return(NULL)#end the game
  }
  matches <- animals[ #create vector of matches from dataset
    animals[[col_name_2]] == col_2 & #if col_2 input matches col_2 in dataset
    animals[[col_name_3]] == col_3 & #and if col_3 input col_3 dataset
    animals[[col_name_4]] == col_4, #and if col_4 input matches col_4 dataset
    col_name_1 #find the species in the dataset that these all match, this becomes the vector's value
  ]
  if (length(matches) == 0) { #if there are no matches (if matches = NA)
    print("No rodent like this exists. You've made a new species!") #print this message
  } else { #if there are matches
    print(paste("You are a: ",matches)) #print the value of matches
    photo <- readline(prompt = "Do you want to see a photo?")#display this prompt, save as photo vector (to continue)
    if (!toupper(photo) %in% c("YES", "Y", "YEAH")) { #if user prints something other than yes (in any format i.e. YES/yes/Yes/yeS)
      print("No worries! Game cancelled - reload to play again!") #print this message
      return(NULL)#end the game
    } else { #check internet
    Search_Inat <- get_inat_obs(taxon_name = matches, photo_license = "any", maxresults = 100)#search rinat - check photo license
    ImageCheck <- Search_Inat[Search_Inat$image_url != "", ]
    img_url <- ImageCheck$image_url[2]#get an image url by index - add in option to ask if want more than one image if more than three images, if so, ask multiple from inat
    picture_raw <- readJPEG(getURLContent(img_url), native = TRUE)#read in in the jpeg
    res = dim(picture_raw)[2:1]#get the resolution [x,y]
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),
         asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(picture_raw,1,1,res[1],res[2])
    } ###needs to fail gracefully if iNat API down
  }
}


#### demo code ####
#THE RAT GAME#
#step 1: choose arguments for the following funcitons
  #col_2 - size: choose one character value from: "small"; "medium"; "large"
  #col_3 - habitat: choose one character value from: "woodland"; "desert"; "water"
  #col_4 - food : one character value from: "seeds" ; "roots" ; "grass"

#step 2: use the function - Play the rodent game!

play_rodent_game(col_2 = "small",	col_3 = "woodland",
                 col_4 ="roots") #use function with user input for arguments

