#load data
library(tidyverse)
library(proxy)
library(shiny)
#game <- read.csv("C:/Users/Lem/Dropbox (CBQG)/BST260/boardgameproject/recommender_data.csv")

# import data
df <- read.csv("C:/Users/Lem/Dropbox (CBQG)/BST260/boardgameproject/df_recode_final_1127.txt", sep = "|")

# only keep data on or after 1980
df1 <- df %>% filter(year >= 1980)

# omit na
df1_na_omit <- na.omit(df1)

# drop columns not wanted
drops <- c("rank", "bgg_url", "game_id", "image_url", "mechanic", "category", "designer")
df_rec <- df1_na_omit[ , !(names(df1_na_omit) %in% drops)]
#Recommender function
# function to get similarity between two boardgames using Euclidean distance
get_most_simi <- function(game_name, df) {
  # get only the mechanics and category columns
  df_new <- df[, c(1, 20:153)]
  #df_new <- df
  if (game_name %in% df_new$names) {
    print("Yay your game is found!")
    # create a vector of the features of the user's favorite game
    game_played <- as.numeric(as.vector(df_new[df_new$names == game_name, ]))[-1]
    score <- numeric(0)
    for (i in 1:dim(df_new)[1]) {
      score <- c(score, 
                 dist(list(game_played, as.numeric(df_new[i, -1])), method = "Euclidean"))
    }
    names(score) <- df_new[, 1]
    
    games <- names(score)
    score <- as.data.frame(score)
    score$game <- games
    score <- score[order(score$score), ]
    similar_games <- score %>% 
      filter(score < quantile(score, 0.02) & 
               score != 0)
    game_list <- df %>%
      filter(names %in% similar_games$game)
    recommendations <- game_list[order(game_list$geek_rating, decreasing = TRUE), ] %>%
      select(names) %>%
      head(10)
    return(recommendations)
  }
  else 
    print("Loading should only take a few seconds! If no games appear, please try another :)")
  
  # calculate Euclidean distance between user's favorite game and every other game in our data
  
  
}

#just testing here
#get_most_simi("Sushi Go!", game)
#output error:  Error in dist(list(game_played, as.numeric(df_new[i, -1])), method = "Euclidean") : 
#invalid distance method 

#shiny app
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
  # add a title
  titlePanel("Board Game Recommender"),
  textInput("text", label = h3("Game"), value = "Name a game :)" )),
  mainPanel(
    titlePanel("Recommended Games"),
  tableOutput("table"))
)
)
server <- function(input, output) {
  output$value <- renderText({ input$text})
  output$table <- renderTable({
    recommendations <- get_most_simi(input$text, df_rec)
  })
}
shinyApp(ui=ui,server=server)