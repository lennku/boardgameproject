
#load data
library(tidyverse)
game <- read.csv("C:/Users/Lem/Dropbox (CBQG)/BST260/boardgameproject/recommender_data.csv")

#Recommender function
# function to get similarity between two boardgames using Euclidean distance
get_most_simi <- function(game_name, df) {
  # get only the mechanics and category columns
  df_new <- df[, c(1, 20:153)]
  
  # create a vector of the features of the user's favorite game
  game_played <- as.numeric(as.vector(df_new[df_new$names == game_name, ]))[-1]
  
  # calculate Euclidean distance between user's favorite game and every other game in our data
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

#just testing here
#get_most_simi("sushi", game)
#output error:  Error in dist(list(game_played, as.numeric(df_new[i, -1])), method = "Euclidean") : 
#invalid distance method 

#shiny app
library(shiny)
ui <- fluidPage(
  textInput("text", label = h3("Game"), value = "Name a game :)" ),
  tableOutput("table")
  )
server <- function(input,output){
  output$table <- renderTable({
    recommendations <- get_most_simi(input$text, game)
  })
}

shinyApp(ui=ui,server=server)

