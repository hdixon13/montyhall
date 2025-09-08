#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door for the game.
#'   
#' @description
#'   `select_door()` random generates a chosen door numbered 1-3
#'   
#' @details
#'   One of the three doors will be selected at random. Two 
#'   of the doors have goats behind them, and one of the doors
#'   has a car behind it. 
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a numeric vector
#'   indicating the number of the chosen door.
#'   
#' @examples
#'   first pick <- select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a door with a goat behind it.
#'   
#' @description
#'    `open_goat_door()` generates a door with a goat behind it
#'    to be opened.
#'  
#' @details
#'   After a door is chosen, the host must open a door that is
#'   different than the chosen door. Regardless if the player
#'   opened a goat door or a car door, the host must open a door
#'   with a goat behind it, because they cannot open a car door.
#' 
#' @param game A length 3 character vector represents the label 
#'   of the doors, where one door is labeled "car" and the other
#'   two are labeled "goat".
#' @param a.pick An integer that represents the initially chosen door, 
#'   a number between 1 and 3.
#'  
#' @return The function returns a length 1 numeric vector
#'   indicating the opened door of the host. It will be a 
#'   different number than the door from select_door()
#'   
#' @examples
#'   open_goat_door()
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change door or stay
#'   
#' @description
#'   `change_door()` generates a door number based on if the 
#'   player decides to keep their initial door pick or switch.
#'   
#' @details
#'   After the host opens the goat door, the contestant has the
#'   option to stay with their initially chosen door, or "switch" 
#'   to choose the unopened door.
#'   
#' @param stay indicates if the contestant keeps their initially 
#'   chosen door
#' @param opened.door The goat door opened by the host
#' @param a.pick An integer that represents the initially chosen door, 
#'   a number between 1 and 3.
#'   
#' @return The function returns a length 1 numeric vector of a 
#'   number between 1 and 3 indicating the final opened door of 
#'   the contestant. It will either be the same door or a 
#'   different door than the initial pick.
#'   
#' @examples
#'   change_door( stay = T, opened.door = 2, a.pick=3)
#'     # 2 
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant wins or loses the game.
#'   
#' @description
#'   `determine_winner()` generates a win or loss based on if
#'   the contestants final door is a car door or not.
#'   
#' @details
#'   After the contestant chooses to stay or switch their door,
#'   their final door is chosen. If the final door has a goat 
#'   behind it the contestant loses, and if the final door has 
#'   a car behind it, the contestant wins.
#'   
#' @param final.pick The door number that the player chooses
#'   as their final door, a number between 1 and 3
#' @param game A length 3 character vector represents the label 
#'   of the doors, where one door is labeled "car" and the other
#'   two are labeled "goat".
#' 
#' @return The function returns a character vector indicating 
#'   if the contest wins or loses the game. The contestant 
#'   wins the game if there is a car behind the final door.
#' 
#' @examples
#'   determine_winner(final.pick = 2, game = c("goat", "car", "goat"))
#'     # "WIN"
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'    Play a simulation game of the montyhall scenario.
#' @description
#'    `play_game()` uses the previous code to construct a
#'    comprehensive simulation of the entire game.
#'    
#' @details
#'   The simulation begins by the game being created. Next, the 
#'   contestant chooses the first door. After the door is selected, 
#'   the host opens a door with a goat behind it. Next, the 
#'   contestant decides if they want to stay with their chosen door 
#'   or choose the last unopened door. Finally, the door they 
#'   chose is revealed, and it is determined if they win or lose 
#'   the game. 
#'   
#' @param ... no arguments are used by the function. 
#' 
#' @return The function returns a data frame in the form of
#'   a table with columns of "stay" and "switch" and results 
#'   of WIN" or "LOSE" indicating if the contest wins or loses 
#'   the game. The contestant wins the game if there is a car 
#'   behind the final door.
#'   
#' @examples play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Play multiple simulations of the Monty Hall game
#'  
#' @description
#'  `play_n_games()` simulates the Monty Hall game being played 
#'  multiple times
#'  
#' @details
#'   The simulation runs the game 100 times by default, but can be 
#'   changed by changing "n". For each n, a new game is created, and
#'   the Monty Hall game is played based off of the functions above. 
#'   This function calculates the amounts of wins and losses for all 
#'   the games played.
#'   
#' @param n represents the number of games that are simulated.
#' 
#' @return A data frame that displays the results of each game in a 
#'   table format. 
#'   
#' @examples
#'   results <- play_n_games(10000)
#'     # shows results for 10,000 games for both staying and switching
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
