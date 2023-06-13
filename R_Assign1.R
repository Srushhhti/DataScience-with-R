library(tidyverse)
library(rvest)
library(stringr)

#1
#2


#--------------------------------------------------------------------------------------------------#
#3
tennis <- function(p) {
  x <- 0  
  while (x < 5) {
    if (runif(1) < p) {
      x <- x + 1  # Increment the number of sets played if player A wins the set
    } else {
      break  # Exit the loop if player B wins the set
    }
  }
  return(x)
}
matches <- numeric(1000)
sum <- 0
for(i in 1:1000){
  matches[i]<- tennis(0.7)
  sum = sum + matches[i]
}
ans <- mean(matches)

#--------------------------------------------------------------------------------------------------#
#4
MontyHall <- function() {
  prize <- sample(1:3, 1)
  initial_choice <- sample(1:3, 1)
  monty_choice <- sample(setdiff(1:3, c(initial_choice, prize)), 1)
  final_choice <- sample(setdiff(1:3, monty_choice), 1)
  if (final_choice == prize) {
    return(1)  
  } else {
    return(0)
  }
}

ans=0
for(i in 1:1000){
  result <- MontyHall()
  ans = ans + result
}
prob = ans/1000

#--------------------------------------------------------------------------------------------------#
#5
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking <- html %>% html_elements(".countdown-index") %>% html_text() %>% substr(2,3) %>% as.numeric()
nameMovie <- html %>% html_elements(".article_movie_title a") %>% html_text()
tscore <- html %>% html_elements(".tMeterScore") %>% html_text()
year <- html %>% html_elements(".start-year") %>% html_text() %>% substr(2,5) %>% as.numeric()
