library(imager)
library(tidyverse)
library(rvest)
library(dplyr)
library(MASS)

#--------------------------------------------------------------------------------------------------#
#a question
data(iris)
par(mfrow = c(1, 3))

# compare the distribution of sepal lengths across different species (Sepal.Length ~ Species)
boxplot(Sepal.Length ~ Species, data = iris, main = "Sepal Length", ylab = "Length")
# similarly, compare the distribution of petal lengths across different species 
boxplot(Petal.Length ~ Species, data = iris, main = "Petal Length", ylab = "Length")
# similarly, compare the distribution of sepal widths across different species
boxplot(Sepal.Width ~ Species, data = iris, main = "Sepal Width", ylab = "Width")

plot(Sepal.Length ~ Petal.Length, data = iris, col = iris$Species, xlab = "Petal Length", ylab = "Sepal Length", main = "Scatterplot by Species")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 1)

#from scatterplot i can conclude that setosa have the smallest sepal length 
#and petal length whereas versicolor and virginica have high petal length and sepal length


#--------------------------------------------------------------------------------------------------#
#b question
flip <- function(imagedef){
  col.mat <- as.array(imagedef[, ,1, ])
  dims <- dim(col.mat)
  flippedimage <- array(0, dim = dims)
  for(j in 1:dims[1])
  {
    flippedimage[j, , ] <- col.mat[dims[1] - j + 1, , ]
  }
  return(as.cimg(flippedimage))
}

imagedef<-load.image("dog.jpeg")
flippedIm <- flip(imagedef)

# Let's plot side by side
par(mfrow = c(1,2))
plot(imagedef,main = "default image")
plot(flippedIm,main = "flipped image")

#--------------------------------------------------------------------------------------------------#
#c question
data(ships)
#boxplot one of the best to see why B ship is the most dangerous
boxplot(incidents ~ type, data = ships, main = "Incidents of all types of ships", ylab = "Incidents")
#add more plots...

#--------------------------------------------------------------------------------------------------#
#d question
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
titles_ques <- html %>% html_elements(".s-post-summary--content-title .s-link") %>% html_text()
views <- html %>% html_elements(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
answers <- html %>% html_elements(".has-answers .s-post-summary--stats-item-number") %>% html_text()
votes <- html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text() 
stats_data <- data.frame("Title" = titles_ques, "Views" = views, "Answers" = answers, "Votes" = votes )

#--------------------------------------------------------------------------------------------------#
#e question

vitamins <- function()
{
  #assigned 1 to all tablets
  bottle = rep(1,100)
  daystotal <- 0
  while(TRUE){
    #count days
    daystotal = daystotal+1
    #take out a random tablet 
    tablet <- sample(bottle,1)
    #if tablet ==1
    if(tablet==1){
      bottle = c(bottle,0.5,0.5)
    }
    #tablet==0.5 then return the number of days
    else {
      return(daystotal)
    }
  }
}
#can change the number of trials
numsOfTrials <- 1000
totalcount <- 0
for(i in 1:numsOfTrials){
  totalcount = totalcount + vitamins();
}
avg = totalcount/numsOfTrials
avg
