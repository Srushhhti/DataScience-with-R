library(ggplot2)
library(tidyverse)
library(rvest)
library(dplyr)
library(datasets)

#1---------------------------------------------------------------------------------------------
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", color = "Species")
#####-----------------------------------------------------------------------------------------------#####

#2---------------------------------------------------------------------------------------------
data(txhousing)
# Check the structure of the dataset
str(txhousing)
# Remove rows with missing values
txhousing <- txhousing[complete.cases(txhousing), ]

# Create a scatterplot of median price by year
ggplot(txhousing, aes(x = year, y = median)) +
  geom_point() +
  labs(x = "Year", y = "Median Price")

# Create a boxplot of median price by month
ggplot(txhousing, aes(x = month, y = median)) +
  geom_boxplot() +
  labs(x = "Month", y = "Median Price")
#####-----------------------------------------------------------------------------------------------#####

#3---------------------------------------------------------------------------------------------
titanic <- read.csv("titanic.csv")
survive_dead <- c('0' = "Died", '1' = "Survived")
survived_label <- function(variable,value){
  return(survive_dead[value])
}
final_Plot <- ggplot(titanic, aes(x = Fare, color = factor(Sex))) +
  geom_boxplot() +
  facet_wrap(~Survived, dir="v", labeller  = as_labeller(survive_dead), strip.position = "left") +
  labs(x = "Fare", color = "Sex") +
  scale_color_manual(values = c("red", "blue"))

final_Plot




