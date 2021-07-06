##########################################
# 
# Generating cooking recipes
#
# Series:
# Little Useless-useful R functions #24
# Created: June 20, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# 
# Changelog: 
###########################################

ingredients <- c("Pasta","Rice","All-purpose flour","sugar","Baking powder","Butter","Eggs","Lemons", "Salt")
quantites <- c("Piece(s)", "Gram(s)", "Liter(s)", "Bowl")
actions <- c("Slice","Bake","Refrigerate","Cook","Steam","Dip","Leave to Rest", "grill")
Kitchen_supply <- c("spatula", "oven", "refrigerator", "pan", "sauce-pan", "whisk")
steps <- c("1-1", "1-2-1", "1-2-2", "2", "1")
steps_des <- c("Take ", "and")


string_1 <- as.vector(sample(ingredients,1,replace=F))
string_2 <- as.vector(sample(actions,1,replace=F))

recipe <- paste(string_1,string_2,sep = " ")



RandomRecipe <- function(s=0, numbers=0, symbols=0, lowerCase=0, upperCase=0) {
  ASCII <- NULL
  symbols <- 0
  if(symbols>0)    ASCII <- c(ASCII, sample(c(quantites)))
  if(numbers>0)    ASCII <- c(ASCII, sample(ingredients))
  if(upperCase>0)  ASCII <- c(ASCII, sample(actions))
  if(lowerCase>0)  ASCII <- c(ASCII, sample(actions))
  if(steps_des>0) ASCII <- c(ASCII, sample(steps_des))
  
  cat(rawToChar(as.raw(sample(ASCII, length(ASCII)))) )
}

RandomRecipe(10)




