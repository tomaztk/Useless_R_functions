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
steps_des <- c("Take ", "and", "make", "bake")


string_1 <- as.vector(sample(ingredients,1,replace=F))
string_2 <- as.vector(sample(actions,1,replace=F))

recipe <- paste(string_1,string_2,sep = " ")



#combn(ingredients,2)

Rec_Ing <- c(Rec_Ing, sample(c(quantites)))



RandomRecipe <- function(actions=0, ingredients=0, quantites=0, Steps=0, Kitchen_supply=0) {
  Rec_Ing <- " "
  if(quantites>0)    Rec_Ing <- c(Rec_Ing, sample(c(quantites)))
  if(ingredients>0)    Rec_Ing <- c(Rec_Ing, sample(ingredients))
  if(actions>0)  Rec_Ing <- c(Rec_Ing, sample(actions))
  if(Steps>0)  Rec_Ing <- c(Rec_Ing, sample(Steps))
  if(Kitchen_supply>0)  Rec_Ing <- c(Rec_Ing, sample(Kitchen_supply))
  
  cat(sample(Rec_Ing, length(Rec_Ing)))
}

RandomRecipe(ingredients =10, quantites=2, Steps=4)




