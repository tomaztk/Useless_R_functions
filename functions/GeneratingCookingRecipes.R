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

s1 <- sample(ingredients, 1, replace = F)

