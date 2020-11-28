##########################################
# 
# Script that generates script for basic
# calculator functions for integers
# between 1 and 10
# Series:
# Little Useless-useful R functions #9
# Created: November 28, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - adding x11()
###########################################

# Basic concept
calc <- function(a,b,oper){
  if(a==1 & b==1 & oper=="+"){print("Result is 2")}
  if(a==1 & b==1 & oper=="-"){print("Result is 0")}
  if(a==1 & b==1 & oper=="*"){print("Result is 1")}
  if(a==1 & b==1 & oper=="/"){print("Result is 1")}
  }

calc(1,1,"-")
calc(1,1,"+")
calc(1,1,"*")
calc(1,1,"/")
