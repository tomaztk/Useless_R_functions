##########################################
# 
# Useless (and Quarky) R Job Title Generator
# Series:
# Little Useless-useful R functions #3
# Created: October 24, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

RJobTitle <- function(){

### Values
 Fancystuff = c( 'Regional','Only the best','Insane','Qualitative','Virtuous','Senior'
                  ,'Junior','In-House','Outsourced','Magnificent','Evolutionary','Customer orientated','Product'
                  ,'Dynamic','Corporate Lead','Legacy','Investor','Direct','International','Over-seas','Internal','Human'
                  ,'Creative','Volunteer','Lead','4 Stages of','Complete','Most Advanced','State of the art','Super high'
                  ,'First Class','Powerful','Data','Head of','Master of','Chief of','Officer','Lead','Special')
  
 RStuff <- c('CRAN Lover','R Environment','userR! conference','R Package','Lattice','Graphics','Factors','Probability distribution'
             ,'Sampling','Vectors and numbers','package dependencies','Set.seed','Visualization','Data manipulation','Machine Learning'
             ,'Plot.ly','Shiny','Sys.getenv','Lubridate','NA','NaN','Environment history','R Script Editor','S3 Class'
             ,'Box Plot','Infix Operator','Parametrization','Slow Script','Long running Script','R ggplot library','Statistical Models'
             ,'%>% clause','R WHILE loop','Kronecker product','Matching operator','Integer division','dplyr and data.table' )
 

 Roles = c('Analyst','Project Manager','Expert','Manager','Programmer','Artist'
           ,'Tamer','Developer','Wrangler','DataFramer','Statistician','Philosopher','ggPlotter'
           ,'Data Manipulator','tEvangelist','Hero','Guru','partz professional','Composer','Reader','Outliner'
           ,'Proof-reader','zoo Assistant','data.frame Operator','Matrix Maker','dpylr lover'
           ,'Tester','Deep tester','Backward tester','Office hater','Hollister','Warrior','Junkie'
           ,'Wizard','Leader','King','Github Approver','CARET Engineer','e1071 Architect','Rockstar','Ninja','R Coder'
           ,'Python Hater','Ninja','Captain','Strategist','Consultant','Counsellor','Organizer'
           ,'Emacs Endorser','Dog','Library Installer','Cheever','RStudio specialist','R Fanboy','Functionist'
           ,'Researcher','Shadower','Variable lover','Knitter Helper','Debugger','Data Scientists'
           ,'Statistician','Coffee Addict','Tidyverser','Knitter'
           ,'R-Studio dispatcher','Advanced Copy/paster','R-Bloggers subscriber','Markdown Writter' )
 

### Ordering of subsets and generating R Job Title
RTitle <- paste(sample(Fancystuff,1, replace=TRUE),sample(RStuff,1, replace=TRUE),sample(Roles,1, replace=TRUE), sep= " ")
RTitle <- paste("Your Greatest of them greatest made-up R job title is: ", RTitle)
return(RTitle)

}

#Run the script
RJobTitle()
