##########################################
# 
# Useless (and Quarky) R Job Title Generator
# Series:
# Little Useless-useful R functions #3
# Created: October 21, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


### Values
Roles = c('Analyst','Project Manager','Expert','Manager','Programmer','Artist'
,'Tamer','Developer','Wrangler','DataFramer','Sherpa','Philosopher','ggPlotter'
,'Data Manipulator','Evangelist','Hero','Guru','Slayer','Composer','Reader','Outliner'
,'Proof-reader','Assistant','Operator','Coffee Maker','Pizza re-heater'
,'Tester','Deep tester','Backward tester','Office hater','Hater','Warrior','Junkie'
,'Wizard','Leader','King','Approver','Engineer','Architect','Rockstar','Ninja','R Coder'
,'Python Hater','Ninja','Captain','Strategist','Consultant','Counsellor','Organizer'
,'Endorser','Cow','Dog','Library Installer','Cheever','Lazy Lasagna Eater','Fanboy','Copy/Paster'
,'Researcher','Shadower','Guerilla','Helper','Debugger','Data Scientists'
,'Statistician','Coffee Addict','Tidyverser','Knitter'
,'R-Studio dispatcher','Advanced Copy/paster','R-Bloggers subscriber','Markdown Writter' )

RStuff <- c('CRAN Lover','R Environment','userR! conference','R Package','Lattice','Graphics','Factors','Probability distribution'
,'Sampling','Vectors and numbers','package dependencies','Set.seed','Visualization','Data manipulation','Machine Learning'
,'Plot.ly','Shiny','Sys.getenv','Lubridate','NA','NaN','Environment history','R Script Editor','S3 Class'
,'Box Plot','Infix Operator','Parametrization','Slow Script','Long running Script','R ggplot library','Statistical Models'
,'WHERE clause','WHILE loop','Kronecker product','Matching operator','Integer division','dplyr and data.table' )

Fancystuff = c( 'Regional','Only the best','Insane','Qualitative','Virtuous','Senior'
,'Junior','In-House','Outsourced','Magnificent','Evolutionary','Customer orientated','Product'
,'Dynamic','Corporate Lead','Legacy','Investor','Direct','International','Over-seas','Internal','Human'
,'Creative','Volunteer','Lead','4 Stages of','Complete','Most Advanced','State of the art','Super high'
,'First Class','Powerful','Data','Head of','Master of','Chief of','Officer','Lead','Special')


### Ordering of subsets
JobTitle <- paste(sample(Fancystuff,1, replace=TRUE),sample(RStuff,1, replace=TRUE),sample(Roles,1, replace=TRUE), sep= " ")

JobTitle

#similarity?
#Fancystuff[floor(runif(Fancystuff, min=1, max=39))[1]]
#sample(Fancystuff,1,replace=TRUE)
