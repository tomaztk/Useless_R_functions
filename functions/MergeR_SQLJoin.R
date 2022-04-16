
##########################################
# 
# Comparing MERGE R  Method with 
# T-SQL JOIN Clause
#
# Series:
# Little Useless-useful R functions #35
# Created: April 15, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


Users = data.frame(UserID = c(1,1,2,2,3,4) 
                     ,GroupID = c("X","X","Y","Y","X","Z")
                     ,DateCreated = c("2022-04-10","2022-04-11","2022-04-13","2022-04-14","2022-04-11","2022-04-13")
                     ,TotalKM = c(22,33,33,42,6,8)
                     ,Age = c(34,34,41,41,18,56)
)

UserRun = data.frame(UserID = c(1,1,2,2,3,4,5)
                     ,GroupID = c("X","X","Y","Y","X","Z","T")
                     ,Run = c(1,0,0,0,0,0,0)
                     ,RunName = c("Short", "Over Hill","Short","Miller","River","Mountain top","City")
                     
)


## joins

# inner join
IJ_Us_UsR <- merge(x=Users, y=UserRun, by = c("UserID", "GroupID"))


# Left join all.x = TRUE
LJ_Us_UsR <- merge(x = Users, y = UserRun, by = c("UserID", "GroupID") , all.x = TRUE) 
LJ_Us_UsR <- merge(x = UserRun, y = Users, by = c("UserID", "GroupID") , all.y = TRUE) 


#right join all.x = TRUE
RJ_Us_UsR <- merge(x = Users, y = UserRun, by = c("UserID", "GroupID") , all.y = FALSE) 


# Cross JOIN
CJ_Us_UsR <- merge(x = Users, y = UserRun, by = NULL )  

# Join by Row Names / Internal uniequefier
RowNameJ_Us_UsR <- merge(x=Users, y=UserRun, by = "row.names")
## Alternative
## RowNameJ_Us_UsR <- merge(x=Users, y=UserRun, by = 0)  


# Joining more than two data.frames
Run = data.frame(UserID = c(1,1,2,2,3,4,6,8) 
                   ,GroupID = c("X","X","Y","Y","X","Z","H","K")
                   ,Trainer = c(1,1,1,1,1,0,0,0)
)


merge(merge(x=Users, y=UserRun, by = c("UserID", "GroupID")), y=Run, by.y = c("UserID", "GroupID"))



#######################
##### SQL Code ########
#######################
# 
# DROP TABLE dbo.Users
# DROP TABLE dbo.UserRun
# 
# CREATE TABLE dbo.Users (
#   UserID INT
#   ,GroupID CHAR(1)
#   ,DateCreated datetime
#   ,TotalKM INT
#   ,Age INT
# )
# 
# CREATE TABLE dbo.UserRun (
#   UserID INT
#   ,GroupID CHAR(1)
#   ,Run INT
#   ,RunName VARCHAR(50)
# )
# 
# INSERT INTO dbo.Users
# SELECT 1, 'X','2022/04/10',22,34
# UNION ALL SELECT 1, 'X','2022/04/11',33,34
# UNION ALL SELECT 1, 'X','2022/04/12',33,34
# UNION ALL SELECT 2, 'Y','2022/04/13',33,41
# UNION ALL SELECT 2, 'Y','2022/04/14',42,41
# UNION ALL SELECT 3, 'X','2022/04/11',6,18
# UNION ALL SELECT 4, 'Z','2022/04/13',8,56
# 
# INSERT INTO dbo.UserRun
# SELECT 1, 'X',1,'Short'
# UNION ALL SELECT 1, 'X',0,'Over Hill'
# UNION ALL SELECT 2, 'Y',0,'Short'
# UNION ALL SELECT 2, 'Y',0,'Miller'
# UNION ALL SELECT 3, 'X',0,'River'
# UNION ALL SELECT 4, 'Z',0,'Mountain top'
# UNION ALL SELECT 5, 'T',0,'City'
# 
# 
# 
# SELECT 
# *
#   FROM dbo.Users as U 
# INNER JOIN dbo.UserRun AS UR
# ON U.UserID = UR.UserID
# AND U.GroupID = UR.GroupID
# 
# 
# SELECT 
# *
#   FROM dbo.UserRun as U 
# LEFT JOIN dbo.Users AS UR
# ON U.UserID = UR.UserID
# AND U.GroupID = UR.GroupID
# 
# 
# SELECT * FROM users
# SELECT * FROM userrun




