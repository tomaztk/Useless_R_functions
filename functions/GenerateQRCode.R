##########################################
# 
# DGenerating QR code
#
# Series:
# Little Useless-useful R functions #20
# Created: March 5, 2021 
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - adding new functions
###########################################


library(qrcode)

#Saves to file
png("code.png")
qrcode_gen("https://tomaztsql.wordpress.com/")
dev.off()


#Plots in Studio
qrcode_gen("https://tomaztsql.wordpress.com/")
