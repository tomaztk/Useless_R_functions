##########################################
# 
# Encrypthing with password
#
# Series:
# Little Useless-useful R functions #21
# Created: March 5, 2021 
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
###########################################


#install.packages("sodium")

library(sodium)

#Encrypt
passkey <- sha256(charToRaw("geslo123"))
plaintext <- "primer teksta"
plaintext.serialized <- serialize(plaintext, NULL)

#Decrypth
ciphertext <- data_encrypt(plaintext.serialized, key = passkey)
unserialize(data_decrypt(ciphertext, key = sha256(charToRaw("geslo123"))))
