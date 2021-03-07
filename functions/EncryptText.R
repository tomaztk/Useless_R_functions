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


### Store passkey as variable
unpkk <- "136 242  93 138  72 225  66  50 253 250  82 192  58 189   8  47 118  53 141  62  33  28  41 119 227 195 172  85 213  77 241  61"

ciphertext <- data_encrypt(serialize(plaintext, NULL), key = unpkk)
unserialize(data_decrypt(ciphertext, key = sha256(charToRaw("geslo123"))))
