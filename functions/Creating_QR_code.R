getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")


##########################################
# 
# Absurd attempt to create QR Code reader
# step by step
#
# Series:
# Little Useless-useful R functions #70
# Created: May 10, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com

# ToDo:
# work in progress
# add ECC
# add replication and redundancy

###########################################


library(ggplot2)


QRCode <- setRefClass("QRCode",
                      fields = list(
                        text = "character",
                        version = "numeric",
                        error_correction = "character",
                        modules = "matrix",
                        size = "numeric",
                        data_bits = "character",
                        gf_exp = "numeric",
                        gf_log = "numeric"
                      ),
                      
                      methods = list(
                        
                        initialize = function(text_input) {
                          text <<- text_input
                          version <<- 1
                          error_correction <<- "L"
                          size <<- 21
                          modules <<- matrix(NA, nrow = size, ncol = size)
                          data_bits <<- ""
                          .self$init_gf()
                          .self$generate()
                        },
                        
                        init_gf = function() {
                         
                          gf_exp <<- numeric(512)
                          gf_log <<- numeric(256)
                          
                          x <- 1
                          for (i in 0:255) {
                            gf_exp[i + 1] <<- x
                            gf_log[x + 1] <<- i
                            x <- bitwShiftL(x, 1)
                            if (x >= 256) {
                              x <- bitwXor(x, 0x11D)
                            }
                          }
                          for (i in 256:511) {
                            gf_exp[i + 1] <<- gf_exp[i - 255 + 1]
                          }
                        },
                        
                        gf_mul = function(x, y) {
                          if (x == 0 || y == 0) return(0)
                          exp_sum <- gf_log[x + 1] + gf_log[y + 1]
                          return(as.integer(gf_exp[exp_sum %% 255 + 1]))
                        },
                        
                        to_binary = function(value, bits) {
                          paste0(rev(as.integer(intToBits(value))[1:bits]), collapse = "")
                        },
                        
                        encode_text = function() {
                          mode_indicator <- "0100"
                          char_count <- to_binary(nchar(text), 8)
                          data <- paste(sapply(strsplit(text, "")[[1]], function(c) {
                            to_binary(as.integer(charToRaw(c)), 8)
                          }), collapse = "")
                          paste0(mode_indicator, char_count, data)
                        },
                        
                        pad_data = function(bitstring) {
                          max_bits <- 152
                          bitstring <- paste0(bitstring, "0000")
                          
                          while (nchar(bitstring) %% 8 != 0) {
                            bitstring <- paste0(bitstring, "0")
                          }
                          
                          pads <- c("11101100", "00010001")
                          i <- 1
                          while (nchar(bitstring) < max_bits) {
                            bitstring <- paste0(bitstring, pads[i])
                            i <- ifelse(i == 1, 2, 1)
                          }
                          
                          substr(bitstring, 1, max_bits)
                        },
                        
                        string_to_bytes = function(bits) {
                          sapply(seq(1, nchar(bits), by = 8), function(i) {
                            strtoi(substr(bits, i, i + 7), base = 2)
                          })
                        },
                        
                        reed_solomon = function(data_bytes, ecc_len = 7) {
                          gen_poly <- c(1)
                          for (i in 0:(ecc_len - 1)) {
                            gen <- c(1, gf_exp[i + 1])
                            gen_poly <- .self$poly_mul(gen_poly, gen)
                          }
                          
                          message <- c(data_bytes, rep(0, ecc_len))
                          for (i in 1:length(data_bytes)) {
                            coef <- message[i]
                            if (coef != 0) {
                              for (j in 1:length(gen_poly)) {
                                message[i + j - 1] <- bitwXor(message[i + j - 1], gf_mul(coef, gen_poly[j]))
                              }
                            }
                          }
                          tail(message, ecc_len)
                        },
                        
                        poly_mul = function(p, q) {
                          res <- rep(0, length(p) + length(q) - 1)
                          for (i in seq_along(p)) {
                            for (j in seq_along(q)) {
                              res[i + j - 1] <- bitwXor(res[i + j - 1], gf_mul(p[i], q[j]))
                            }
                          }
                          res
                        },
                        
                        make_final_bitstream = function(data_bytes, ecc_bytes) {
                          all_bytes <- c(data_bytes, ecc_bytes)
                          paste(sapply(all_bytes, function(b) to_binary(b, 8)), collapse = "")
                        },
                        
                        place_finder_patterns = function() {
                          pattern <- matrix(c(
                            1,1,1,1,1,1,1,
                            1,0,0,0,0,0,1,
                            1,0,1,1,1,0,1,
                            1,0,1,1,1,0,1,
                            1,0,1,1,1,0,1,
                            1,0,0,0,0,0,1,
                            1,1,1,1,1,1,1
                          ), nrow = 7, byrow = TRUE)
                          
                          modules[1:7, 1:7] <<- pattern
                          modules[1:7, (size-6):size] <<- pattern
                          modules[(size-6):size, 1:7] <<- pattern
                        },
                        
                        place_data = function(bits) {
                          row <- size
                          col <- size
                          bit_index <- 1
                          direction <- -1
                          
                          while (col > 0) {
                            if (col == 7) col <- col - 1
                            repeat {
                              for (j in 0:1) {
                                c <- col - j
                                if (modules[row, c] %in% c(NA)) {
                                  if (bit_index <= nchar(bits)) {
                                    modules[row, c] <<- as.integer(substr(bits, bit_index, bit_index))
                                    bit_index <- bit_index + 1
                                  } else {
                                    modules[row, c] <<- 0
                                  }
                                }
                              }
                              row <- row + direction
                              if (row < 1 || row > size) {
                                direction <- -direction
                                row <- row + direction
                                break
                              }
                            }
                            col <- col - 2
                          }
                        },
                        
                        draw_modules = function() {
                          df <- data.frame()
                          for (r in 1:size) {
                            for (c in 1:size) {
                              val <- modules[r, c]
                              df <- rbind(df, data.frame(x = c, y = size - r + 1, fill = ifelse(is.na(val), 0, val)))
                            }
                          }
                          ggplot(df, aes(x = x, y = y)) +
                            geom_tile(aes(fill = as.factor(fill)), color = "grey50") +
                            scale_fill_manual(values = c("white", "black")) +
                            theme_void() +
                            coord_fixed() +
                            theme(legend.position = "none")
                        },
                        
                        generate = function() {
                          bits <- encode_text()
                          bits <- pad_data(bits)
                          data_bits <<- bits
                          data_bytes <- string_to_bytes(bits)
                          ecc_bytes <- reed_solomon(data_bytes, 7)
                          final_bits <- make_final_bitstream(data_bytes, ecc_bytes)
                          
                          place_finder_patterns()
                          place_data(final_bits)
                          print(draw_modules())
                        }
                      )
)


# run
qr <- QRCode$new("Foo Bar")




### Adding ECC and rewrite

QRCode2 <- setRefClass("QRCode",
                      fields = list(
                        text = "character",
                        version = "numeric",
                        error_correction = "character",
                        modules = "matrix",
                        size = "numeric",
                        data_bits = "character",
                        gf_exp = "numeric",
                        gf_log = "numeric"
                      ),
                      
                      methods = list(
                        
                        initialize = function(text_input) {
                          text <<- text_input
                          version <<- 1
                          error_correction <<- "L"
                          size <<- 21
                          modules <<- matrix(NA, nrow = size, ncol = size)
                          data_bits <<- ""
                          .self$init_gf()
                          .self$generate()
                        },
                        
                        init_gf = function() {
                          # Initialize Galois Field tables
                          gf_exp <<- numeric(512)
                          gf_log <<- numeric(256)
                          
                          x <- 1
                          for (i in 0:255) {
                            gf_exp[i + 1] <<- x
                            gf_log[x + 1] <<- i
                            x <- bitwShiftL(x, 1)
                            if (x >= 256) {
                              x <- bitwXor(x, 0x11D)
                            }
                          }
                          for (i in 256:511) {
                            gf_exp[i + 1] <<- gf_exp[i - 255 + 1]
                          }
                        },
                        
                        gf_mul = function(x, y) {
                          if (x == 0 || y == 0) return(0)
                          exp_sum <- gf_log[x + 1] + gf_log[y + 1]
                          return(as.integer(gf_exp[exp_sum %% 255 + 1]))
                        },
                        
                        to_binary = function(value, bits) {
                          paste0(rev(as.integer(intToBits(value))[1:bits]), collapse = "")
                        },
                        
                        encode_text = function() {
                          mode_indicator <- "0100"
                          char_count <- to_binary(nchar(text), 8)
                          data <- paste(sapply(strsplit(text, "")[[1]], function(c) {
                            to_binary(as.integer(charToRaw(c)), 8)
                          }), collapse = "")
                          paste0(mode_indicator, char_count, data)
                        },
                        
                        pad_data = function(bitstring) {
                          max_bits <- 152
                          bitstring <- paste0(bitstring, "0000")
                          
                          while (nchar(bitstring) %% 8 != 0) {
                            bitstring <- paste0(bitstring, "0")
                          }
                          
                          pads <- c("11101100", "00010001")
                          i <- 1
                          while (nchar(bitstring) < max_bits) {
                            bitstring <- paste0(bitstring, pads[i])
                            i <- ifelse(i == 1, 2, 1)
                          }
                          
                          substr(bitstring, 1, max_bits)
                        },
                        
                        string_to_bytes = function(bits) {
                          sapply(seq(1, nchar(bits), by = 8), function(i) {
                            strtoi(substr(bits, i, i + 7), base = 2)
                          })
                        },
                        
                        reed_solomon = function(data_bytes, ecc_len = 7) {
                          gen_poly <- c(1)
                          for (i in 0:(ecc_len - 1)) {
                            gen <- c(1, gf_exp[i + 1])
                            gen_poly <- .self$poly_mul(gen_poly, gen)
                          }
                          
                          message <- c(data_bytes, rep(0, ecc_len))
                          for (i in 1:length(data_bytes)) {
                            coef <- message[i]
                            if (coef != 0) {
                              for (j in 1:length(gen_poly)) {
                                message[i + j - 1] <- bitwXor(message[i + j - 1], gf_mul(coef, gen_poly[j]))
                              }
                            }
                          }
                          tail(message, ecc_len)
                        },
                        
                        poly_mul = function(p, q) {
                          res <- rep(0, length(p) + length(q) - 1)
                          for (i in seq_along(p)) {
                            for (j in seq_along(q)) {
                              res[i + j - 1] <- bitwXor(res[i + j - 1], gf_mul(p[i], q[j]))
                            }
                          }
                          res
                        },
                        
                        make_final_bitstream = function(data_bytes, ecc_bytes) {
                          all_bytes <- c(data_bytes, ecc_bytes)
                          paste(sapply(all_bytes, function(b) to_binary(b, 8)), collapse = "")
                        },
                        
                        place_finder_patterns = function() {
                          pattern <- matrix(c(
                            1,1,1,1,1,1,1,
                            1,0,0,0,0,0,1,
                            1,0,1,1,1,0,1,
                            1,0,1,1,1,0,1,
                            1,0,1,1,1,0,1,
                            1,0,0,0,0,0,1,
                            1,1,1,1,1,1,1
                          ), nrow = 7, byrow = TRUE)
                          
                          modules[1:7, 1:7] <<- pattern
                          modules[1:7, (size-6):size] <<- pattern
                          modules[(size-6):size, 1:7] <<- pattern
                        },
                        
                        place_data = function(bits) {
                          row <- size
                          col <- size
                          bit_index <- 1
                          direction <- -1
                          
                          while (col > 0) {
                            if (col == 7) col <- col - 1
                            repeat {
                              for (j in 0:1) {
                                c <- col - j
                                if (modules[row, c] %in% c(NA)) {
                                  if (bit_index <= nchar(bits)) {
                                    modules[row, c] <<- as.integer(substr(bits, bit_index, bit_index))
                                    bit_index <- bit_index + 1
                                  } else {
                                    modules[row, c] <<- 0
                                  }
                                }
                              }
                              row <- row + direction
                              if (row < 1 || row > size) {
                                direction <- -direction
                                row <- row + direction
                                break
                              }
                            }
                            col <- col - 2
                          }
                        },
                        
                        
                        
                        
                        draw_modules = function() {
                          quiet_zone <- 4
                          full_size <- size + 2 * quiet_zone
                          df <- data.frame()
                          
                          for (r in 1:full_size) {
                            for (c in 1:full_size) {
                      
                              qr_r <- r - quiet_zone
                              qr_c <- c - quiet_zone
                              if (qr_r >= 1 && qr_r <= size && qr_c >= 1 && qr_c <= size) {
                                val <- modules[qr_r, qr_c]
                                val <- ifelse(is.na(val), 0, val)
                              } else {
                                val <- 0  
                              }
                              df <- rbind(df, data.frame(x = c, y = full_size - r + 1, fill = as.factor(val)))
                            }
                          }
                          
                          ggplot(df, aes(x = x, y = y)) +
                            geom_tile(aes(fill = fill), color = NA) +
                            scale_fill_manual(values = c("white", "black")) +
                            theme_void() +
                            coord_fixed(expand = FALSE) +
                            theme(legend.position = "none")
                        },
                        
                        generate = function() {
                          bits <- encode_text()
                          bits <- pad_data(bits)
                          data_bits <<- bits
                          data_bytes <- string_to_bytes(bits)
                          ecc_bytes <- reed_solomon(data_bytes, 7)
                          final_bits <- make_final_bitstream(data_bytes, ecc_bytes)
                          
                          place_finder_patterns()
                          place_data(final_bits)
                          print(draw_modules())
                        }
                      )
)

# run QR
qr <- QRCode$new("Foo Bar")


# run  QR Code with ECC
qr <- QRCode2$new("Foo Bar")
