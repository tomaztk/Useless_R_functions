
############################
#
# Converts SQL to dplyr
#
# ToDO: smallBig Caps
#       concatenate function
############################


library(dplyr)


query <- "SELECT Species,Sepal.Length FROM iris WHERE Species = 'setosa' AND Sepal.Length >= 4.6"
query <- "SELECT * FROM iris"
query <- "SELECT * FROM iris WHERE Species = 'setosa'"

#result <- "iris %>% filter(Species == 'setosa', Sepal.Length >= 4.6) %>% select(Species,Sepal.Length)"
#eval(parse(text=result))


ArrangeSelectList <- function(q){
  
  # create df
  # sapply the text
  qq <<- as.data.frame(sapply(strsplit(q, " "), function(x) print(x)))
  colnames(qq) <<- "qt"
  
  SQL_Reserved_words <- c("SELECT", "FROM","WHERE","IS","GROUP BY", "AS", "ORDER BY", "TOP", "OR", "ELSE", "CASE", "IN", "NULL", "NOT", "CASE",
                          "BY", "HAVING BY", "LIKE", "OVER", "PERCENT", "WHEN", "THEN", "CONVERT", "CAST", "DISTINCT", "EXISTS", "AND", "BETWEEN")
  qq$yn <- 0
  qq$level <- 0
  id_pos <- match(SQL_Reserved_words, qq$qt)
  print(qq[id_pos,])
}


toDplyr <- function(query){
  #print(query)
  # parse query
  #get table name
  # from FROM onward
  #sub(".*(FROM*)", "\\1", query)
  
  # from FROM onward without word FROM
  #sub(".*(FROM*)", "", query)
  
  # between FROM and WHERE
  table <- sub("WHERE.*","",sub(".*(FROM*)", "", query))
  
  # Select list
  select_list <- trimws(sub("FROM.*","",sub(".*(SELECT*)", "", query)))
  
      # Get between select and from
      a <- gregexpr("SELECT", st)
      b <- gregexpr("FROM", st)
      pos_a <- a[[1]] + 6
      pos_b<- b[[1]] - 1
      sell <- substr(st, pos_a,pos_b)
      df <- data.frame(v=t(do.call(rbind,strsplit(sell, split = ","))))
      
      listReservedWords <- data.frame(rs=c("SUM", "AVG", "COUNT", "*", "AS", "MIN","MAX")) 
    
      
      lrw <- data.frame(rs=c("SUM", "AVG", "COUNT", "*", "AS", "MIN","MAX")) 

      for (i in 1:nrow(lrw)) {
        wo <- lrw$rs[i]
        print(wo)
        df$tr <- grepl(wo, df$v) #df$v[j])
        # for (j in 1:nrow(df)){
        #   print(df$v[j])
        #   df$tr <- grepl(wo, df$v[j])
        # }
        #j <- j +1
        #i <- i + 1
        
      }
      
      df
      
      
      
      #grepl(listReservedWords$rs[1],df$v[3])
  
      FindMatch <- function(v1, v2)  {
        v1[match(v1,v2, nomatch=0)]
      }
  
  # where
  where_clause <- sub(".*(WHERE*)","", query)
  
  # single to double = sign
  where_clause <- gsub(" = ", "==", where_clause)
  where_clause <- gsub("AND", ",", where_clause)  
  
  ## construct dplyr
  if ((select_list == "*")  & (!is.null(grep('WHERE', query))) )  { res <- paste0(table) }
  #if (is.null(grep('WHERE', query))) { res <- paste0(table) }
  
  if (select_list != "*" & !is.null(grep('WHERE', query)))   {
    
    res <- paste0(table, ' %>% ', 'select( ', select_list, ' ) %>% filter( ', where_clause ,' )')
  }
  
  if (select_list == "*" & !is.null(grep('WHERE', query)))   {
    res <- paste0(table, '  %>% filter( ', where_clause ,' )')
  }
  
  
  #eval(parse(text=res))
  
  return(res)
}


toDplyr(query)


# group_by()
# summarise()
# arrange()
# filter()
# mutate()





######

q <- as.character ("SELECT * FROM iris WHERE species = 'setosa' AND  Petal.Lenght => 1.3")



insertToDataFrame(q)

#check data.frame
qq




