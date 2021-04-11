library(bitops)
library(RCurl)
library(XML)
library(stringr)


extract <- function(year = 2020){
  
  data <- c("DOI", "Article Title", "Published Date", "Author", "Abstract", "Author Affiliation", "Corresponding Author", "Full Text Link", "Corresponding Author Mail ID")
  data.to.file <- matrix(data, 1,8)
  write.table(data.to.file, "C:\\Gayathri\\NJIT\\Sem - 1 - Spring 2020\\Data Analytics with R\\Project 1\\output_data.csv", row.names = FALSE, col.names=FALSE, sep = ",",append = FALSE)
  
  
  site.url = "https://www.hindawi.com/journals/aaa/"
  if (year %in% seq(1996, 2020, by=1)){
    URL = paste(site.url, "contents/", "year/", year, sep="")
    print (URL)
  }else{
    print("Please enter a valid year...the articles are available from 1996 to 2020.")
    print ("Kinldy run the code with right year as the input ... !")
    return(0)
  }
  
  main.page = readLines(paste(URL, sep=""))
  options(warn=-1)
  doc = htmlParse(main.page, asText=TRUE)
  
  if (year == 2020){
    article.list <- xpathSApply(doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div", xmlValue)
    article.id.index <- gregexpr("Article ID ([0-9]+)", article.list)
    article.id <- regmatches(article.list, article.id.index)[[1]]
    print ("2020 ARTICLE IDSSSSSSS ")
    
  }else{
    last.content <- as(xpathSApply(doc, "//*[@id='__next']/div/div/main/div/div[3]/div[2]/div/div/div/div/div[16]/div[2]/div/a[2]")[[1]], "character")
    last.content.1 <- gregexpr("/page/[0-9]+/", last.content)
    last.content.2 <- str_sub(regmatches(last.content, last.content.1)[[1]][1], -2, -2)
    last.page.number <- as(last.content.2, "numeric")
    article.id <- c()
    
    for (i in 1:last.page.number){
      doc = htmlParse(main.page, asText=TRUE)
      page.content.url <- paste(URL, "/page/", as(i, "character"), "/", sep = "")
      print("PAGE CONTENT URL .... ")
      print (page.content.url)
      page.content <- readLines(page.content.url)
      doc = htmlParse(page.content, asText=TRUE)
      article.list <- xpathSApply(doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div", xmlValue)
      article.id.index <- gregexpr("Article ID ([0-9]+)", article.list)
      print("PAGE WISE ARTICLE IDS ........")
      print (typeof(c(regmatches(article.list, article.id.index))))
      article.id <- append(article.id, c(regmatches(article.list, article.id.index)[[1]]))
      print ("INSIDE LOOP ...... ARTICLE ID.....PAGE WISE ")
      print (article.id)
    }
  }
  
  print (article.id)
  
  for (id in article.id){
    
    only.id <- regmatches(id,gregexpr("([0-9]+)", id))
    print (only.id)
    article.url <- paste(site.url, year, "/", paste(unlist(only.id), collapse = ""), "/", sep="")
    article.page.content <- readLines(article.url)
    atricle.doc <- htmlParse(article.page.content, asText=TRUE)
    article.title <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[1]/h1/text()")[[1]]
    article.pub.date <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[3]/div[2]/div[3]/span[2]/text()")[[1]]
    article.abstract <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/article/div/div[1]/p[1]/text()")[[1]]
    article.url.copy <- article.url
    article.full.article <- paste(str_sub(str_replace(article.url.copy, "https://www.", "http://downloads."), end=-2), ".pdf", sep="")
    article.auth.plus.affiliation <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]", xmlValue)
    article.auth.affiliation <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]/div", xmlValue)
    article.author <- str_remove_all(strsplit(article.auth.plus.affiliation,article.auth.affiliation,fixed = T)[[1]][1], "[0-9]")
    article.corresponding.auth <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]/span/b/text()", xmlValue)
    data <- c(only.id, as(article.title, "character"), as(article.pub.date, "character"), article.author, as(article.abstract, "character"), article.auth.affiliation, article.corresponding.auth, article.full.article, NA)
    data.to.file <- matrix(data, 1,8)
    write.table(data.to.file, "C:\\Gayathri\\NJIT\\Sem - 1 - Spring 2020\\Data Analytics with R\\Project 1\\output_data.csv", row.names = FALSE, col.names=FALSE, sep = ",",append = TRUE)
    print ("------------------------------------------------------------------------------------")
    
  } 
}


