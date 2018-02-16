#Libraries----------------------------------------------------
library(pacman) # loading packages
pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
pacman::p_load_gh("trinker/sentimentr") 
#library(SentimentAnalysis)


# Parse Amazon html pages for data------------------------------------------------------
amazon_scraper <- function(doc, reviewer = T, delay = 0){
  
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes(".review-byline .author") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  helpful <- doc %>%
    html_nodes(".cr-vote-buttons .a-color-secondary") %>%
    html_text() %>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric()
  
  if(reviewer == T){
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>% 
          trim()) %>% as.numeric()
    
    df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, 
                     rver_rank, stringsAsFactors = F)
    
  } else df <- data.frame(title, author, date, ver.purchase, format, stars, 
                          comments, helpful, stringsAsFactors = F)
  
  return(df)
}

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
prod


######################################
# Reviews of 26 iPhone/Samsung from 25 comment pages from Amazon ---------------------------
######################################

#Vector with ASIN codes from Amazon. 

x <- c('B07539DSV3','B06Y137TLR','B06Y14T5YW','B074MJ3Y7L','B01CJSF8IO','B01N5UJM8M','B01M0QXN7K','B06XH73MKR','B018WFZ5N6','B018WFZ4J6','B01MUSD2ST','B01LWYMM4R','B073V7NPJ9','B01LZVO6WR','B075358NQF','B01B4S4OFS','B075QN8NDJ','B075QNGHS8','B075QJSQLH','B01LXU4VO7','B01LYT95XR','B01J8PBEUM','B00YD54HZ2','B00YD545CC','B071W3DDM7','B075QMZH2L')
#   Length = 26.
#   Iphones selection:  from iPhone6 (Sep 2014) to Iphone X as these are still very popular
#   Samsung selection:  All models from 2016 above. As Samsung had much more models, we have limited it for those released in the last 2 years.
#   More info: https://docs.google.com/spreadsheets/d/1hiLNsjI4vtqvo2GphdEGOJOGFKeQ9_ItWpTVfC23CjQ/edit#gid=0



pages <- 25
reviews_all <- NULL

for(page_num in 1:pages){ #<-- loop to get the #of pages defined in "Pages"
  
  doc_all <- list()
  for(i in seq_along(x)){ #<-- loop to build the html for all the products listed in "x" and in the qtty of pages
    assign(paste('doc'), read_html(paste0("http://www.amazon.com/product-reviews/",x[i],"/?pageNumber=",page_num)))
    print(paste0("Doc"))
    print(paste0("Page Number",page_num))
    doc_all <- append(doc_all,list(doc))
  }
  
  for(j in 1:length(doc_all)){ #<-- loop to Scrap all the pages review built in doc_all
    reviews <- amazon_scraper(doc_all[[j]], reviewer = F, delay = 2) 
    
    if(nrow(reviews) > 0){  #<-- Security check to stop in case there are no observations for such product
      reviews$ASIN <- x[j]
      reviews_all <- rbind(reviews_all,reviews)
    }
  }
}

write.csv(reviews_all, "Reviews_Amazon.csv",row.names = FALSE)


# Sentiment analysis -----------------------------------------------------------------------------------
Reviews_Amazom <- read.csv("X")
reviews_all = Reviews_Amazom
summary(reviews_all$helpful) 

# max 924 / NA 1676
which(grepl(924,reviews_all$helpful)) #find the row that corresponds
reviews_all[223,] #show content of row


#crate subset with NA helpful:
NA_helpful <- reviews_all[which(is.na(reviews_all$helpful)),]
summary(NA_helpful)


#######----------------------------------------------------------------
pacman::p_load_gh("trinker/SentimentAnalysis")

sent_agg <- with(reviews_all, analyzeSentiment(comments))
View(sent_agg)

head(sent_agg)

par(mfrow=c(1,2))
with(reviews_all, hist(stars))
with(sent_agg, hist(ave_sentiment))
sent_agg$tert <- convertToDirection(sent_agg$SentimentGI)
plot(sent_agg$tert)
plot(sent_agg$SentimentGI)
summary(sent_agg$SentimentGI)
str(sent_agg)

mean(reviews_all$stars)

response <- sentiment[[1]] + rnorm(10)
sentiment <- data.frame(Dictionary=runif(10))
plotSentimentResponse(sent_agg$SentimentGI, response)



#Dictionaries plot to see difference on interpretation---------------------------------------------------
par(mfrow=c(2,2))

sent_agg <- with(reviews_all , analyzeSentiment(comments))

sent_agg$GI <- convertToDirection(sent_agg$SentimentGI)
sent_agg$HE <- convertToDirection(sent_agg$SentimentHE)
sent_agg$LM <- convertToDirection(sent_agg$SentimentLM)
sent_agg$QDAP <- convertToDirection(sent_agg$SentimentQDAP)

plot(sent_agg$GI, main= 'Measured Sentiment GI Dictionary')
plot(sent_agg$HE, main= 'Measured Sentiment for HE Dictionary')
plot(sent_agg$LM, main= 'Measured Sentiment for LM Dictionary')
plot(sent_agg$QDAP, main= 'Measured Sentiment for QDAP Dictionary')

