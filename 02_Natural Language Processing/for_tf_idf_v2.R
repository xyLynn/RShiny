library(tm)
library(wordcloud2)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(writexl)

dataUrl <- getURL("https://raw.githubusercontent.com/xyLynn/RShiny/master/02_Natural%20Language%20Processing/FewProducts.csv")
test <- read.csv(text = dataUrl)
#test <- read.csv('FewProducts.csv', stringsAsFactors=FALSE)
dataUrl <- getURL("https://raw.githubusercontent.com/xyLynn/RShiny/master/02_Natural%20Language%20Processing/AFINN-111.txt")
afinn <- read.csv(text = dataUrl)
# afinn <- read.table('AFINN-111.txt', stringsAsFactors=FALSE)

# Q1
review1 <- gsub("(\n|<br />)"," ",test$Text)
review1 <- stripWhitespace(test$Text)
review1 <- removePunctuation(review1)
review2 <- sapply(review1, tolower)
review2 <- stemDocument(review2, language = "english")

stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
review3 <- stringr::str_replace_all(review2, stopwords_regex, '')

normalized_review <- cbind(test[, c(1:3, 10)], review3)
colnames(normalized_review)[4:5] <- c("Review", "Normalized Review") 

# Q7
forcalcu_review <- cbind(test[, 1], review2)
colnames(forcalcu_review)[1] <- "Id"
row.names(forcalcu_review) <- NULL
forcalcu_review <- data.frame(forcalcu_review)
forcalcu_review$review2 <- as.character(forcalcu_review$review2)

words <- forcalcu_review %>%
  unnest_tokens(word, review2) %>%
  group_by(Id, word) %>% 
  tally()

total_words <- aggregate(words$n, by=list(Id = words$Id), FUN=sum)

review_words <- left_join(words, total_words, by = c("Id" = "Id"))
colnames(review_words)[4] <- "total"

review_tf_idf <- review_words %>%
  bind_tf_idf(word, Id, n)

score_review_words <- left_join(review_tf_idf, afinn, by = c("word" = "V1"))
score_review_words$V2[is.na(score_review_words$V2)] <- 0
score_review_words$score <- score_review_words$tf_idf*score_review_words$V2


srw_table <- aggregate(score_review_words$score, by=list(Id = score_review_words$Id), FUN=sum)
srw_table$Id <- as.integer(srw_table$Id)

score_word <- cbind(test[, c(1:3, 7)], srw_table[,2])
colnames(score_word)[5] <- c("Sentiment Score") 
score_word$`Sentiment Score` <- as.numeric(as.character(score_word$`Sentiment Score`))


# Q8 
test_score_word <- score_word[c("ProductId", "Sentiment Score")]
sum_product <- ddply(test_score_word, .(ProductId), summarize, 
                     Number_of_Reviews=length(ProductId), 
                     Average_Score=mean(`Sentiment Score`))


# Q9
temp <- sum_product[order(sum_product$Number_of_Reviews, decreasing = T), ]
top_reviews <- unique(temp$Number_of_Reviews)[1:6]
top_table <- temp[as.character(temp$Number_of_Reviews) %in% as.character(top_reviews) == TRUE, ]


# Q10
top_product <- score_word[score_word$ProductId %in% top_table$ProductId == TRUE,]
ggplot(top_product)+geom_point(aes(`Sentiment Score`, Score))+facet_grid(ProductId ~ .)

result <- by(top_product[,4:5], top_product$ProductId, function(top_product) {cor(top_product$`Sentiment Score`, top_product$Score)})
corr_df <- as.data.frame(as.matrix(result))
corr_df <- t(corr_df)
rownames(corr_df) <- "Correaltion"

corr_df

#            B000KV61FC B0013NUGDE B001EO5Q64 B0026RQTGE B002QWP89S   B003GTR8IO B005K4Q37A  B007M83302 B0090X8IPM
# Correaltion 0.04272221 -0.0229633  0.1100829 0.01581265 -0.0255755 -0.006800036  0.0574318 -0.09418172 0.05572269


