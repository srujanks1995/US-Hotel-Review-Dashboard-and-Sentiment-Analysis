library(RTextTools)
library(tm)
dat<-read.csv('Datafiniti_Hotel_Reviews_Jun19.csv',header=TRUE)
trainingfile<-'subjectivity.csv'
data<-dat['reviews.title']
names(data)<-c("title")
head(data,20)



classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv(trainingfile,header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }   
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}
data$sentiment <- classify_polarity(data,algorithm="bayes",verbose=FALSE)[,4] 

head(data) 
write.csv(data,"Datafiniti_Hotel_Reviews_Jun19_outt.csv", row.names = FALSE) 


###########################################################################################

topN<-1000
dat<-read.csv('Datafiniti_Hotel_Reviews_Jun19_outt.csv',header=TRUE)

trainingfile<-"subjectivity2.csv"

library(tm)
library(RSentiment)

review_text <- paste(dat$title, collapse=" ")
review_source <- VectorSource(review_text)



corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))



dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

res <- as.data.frame(as.matrix(frequency))
colnames(res)<-"TermFrequency"
res$RecTxt <- rownames(res)
res$RecID <- 0

top_terms <- head (res,as.numeric(topN))


top_terms$sentiment <- calculate_sentiment(top_terms$RecTxt)       #,algorithm="bayes",verbose=FALSE)[,4] 

head(top_terms) 

write.csv(top_terms, file = "Datafiniti_Hotel_Reviews_Jun19_outtt.csv",row.names = FALSE)






