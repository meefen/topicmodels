library(reshape2)
library(ggplot2)
library(googleVis)
library(topicmodels)
library(XML)
library(tm)
library(slam)
library(lle)

preprocessing <- function() {
  ## Function for preprocessing data
  ## Subject to change when new data come
  
  ### Read data
  notes <- read.csv("electricity.csv", stringsAsFactors=FALSE)
  notes$view <- factor(notes$view, labels=c("Class A", "Class B"))
  
  ### Create corpus
  corpus <- Corpus(VectorSource(paste(notes$title, notes$text)))
  academicWords <- c("need", "understand", "theory", "information", 
                     "source", "think", "because", "also")
  corpus <- tm_map(corpus, function(x) 
    removeWords(x, academicWords))
  
  ### Export corpus to a document-term matrix
  dtm <- DocumentTermMatrix(corpus, control = list(
    stemming = TRUE, stopwords = TRUE, minWordLength = 3, 
    removeNumbers = TRUE, removePunctuation = TRUE))
  # select the vocabulary based on mean tf-idf
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
    log2(nDocs(dtm)/col_sums(dtm > 0))
  dtm <- dtm[ , term_tfidf >= 0.1]
  dtm <- dtm[row_sums(dtm) > 0, ]
  
  ### Fit LDA model
  lda <- LDA(dtm, k = 20, control = list(seed = 2013))
  
  ### Get loading matrix of documents on topics
  loading <- posterior(lda, newdata = dtm)
  
  ## Locally Linear Embedding (LLE) -- reduce dimension
  X <- loading$topics
  calc_k(X, 2, 1, 15, FALSE) # choose k with the smallest rho
  results <- lle(X=X, m=2, k=9, reg=2, ss=FALSE, id=TRUE, v=0.9)
  points <- data.frame(x=results$Y[, 1], 
                       y=results$Y[, 2], 
                       id=notes$id,
                       view=notes$view, 
                       author=notes$from_user, 
                       time=notes$time, 
                       topic=factor(topics(lda, 1)),
                       text=notes$text)
  
  save(lda, file="lda.Rdata")
  save(points, file="points.Rdata")
}

## Check whether need to run preprocessing()
if(file.exists("lda.Rdata") && file.exists("points.Rdata")) {
  load("lda.Rdata")
  load("points.Rdata")
} else {
  preprocessing()
}

shinyServer(function(input, output) {
  
  # View count
  viewCount <- as.numeric(read.table("viewFile.txt", header=FALSE)[1, 1]) + 1
  write(viewCount, file = "viewFile.txt")
  
  #Output for hits
  output$hits <- renderText({
    paste0("App Hits: ", viewCount)
  })

#   ## Visualize results
#   output$main_plot <- renderPlot({
#     p <- ggplot(points, aes(x=x,y=y)) + 
#       geom_point(data=points, size=4, aes(x=x, y=y, color=view, shape=topic)) + 
#       scale_shape_manual(values=1:length(levels(points$topic)))
#     print(p)
#   })
  
  ## (fake) Motion Chart to visualize results
  output$gvPlot <- renderGvis({
    motion <- cbind(time=rep(2013, nrow(points)), # fake time data
                    points[, c("id", "x", "y", "topic", "view", "author")])
    # motion$topic <- as.integer(motion$topic)
    gvisMotionChart(motion, idvar="id", timevar="time", 
                    xvar="x", yvar="y", colorvar="topic",
                         options=list(showXScalePicker=FALSE,
                                      showYScalePicker=FALSE,
                                      showChartButtons=FALSE))
    # gvisScatterChart(points[, c(1, 2)])
  })
  
  ## conductor for topic summary
  getTopics <- reactive({
    topics <- terms(lda, input$num_words)
    words <- apply(topics, 2, function(x) paste(x, collapse = " "))
    data.frame(topic=names(words), words=as.vector(words))
  })
  
  ## Table of topic summary
  output$gvSummary <- renderGvis({
    topics <- getTopics()
    summary <- table(points$topic)
    topics$count <- as.vector(summary)
    gvisTable(topics, options=list(page="enable", height="330", width="500"))
  })
  
  ## Details of all notes and their topics
  output$gvDetails <- renderGvis({
    points$text <- gsub("\n", " ", points$text)
    gvisTable(points[, c("id", "view", "author", "topic", "text")])
  })
  
  ## Table of topic summary
  # similar to gvSummary, but without count
  output$gvTopics <- renderGvis({
    topics <- getTopics()
    gvisTable(topics, options=list(page="enable", height="330"))
  })

})
