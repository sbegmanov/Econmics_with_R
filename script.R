data <- read.csv("data/data_newly_opened_votes_above_100_group_by_resttype.csv")
text_unique <- data$reviews_list

# load text mining package
library(tm)

# convert text vector to a collection of documents
words.vec <- VectorSource(text_unique)
words.corpus <- Corpus(words.vec)

# create a content transformer that replace string pattern by a space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(words.corpus, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# convert text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# remove numbers
docs <- tm_map(docs, removeNumbers)

# remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# remove punctuations
docs <- tm_map(docs, removePunctuation)

# eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# remove your own stop word
docs <- tm_map(docs, removeWords, c("rated", "ratedn", "place"))

# convert corpus to a data.frame
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
df <- data.frame(word = names(v),freq = v)

# create wordcloud
library(wordcloud)
library(RColorBrewer)
set.seed(123)
wordcloud(words = df$word, freq = df$freq, min.freq = 9,
          max.words = 50, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
