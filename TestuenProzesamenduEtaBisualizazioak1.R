library(plyr); library(dplyr)
library(readr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(tidyr)
library(tm)
library(wordcloud)
library(udpipe)
library(lattice)
library(igraph)
library(ggraph)


# https://bnosac.github.io/udpipe/en/index.html

# read text
#text <- readLines(file.choose())
text <- readLines("FILE.txt")

# chose language and charge model in ud-pipes
udmodel <- udpipe_download_model(language = "basque")
udmodel <- udpipe_load_model(file = udmodel$file_model)

#analyse text
x <- udpipe_annotate(udmodel, x = text)

head(x)

# covert analysis to data frame
x <- as.data.frame(x, detailed = TRUE)

head(x)

x$token <- tolower(x$token)
#x$lemma <- tolower(x$lemma)

head(x)




#############basic statistics ##############

stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech) \n kategoria gramatikalen maiztasunak", 
         xlab = "Maiztasunak")

#izen erabilienak (formak)
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Maiztasun altuena duten izenak (formak)", xlab = "Freq")



#izen erabilienak (lemak)
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Maiztasun altuena duten izenak (lemak)", xlab = "Freq")








####### Working with content words and lemmas ############



# filter dataframe to get only content words 
FilteredX <- filter(x, upos %in% c("PROPN", "NOUN", "ADJ", "VERB")) #"ADV" 


# get the lemma
lemmas <- as.vector(FilteredX$lemma)

# convert lema vector to corpus
docs <- Corpus(VectorSource(lemmas))

# inspect corpus
#inspect(docs)

# replace special characthers
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

#add stop words (for Basquw)

docs <- tm_map(docs, removeWords, c( "egin", "egon", "izan", "ukan", "eman", "aurre", "nahi", "alde", "behar", "orbit"))


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#create wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

