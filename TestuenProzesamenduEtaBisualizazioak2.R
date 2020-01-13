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


# read tplain ext
#text <- readLines(file.choose())
#text <- readLines("FILE.txt")

# chose language and charge model in ud-pipes
#udmodel <- udpipe_download_model(language = "basque")
#udmodel <- udpipe_load_model(file = udmodel$file_model)

#analyse text
#x <- udpipe_annotate(udmodel, x = text)

# covert analysis to data frame
#x <- as.data.frame(x, detailed = TRUE)
#head(x)


#write.csv(x,"Wiki_informatika_connlu.csv", row.names = FALSE)

# import analised text


x <- read_csv("Wiki_informatika_connlu.csv")

########### coocurrences ######
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)


wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "blue") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Kookurrentziak esaldi barnean", subtitle = "Izenak eta adjektiboak")


cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Kookurrentziak testuan", subtitle = "Izenak eta adjektiboak")



############# Finding keywords ###############

## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "RAKEk identifikatutako gako hitzak", 
         xlab = "Rake")



## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Gako hitzak - Izen sintagma sinpleak (formak)", xlab = "Maiztasuna")



## Using a sequence of POS tags (noun phrases / verb phrases) LEMAK
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = x$lemma, 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Gako hitzak - Izen sintagma sinpleak (lemak)", xlab = "Maiztasunak")



