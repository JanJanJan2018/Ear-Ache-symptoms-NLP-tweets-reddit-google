# This script takes ten articles from the abstracts on earache articles 
# from NCBI's PubMed

Auto <- read.csv('NCBI-EarAche-PubMed.csv', sep=',',
                 header=FALSE, na.strings=c('',' '))
colnames(Auto) <- c('abstract','source')

auto <- Auto[complete.cases(Auto$abstract),]


dir.create('./Earache')

ea <- as.character(auto$abstract)
setwd('./Earache')

for (j in 1:length(ea)){
  write(ea[j], paste(paste('EA',j, sep='.'), '.txt', sep=''))
}
setwd('../')



###############################################################################

#NLP and preprocessing 

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

Earache <- Corpus(DirSource("Earache"))


Earache

Earache <- tm_map(Earache, removePunctuation)
Earache <- tm_map(Earache, removeNumbers)
Earache <- tm_map(Earache, tolower)
Earache <- tm_map(Earache, removeWords, stopwords("english"))
Earache <- tm_map(Earache, stripWhitespace)
Earache <- tm_map(Earache, stemDocument)

dtmEarache <- DocumentTermMatrix(Earache)

freq <- colSums(as.matrix(dtmEarache))

FREQ <- data.frame(freq)
ord <- order(freq, decreasing=TRUE)

freq[head(ord, 25)]

findAssocs(dtmEarache, "patient", corlimit=0.5)

findAssocs(dtmEarache, "ear", corlimit=0.5)


findAssocs(dtmEarache, "pain", corlimit=0.5)

wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>4), aes(word, freq))
p <- p + geom_bar(stat= 'identity') 
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 
p


wordcloud(names(freq), freq, min.freq=4,colors=brewer.pal(3,'Dark2'))

wordcloud(names(freq), freq, max.words=40,colors=brewer.pal(6,'Dark2'))

############################################################################

# The above abstracts were stemmed, this version is lemmatized

library(textstem)

lemma <- lemmatize_strings(auto$abstract, dictionary=lexicon::hash_lemmas)

Lemma <- as.data.frame(lemma)
Lemma <- cbind(Lemma, auto)

colnames(Lemma) <- c('lemmatizedAbstract','abstract', 'source')

write.csv(Lemma, 'LemmatizedEarAche.csv', row.names=FALSE)


# write the lemmatized/formatted abstracts to a new folder to 
# build word clouds and word frequencies with

dir.create('./EarAche-Lemma')

ea <- as.character(Lemma$lemmatizedAbstract)
setwd('./EarAche-Lemma')

for (j in 1:length(ea)){
  write(ea[j], paste(paste('EAL',j, sep='.'), '.txt', sep=''))
}
setwd('../')

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

Earache <- Corpus(DirSource("EarAche-Lemma"))

Earache

Earache <- tm_map(Earache, removePunctuation)
Earache <- tm_map(Earache, removeNumbers)
Earache <- tm_map(Earache, tolower)
Earache <- tm_map(Earache, removeWords, stopwords("english"))
Earache <- tm_map(Earache, stripWhitespace)

dtmEarache <- DocumentTermMatrix(Earache)
dtmEarache
# <<DocumentTermMatrix (documents: 10, terms: 432)>>
#   Non-/sparse entries: 669/3651
# Sparsity           : 85%
# Maximal term length: 17
# Weighting          : term frequency (tf)

freq <- colSums(as.matrix(dtmEarache))

FREQ <- data.frame(freq)
ord <- order(freq, decreasing=TRUE)

freq[head(ord, 25)]
# patient     earache     otalgia         ear     symptom        pain 
# 31          26          26          22          17          15 
# cause examination       refer   agreement   treatment      common 
# 14          12          12          11           8           7 
# ent         can     history     medical  antibiotic      report 
# 7           7           7           7           7           7 
# evaluation        many        much      normal   diagnosis     disease 
# 6           6           6           6           6           6 
# present 
# 6 

patient <- as.data.frame(findAssocs(dtmEarache, "patient", corlimit=0.6))

earache <- as.data.frame(findAssocs(dtmEarache, "earache", corlimit=0.55))


treatment <- as.data.frame(findAssocs(dtmEarache, "treatment", corlimit=0.55))

wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>4), aes(word, freq))
p <- p + geom_bar(stat= 'identity') 
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 
p


wordcloud(names(freq), freq, min.freq=5,colors=brewer.pal(3,'Dark2'))

wordcloud(names(freq), freq, max.words=40,colors=brewer.pal(6,'Dark2'))

