#' Title: A3 Assignments _ Hult Natural Processing Analysis
#' Purpose: Deep analysis on sentiment
#' Author: Giovanni Ghedini
#' Date: Feb 05, 2024

#libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tm)
library(wordcloud)
library(tidytext)
library(radarchart)
library(textdata)
library(tidyr)
library(tidytext)

# wd
setwd("~/Desktop/Dual Degree/R/hult_class/Personal_folder/A3_Assignment")

# options and custom function
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# stop words -- ADD STUDENT
customStopwords <- c(stopwords('english'), 'hult', 'school', 'university', 'name', 'hello', 'also', 'currently','name')

# load data
studentBios <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Cases/A3_NLP/Student%20Ambassador%20Bios/final_student_data.csv')

# create curpos (reads as a vector) --- i will need to do the DataFrame source
studentCorpus <- VCorpus(VectorSource(studentBios$bio))

# clean curpos
studentCorpus <- cleanCorpus(studentCorpus, customStopwords)

# Examine one record
content(studentCorpus[[1]])

# DTM (Document Term Metrix)
studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTMm <- as.matrix(studentDTM) # pass to a simple matrix
# examine dimensions
dim(studentDTMm)

# Word Frequency Matrix -- colSums to know the frequency
studentFrequency <- colSums(studentDTMm)
studentFrequency <- data.frame(word=names(studentFrequency),
                               frequency=studentFrequency,
                               row.names = NULL)

# Examine a portion of data set to make sure it's right
head(studentFrequency, 10)


# bar chart
# Simple barplot; values greater than 50 
topWords      <- subset(studentFrequency, studentFrequency$frequency >= 25) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='skyblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="black",hjust=1.25, size=5.0)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# word cloud
# Reminder to expand device pane
wordcloud(studentFrequency$word,
          studentFrequency$freq,
          max.words=50,
          random.order=FALSE,
          colors=pal)

## commonality cloud

#Subset
columns <- c("cid","bio")
female_studentBios <- subset(studentBios, studentBios$namSorGender.likelyGender == "female")
female_studentBios <- female_studentBios[,columns]
male_studentBios <- subset(studentBios, studentBios$namSorGender.likelyGender == "male")
male_studentBios <- male_studentBios[,columns]


# Female Vcorpus
# create curpos (reads as a vector) --- i will need to do the DataFrame source
femaleCorpus <- VCorpus(VectorSource(female_studentBios$bio))
# clean curpos
femaleCorpus <- cleanCorpus(femaleCorpus, customStopwords)

# Male Vcorpus
# create curpos (reads as a vector) --- i will need to do the DataFrame source
maleCorpus <- VCorpus(VectorSource(male_studentBios$bio))
# clean curpos
maleCorpus <- cleanCorpus(maleCorpus, customStopwords)

## extract out the text. This way I extract the overall female and male text 
# maleCorpus
maleCorpus <- unlist(sapply(maleCorpus, `[`, "content"))
maleCorpus <- paste(maleCorpus, collapse = ' ')

# femaleCorpus
femaleCorpus <- unlist(sapply(femaleCorpus, `[`, "content"))
femaleCorpus <- paste(femaleCorpus, collapse = ' ')

# Create Corpus (has two documents - male and female)
alltext_gender <- c(maleCorpus, femaleCorpus)
allCorp_gender <- VCorpus(VectorSource(alltext_gender))

# Create Term Document Matrix 
gender_TDM <- TermDocumentMatrix(allCorp_gender)
gender_TDMm <- as.matrix(gender_TDM) ## remeber first column is male and second is female

# substitute column names
col_names_gender <- c('male','female')
colnames(gender_TDMm) <- col_names_gender

# Pallette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Commonality Cloud
dev.off()
commonality.cloud(gender_TDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))
# Comparison Cloud
dev.off()
comparison.cloud(gender_TDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('skyblue', 'pink'),
                 scale=c(3,0.1))


## COMPARISON BY COUNTRY
unique(studentBios$campus)

# stop words -- modified version for campuses
customStopwords_campus <- c(stopwords('english'), 'hult', 'school', 'university', 'name', 'hello', 'also', 'currently','name',
                     'San Francisco', 'san francisco', 'Boston', 'boston', 'London', 'london', 'Dubai', 'dubai')
# subset for Campuses
SanFrancisco_bios <- studentBios %>% filter(studentBios$campus == 'San Francisco')
Boston_bios <- studentBios %>% filter(studentBios$campus == 'Boston')
London_bios <- studentBios %>% filter(studentBios$campus == 'London')
Dubai_bios <- studentBios %>% filter(studentBios$campus == 'Dubai')

# create Corpuses
sfCorpus   <- VCorpus(VectorSource(SanFrancisco_bios$bio))
bosCorpus  <- VCorpus(VectorSource(Boston_bios$bio))
lonCorpus  <- VCorpus(VectorSource(London_bios$bio))
dubCorpus  <- VCorpus(VectorSource(Dubai_bios$bio))

# clean Corpuses
sfCorpus <- cleanCorpus(sfCorpus, customStopwords_campus)
bosCorpus <- cleanCorpus(bosCorpus, customStopwords_campus)
lonCorpus <- cleanCorpus(lonCorpus, customStopwords_campus)
dubCorpus <- cleanCorpus(dubCorpus, customStopwords_campus)

## extract out the text. This way I extract the overall text for different campuses
sfCorpus <- unlist(sapply(sfCorpus, `[`, "content"))
sfCorpus <- paste(sfCorpus, collapse = ' ')

bosCorpus <- unlist(sapply(bosCorpus, `[`, "content"))
bosCorpus <- paste(bosCorpus, collapse = ' ')

lonCorpus <- unlist(sapply(lonCorpus, `[`, "content"))
lonCorpus <- paste(lonCorpus, collapse = ' ')

dubCorpus <- unlist(sapply(dubCorpus, `[`, "content"))
dubCorpus <- paste(dubCorpus, collapse = ' ')

# Create unique Corpus (has 4 documents for the 4 campuses)
alltext_campus <- c(sfCorpus, bosCorpus, lonCorpus, dubCorpus)
allCorp_campus <- VCorpus(VectorSource(alltext_campus))

# Create Term Document Matrix 
campus_TDM <- TermDocumentMatrix(allCorp_campus)
campus_TDMm <- as.matrix(campus_TDM) ## remember the order: San Francisco, Boston, London, Dubai

# substitute column names
col_names_campus <- c('San Francisco','Boston', 'London', 'Dubai')
colnames(campus_TDMm) <- col_names_campus

# Pallette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Commonality Cloud
dev.off()
commonality.cloud(campus_TDMm, 
                  max.words=100, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))
# Comparison Cloud
dev.off()
comparison.cloud(campus_TDMm, 
                 max.words=70, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('skyblue', 'purple', 'green', 'blue'),
                 scale=c(3,0.1))



## BI-GRAM TOKENIZATION
# Make bi-gram DTM according to the tokenize control & convert it to matrix
student_bi_DTM  <- DocumentTermMatrix(studentCorpus, 
                                      control=list(tokenize=bigramTokens))
student_bi_DTMm <- as.matrix(student_bi_DTM)

# See a bi-gram
idx <- grep('international business', colnames(student_bi_DTMm))
student_bi_DTMm[1:nrow(student_bi_DTMm),idx] 
## 'business school': only twice
## 'international business': 17 times (20% of the obs)

# Get Row Sums & organize
student_bi_DTMm_Vec <- sort(colSums(student_bi_DTMm), decreasing = TRUE)
wordFreqDF   <- data.frame(word      = names(student_bi_DTMm_Vec), 
                           freq      = student_bi_DTMm_Vec, 
                           row.names = NULL)

# Wordcloud
# Make simple word cloud
# Reminder to expand device pane
wordcloud(wordFreqDF$word,
          wordFreqDF$freq,
          max.words=20,
          random.order=FALSE,
          colors=pal)

# bar chart
# Simple barplot; values greater than 50 
topWords_bi      <- subset(wordFreqDF, wordFreqDF$freq >= 8) 
topWords_bi      <- topWords_bi[order(topWords_bi$freq, decreasing=F),]

# Chg to factor for ggplot
topWords_bi$word <- factor(topWords_bi$word, 
                        levels=unique(as.character(topWords_bi$word))) 

ggplot(topWords_bi, aes(x = word, y = freq)) + 
  geom_bar(stat = "identity", fill = 'skyblue') + 
  coord_flip() + theme_gdocs() +
  geom_text(aes(label = freq), colour = "black", hjust = 1.25, size = 5.0) +
  theme_minimal()


# Sentiment Analysis
# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
bing

# Perform Inner Join
bingSent <- inner_join(studentFrequency,
                       bing, 
                       by=c('word'='word'))
bingSent
bingSent <- as.data.frame(bingSent)

# Quick Analysis - count of words
bingResults <- aggregate(frequency ~ word+sentiment, bingSent, sum)
pivot_wider(bingResults, names_from = word, values_from = frequency)

# Get afinn lexicon
afinn <- get_sentiments(lexicon = c("afinn")) 
afinn
# Word Sequence
studentFrequency$idx       <- as.numeric(ave(studentFrequency$word, 
                                             studentFrequency$word, FUN=seq_along))
# Perform Inner Join
afinnSent <- inner_join(studentFrequency,
                        afinn, 
                        by=c('word'='word'))
afinnSent

# Calc
afinnSent$ValueCount <- afinnSent$value * afinnSent$frequency 
afinnSent

# Scatter plot Sentiment
ggplot(afinnSent, aes(x = value, y = frequency, color = factor(sign(ValueCount)))) +
  geom_point() +
  labs(title = "Density Curve of Sentiment",
       x = "value",
       y = "frequency") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()

# Sort for Value count (top 10)
afinnSent_sorted <- afinnSent %>% 
  arrange(desc(ValueCount)) %>%
  slice_head(n = 10)
afinnSent_sorted

# Visualization - Top Positive
# Bar chart
ggplot(afinnSent_sorted, aes(x = reorder(word, ValueCount), y = ValueCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = ValueCount), vjust = -0.5) +
  labs(title = "ValueCount of Sentiment", x = "Words", y = "ValueCount") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# Sort to show Negative
afinnSent_bottom <- afinnSent %>% 
  arrange(ValueCount) %>%
  slice_head(n = 10)
afinnSent_bottom

# Visualization - Top Negative
# Bar chart
lighter_red <- brewer.pal(2, "Reds")[2]

ggplot(afinnSent_bottom, aes(x = reorder(word, ValueCount), y = ValueCount)) +
  geom_bar(stat = "identity", fill = lighter_red) +
  geom_text(aes(label = ValueCount), vjust = -0.5) +
  labs(title = "Top Negative Sentiment", x = "Words", y = "ValueCount") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# ASSOCIATION
# Most frequents words (experience, business, people, different)
associations <- findAssocs(studentDTM, terms = 'different', 0.50) # 0.50 indicates i want only associations above 0.10
associations

# Organize the word associations
studentsDF_tot <- data.frame(terms = names(studentsDF_tot[[1]]),
                         value = unlist(studentsDF_tot),
                         row.names = NULL)
studentsDF_tot$terms <- factor(studentsDF_tot$terms, levels=studentsDF_tot$terms)
studentsDF_tot

ggplot(studentsDF_tot, aes(y=reorder(terms,value))) +
  geom_point(aes(x=value), data=studentsDF_tot, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" ) +
  labs(title = "Word Association", x = "Value", y = "Word associated") +
  theme(panel.grid = element_blank()) 


# Association between the most negative words (alone, ridiculous, challenge, exposed, ill)
associations_neg <- findAssocs(studentDTM, terms = 'ill', 0.50) # 0.10 indicates i want only associations above 0.10
associations_neg

# Organize the word associations
studentsDF <- data.frame(terms = names(associations_neg[[1]]),
                      value = unlist(associations_neg),
                      row.names = NULL)
studentsDF$terms <- factor(studentsDF$terms, levels=studentsDF$terms)
studentsDF

# Relevant association with ridiculous is classrooms (someone was not happy about classrooms) 

# Make a dot plot
ggplot(studentsDF, aes(y=reorder(terms,value))) +
  geom_point(aes(x=value), data=studentsDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" ) +
  theme(panel.grid = element_blank()) 

# Association Positive (love, best, amazing, opportunity, opportunities)
associations_pos <- findAssocs(studentDTM, terms = 'opportunities', 0.40) # 0.10 indicates i want only associations above 0.10
associations_pos

# Organize the word associations
studentsDF_pos <- data.frame(terms = names(associations_pos[[1]]),
                         value = unlist(associations_pos),
                         row.names = NULL)
studentsDF_pos$terms <- factor(studentsDF_pos$terms, levels=studentsDF_pos$terms)
studentsDF_pos

# Love associated with: food and health
# Best associated with: Decisions  (0.52)
# Amazing associated with: Backgrounds (0.41), Achieving and classmates
# Opportunity associated with: aspire, amicable, businesswoman
# Opportunities associated with:  countries (0.47), brand, create

ggplot(studentsDF_pos, aes(y=reorder(terms,value))) +
  geom_point(aes(x=value), data=studentsDF_pos, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" ) +
  labs(title = "Word Association - People", x = "Value", y = "Word associated") +
  theme(panel.grid = element_blank()) 


# Emotions
# Get nrc lexicon,notice that some words can have multiple sentiments
nrc <- lexicon_nrc()

# Perform Inner Join
nrcSent <- inner_join(studentFrequency,
                      nrc, 
                      by = c('word' = 'word'),
                      multiple = "all")
nrcSent

# Drop pos/neg leaving only emotion
nrcSent <- nrcSent[-grep('positive|negative',nrcSent$sentiment),]
# Quick check
table(nrcSent$sentiment,nrcSent$word)

# Manipulate for radarchart
nrcSentRadar <- as.matrix(table(nrcSent$sentiment, nrcSent$word))
nrcSentRadar

# Normalize for length; prop.table by column is "2"
nrcSentRadar <- prop.table(nrcSentRadar,2)
nrcSentRadar
colSums(nrcSentRadar) #quick check to see what prop table did

pivot_longer(as.data.frame.matrix(nrcSentRadar), col = everything())

# Organize
plotDF <- data.frame(labels = rownames(nrcSentRadar),
                     as.data.frame.matrix(nrcSentRadar),
                     row.names = NULL)

# Chart
chartJSRadar(scores = plotDF, labelSize = 10, showLegend = FALSE, lineColor = "blue")


# End
