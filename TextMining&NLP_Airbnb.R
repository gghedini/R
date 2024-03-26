# Giovanni Ghedini
# Text mining and NLP -- Airbnb
# 03/15/2024


#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://GGhedini:MPGEy8VRHCf6DTHZ@cluster0.gyxncja.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

##################################################################################

# libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(scales)
library(textcat)
library(tm)
library(topicmodels)
library(plotly)
library(igraph)
library(ggraph)

# stop words
library(stringr)
data(stop_words) # predefined library of stopwords


colnames(airbnb_all)

head(airbnb_all['reviews'],1)


# Extract Country
airbnb_all$country <- airbnb_all$address$country


### Investigate DESCRIPTION
colnames(airbnb_all)[5] <- 'text'

# Filter by English language
language_identification <- textcat(airbnb_all$text)
language_frequencies<-table(language_identification)
airbnb_all <- airbnb_all[language_identification == "english", ]

# tokenize 'description'
tidy_description <- airbnb_all %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

# Visualize
token_frequency <- tidy_description %>%
  mutate(word=reorder(word,n)) %>% # create another variable 'word' overwriting the previous one
  filter (n>1000) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(token_frequency)

# n-grams tokenization of 'description'
description_bigrams <- airbnb_all %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
description_bigrams

# ungroup to remove stop words
bigrams_separated <- description_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)


# visualize
bigram_graph <- bigram_counts %>%
  filter(n>45) %>%
  graph_from_data_frame()
bigram_graph

ggraph(bigram_graph, layout = "fr") + ## fr = frequency 
  geom_edge_link()+ # edge
  geom_node_point()+ # node
  geom_node_text(aes(label=name), vjust =1, hjust=1) # label the node with the name


# Sentiment analysis with Afinn, Bing, Nrc
description_token <- airbnb_all %>%
  unnest_tokens(word, text)

desc_country<- description_token %>%
  filter(country == "United States") ## in SHINY insert the input$ --> 'country' (input needs to be drop down)

# Overall Sentiment (afinn) of Description from country selected
afinn <- desc_country %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%  # summarize values (numbers between -5 and +5)
  mutate(method="AFINN")

# Union of Bing and Nrc Lexicon for Country Selected
bing_and_nrc <- bind_rows(
  desc_country%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  desc_country %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

# Visualize
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


## ZIPFs law and tf_idf for token importance

# count of tokens per country
description_token <- airbnb_all %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>% # counting including location informations
  ungroup()

total_words <- description_token %>%
  group_by(country) %>%
  summarise(total=sum(n, na.rm = TRUE))

# joining previous 2 tables and filter for 3 countries
description_words <- left_join(description_token, total_words)%>%
  filter(country %in% c("United States", "Canada", "Australia")) ## SHINY --> insert user input


ggplot(description_words, aes(n/total, fill = country))+  # n/total is the proportion of the tokens
  geom_histogram(show.legend=FALSE)+  # using a histogram to see frequency
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

# Very similar, united states seems to be the one with less noise

# ZIPFs
freq_by_rank <- description_words %>%
  group_by(country) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=country))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()
# based on the chart, from all the countries selected we find relevant insights in the tokens ranked 1 to 90ish

# TF_IDF
country_words <- description_words %>% # netflix_words is the frequency table of tokens for each country
  bind_tf_idf(word, country, n) ## Calculate TF_IDF

country_words %>%
  arrange(desc(tf_idf)) %>% 
  head(20)

# Graphical Approach
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()


# TF_IDF for BI-GRAM 
bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(country, bigram) %>%
  bind_tf_idf(bigram, country, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  filter(country == "United States")

bigram_tf_idf %>%
  filter(country == "Canada") %>% 
  arrange(desc(tf_idf)) %>% 
  head(20)


## CORRELOGRAMS (description by country)

# creating a tidy format for United States movies
usa <- airbnb_all %>%
  filter(country== "United States")

tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

# creating a tidy format for Australia movies
australia <- airbnb_all %>%
  filter(country== "Australia")

tidy_australia <- australia %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_australia)

# creating a tidy format for Canada movies
canada <- airbnb_all %>%
  filter(country== "Canada")

tidy_canada <- canada %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_canada)

frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_australia, author= "Australia"),
                       mutate(tidy_canada, author="Canada")
) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Australia`, `Canada`)

#let's plot the CORRELOGRAMS: (template where all to modify is 'United States')
ggplot(frequency, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)


# CREATING DTM
aibnb_dtm <- airbnb_all %>%
  unnest_tokens(word, text) %>%
  count(country, word) %>% # until now I created Tidy format
  cast_dtm(country, word, n) # line that creates DTM

aibnb_dtm

### LDA

# Latent Dirichlet Allocation algorithm
airbnb_lda <- LDA(aibnb_dtm, k=2, control=list(seed=123))
airbnb_lda

# BETA - what is the probability that "this term" will be generated by "this topic"
airbnb_topics <- tidy(airbnb_lda, matrix="beta")
airbnb_topics

top_terms <- airbnb_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>% 
  arrange(topic, -beta)
top_terms # we see highest probable tokens for each topic


# GAMMA (per document classification)
airbnb_gamma <- tidy(airbnb_lda, matrix="gamma")
airbnb_gamma

airbnb_gamma %>% 
  filter(document == "China")



### ANLAYSIS on REVIEWS
## Investigate Reviews

reviews <- airbnb_all$reviews # is a list

reviews_df <- bind_rows(reviews) # convert in dataframe

names(reviews_df) # 'comments' is the variable we are interested in

colnames(reviews_df)[6] <- 'text'

# Tidy format and remove stop words
tidy_reviews <- reviews_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)
tidy_reviews

# Visualize
review_token_frequency <- tidy_reviews %>%
  mutate(word=reorder(word,n)) %>% # create another variable 'word' overwriting the previous one
  filter (n>15000) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(review_token_frequency)


# bi-grams tokenization of 'reviews'
reviews_bigrams <- reviews_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort = TRUE)
reviews_bigrams

# ungroup to remove stop words
reviews_bigrams_separated <- reviews_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

reviews_bigrams_filtered <- reviews_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
reviews_bigrams_filtered

# visualize
reviews_bigrams_filtered %>%
  unite(word, word1, word2, sep = " ") %>% 
  mutate(word=reorder(word,n)) %>% # create another variable 'word' overwriting the previous one
  filter (n>1100) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
# most tokens express how convenient the location was


## SENTIMENT analysis
# Sentiment analysis with Bing

reviews_token <- reviews_df %>%
  unnest_tokens(word, text)

reviews_bing <- reviews_token %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(count = 1)

reviews_bing %>% 
  group_by(sentiment) %>% 
  summarise(count=sum(count)) %>% 
  ggplot(aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = "black") +  # Add labels
  labs(x = "Sentiment", y = "Count") +
  theme_minimal()

# Negative Reviews -- > most common negative reviews
reviews_bing %>% 
  filter(sentiment == 'negative') %>% 
  count(word, sort = TRUE) %>%
  mutate(word=reorder(word,n)) %>% # create another variable 'word' overwriting the previous one
  filter (n>500) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


### NUMERICAL ANALYTICS
# review_scores$review_scores_rating 
# price

# PRICE Descriptive Statistics
price_stats <- summarise(airbnb_all,
                         mean_price = mean(price, na.rm = TRUE),
                         median_price = median(price, na.rm = TRUE),
                         sd_price = sd(price, na.rm = TRUE),
                         min_price = min(price, na.rm = TRUE),
                         max_price = max(price, na.rm = TRUE))

print(price_stats)

# Handling Outliers
q1 <- quantile(airbnb_all$price, 0.25, na.rm=TRUE)
q3 <- quantile(airbnb_all$price, 0.75, na.rm=TRUE)
IQR <- q3 - q1

# Define the upper and lower bounds
lower_bound <- q1 - 1.5 * IQR
upper_bound <- q3 + 1.5 * IQR

airbnb_all_no_outliers <- airbnb_all[airbnb_all$price >= lower_bound & airbnb_all$price <= upper_bound, ]


# Calculate frequency of each price
price_freq <- airbnb_all_no_outliers %>% 
  group_by(price) %>% 
  summarise(frequency = n())

# Price Distribution for each COUNTRY
countries_for_boxplot <- airbnb_all_no_outliers %>% 
  filter(country %in% c('United States', 'Canada', 'Australia')) ## this will be the user input

plot_ly(countries_for_boxplot, x = ~country, y = ~price, type = 'box', color = ~country) %>%
  layout(xaxis = list(title = 'Country'),
         yaxis = list(title = 'Price'),
         hovermode = 'closest') %>%
  layout(xaxis = list(title = 'Country', tickangle = 45))

# Line Chart with Density Curve
ggplot(price_freq, aes(x = price)) +
  geom_density(color = "skyblue", fill = "skyblue", alpha = 0.5) +  # Density curve
  labs(title = "Distribution of Prices", x = "Price", y = "Density") +
  theme_minimal()


## Investigate PROPERTY TYPE
# remove NA property type
airbnb_all_no_outliers <- airbnb_all_no_outliers[!is.na(airbnb_all_no_outliers$property_type), ]

# Calculate average price by property type
avg_price_by_property <- airbnb_all_no_outliers %>%
  group_by(property_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(avg_price)  # Arrange by ascending average price

# Create the plot
plot_ly(avg_price_by_property, y = ~reorder(property_type, avg_price), x = ~avg_price, type = 'bar', orientation = 'h', marker = list(color = 'skyblue', line = list(color = 'black', width = 1))) %>%
  layout(title = 'Average Price by Property Type', xaxis = list(title = 'Average Price'), yaxis = list(title = 'Property Type'))


## Reviews score per property type for countries
country_reviews <- airbnb_all_no_outliers %>% 
  filter(country %in% c('United States')) ## User Input

avg_score_per_property <- country_reviews %>% 
  group_by(property_type) %>% 
  summarise(score = mean(review_scores$review_scores_rating, na.rm = TRUE)) %>% 
  arrange(score)

# Create the plot
plot_ly(avg_score_per_property, y = ~reorder(property_type, score), x = ~score, type = 'bar', orientation = 'h', marker = list(color = 'skyblue', line = list(color = 'black', width = 1))) %>%
  layout(title = 'Reviews Score by Property Type', xaxis = list(title = 'Average Score'), yaxis = list(title = 'Property Type'))


## END




