#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
install.packages("tm")
install.packages("dplyr")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("igraph")
install.packages("ggraph")
install.packages("textdata")
install.packages("writexl")
install.packages("stringr")


library(mongolite)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(tm)
library(textdata)
library(topicmodels)
library(writexl)
library(stringr)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://scompagnone:i7cEQ7YpcO0JPQ3j@cluster0.uew8phc.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

# Print the columns
print(names(airbnb_all))

# Printing column types
for (column_name in colnames(airbnb_all)) {
  column_type <- class(airbnb_all[[column_name]])
  print(paste(column_name, ":" ,column_type))
}

############################
###### Data cleaning  ######
############################

# Selecting columns based on their data types EXCEPT LISTS AND DFs
selected_columns <- airbnb_all[, !sapply(airbnb_all, function(x) any(class(x) %in% c("list", "data.frame")))]

#####amenities#######
# converting into lists COLUMNS amenities AND reviews into strings 
selected_columns$amenities <- sapply(airbnb_all$amenities, function(x) paste(x, collapse=", "))
selected_columns$reviews <- sapply(airbnb_all$reviews, function(x) paste(x, collapse=", "))


#####HOST#######
# Convert the 'host' column into a data frame where each property is prefixed with 'host_'
host_df <- do.call(rbind, lapply(airbnb_all$host, function(x) {
  as.data.frame(t(x))
})) %>%
  # This renames the columns by adding 'host_' prefix
  setNames(paste0('host_', names(.)))

# Transpose host_df
transposed_host_df <- t(host_df)

# Append transposed_host_df to selected_columns
selected_columns <- cbind(selected_columns, transposed_host_df)

# convert list into string "host_verifications"
selected_columns$host_verifications_f <- sapply(selected_columns$host_verifications, function(x) paste(x, collapse=", "))

# Drop the column "host_verifications"
selected_columns <- subset(selected_columns, select = -c(host_verifications))


#####review_scores#######

# Convert the 'review_scores' column into a data frame where each property is prefixed with 'review_scores_'
review_scores_df <- do.call(rbind, lapply(airbnb_all$review_scores, function(x) {
  as.data.frame(t(x))
})) %>%
  # This renames the columns by adding 'review_scores_' prefix
  setNames(paste0('review_scores_', names(.)))

# Transpose review_scores_df
transposed_review_scores_df <- t(review_scores_df)

# Append transposed_review_scores_df to selected_columns
selected_columns <- cbind(selected_columns, transposed_review_scores_df)


#####availability#######

# Convert the 'availability' column into a data frame where each property is prefixed with 'availability_'
availability_df <- do.call(rbind, lapply(airbnb_all$availability, function(x) {
  as.data.frame(t(x))
})) %>%
  # This renames the columns by adding 'availability_' prefix
  setNames(paste0('availability_', names(.)))

# Transpose availability_df
transposed_availability_df <- t(availability_df)

# Append transposed_availability_df to selected_columns
selected_columns <- cbind(selected_columns, transposed_availability_df)


#####airbnb_all$address$location$coordinates#######

selected_columns$coordinates <- sapply(airbnb_all$address$location$coordinates, function(x) paste(x, collapse=", "))

#####airbnb_all$address$location#######
# Convert the 'location' column into a data frame where each property is prefixed with 'location_'
location_df <- do.call(rbind, lapply(airbnb_all$address$location, function(x) {
  as.data.frame(t(x))
})) %>%
  # This renames the columns by adding 'location_' prefix
  setNames(paste0('location_', names(.)))

# Transpose location_df
transposed_location_df <- t(location_df)

# Append transposed_location_df to selected_columns
selected_columns <- cbind(selected_columns, transposed_location_df)

#####airbnb_all$address#######

# Select the columns from airbnb_all$address
selected_from_address <- airbnb_all$address %>%
  select(street, suburb, government_area, market, country, country_code)

# Check if selected_columns is not null and has rows to ensure compatibility
if(!is.null(selected_columns) && nrow(selected_columns) > 0) {
  # Bind the columns to selected_columns
  selected_columns <- bind_cols(selected_columns, selected_from_address)
} else {
  # If selected_columns is null or empty, simply assign the selected columns to it
  selected_columns <- selected_from_address
}

selected_columns$coordinates_l <- sapply(selected_columns$coordinates...66, function(x) paste(x, collapse=", "))
selected_columns$is_location_exact_l <- sapply(selected_columns$is_location_exact, function(x) paste(x, collapse=", "))

selected_columns <- selected_columns %>%
  select(-is_location_exact, -`coordinates...66`,-type)

# Print the columns
print(names(selected_columns))


# Find indices of columns containing "url"
url_columns <- grep("url", names(selected_columns))

# Drop columns containing "url"
selected_columns <- selected_columns[, -url_columns]



# Define the column types
column_types <- list(
  "host_id" = "integer",
  "host_name" = "character",
  "host_about" = "character",
  "host_response_time" = "character",
  "host_neighbourhood" = "character",
  "host_response_rate" = "character",
  "host_identity_verified" = "logical",
  "host_listings_count" = "integer",
  "host_total_listings_count" = "integer",
  "host_verifications_f" = "character",
  "host_has_profile_pic"= "logical",
  "host_is_superhost" = "logical",
  "host_location" = "character"
)

# Convert each column to the specified type
for (column_name in names(column_types)) {
  selected_columns[[column_name]] <- as(selected_columns[[column_name]], column_types[[column_name]])
}

# Print the data types of the columns after conversion
for (column_name in colnames(selected_columns)) {
  column_type <- class(selected_columns[[column_name]])
  print(paste(column_name, ":" ,column_type))
}

# Selecting specific columns
cleaned_for_tableau_columns <- selected_columns[, c(
  "name",
  "property_type",
  "room_type",
  "bed_type",
  "minimum_nights",
  "maximum_nights",
  "cancellation_policy",
  "last_scraped",
  "calendar_last_scraped",
  "first_review",
  "last_review",
  "accommodates",
  "bedrooms",
  "beds",
  "number_of_reviews",
  "bathrooms",
  "price",
  "security_deposit",
  "cleaning_fee",
  "extra_people",
  "guests_included",
  "weekly_price",
  "monthly_price",
  "reviews_per_month",
  "amenities",
  "host_id",
  "host_name",
  "host_location",
  "host_response_time",
  "host_neighbourhood",
  "host_response_rate",
  "host_is_superhost",
  "host_has_profile_pic",
  "host_identity_verified",
  "host_listings_count",
  "host_total_listings_count",
  "host_verifications_f",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_location",
  "review_scores_value",
  "review_scores_rating",
  "availability_30",
  "availability_60",
  "availability_90",
  "availability_365",
  "coordinates...64",
  "street",
  "suburb",
  "government_area",
  "market",
  "country",
  "country_code",
  "coordinates_l",
  "is_location_exact_l"
)]

# Writing to Excel
write_xlsx(cleaned_for_tableau_columns, path = "selected_columns.xlsx")

#################
###### EDA ######
#################
# Print remaining column names
print(names(selected_columns))


# Summary statistics for numerical columns
summary(selected_columns[, c("minimum_nights", "maximum_nights", "accommodates", "bedrooms", "beds", "number_of_reviews", "bathrooms", "price")])

# Frequency distributions for categorical columns4
table(selected_columns$room_type)
table(selected_columns$bed_type)
table(selected_columns$cancellation_policy)


# Plot for room_type
ggplot(selected_columns, aes(x = room_type)) +
  geom_bar() +
  labs(title = "Frequency distribution of room types") +
  theme_minimal()

# Plot for bed_type
ggplot(selected_columns, aes(x = bed_type)) +
  geom_bar() +
  labs(title = "Frequency distribution of bed types") +
  theme_minimal()

# Plot for cancellation_policy with rotated x-axis labels
ggplot(selected_columns, aes(x = cancellation_policy)) +
  geom_bar() +
  labs(title = "Frequency distribution of cancellation policies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



################################
###### DATA PREPROCESSING ######
################################

# 1. Text Importing and Concatenation
data <- selected_columns
# Combine relevant text columns into one for analysis
data$combined_text <- paste(data$name, data$summary, data$description, data$neighborhood_overview, data$notes, data$transit, data$reviews, sep = " ")

# 2. Text Tokenization and Cleaning
library(tidytext)
library(stringr)
library(dplyr)
tokens <- data %>%
  unnest_tokens(word, combined_text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))

# Adjusting tokenization to include a document identifier for further analyses
tokens <- data %>%
  mutate(listing_id = row_number()) %>%
  unnest_tokens(word, combined_text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]")) %>%
  select(listing_id, word)

################################
####### FREQUENCY ANALYSIS #####
################################

# 3. Word Frequency Analysis
word_freq <- tokens %>%
  count(word, sort = TRUE)

# 4. Visualization of Word Frequency
library(ggplot2)
ggplot(word_freq %>%
         top_n(20), aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Words in Airbnb Listings")

################################
######## SENTIMENT ANALYSIS ####
################################

# 5. Sentiment Analysis
# Load AFINN sentiments
sentiments <- get_sentiments("afinn")

# Join tokens with sentiments for overall sentiment analysis
token_sentiment <- tokens %>%
  inner_join(sentiments, by = "word")

# Calculate overall sentiment score
overall_sentiment_score <- token_sentiment %>%
  summarise(total_sentiment = sum(value))

# Bing Sentiment Analysis
bing_sentiments <- get_sentiments("bing")
bing_score <- tokens %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(listing_id, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(bing_sentiment_score = positive - negative)

################################
########### TF-IDF #############
################################

# 6. TF-IDF Framework
# Now recalculating term frequency with the correct document identifier
tf <- tokens %>%
  count(listing_id, word, sort = TRUE) %>%
  ungroup()

# Calculate TF-IDF with the correct structure
tf_idf <- tf %>%
  bind_tf_idf(word, listing_id, n)

# Visualization of words with the highest TF-IDF scores
tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word_length = str_length(word)) %>%
  ggplot(aes(x = word_length, y = tf_idf)) +
  geom_point() +
  labs(x = "Word Length", y = "TF-IDF Score", title = "Word Length vs. TF-IDF Score")

################################
########## N-GRAMS #############
################################

# 7. N-grams and Tokenization
bigrams <- data %>%
  unnest_tokens(bigram, combined_text, token = "ngrams", n = 2)

# Frequency analysis of bigrams
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

# Visualization of Top Bigrams
ggplot(bigram_freq %>%
         arrange(desc(n)) %>%
         head(20), aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "plum") +
  coord_flip() +
  labs(title = "Top 20 Bigrams in Airbnb Listings",
       x = "Bigram",
       y = "Frequency") +
  theme_minimal()

################################
#### TOPIC MODELING & EMOTIONS ##
################################

# 8. Topic Modeling with LDA
library(topicmodels)
# For LDA, you first need to create a Document-Term Matrix (DTM)
dtm <- tokens %>%
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)

# Running LDA
lda_model <- LDA(dtm, k = 2) # Adjust the number of topics (k) as needed

# 9. NRC Emotion Analysis
nrc_sentiments <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"))

nrc_emotion_score <- tokens %>%
  left_join(nrc_sentiments, by = "word") %>%
  count(listing_id, sentiment) %>%
  spread(sentiment, n, fill = 0)

# Summarize and visualize emotions
nrc_emotion_summary <- nrc_emotion_score %>%
  gather(sentiment, count, -listing_id) %>%
  group_by(sentiment) %>%
  summarize(total = sum(count)) %>%
  mutate(percentage = total / sum(total) * 100) %>%
  arrange(desc(total))

# Plot
ggplot(nrc_emotion_summary, aes(x = reorder(sentiment, total), y = total, fill = sentiment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%"), y = total + 1),
            vjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Distribution of Emotions in Airbnb Listings",
       x = "Emotion",
       y = "Total Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

