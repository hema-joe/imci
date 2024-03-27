# Load required libraries
library(tm)  # Text mining library
library(dplyr)  # Data manipulation library
library(stringr)  # String manipulation library
library(tidyr)  # Data tidying library
library(tm)
library(wordcloud)
install.packages("tidyverse")
library(tidyverse)

#Load Data
library(readxl)
imci <- read_excel("imci.xlsx")
View(imci)

imci
colnames(imci)

#Rename the columns to make them shorter
colnames(imci) <- c ("Timestamp", "Gender", "Age", "Facility", "IMCI pre-trained?",
                     " Management Challenges", "Chatbot Experience",
                     "Chatbots Usefulness", "Chatbot Usefulness reason",
                     "Positive Impression", "Speed", "Easiness", "Usefulness",
                     "General feedback")

colnames(imci)
imci

ncol(imci)
nrow(imci)

#Viewing Column responses

#Timestamp column
imci$Timestamp
class(imci$Timestamp)
imci <- imci[,-1]

imci
view(imci)

#Gender Column
imci$Gender
class(imci$Gender)
unique(imci$Gender)

# Summarize gender distribution
library(scales)

gender_summary <- imci %>%
  group_by(Gender) %>%
  summarise(count = n())

# Create pie chart
ggplot(gender_summary, aes(x = "", y = count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(count, " (", percent(count/sum(count)), ")")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Gender Distribution",
       fill = "Gender") +
  theme_void() +
  theme(legend.position = "bottom")

#Age Column

imci$Age
class(imci$Age)
imci$Age <- ifelse(imci$Age == "27years", "27", imci$Age)
imci$Age


# Convert 'Age' column to numeric
imci$Age <- as.numeric(imci$Age)


# Handle 1 missing value (represented as NA)
imci$Age[is.na(imci$Age)] <- median(imci$Age, na.rm = TRUE)  # Replace missing values with median age

mean(imci$Age)
median(imci$Age)

# Create histogram for age distribution
ggplot(imci, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal()


#Facility column
imci$Facility
class(imci$Facility)
unique(imci$Facility)

# Replace responses containing "St" or "st" with "St Joseph Hospital" in the 'Facility' column
imci$Facility <- ifelse(grepl("st", imci$Facility, ignore.case = TRUE), "St Joseph Hospital", imci$Facility)

imci$Facility
unique(imci$Facility)

imci$Facility <- ifelse(imci$Facility == "Hospital", "St Joseph Hospital", imci$Facility)

imci$Facility
unique(imci$Facility)

#Renaming responses for consistency
imci$Facility <- ifelse(imci$Facility == "CTK Pamoja", "CTK Pamoja Dispensary", imci$Facility)
imci$Facility <- ifelse(tolower(imci$Facility) == "seha" | tolower(imci$Facility) == "seha", "St Elizabeth Hospital", imci$Facility)
imci$Facility <- ifelse(imci$Facility == "Horebu Hill", "Horebu Hill Dispensary", imci$Facility)
imci$Facility <- ifelse(imci$Facility == "COGI OLDONYOSAMBU", "COGI Oldonyosambu Dispensary", imci$Facility)

imci$Facility
unique(imci$Facility)


facility_counts <- imci %>%
  group_by(Facility) %>%
  summarise(Total_Participants = n())

# Calculate percentages
facility_counts <- facility_counts %>%
  mutate(Percentage = scales::percent(Total_Participants / sum(Total_Participants)))

# Visualize the summary
ggplot(facility_counts, aes(x = reorder(Facility, Total_Participants), y = Total_Participants)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(Total_Participants, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Total Number of Participants by Facility",
       x = "Facility",
       y = "Total Participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Adding a new column showing Facility levels

imci$Facility_Level <- ifelse(imci$Facility %in% c("Horebu Hill Dispensary", "CTK Pamoja Dispensary", "COGI Oldonyosambu Dispensary"), "Dispensary",
                              ifelse(imci$Facility == "St Elizabeth Hospital", "District",
                                     ifelse(imci$Facility == "St Joseph Hospital", "Regional Referral", NA)))

# Reorder columns to have 'Facility Level' next to 'Facility'
imci <- cbind(imci[, c("Facility", "Facility_Level")], imci[, !(names(imci) %in% c("Facility", "Facility_Level"))])


# Count total number of participants for each facility level
facility_level_counts <- imci %>%
  group_by(Facility_Level) %>%
  summarise(Total_Participants = n())

# Calculate percentages
facility_level_counts <- facility_level_counts %>%
  mutate(Percentage = scales::percent(Total_Participants / sum(Total_Participants)))


# Visualize the summary
ggplot(facility_level_counts, aes(x = reorder(Facility_Level, Total_Participants),
                                  y = Total_Participants)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(Total_Participants, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Total Number of Participants by Facility Level",
       x = "Facility Level",
       y = "Total Participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#IMCI Training
imci$`IMCI pre-trained?`
class(imci$`IMCI pre-trained?`)
unique(imci$`IMCI pre-trained?`)

# Create a summary of counts for "IMCI pre-trained?" column
imci_summary <- table(imci$`IMCI pre-trained?`, useNA = "ifany")

# Convert summary to dataframe
imci_summary <- as.data.frame(imci_summary)
names(imci_summary) <- c("IMCI_pre_trained", "Count")

# Calculate percentages
imci_summary <- imci_summary %>%
  mutate(Percentage = scales::percent(Count / sum(Count)))

# Visualize the summary
ggplot(imci_summary, aes(x = IMCI_pre_trained, y = Count, fill = IMCI_pre_trained)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Summary of IMCI Pre-trained Status",
       x = "IMCI Pre-trained",
       y = "Count") +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "brown", "NA" = "grey")) +
  theme_minimal()


#Challenges of Management of Childhood Illness
imci$` Management Challenges`
class(imci$` Management Challenges`)

#Text Analysis

# Visualize sentiment analysis results (if applicable)
text_corpus <- Corpus(VectorSource(imci$` Management Challenges`))

# Convert to lowercase
text_corpus <- tm_map(text_corpus, content_transformer(tolower))

# Remove punctuation
text_corpus <- tm_map(text_corpus, removePunctuation)

# Remove numbers
text_corpus <- tm_map(text_corpus, removeNumbers)

# Remove common stopwords
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))


tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
wordcloud(words = names(word_freqs), freq = word_freqs, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))



#Chatbot Experience
imci$`Chatbot Experience`
class(imci$`Chatbot Experience`)
unique(imci$`Chatbot Experience`)

summary_data <- imci %>%
  group_by(`Chatbot Experience`) %>%
  summarise(Total_Count = n()) %>%
  mutate(Percentage = scales::percent(Total_Count / sum(Total_Count)))

# Visualize the summary
ggplot(summary_data, aes(x = `Chatbot Experience`, y = Total_Count,
                         fill = `Chatbot Experience`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Total_Count, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Summary of Chatbot Experience",
       x = "Chatbot Experience",
       y = "Count") +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "brown", "NA" = "grey"),
                    na.value = "grey") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Chatbot usefulness
imci$`Chatbots Usefulness`
class(imci$`Chatbots Usefulness`)
unique(imci$`Chatbots Usefulness`)



# Summarize counts and percentages
summary_data <- imci %>%
  group_by(`Chatbots Usefulness`) %>%
  summarise(Total_Count = n()) %>%
  mutate(Percentage = scales::percent(Total_Count / sum(Total_Count)))

# Visualize the summary
ggplot(summary_data, aes(x = `Chatbots Usefulness`, y = Total_Count,
                         fill = `Chatbots Usefulness`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Total_Count, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Summary of Chatbots Usefulness",
       x = "Chatbots Usefulness",
       y = "Count") +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "brown"),
                    na.value = "grey") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Chatbot usefulness reason
imci$`Chatbot Usefulness reason`
class(imci$`Chatbot Usefulness reason`)

#Text Analysis

# Visualize sentiment analysis results (if applicable)

text_corpus_1 <- Corpus(VectorSource(imci$`Chatbot Usefulness reason`))

# Preprocessing steps
text_corpus_1 <- tm_map(text_corpus_1, content_transformer(tolower))
text_corpus_1 <- tm_map(text_corpus_1, removePunctuation)
text_corpus_1 <- tm_map(text_corpus_1, removeNumbers)
text_corpus_1 <- tm_map(text_corpus_1, removeWords, stopwords("english"))

# Create term-document matrix
tdm_1 <- TermDocumentMatrix(text_corpus_1)
m_1 <- as.matrix(tdm_1)
word_freqs_1 <- sort(rowSums(m_1), decreasing = TRUE)

# Create word cloud
wordcloud(words = names(word_freqs_1), freq = word_freqs_1, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


#Positive Impression of the IMCI Chatbot
imci$`Positive Impression`
class(imci$`Positive Impression`)
unique(imci$`Positive Impression`)


summary_data <- imci %>%
  group_by(`Positive Impression`) %>%
  summarise(Total_Count = n()) %>%
  mutate(Percentage = scales::percent(Total_Count / sum(Total_Count)))


# Visualize the summary
ggplot(summary_data, aes(x = `Positive Impression`, y = Total_Count,
                         fill = `Positive Impression`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Total_Count, " (", Percentage, ")")), 
            vjust = -0.5, size = 3) +
  labs(title = "Summary of Positive Impression",
       x = "Positive Impression",
       y = "Count") +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "brown"),
                    na.value = "grey") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Speed of the IMCI Chatbot
imci$Speed
class(imci$Speed)
unique(imci$Speed)

# Convert 'Speed' to numeric, excluding non-numeric values
imci$Speed <- as.numeric(gsub("[^0-9]", "", imci$Speed))

# Plot
hist(imci$Speed, main = "Distribution of Speed", xlab = "Speed",
     ylab = "Frequency", col = "skyblue", border = "white")


#Easiness of the IMCI Chatbot
imci$Easiness
class(imci$Easiness)
unique(imci$Easiness)

# Convert 'Easiness' to numeric, excluding non-numeric values
imci$Easiness <- as.numeric(gsub("[^0-9]", "", imci$Easiness))

# Plot
hist(imci$Easiness, main = "Distribution of Easiness", xlab = "Easiness",
     ylab = "Frequency", col = "skyblue", border = "white")


#Usefulness of the IMCI Chatbot
imci$Usefulness
class(imci$Usefulness)
unique(imci$Usefulness)

# Convert 'Usefulness' to numeric, excluding non-numeric values
imci$Usefulness <- as.numeric(gsub("[^0-9]", "", imci$Usefulness))

# Plot
hist(imci$Usefulness, main = "Distribution of Usefulness", xlab = "Usefulness",
     ylab = "Frequency", col = "skyblue", border = "white")


#General Feedback of the IMCI Chatbot
imci$`General feedback`
class(imci$`General feedback`)


#Text Analysis
text_corpus <- Corpus(VectorSource(imci$`General feedback`))

# Pre-processing steps
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))

# Create term-document matrix
tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create word cloud
wordcloud(words = names(word_freqs), freq = word_freqs, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#Saving
install.packages("openxlsx")
library(openxlsx)
# Save the updated 'imci' dataframe as an Excel file
write.xlsx(imci, "imci_with_facility_level.xlsx")
