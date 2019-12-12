# This is a replication material for the final project
# Paper title: Sentiment Analysis of U.S. Presidential Speeches:
# Inaugural and State of the Union Addresses from 1789 to 2019

# MACS 40500: Computational Methods for American Politics (Autumn 2019)
# Author: Sanittawan Tan

library(tidyverse)
library(sotu)
library(tm)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(ggplot2)
library(ngram)
library(tidytext)
library(topicmodels)

# please change your working directory before running this code
working_dir <- "Documents/Nikki/UChicago/Classes/Autumn_2019/com_methods_am_politics/local_final_project/"

# Note: all figures are saved in a directory /presidential_speeches/images/ 
# which is a subdirectory of local_final_project
# please create such folders before running the below code
# also needs to create ./presidential_speeches/data/ subfolder
# which should contain trump_sotu_2018.txt and trump_sotu_2019.txt

setwd(working_dir)
inaug_speech <- read_csv(file.choose())
# please select analysis_inaug_speech.csv

# change name of a field
names(inaug_speech)[1] <- "president"
names(inaug_speech)

# Find the length of all inaug speeches

primary_clean_text <- function(doc) {
    # convert to lower case
    doc <- tm_map(doc, tolower)
    # remove whitespace
    doc <- tm_map(doc, stripWhitespace)
    # convert to plaintext
    doc <- tm_map(doc, PlainTextDocument)
}

prim_clean_text_col <- function(text_col) {
    corpus <- VCorpus(VectorSource(text_col))
    cleaned_txt <- primary_clean_text(corpus)
    return(cleaned_txt[[1]]$content)
}

prim_cleaned_inaug <- lapply(inaug_speech$text, prim_clean_text_col)

inaug_lengths <- lapply(prim_cleaned_inaug, wordcount)

ia_len_df <- data.frame("president" = inaug_speech$president, "length" = unlist(inaug_lengths), "year" = inaug_speech$speech_year)
ia_len_mean <- mean(ia_len_df$length) # 2328.466

# clean the inaugural speech

process_text <- function(doc) {
    # convert to lower case
    doc <- tm_map(doc, tolower)
    # remove stopwords
    doc <- tm_map(doc, removeWords, stopwords("english"))
    # remove numbers
    doc <- tm_map(doc, removeNumbers)
    # remove punctuation
    doc <- tm_map(doc, removePunctuation,
                  preserve_intra_word_dashes = TRUE)
    # remove whitespace
    doc <- tm_map(doc, stripWhitespace)
    # convert to plaintext
    doc <- tm_map(doc, PlainTextDocument)
}

remove_words <- function(doc, word) {
    for (i in seq(doc)) {
        # substitute word with empty string
        doc[[i]]$content <- gsub(word, "", doc[[i]]$content)
        
        # remove leading and trailing whitespace
        doc[[i]]$content <- gsub("^\\s+|\\s+$", "", doc[[i]]$content)
    }
    return(doc)
}

clean_text_col <- function(text_col) {
    corpus <- VCorpus(VectorSource(text_col))
    cleaned_txt <- process_text(corpus)
    return(cleaned_txt[[1]]$content)
}

inaug_speech$text <- lapply(inaug_speech$text, clean_text_col)

# done cleaning inaugural speeches, ready for analysis

all_inaug <- VCorpus(VectorSource(inaug_speech$text))
writeLines(as.character(all_inaug[1]))


# 1. make wordclouds for all the inaug speeches

all_inaug_dtm <- DocumentTermMatrix(all_inaug)

inaug_frequency <- sort(colSums(as.matrix(all_inaug_dtm)), 
                    decreasing=TRUE)
head(inaug_frequency)

set.seed(986)
jpeg(
    filename="./presidential_speeches/images/wordcloud1.jpeg",
    width = 7,
    height = 5,
    units = "in",
    res = 300)
wordcloud(names(inaug_frequency), inaug_frequency, 
          scale=c(2,0.5), max.words = 400,
          random.order = FALSE,
          rot.per = 0.30,
          main = "Title",
          colors = brewer.pal(6, "Set2")
)
dev.off()

# 2. calculate sentiment scores for each inaug speech

all_inaug_tidy <- tidy(all_inaug_dtm)

analyze_sentiments <- function(tidy_data, sent_dict) {
    rv <- tidy_data %>%
        inner_join(get_sentiments(sent_dict), by = c(term = "word"))
    return(rv)
}

all_inaug_afinn <- analyze_sentiments(all_inaug_tidy, "afinn")
all_inaug_afinn <- distinct(all_inaug_afinn, term, .keep_all = TRUE)

stm_vec <- c() 
for (i in seq(1:length(all_inaug))) {
    corpus <- VCorpus(VectorSource(c(all_inaug[[i]]$content)))
    dtm <- DocumentTermMatrix(corpus)
    tidy_df <- tidy(dtm)
    stm <- analyze_sentiments(tidy_df, "bing")
    stm <- stm %>%
        mutate(score = ifelse(sentiment == "positive", 1, -1))
    stm <- distinct(stm, term, .keep_all = TRUE)
    final_stm <- sum(stm$score * stm$count) / length(stm$score)
    print(length(stm$score))
    stm_vec <- append(stm_vec, final_stm)
}
print(stm_vec)

result_df <- ia_len_df
result_df$sentiment <- stm_vec
overall_inaug_sent <- mean(result_df$sentiment) # 68.44828 or 0.512285 normalized version

# 3. find common topics on all inaug speeches

remove_all_zeros <- function(dtm) {
    row_total <- apply(dtm, 1, sum)
    rv <- dtm[row_total > 0,]
    return(rv)
}
new_all_inaug_dtm <- remove_all_zeros(all_inaug_dtm)
# check 
rows_with_zero_sum <- apply(new_all_inaug_dtm, 1, sum)
print(new_all_inaug_dtm[rows_with_zero_sum == 0,]$dimnames[1][[1]])

get_top_terms <- function(topics) {
    top_terms <- topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    return(top_terms)
}

plot_by_topics <- function(top_terms, party, num_topics) {
    Title <- paste("Topics in", party, "Inaugural Addresses", sep=" ")
    Subtitle <- paste("When the number of topics is", num_topics, sep=" ")
    
    top_terms %>%
        mutate(topic = as.factor(topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = topic)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        labs(y = "beta",
             x = NULL,
             title = Title,
             subtitle = Subtitle)
}

# 5 topics

all_inaug_lda <- LDA(new_all_inaug_dtm, k = 5, control = list(seed = 54))
all_inaug_lda_5topics <- tidy(all_inaug_lda, matrix = "beta")
all_inaug_lda_5_perpl <- c(perplexity(all_inaug_lda, newdata = new_all_inaug_dtm), 5)
print(all_inaug_lda_5_perpl)

all_inaug_top_terms <- get_top_terms(all_inaug_lda_5topics)
jpeg(
    filename="./presidential_speeches/images/inaug5topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics(all_inaug_top_terms, "US", "5")
dev.off()

# 2 topics

all_inaug_lda2 <- LDA(new_all_inaug_dtm, k = 2, control = list(seed = 12))
all_inaug_lda_2topics <- tidy(all_inaug_lda2, matrix = "beta")
all_inaug_lda_2_perpl <- c(perplexity(all_inaug_lda2, newdata = new_all_inaug_dtm), 2)
print(all_inaug_lda_2_perpl)

all_inaug_top_terms2 <- get_top_terms(all_inaug_lda_2topics)
jpeg(
    filename="./presidential_speeches/images/inaug2topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics(all_inaug_top_terms2, "US", "2")
dev.off()

# 10 topics

all_inaug_lda10 <- LDA(new_all_inaug_dtm, k = 10, control = list(seed = 12))
all_inaug_lda_10topics <- tidy(all_inaug_lda10, matrix = "beta")
all_inaug_lda_10_perpl <- c(perplexity(all_inaug_lda10, newdata = new_all_inaug_dtm), 2)
print(all_inaug_lda_10_perpl)

all_inaug_top_terms3 <- get_top_terms(all_inaug_lda_10topics)
jpeg(
    filename="./presidential_speeches/images/inaug10topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics(all_inaug_top_terms3, "US", "10")
dev.off()

# clean the SOTU speech here

sotu_df <- sotu_meta

# add SOTU speech ID before joining
sotu_df$speech_id <- seq.int(nrow(sotu_df))
names(sotu_df)

trump1 <- read_file("./presidential_speeches/data/trump_sotu_2018.txt")
trump2 <- read_file("./presidential_speeches/data/trump_sotu_2019.txt")

sotu_speeches <- as.data.frame(sotu_text)
sotu_speeches$speech_id <- sotu_df$speech_id
names(sotu_speeches)[1] <-  "text"
sotu_speeches <- sotu_speeches %>%
    add_row(text = trump1, speech_id = 237) %>%
    add_row(text = trump2, speech_id = 238)

sotu_df <- sotu_df %>%
    add_row(president= "Donald J. Trump", year = 2018, years_active = "2017-2019",
            party = "Republican", sotu_type = "speech", speech_id = 237) %>%
    add_row(president= "Donald J. Trump", year = 2019, years_active = "2017-2019",
            party = "Republican", sotu_type = "speech", speech_id = 238)

# Lengths trend of SOTU
 
prim_cleaned_sotu <- lapply(sotu_speeches$text, prim_clean_text_col)

sotu_lengths <- lapply(prim_cleaned_sotu, wordcount)

sotu_len_df <- data.frame("president" = sotu_df$president, "length" = unlist(sotu_lengths), "year" = sotu_df$year)
sotu_len_mean <- mean(sotu_len_df$length) # 8263.592

# clean the speeches for analysis

sotu_speeches$text <- lapply(sotu_speeches$text, clean_text_col)

# done cleaning inaugural speeches, ready for analysis

all_sotu <- VCorpus(VectorSource(sotu_speeches$text))
writeLines(as.character(all_sotu[1]))

# 1. make wordclouds for all the inaug speeches

all_sotu_dtm <- DocumentTermMatrix(all_sotu)

sotu_frequency <- sort(colSums(as.matrix(all_sotu_dtm)), 
                        decreasing=TRUE)
head(sotu_frequency)

set.seed(986)
jpeg(
    filename="./presidential_speeches/images/wordcloud2.jpeg",
    width = 7,
    height = 5,
    units = "in",
    res = 300)
wordcloud(names(sotu_frequency), sotu_frequency, 
          scale=c(2,0.5), max.words = 400,
          random.order = FALSE,
          rot.per = 0.30,
          main = "Title",
          colors = brewer.pal(6, "Set2")
)
dev.off()

# 2. calculate sentiment scores for each inaug speech

all_sotu_tidy <- tidy(all_sotu_dtm)

analyze_sentiments <- function(tidy_data, sent_dict) {
    rv <- tidy_data %>%
        inner_join(get_sentiments(sent_dict), by = c(term = "word"))
    return(rv)
}

all_sotu_afinn <- analyze_sentiments(all_sotu_tidy, "afinn")
all_sotu_afinn <- distinct(all_sotu_afinn, term, .keep_all = TRUE)

sotu_stm_vec <- c() 
for (i in seq(1:length(all_sotu))) {
    corpus <- VCorpus(VectorSource(c(all_sotu[[i]]$content)))
    dtm <- DocumentTermMatrix(corpus)
    tidy_df <- tidy(dtm)
    stm <- analyze_sentiments(tidy_df, "bing")
    stm <- stm %>%
        mutate(score = ifelse(sentiment == "positive", 1, -1))
    stm <- distinct(stm, term, .keep_all = TRUE)
    final_stm <- sum(stm$score * stm$count) / length(stm$score)
    sotu_stm_vec <- append(sotu_stm_vec, final_stm)
}
print(sotu_stm_vec)

sotu_result_df <- sotu_len_df
sotu_result_df$sentiment <- sotu_stm_vec
overall_sotu_sent <- mean(sotu_result_df$sentiment) # 154.4622

# 3. find common topics on all inaug speeches

remove_all_zeros <- function(dtm) {
    row_total <- apply(dtm, 1, sum)
    rv <- dtm[row_total > 0,]
    return(rv)
}
new_all_sotu_dtm <- remove_all_zeros(all_sotu_dtm)
# check 
rows_with_zero_sum <- apply(new_all_sotu_dtm, 1, sum)
print(new_all_sotu_dtm[rows_with_zero_sum == 0,]$dimnames[1][[1]])

get_top_terms <- function(topics) {
    top_terms <- topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    return(top_terms)
}

plot_by_topics_sotu <- function(top_terms, party, num_topics) {
    Title <- paste("Topics in", party, "State of the Union Addresses", sep=" ")
    Subtitle <- paste("When the number of topics is", num_topics, sep=" ")
    
    top_terms %>%
        mutate(topic = as.factor(topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = topic)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        labs(y = "beta",
             x = NULL,
             title = Title,
             subtitle = Subtitle)
}

# 5 topics

all_sotu_lda <- LDA(new_all_sotu_dtm, k = 5, control = list(seed = 54))
all_sotu_lda_5topics <- tidy(all_sotu_lda, matrix = "beta")
all_sotu_lda_5_perpl <- c(perplexity(all_sotu_lda, newdata = new_all_sotu_dtm), 5)
print(all_sotu_lda_5_perpl)

all_sotu_top_terms <- get_top_terms(all_sotu_lda_5topics)
jpeg(
    filename="./presidential_speeches/images/sotu5topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_sotu(all_sotu_top_terms, "US", "5")
dev.off()

# 2 topics

all_sotu_lda2 <- LDA(new_all_sotu_dtm, k = 2, control = list(seed = 54))
all_sotu_lda_2topics <- tidy(all_sotu_lda2, matrix = "beta")
all_sotu_lda_2_perpl <- c(perplexity(all_sotu_lda2, newdata = new_all_sotu_dtm), 2)
print(all_sotu_lda_2_perpl)

all_sotu_top_terms2 <- get_top_terms(all_sotu_lda_2topics)
jpeg(
    filename="./presidential_speeches/images/sotu2topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_sotu(all_sotu_top_terms2, "US", "2")
dev.off()

# 10 topics

all_sotu_lda10 <- LDA(new_all_sotu_dtm, k = 10, control = list(seed = 12))
all_sotu_lda_10topics <- tidy(all_sotu_lda10, matrix = "beta")
all_sotu_lda_10_perpl <- c(perplexity(all_sotu_lda10, newdata = new_all_sotu_dtm), 10)
print(all_sotu_lda_10_perpl)

all_sotu_top_terms3 <- get_top_terms(all_sotu_lda_10topics)
jpeg(
    filename="./presidential_speeches/images/sotu10topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_sotu(all_sotu_top_terms3, "US", "10")
dev.off()

# 20 topics

all_sotu_lda20 <- LDA(new_all_sotu_dtm, k = 20, control = list(seed = 12))
all_sotu_lda_20topics <- tidy(all_sotu_lda20, matrix = "beta")
all_sotu_lda_20_perpl <- c(perplexity(all_sotu_lda20, newdata = new_all_sotu_dtm), 20)
print(all_sotu_lda_20_perpl)

all_sotu_top_terms4 <- get_top_terms(all_sotu_lda_20topics)
jpeg(
    filename="./presidential_speeches/images/sotu20topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_sotu(all_sotu_top_terms4, "US", "20")
dev.off()

# Plot length of the inaugural addresses

jpeg(
    filename="./presidential_speeches/images/inaug_len.jpeg",
    width = 7,
    height = 5,
    units = "in",
    res = 300)
inaug_p <- ggplot(data = ia_len_df, aes(x = year, y = length)) +
    geom_line()+
    geom_point() +
    theme_bw()

inaug_p + 
    ggtitle("Trend in the Length of Inaugural Addresses")
dev.off()

# Plot length of SOTU

# make changes to the year to make it unique

sotu_len_df[1,3] <- "1790a"
sotu_len_df[2,3] <- "1790b"
sotu_len_df[200,3] <- "1981b"
sotu_len_df[201,3] <- "1981a"
sotu_len_df[194,3] <- "1978a"
sotu_len_df[195,3] <- "1978b"
sotu_len_df[196,3] <- "1979a"
sotu_len_df[197,3] <- "1979b"
sotu_len_df[198,3] <- "1980a"
sotu_len_df[199,3] <- "1980b"
sotu_len_df[189,3] <- "1974a"
sotu_len_df[190,3] <- "1974b"
sotu_len_df[187,3] <- "1972a"
sotu_len_df[188,3] <- "1972b"
sotu_len_df[175,3] <- "1961b"
sotu_len_df[176,3] <- "1961a"
sotu_len_df[169,3] <- "1956a"
sotu_len_df[170,3] <- "1956b"
sotu_len_df[165,3] <- "1953b"
sotu_len_df[166,3] <- "1953a"
sotu_len_df[156,3] <- "1945a"
sotu_len_df[157,3] <- "1945b"

jpeg(
    filename="./presidential_speeches/images/sotu_len.jpeg",
    width = 10,
    height = 6,
    units = "in",
    res = 300)
inaug_p <- ggplot(data = sotu_len_df, aes(x = year, y = length, group = 1)) +
    geom_line()+
    geom_point() +
    theme_bw() +
    scale_x_discrete(breaks = sotu_len_df$year[c(T,F,F,F,F,F,F,F,F,F)]) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

inaug_p + 
    ggtitle("Trend in the Length of the State of the Union Addresses")

dev.off()

# top 3 longest speech 

sorted_len <- sotu_len_df[order(sotu_len_df$length),]
tail(sorted_len)

sorted_len_inaug <- ia_len_df[order(ia_len_df$length),]
tail(sorted_len_inaug)

# Main Analysis Results

# Sentiments - State of the Union Addresses 
length(sotu_len_df$year)
length(sotu_result_df$year)
sotu_result_df$year <- sotu_len_df$year

# Sentiments - Inaugural Addresses 

presidents_to_drop <- c("William Henry Harrison",
                        "John Tyler",
                        "Andrew Johnson",
                        "Chester A. Arthur",
                        "Gerald R. Ford",
                        "Millard Fillmore",
                        "James A. Garfield")

rows_to_keep <- which(!(result_df$president %in% presidents_to_drop))
final_inaug_df <- result_df[rows_to_keep,]

rows_to_keep <- which(!(sotu_result_df$president %in% presidents_to_drop))
final_sotu_df <- sotu_result_df[rows_to_keep,]

# plot 

# 18th century presidents

final_sotu_df %>%
    filter(year >= 1700 & year <= 1799) %>%
    distinct(president)

# create data for plotting

# make it until 1800 because of John Adams
pres_18_inaug <- final_inaug_df %>%
    filter(year >= 1700 & year <= 1800)

pres_18_sotu <- final_sotu_df %>%
    filter(year >= 1700 & year <= 1800)

# combine

df_plot18 <- rbind(pres_18_inaug, pres_18_sotu)
df_plot18 <- df_plot18[order(df_plot18$year),]

# manually change some years
df_plot18[6,3] <- "1793a"
df_plot18[7,3] <- "1793b"
df_plot18[11,3] <- "1797a"
df_plot18[12,3] <- "1797b"

# plot
jpeg(
    filename="./presidential_speeches/images/18thsentiments.jpeg",
    width = 7,
    height = 5,
    units = "in",
    res = 300)
ggplot(data = df_plot18, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president) +
    labs(title= "Sentiment Comparison: 18th Century Presidents") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# 19th century presidents (first half)

# make it to 1900 because of William McKinley
final_sotu_df %>%
    filter(year >= 1801 & year <= 1900) %>%
    distinct(president)

# create data for plotting

pres_19_inaug1 <- final_inaug_df %>%
    filter(year >= 1801 & year <= 1856)

pres_19_sotu1 <- final_sotu_df %>%
    filter(year >= 1801 & year <= 1856)

# combine

df1_plot19 <- rbind(pres_19_inaug1, pres_19_sotu1)
df1_plot19 <- df1_plot19[order(df1_plot19$year),]
rownames(df1_plot19 ) <- 1:nrow(df1_plot19)

levels(df1_plot19$president)[levels(df1_plot19$president) == "James Knox Polk"] <- "James K. Polk"

# make changes to the years, so it is easy to read

for (i in 1:nrow(df1_plot19)) {
    if ((i %% 5) == 1 & i != 61) {
        cur_year <- df1_plot19[i, 3]
        new_year <- paste(cur_year, "a", sep="")
        df1_plot19[i, 3] <- new_year
    } else if ((i %% 5) == 2 & i != 62) {
        cur_year <- df1_plot19[i, 3]
        new_year <- paste(cur_year, "b", sep="")
        df1_plot19[i, 3] <- new_year
    } else if (i == 58) {
        cur_year <- df1_plot19[i, 3]
        new_year <- paste(cur_year, "a", sep="")
        df1_plot19[i, 3] <- new_year
    } else if (i == 59) {
        cur_year <- df1_plot19[i, 3]
        new_year <- paste(cur_year, "b", sep="")
        df1_plot19[i, 3] <- new_year
    }
}

distinct(df1_plot19, president)
df1_plot19$president_f = factor(df1_plot19$president,
                                levels=c("Thomas Jefferson",
                                         "James Madison",
                                         "James Monroe",
                                         "John Quincy Adams",
                                         "Andrew Jackson",
                                         "Martin Van Buren",
                                         "James K. Polk",
                                         "Zachary Taylor",
                                         "Franklin Pierce"))

# plot
jpeg(
    filename="./presidential_speeches/images/19thsentiments1.jpeg",
    width = 15,
    height = 10,
    units = "in",
    res = 300)
ggplot(data = df1_plot19, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president_f) +
    labs(title= "Sentiment Comparison: 19th Century Presidents (1801-1856)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# 19th century presidents (second half)

# make it to 1900 because of William McKinley
final_sotu_df %>%
    filter(year >= 1857 & year <= 1900) %>%
    distinct(president)

# create data for plotting

pres_19_inaug <- final_inaug_df %>%
    filter(year >= 1857 & year <= 1901)

pres_19_sotu <- final_sotu_df %>%
    filter(year >= 1857 & year <= 1900)

# combine

df2_plot19 <- rbind(pres_19_inaug, pres_19_sotu)
df2_plot19 <- df2_plot19[order(df2_plot19$year),]
rownames(df2_plot19 ) <- 1:nrow(df2_plot19)

# make changes to the years, so it is easy to read
for (i in 1:nrow(df1_plot19)) {
    if (i %in% c(1,6,12,17,22,27,32,37,42)) {
        cur_year <- df2_plot19[i, 3]
        new_year <- paste(cur_year, "a", sep="")
        df2_plot19[i, 3] <- new_year
    } else if (i %in% c(2,7,13,18,23,28,33,38,43)) {
        cur_year <- df2_plot19[i, 3]
        new_year <- paste(cur_year, "b", sep="")
        df2_plot19[i, 3] <- new_year
    }
}

distinct(df2_plot19, president)
df2_plot19$president_f = factor(df2_plot19$president,
                                levels=c("James Buchanan",
                                         "Abraham Lincoln",
                                         "Ulysses S. Grant",
                                         "Rutherford B. Hayes",
                                         "Grover Cleveland",
                                         "Benjamin Harrison",
                                         "William McKinley"))


# plot
jpeg(
    filename="./presidential_speeches/images/19thsentiments2.jpeg",
    width = 15,
    height = 10,
    units = "in",
    res = 300)
ggplot(data = df2_plot19, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president_f) +
    labs(title= "Sentiment Comparison: 19th Century Presidents (1857-1900") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


# 20th century presidents (first half)

# make it to 2000 for Clinton
final_sotu_df %>%
    filter(year >= 1901 & year == "1953a") %>%
    filter(-c(1))
    distinct(president)

# create data for plotting

pres_20_inaug1 <- final_inaug_df %>%
    filter(year > 1901 & year <= 1949)

pres_20_sotu1 <- final_sotu_df %>%
    filter(year >= 1901 & year <= 1953 | year == "1953a")

# combine

df1_plot20 <- rbind(pres_20_inaug1, pres_20_sotu1)
df1_plot20 <- df1_plot20[order(df1_plot20$year),]

rownames(df1_plot20) <- 1:nrow(df1_plot20)

# make changes to the years, so it is easy to read
for (i in 1:nrow(df1_plot20)) {
    if (i %in% c(5,10,15,20,25,30,35,44,49,60)) {
        cur_year <- df1_plot20[i, 3]
        new_year <- paste(cur_year, "a", sep="")
        df1_plot20[i, 3] <- new_year
    } else if (i %in% c(6,11,16,21,26,31,36,45,50,61)) {
        cur_year <- df1_plot20[i, 3]
        new_year <- paste(cur_year, "b", sep="")
        df1_plot20[i, 3] <- new_year
    }
}

distinct(df1_plot20, president)
df1_plot20$president_f = factor(df1_plot20$president,
                                levels=c( "Theodore Roosevelt",
                                          "William Howard Taft",
                                          "Woodrow Wilson",
                                          "Warren G. Harding",
                                          "Calvin Coolidge",
                                          "Herbert Hoover",
                                          "Franklin D. Roosevelt",
                                          "Harry S Truman"))

# plot
jpeg(
    filename="./presidential_speeches/images/20thsentiments1.jpeg",
    width = 15,
    height = 10,
    units = "in",
    res = 300)
ggplot(data = df1_plot20, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president_f) +
    labs(title= "Sentiment Comparison: 20th Century Presidents (1901-1953)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# 20th century presidents (second half)

# make it to 2000 for Clinton
final_sotu_df %>%
    filter(year >= 1901 & year <= 2000) %>%
    filter(-c(1))
distinct(president)

# create data for plotting

pres_20_inaug2 <- final_inaug_df %>%
    filter(year >= 1953 & year <= 2000)

pres_20_sotu2 <- final_sotu_df %>%
    filter(year >= 1954  & year <= 2000 | year == "1953b")

# combine

df2_plot20 <- rbind(pres_20_inaug2, pres_20_sotu2)
df2_plot20 <- df2_plot20[order(df2_plot20$year),]

rownames(df2_plot20) <- 1:nrow(df2_plot20)

# make changes to the years, so it is easy to read
for (i in 1:nrow(df2_plot20)) {
    if (i %in% c(7,18,50,55,60)) {
        cur_year <- df2_plot20[i, 3]
        new_year <- paste(cur_year, "a", sep="")
        df2_plot20[i, 3] <- new_year
    } else if (i %in% c(8,19,51,56,61)) {
        cur_year <- df2_plot20[i, 3]
        new_year <- paste(cur_year, "b", sep="")
        df2_plot20[i, 3] <- new_year
    }
}

distinct(df2_plot20, president)
df2_plot20$president_f = factor(df2_plot20$president,
                                levels=c("Dwight D. Eisenhower",
                                          "John F. Kennedy",
                                          "Lyndon B. Johnson",
                                          "Richard M. Nixon",
                                          "Jimmy Carter",
                                          "Ronald Reagan",
                                          "George Bush",
                                          "William J. Clinton"))


# plot
jpeg(
    filename="./presidential_speeches/images/20thsentiments2.jpeg",
    width = 15,
    height = 10,
    units = "in",
    res = 300)
ggplot(data = df2_plot20, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president_f) +
    labs(title= "Sentiment Comparison: 20th Century Presidents (1953-2000)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# 21st century presidents

final_sotu_df %>%
    filter(year >= 2001 & year <= 2019) %>%
    distinct(president)

# create data for plotting

pres_21_inaug <- final_inaug_df %>%
    filter(year >= 2001 & year <= 2019)

pres_21_sotu <- final_sotu_df %>%
    filter(year >= 2001 & year <= 2019)

# combine

df_plot21 <- rbind(pres_21_inaug, pres_21_sotu)
df_plot21 <- df_plot21[order(df_plot21$year),]

distinct(df_plot21, president)
df_plot21$president_f = factor(df_plot21$president,
                                levels=c("George W. Bush",
                                         "Barack Obama",
                                         "Donald J. Trump"))

# plot
jpeg(
    filename="./presidential_speeches/images/21thsentiments.jpeg",
    width = 7,
    height = 5,
    units = "in",
    res = 300)
ggplot(data = df_plot21, aes(x = year, y = sentiment, colour = president, group = 1)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ president_f) +
    labs(title= "Sentiment Comparison: 21th Century Presidents") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# generate the average sentiment plot by presidents

avg_stm_df <- rbind(final_sotu_df, final_inaug_df)
rownames(avg_stm_df) <- 1:nrow(avg_stm_df)
levels(avg_stm_df$president)[levels(avg_stm_df$president) == "James Knox Polk"] <- "James K. Polk"

new_levels <- unique(unlist(lapply(avg_stm_df$president, as.character)))

avg_stm_df$president_f = factor(avg_stm_df$president,
                                levels=new_levels)

avg_sent_pres <- avg_stm_df %>%
    select(president_f, sentiment) %>%
    group_by(president_f) %>%
    summarize(pres_mean_stm = mean(sentiment))

jpeg(
    filename="./presidential_speeches/images/avg_sentiments.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
stm_p <- ggplot(data = avg_sent_pres, aes(x = president_f, y = pres_mean_stm, group = 1)) +
    geom_line()+
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
stm_p + 
    labs(title = "Trend in the Average Sentiments by Presidents",
         x = "presidents", y = "average sentiment")
dev.off()


################################################################
# DO NOT RUN
# this section is not in the report
# Topic models before and after Wilson

inaug_bf_wilson <- inaug_speech %>%
    filter(speech_year >= 1789 & speech_year <= 1909)

inaug_aft_wilson <- inaug_speech %>%
    filter(speech_year >= 1913)

sotu_bf_wilson <- sotu_speeches %>%
    filter(speech_id >= 1 & speech_id <= 124)

sotu_aft_wilson <- sotu_speeches %>%
    filter(speech_id >= 125)

# Before Wilson

bf_wilson <- c(inaug_bf_wilson$text, sotu_bf_wilson$text)
bf_wilson_corpus <- VCorpus(VectorSource(bf_wilson))
bf_wilson_dtm <- DocumentTermMatrix(bf_wilson_corpus)

# After Wilson

aft_wilson <- c(inaug_aft_wilson$text, sotu_aft_wilson$text)
aft_wilson_corpus <- VCorpus(VectorSource(aft_wilson))
aft_wilson_dtm <- DocumentTermMatrix(aft_wilson_corpus)

remove_all_zeros <- function(dtm) {
    row_total <- apply(dtm, 1, sum)
    rv <- dtm[row_total > 0,]
    return(rv)
}

bf_wilson_dtm  <- remove_all_zeros(bf_wilson_dtm )
# check 
rows_with_zero_sum <- apply(bf_wilson_dtm , 1, sum)
print(bf_wilson_dtm [rows_with_zero_sum == 0,]$dimnames[1][[1]])

aft_wilson_dtm <- remove_all_zeros(aft_wilson_dtm)
# check 
rows_with_zero_sum <- apply(aft_wilson_dtm, 1, sum)
print(aft_wilson_dtm[rows_with_zero_sum == 0,]$dimnames[1][[1]])

get_top_terms <- function(topics) {
    top_terms <- topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    return(top_terms)
}

plot_by_topics_wilson <- function(top_terms, ttime, num_topics) {
    Title <- paste("Topics", ttime, "Wilson", sep=" ")
    Subtitle <- paste("When the number of topics is", num_topics, sep=" ")
    
    top_terms %>%
        mutate(topic = as.factor(topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = topic)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        labs(y = "beta",
             x = NULL,
             title = Title,
             subtitle = Subtitle)
}

# 10 topics - Before Wilson

before_lda <- LDA(bf_wilson_dtm , k = 10, control = list(seed = 54))
before_lda_10topics <- tidy(before_lda, matrix = "beta")
before_lda_10_perpl <- c(perplexity(before_lda, newdata = bf_wilson_dtm), 10)
print(before_lda_10_perpl)

before_lda_top_terms <- get_top_terms(before_lda_10topics)
jpeg(
    filename="./presidential_speeches/images/before_topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_wilson(before_lda_top_terms, "Before", "10")
dev.off()

# 10 topics - After Wilson

after_lda <- LDA(aft_wilson_dtm , k = 10, control = list(seed = 54))
after_lda_10topics <- tidy(after_lda, matrix = "beta")
after_lda_10_perpl <- c(perplexity(after_lda, newdata = aft_wilson_dtm), 10)
print(after_lda_10_perpl)

after_lda_top_terms <- get_top_terms(after_lda_10topics)
jpeg(
    filename="./presidential_speeches/images/after_topics.jpeg",
    width = 11,
    height = 7,
    units = "in",
    res = 300)
plot_by_topics_wilson(after_lda_top_terms, "After", "10")
dev.off()
################################################################
