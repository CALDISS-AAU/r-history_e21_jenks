library(stringr)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pdftools)
library(purrr)
library(forcats)

# Read in data - simple
data_path <- file.path("~", "r-history_nov21", "jenks_lca_iii_1.pdf")
pdf_load <- pdf_text(data_path)
                       
# Read in data - via list files
data_path <- file.path("~", "r-history_nov21")
data_files <- file.path(data_path, list.files(data_path))

pdfs_load <- map(data_files, pdf_text)

# Convert to single text
pdf_text <- paste(pdf_load, collapse = "\n")

# WORD COUNTS (tidy data)
## Convert dataframe to one row per word occurence
pdf_df <- data.frame(text = pdf_load)

tokens_df <- unnest_tokens(pdf_df, word, text)

## Define stopwords
stop_words <- c("precii", "cum")
stop_words_df <- data.frame(word = stop_words)

## Filter out stopwords from data
tokens_df <- anti_join(tokens_df, stop_words_df, by = "word")

## Word counts
counts_df <- tokens_df %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## Filter out numbers (or rather strings starting with numbers)
counts_df <- counts_df %>%
  filter(str_detect(word, "^\\D"))

## Visualize counts
counts_df %>%
  slice_max(count, n = 20) %>%
  ggplot(aes(count, fct_reorder(word, count))) +
  geom_col(show.legend = FALSE)


# BIGRAMS
## Bigrams df
text_bigrams <- unnest_tokens(pdf_df, bigram, text, token = "ngrams", n = 2)

## Word in bigrams as individual columns
text_bigrams <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

## Filter stopwords
bigrams_filtered <- text_bigrams %>%
  filter(!word1 %in% stop_words_df$word) %>%
  filter(!word2 %in% stop_words_df$word) %>%
  filter(str_detect(word1, "^\\D"),
         str_detect(word2, "^\\D"))

## Bigram counts
bigram_counts <- bigrams_filtered %>% 
  group_by(word1, word2) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(bigram_counts)

## Visualize as network
library(igraph)
library(ggraph)
set.seed(4020)

bigrams_graph <- bigram_counts %>%
  filter(count > 100) %>%
  graph_from_data_frame()

bigrams_graph

ggraph(bigrams_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

## Alternate visualization with more details
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = count), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()