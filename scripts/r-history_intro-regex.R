# Objects

a <- 7

a_text <- "When in Rome, do as Romans do"

# Functions
nchar(a_text)
gsub("Rome", "Copenhagen", a_text)

nchar(a)
gsub("Rome", "Copenhagen", a)

# Classes
a * 2
a_text * 2

class(a)

# Booleans
a > 2
startsWith("t", a_text)

# Long texts as objects
greek_text <- "The Greeks believed that the mental qualifications of their gods were of a
much higher order than those of men, but nevertheless, as we shall see,
they were not considered to be exempt from human passions, and we
frequently behold them actuated by revenge, deceit, and jealousy. They,
however, always punish the evil-doer, and visit with dire calamities any
impious mortal who dares to neglect their worship or despise their rites.
We often hear of them visiting mankind and partaking of their hospitality,
and not unfrequently both gods and goddesses {8} become attached to
mortals, with whom they unite themselves, the offspring of these unions
being called heroes or demi-gods, who were usually renowned for their great
strength and courage. But although there were so many points of resemblance
between gods and men, there remained the one great characteristic
distinction, viz., that the gods enjoyed immortality. Still, they were not
invulnerable, and we often hear of them being wounded, and suffering in
consequence such exquisite torture that they have earnestly prayed to be
deprived of their privilege of immortality."

dorian_text <- "Her guests this evening were rather tedious. The fact was, as she explained to Dorian, behind a very shabby fan, one of her married daughters had come up quite suddenly to stay with her, and, to make matters worse, had actually brought her husband with her. “I think it is most unkind of her, my dear,” she whispered. “Of course I go and stay with them every summer after I come from Homburg, but then an old woman like me must have fresh air sometimes, and besides, I really wake them up. You don’t know what an existence they lead down there. It is pure unadulterated country life. They get up early, because they have so much to do, and go to bed early, because they have so little to think about. There has not been a scandal in the neighbourhood since the time of Queen Elizabeth, and consequently they all fall asleep after dinner. You shan’t sit next either of them. You shall sit by me and amuse me."
dorian_text_l <- str_to_lower(dorian_text)

nchar(greek_text)

nchar(greek_text) > 500

# Using stringr

library(stringr)

str_to_lower(greek_text)

str_replace_all(greek_text, "god", "devil")

greek_text <- str_replace_all(greek_text, "\\n", " ")
print(greek_text)

str_detect(greek_text, "god")
str_count(greek_text, "god")


# Using regex

greek_text_l <- str_to_lower(greek_text)

str_extract_all(greek_text_l, ".{10}(god|hero).{10}")

my_texts <- c(dorian_text_l, greek_text_l)
str_extract_all(my_texts, ".{10}(god|hero).{10}")
text_extracts <- str_extract_all(my_texts, ".{10}(god|hero).{10}")

str_extract_all(greek_text_l, "\\bd\\w{1,9}\\b")
str_extract_all(greek_text_l, regex("\\bd\\w+", ignore_case = TRUE))

str_extract_all(dorian_text_l, "\\bd\\w{4,}\\b")
str_count(dorian_text_l, "\\bd\\w{4,}\\b")
str_extract_all(dorian_text_l, "\\b\\w{4,}d\\b")

str_extract_all(dorian_text_l, "\\b\\w+(r{2}|l{2})\\w+\\b")

str_detect(my_texts, "hero")

texts_subset <- str_subset(my_texts, "hero")
texts_subset

text_sentences <- unlist(str_split(greek_text_l, ",|\\."))
sent_subset <- str_subset(text_sentences, "god")

# Tidy text mining

library(tidytext)
library(gutenbergr)
library(dplyr)
library(stringr)

## Read in text from gutenberg
text_df <- gutenberg_download(22381, mirror = "http://mirrors.xmission.com/gutenberg/")

tokens_df <- text_df %>%
  unnest_tokens(word, text)

## Convert dataframe to one row per word occurence
tokens_df <- unnest_tokens(text_df, word, text)

## Read in stopwords
data(stop_words)

## Example - custom stop words
danish_stops <- c("den", "der", "en")
danish_stops_df <- as.data.frame(danish_stops)
colnames(danish_stops_df) <- c("word")

## Filter out stopwords from data
tokens_df <- anti_join(tokens_df, stop_words, by = "word")

## Adding variables
tokens_df$nchar <- nchar(tokens_df$word)
tokens_df$contains_con <- str_detect(tokens_df$word, "con")
