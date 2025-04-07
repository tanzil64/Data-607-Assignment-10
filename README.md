---
title: "Data 607 Assignment 10"
author: "Md. Tanzil Ehsan"
date: "`r Sys.Date()`"
output: openintro::lab_report
---
### #Text mining and natural language processing

The goals of this week’s assignment are as follows: 1. get primary example code from chapter 2 of “Text Mining with R: A Tidy Approach” by Julia Silge and David Robinson.
2. Work with a different corpus. For this, I chose to work with a collection of works by my favorite author, Charles Dickens. I used the gutenberg project to download the  text data since the data is archived.
3. We incorporated the lougharn sentiment.
4. Find most frequent words.
5. Find most important words.


### Load the packges
```{r load-packages, message=FALSE}
library(tidyverse)
library(openintro)
library(tidyverse)
library(openintro)
library(tidytext)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(textdata)
library(wordcloud) 
library(syuzhet)  
library(tm) # text mining
library(lexicon) # sentiment lexicons
library(wordcloud) # visualization
library(irr) # inter-rater reliability for lexicons
library(textstem) # stemming and lemmatization (example purposes only)
library(syuzhet)
library(ggplot2)
library(gutenbergr) 
```

### Download, read and combine  the text Data




```{r}
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
```




```{r}

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
```




```{r}
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


```
`



```{r}
library(tidyr)

library(dplyr)
library(tidytext)
library(tidyr)

# Perform sentiment analysis using the Bing lexicon
options(dplyr.summarise.inform = FALSE)
bing <- get_sentiments("bing") %>%
  distinct(word, sentiment)
jane_austen_sentiment <- tidy_books %>%
  inner_join(bing, by = "word") %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)
tidy_books <- tidy_books %>% distinct()

```




### Tidy the Data

```{r}
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
```

```{r}
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

```


```{r}
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
```
```{r}
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
```
```{r}
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)
```


```{r}
get_sentiments("bing") %>% 
  count(sentiment)
#> # A tibble: 2 × 2
#>   sentiment     n
#>   <chr>     <int>
#> 1 negative   4781
#> 2 positive   2005
```

```{r}
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
#> # A tibble: 2,585 × 3
#>    word     sentiment     n
#>    <chr>    <chr>     <int>
#>  1 miss     negative   1855
#>  2 well     positive   1523
#>  3 good     positive   1380
#>  4 great    positive    981
#>  5 like     positive    725
#>  6 better   positive    639
#>  7 enough   positive    613
#>  8 happy    positive    534
#>  9 love     positive    495
#> 10 pleasure positive    462
#> # ℹ 2,575 more rows
```



```{r}
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
```


```{r}
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words
```


```{r}
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
```



```{r}
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
```



```{r}
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
```

```{r}
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())
```

### Plotting frequnecy with ggplot2
```{r}
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()
              
```

```{r}
# Load Loughran-McDonald sentiment lexicon
library(tidytext)
loughran_sentiments <- get_sentiments("loughran")

# Count word frequencies by sentiment using Loughran-McDonald lexicon
loughran_word_counts <- pride_prejudice %>% 
  inner_join(loughran_sentiments, by = "word") %>%  # Join by 'word'
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

# Perform sentiment analysis by binning the text into groups of 80 lines
loughran <- pride_prejudice %>% 
  inner_join(loughran_sentiments, by = "word") %>%  # Ensure proper join on 'word'
  group_by(index = linenumber %/% 80) %>%  # Group by bin of 80 lines
  summarise(sentiment = sum(as.numeric(sentiment), na.rm = TRUE)) %>%  # Sum sentiment
  mutate(method = "Loughran")  # Add method label

```
```{r}
# Combine different sentiment analysis methods including Loughran-McDonald lexicon
bind_rows(afinn, 
          bing_and_nrc,
          loughran) %>%  
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

```
---
title: "Lab Name"
author: "Author Name"
date: "`r Sys.Date()`"
output: openintro::lab_report
---
### #Text mining and natural language processing

The goals of this week’s assignment are as follows: 1. get primary example code from chapter 2 of “Text Mining with R: A Tidy Approach” by Julia Silge and David Robinson.
2. Work with a different corpus. For this, I chose to work with a collection of works by my favorite author, Charles Dickens. I used the gutenberg project to download the  text data since the data is archived.
3. We incorporated the lougharn sentiment.
4. Find most frequent words.
5. Find most important words.




### Download, read and combine  the text Data




```{r}
dickens_book1 <- readLines("https://raw.githubusercontent.com/tanzil64/Data-607-Assignment-10/refs/heads/main/A%20Christmas%20Carol.txt")
# View the first few lines of the downloaded book
# Assuming you have already loaded the text file

# View the first few lines
head(dickens_book1)
```




```{r}

dickens_book2 <- readLines("https://raw.githubusercontent.com/tanzil64/Data-607-Assignment-10/refs/heads/main/Great%20Expectations.txt")
# View the first few lines of the downloaded book
# Assuming you have already loaded the text file

# View the first few lines
head(dickens_book2)
```




```{r}
# Assuming the character vectors for book texts are properly loaded as single strings:
# `dickens_book1` and `dickens_book2` contain the full texts of each book.

# Convert `dickens_book1` and `dickens_book2` into data frames/tibbles with the correct structure
dickens_book1 <- tibble(book = "A Christmas Carol", text = paste(dickens_book1, collapse = " "))
dickens_book2 <- tibble(book = "Great Expectations", text = paste(dickens_book2, collapse = " "))

# Combine the two books into one tibble
dickens_works <- bind_rows(dickens_book1, dickens_book2)

# Check the combined tibble
glimpse(dickens_works)


```
`







### Tidy the Data

```{r}
tidy_dickens <- dickens_works %>%
  unnest_tokens(word, text) %>%       # Tokenize into words
  anti_join(stop_words, by = "word") 
```






### Counting the words


```{r}
dickens_frequency <- tidy_dickens %>%
  count(word)
```

### Plotting frequnecy with ggplot2

```{r}
library(dplyr)
library(ggplot2)

# Filter the top 7 words with the highest frequencies
dickens_top_words <- dickens_frequency %>%
  filter(n > 5) %>%
  slice_max(n, n = 7)

# Plot the top 7 words
ggplot(dickens_top_words, aes(x = reorder(word, n), y = n)) + 
  geom_col() +
  xlab("Words") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

```

### Get Sentiments



```{r}
get_sentiments("afinn")
```

```{r}
get_sentiments("bing")
```

```{r}
get_sentiments("nrc")
```

```{r}
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
head(nrc_joy)
```


Here I filtered out all of the words related to positive feelings from the nrc lexicon.

Next, I need to find the joyful words in Charles Dickens works.

nrc joy Here we take all of the words that are related to joy from Dickens works.

```{r}
nrc_joy_dickens <- tidy_dickens %>%
  inner_join(get_sentiments("nrc") %>% filter(sentiment == "joy"), by = "word") %>%
  count(word, sort = TRUE)

```





```{r}
head(nrc_joy_dickens)
```






#affrin joy This will pull out the words from Dickens works that afrinn relate as positive.
```{r}
affin_joy <- get_sentiments("afinn") %>%
  filter(value > 0)


dickens_afinn_joy <- tidy_dickens %>%
  inner_join(affin_joy, by = "word") %>%
  count(word, sort = TRUE)
```



#Bing joy Here we find positive Dickens words using the lexicon Bing.
```{r}
bing_joy <- get_sentiments("bing")%>%
  filter(sentiment == "positive")

dickens_bing_joy <- tidy_dickens %>%
  inner_join(bing_joy, by = "word") %>%
  count(word, sort = TRUE)
```


#nrc Joy Here we will use a lexicon not used in the examples of in the book.

```{r}
nrc_joy <- get_sentiments("nrc")%>%
  filter(sentiment == "positive")

dickens_nrc_joy <- tidy_dickens %>%
  inner_join(bing_joy, by = "word") %>%
  count(word, sort = TRUE)
```


#Loughran Joy Here we will use a lexicon not used in the examples of in the book.
```{r}
loughran_joy <- get_sentiments("loughran")%>%
  filter(sentiment == "positive")

dickens_loughran_joy <- tidy_dickens %>%
  inner_join(loughran_joy, by = "word")%>%
  count(word, sort = TRUE)
```




#Graphing the top words This is great way to compare how the sentiments work

```{r}
library(ggplot2)
library(dplyr)

# Keep only the top 10 words for each sentiment
graph_dickens_afinn_top10 <- dickens_afinn_joy %>%
  top_n(10, n)

graph_dickens_bing_top10 <- dickens_afinn_joy %>%
  top_n(10, n)

graph_dickens_loughran_top10 <- dickens_afinn_joy %>%
  top_n(10, n)

graph_dickens_nrc_top10 <- dickens_afinn_joy %>%
  top_n(10, n)

# Plot the graph with the top 10 words
ggplot() +
  geom_point(data = graph_dickens_afinn_top10, aes(x = word, y = n), color = "red") +
  geom_line(data = graph_dickens_afinn_top10, aes(x = word, y = n, group = 1), color = "red") +
  
  geom_point(data = graph_dickens_bing_top10, aes(x = word, y = n), color = "black") +
  geom_line(data = graph_dickens_bing_top10, aes(x = word, y = n, group = 1), color = "black") +
  
  geom_point(data = graph_dickens_loughran_top10, aes(x = word, y = n), color = "green") +
  geom_line(data = graph_dickens_loughran_top10, aes(x = word, y = n, group = 1), color = "green") +
  
  geom_point(data = graph_dickens_nrc_top10, aes(x = word, y = n), color = "blue") +
  geom_line(data = graph_dickens_nrc_top10, aes(x = word, y = n, group = 1), color = "blue") +
  
  theme_minimal() +
  labs(title = "Top 10 Words by Sentiment Score (Outliers Removed)",
       x = "Words",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

```
























#now to pull out word frequencies per book
```{r}
book_words <- tidy_dickens %>%
  count(book, word, sort = TRUE)

# Step 2: Calculate total number of words per book
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n), .groups = "drop")

# Step 3: Join total word count into the main data
book_words <- left_join(book_words, total_words, by = "book")

# View result
head(book_words)
```
#To find which words are the most important we use the tf_idf.
```{r}
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)%>%
  ungroup()
freq_by_rank
```
```{r}
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)
book_tf_idf %>%
  arrange(desc(tf_idf))
```


```{r}
# Step 1: Count words per book
word_counts <- tidy_dickens %>%
  count(book, word, sort = TRUE)

# Step 2: Apply tf-idf
dickens_tf_idf <- word_counts %>%
  bind_tf_idf(word, book, n) %>%
  arrange(desc(tf_idf))

# Step 3: View top distinctive words
head(dickens_tf_idf, 10)
```








#Conclusions

In conclusion, the same words that are most used happen to also be considered to be the most important words. I wonder how this would change if Project Gutenberg had more of Kurt vonnegut’s books. I imagine that the characters wouldn’t be the top words in regards to frequency or importance.

