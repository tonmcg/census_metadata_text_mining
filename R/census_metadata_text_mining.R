packages <- c("tidytext", "jsonlite", "tidyverse", "widyr", "igraph", "ggraph", "AzureML")

install.packages(packages)

library(jsonlite)
metadata <- fromJSON("https://api.census.gov/data.json")
names(metadata$dataset)

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)


## DATA PROCESSING

library(dplyr)

census_title <- data_frame(id = metadata$dataset$identifier,
                         title = metadata$dataset$title)
census_title

census_desc <- data_frame(id = metadata$dataset$identifier,
                        desc = metadata$dataset$description)

census_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)
library(tidyverse)

census_keyword <- data_frame(
  id = metadata$dataset$identifier,
  keyword = metadata$dataset$keyword) %>% 
  filter(!map_lgl(keyword, is.null)) %>%
  unnest(keyword)


census_keyword

library(tidytext)

census_title <- census_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words,
            by = "word"
  )

census_desc <- census_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words,
            by ="word"
  )

# create a list of user-defined stop words
extra_stopwords <- data_frame(word = c(as.character(1:10),
                                    "u.s.", "census", "bureau"))

# remove those extra stop words from title and description
census_title <- census_title %>%
  anti_join(extra_stopwords,
            by = "word"
  )

census_desc <- census_desc %>%
  anti_join(extra_stopwords,
            by = "word"
  )

#What are the most common keywords?
census_keyword %>%
  group_by(keyword) %>%
  count(sort = TRUE)

# make all keywords case sensitive to highlight duplicate words
## TODO: IS THERE A DIFFERENCE BETWEEN MAKING ALL KEYWORDS UPPERCASE V LOWERCASE?
census_keyword <- census_keyword %>%
  mutate(keyword = toupper(keyword))

## WORD CO-OCCURANCES AND CORRELATIONS
## Examine which words commonly occur together in titles, keywords, and descriptions

# Network of Descriptions and Title Words
# Use pairwise_count() to see how many times a pair of words occur together 
# in the title or description fields

library(widyr)

title_word_pairs <- census_title %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs

desc_word_pairs <- census_desc %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

desc_word_pairs

library(ggplot2)
library(igraph)
library(ggraph)

# plot network of co-occuring words for 'title' field
set.seed(1234)
title_word_pairs %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# plot network of co-occuring words for 'description' field
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Network of Keywords
## See which keywords commonly occur together in the same dataset

keyword_pairs <- census_keyword %>%
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

set.seed(1234)
keyword_pairs %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# find the correlations among keywords
keyword_cors <- census_keyword %>%
  group_by(keyword) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors

