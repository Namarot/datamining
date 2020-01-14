library(dplyr)
library(tidytext)
library(gutenbergr)
library(ggplot2)
library(forcats)
library(stringr)

# downloading texts

phil_text <- gutenberg_download(c(2162, 23428, 46423, 39257), meta_fields = "author")

phil_words <- phil_text %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

phil_words

# group by author and calculate tf-idf

plot_phil <- phil_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Marx, Karl",
                                            "Engels, Friedrich", 
                                            "Kropotkin, Petr Alekseevich, kniaz",
                                            "Goldman, Emma")))

plot_phil %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# Note that "kniaz" in "Kropotkin, Petr Alekseevich, kniaz" means Prince in Russian
# Emma Goldman's own name is very frequent in the text due to a Preface about her life
# We also see one letter words like "p", "c", "g" and "e" in Karl Marx's text, let's investigate

phil_text %>% 
  filter(str_detect(text, "\\ p\\.")) %>% 
  select(text)

phil_text %>% 
  filter(str_detect(text, "\\ c\\.")) %>% 
  select(text)

phil_text %>% 
  filter(str_detect(text, "\\ e\\.")) %>% 
  select(text)

phil_text %>% 
  filter(str_detect(text, "\\ g\\.")) %>% 
  select(text)

phil_text %>% 
  filter(str_detect(text, "\\ i\\.")) %>% 
  select(text)

# These are due to the usage of "e. g." meaning "for example", "p." and "c." for page and chapter in the footnotes, etc. 
# Let's remove them

mystopwords <- tibble(word = c("p", "c", "e", "g", "i", "_i.e", "_e.g."))

phil_words <- anti_join(phil_words, mystopwords, 
                           by = "word")

plot_phil <- phil_words %>%
  bind_tf_idf(word, author, n) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Marx, Karl",
                                            "Engels, Friedrich", 
                                            "Kropotkin, Petr Alekseevich, kniaz",
                                            "Goldman, Emma")))

ggplot(plot_phil, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()
