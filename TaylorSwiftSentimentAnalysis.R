#Package imports 
#library(geniusR)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(tidytext)

#If you want to download lyrics from Genius using the geniusR package, uncomment these first lines. 

#List albums and artists to download from Genius 
# albums <-  tibble(
#  artist = c(
#    rep("Taylor Swift", 6)
#  ),
#  album = c(
#    "Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "Reputation"
#  )
# )

#The function genius_album pulls info for each track on each album
# album_lyrics <- albums %>% 
#   mutate(tracks = map2(artist, album, genius_album))

# Unnest the lyrics to expand 
# lyrics <- album_lyrics %>% 
#   unnest(tracks) %>%    # Expanding the lyrics 
#   arrange(desc(artist)) %>% # Arranging by artist name 
#   unnest_tokens(word, lyric) #tidying - each word is a line

#Alternatively: read in a csv as lyrics
lyrics <- read.csv(file = "TaylorSwift.csv", header = TRUE)


#Remove stop words (common words like "and" and pronouns that don't have much meaning on their own)
lyrics_nostop <- lyrics %>%
  anti_join(stop_words)

#Count words appearances per song 
song_words <- lyrics_nostop %>%
  count(album, word, sort = TRUE) %>%
  ungroup()

#Count words per album and words per song. 
#The number of tracks per album below are hardcoded - that could be done better
total_words <- song_words %>%
  group_by(album) %>%
  summarize(total = sum(n)) %>%
  add_column(tracks = c(19,16,21,15,17,15)) %>%
  mutate(words_per_song = total/tracks)

#Graph words per song
#List factors (albums) in release order
total_words$album <- factor(total_words$album, levels = c('Taylor Swift', "Fearless", "Speak Now", "Red","1989","Reputation"))

#Conclusion 1: In general TSwift has been writing wordier songs
ggplot(data = total_words, aes(x = album, y = words_per_song)) +
  geom_bar(stat = "identity")



#Word cloud with bing lexicon
lyrics_nostop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20","gray80"), max.words = 100)

#binary negative or positive word appearences per album
bing_counts <- lyrics_nostop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  left_join(song_words, by = "word") %>%
  group_by(album, sentiment) %>%
  summarize(total = sum(n.y)) 

#Total negative / positive words per album
bing_counts$album <- factor(bing_counts$album, levels = c('Taylor Swift', "Fearless", "Speak Now", "Red","1989","Reputation"))
ggplot(data = bing_counts, aes(x = album, y = total, fill = sentiment)) +
  geom_bar(stat = "identity")


#Average AFINN Score Per Song 
afinn_all <- lyrics_nostop %>%
  inner_join(get_sentiments("afinn") , by = "word") %>%
  group_by(album, track_title) %>%
  summarize(avg_sent = mean(score))


#specify factor order
afinn_all$album <- factor(afinn_all$album, levels = c('Taylor Swift', "Fearless", "Speak Now", "Red","1989","Reputation"))

#Plot 12 most negative and most positive songs
afinn_all %>%
  ungroup() %>%
  top_n(12, wt=avg_sent) %>%
  ggplot(aes(reorder(track_title, avg_sent), avg_sent, fill = album)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = TRUE) +
  ylim(0,3) +
  #facet_wrap(~album, scales = "free_y") +
  labs(y = "Most Positive", x = NULL) +
  coord_flip()

afinn_all %>%
  ungroup() %>%
  top_n(-12, wt=avg_sent) %>%
  ggplot(aes(reorder(track_title, avg_sent), avg_sent, fill = album)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = TRUE) +
  #facet_wrap(~album, scales = "free_y") +
  ylim(-3,0) +
  labs(y = "Most Negative", x = NULL) +
  coord_flip()


#box plot of AFINN sentiments by album
ggplot(data = afinn_all, aes(x = album, y = avg_sent, fill = album)) +
  geom_boxplot()  


### END PRESENTATION PLOTS ###
#below are some of the other things I looked at that didn't make it in the talk

#nrc is another lexicon in the tidytext package
#attempts to distinguish between "anger" and "fear" sentiment for example
nrc_all <- lyrics_nostop %>%
  inner_join(get_sentiments("nrc") , by = "word") %>%
  group_by(album, sentiment) %>%
  summarize(num = n())

nrc_all$album <- factor(nrc_all$album, levels = c('Taylor Swift', "Fearless", "Speak Now", "Red","1989","Reputation"))

#heat plot of sentiment by album... hard to see a clear pattern especially since these are raw word counts
nrc_all %>% 
  count(album, sentiment) %>%  
  ggplot(mapping = aes(x = album, y = sentiment)) +
  geom_tile(mapping = aes(fill = nrc_all$num))


#Word cloud with all words (including non sentiment ones)
lyrics_nostop %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words = 120))
