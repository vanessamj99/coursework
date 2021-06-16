library(scales)
library(tidyverse)
library(knitr)

# set plot theme
theme_set(theme_bw())
ratings <- read_csv('ratings.csv',
                    col_names = c('user_id','movie_id','rating','timestamp'))


# aggregate ratings by movie, computing mean and number of ratings
# hint: use the n() function for easy counting within a group
amount_of_movies <- ratings %>% group_by(movie_id)%>% summarize(count = n(),mean = mean(rating))

# plot distribution of movie popularity (= number of ratings the movie received)
# hint: try scale_x_log10() for a logarithmic x axis
ratings %>% count(rating) %>% ggplot(aes(x = rating, y = n)) + geom_histogram(stat = "identity")

ratings %>% group_by(movie_id) %>% summarize(count = n(), mean = mean(rating)) %>%
  ggplot(aes(x = count)) + geom_histogram() + scale_x_log10() + xlab("each movie") + ylab("number of movies")



# rank movies by popularity and compute the cdf, or fraction of movies covered by the top-k moves https://speakerdeck.com/jhofman/modeling-social-data-lecture-2-introduction-to-counting?slide=30
# hint: use dplyr's rank and arrange functions, and the base R sum and cumsum functions
# store the result in a new data frame so you can use it in creating figure 2 from the paper below
popular_movies <- ratings %>% group_by(movie_id) %>% summarize(num_ratings = n())%>% arrange(desc(num_ratings)) %>% mutate(rank = row_number()) %>%
  mutate(percentage = num_ratings/sum(num_ratings), cum_sum = cumsum(percentage))

num_of_all_ratings = cumsum(popular_movies$num_ratings)

# plot the CDF of movie popularity

ggplot(popular_movies,aes(x = rank,y = cum_sum)) + geom_line() + scale_x_continuous(seq(0,15000, by = 5000))

# aggregate ratings by user, computing mean and number of ratings
by_user <- ratings %>% group_by(user_id)%>% summarize(count = n(),avg = mean(rating))
# plot distribution of user activity (= number of ratings the user made)
# hint: try a log scale here
ratings %>% count(rating) %>% ggplot(aes(x = rating, y = n)) + geom_histogram(stat = "identity")


#table that has 90th and 100th percentile movies for each user
user_sat <- inner_join(ratings,popular_movies, by = 'movie_id') %>% select(user_id,rank) %>% 
  group_by(user_id) %>% filter(rank >= quantile(rank,percent = 0.9)) %>%
  summarize(top90 = min(rank),top100 = max(rank)) %>% ungroup()


#getting user's density and 90th percentile movies
top90per <- user_sat %>% group_by(top90) %>% summarize(count = n()) %>% 
  mutate(cdftop90 = cumsum(count/sum(count)))

#getting 100th percentile and user density
top100per <- user_sat %>% group_by(top100) %>% summarize(count = n()) %>% 
  mutate(cdftop100 = cumsum(count/sum(count)))


#plotting
ggplot()+ geom_line(data = top90per,aes(top90,cdftop90,color= "90%")) + 
  geom_line(data = top100per,aes(top100,cdftop100,color= "100%"))+
  scale_color_manual(name = "Satisfaction",values = c("90%" = "blue", "100%" = "orange"))+
  xlab("Inventory Size") + ylab("Percentage of people")+ ggtitle("User Movie Satisfation")



# generate the equivalent of figure 2 of this paper:
# https://5harad.com/papers/long_tail.pdf

# Specifically, for the subset of users who rated at least 10 movies,
# produce a plot that shows the fraction of users satisfied (vertical
# axis) as a function of inventory size (horizontal axis). We will
# define "satisfied" as follows: an individual user is satisfied p% of
# the time at inventory of size k if at least p% of the movies they
# rated are contained in the top k most popular movies. As in the
# paper, produce one curve for the 100% user satisfaction level and
# another for 90%---do not, however, bother implementing the null
# model (shown in the dashed lines).
ggplot(satisfied_users,aes(x = user_id,y = satisfied)) + geom_line()
