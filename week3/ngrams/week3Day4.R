library(here)
library(scales)
library(tidyverse)
#Add a line below using Rmarkdown's inline syntax to print the total number of lines 
#in each dataframe you've created.

#transforming the data into a form that can be made into a csv file
#tr '\s' '\t' < year_counts.tsv > year_counts.csv
#tr '\t' ',' < year_counts.tsv > year_counts.csv
year_counts <- read_tsv("year_counts.tsv", col_names = c("term", "year", "volume", "book_count_term"))
total_counts <- read_csv("total_counts.csv", col_names = c("year", "total_volume", "page_count", "book_count_total"))

#Join the raw year term counts with the total counts and divide to get a proportion of mentions for
#each term normalized by the total counts for each year.
term_with_count <- year_counts %>% left_join(total_counts,by= "year")

#proportion is the amount of times a term appears divided by all the terms in the same year
term_with_count <- term_with_count %>% mutate(prop_per_year = volume/total_volume)


## Plot the main figure 3a

#Plot the proportion of mentions for the terms "1883", "1910", and "1950" over time from 1850 to 2012,
#as in the main figure 3a of the original paper. Use the `percent` function from the `scales` package
#for a readable y axis. Each term should have a different color, it's nice if these match the original
#paper but not strictly necessary.

term_with_count %>% filter(term == 1883 |term == 1910|term==1950)%>%ggplot()+
  geom_line(aes(x=year, y = prop_per_year,color = factor(term)),size =1) +
  scale_x_continuous(breaks = seq(1875,2000,25), limits = c(1860,2000))


## Plot the main figure 3a with raw counts

#Plot the raw counts for the terms "1883", "1910", and "1950" over time from 1850 to 2012. Use the 
#`comma` function from the `scales` package for a readable y axis. The colors for each term should 
#match your last plot, and it's nice if these match the original paper but not strictly necessary.
term_with_count %>% filter(term == 1883 |term == 1910|term==1950)%>%ggplot()+
  geom_line(aes(x=year, y = volume,color = factor(term)),size =1) +
  scale_x_continuous(breaks = seq(1875,2000,25), limits = c(1860,2000))


## Plot the totals

#Plot the total counts for each year over time, from 1850 to 2012. Use the `comma` function from the 
#`scales` package for a readable y axis. There should be only one line on this plot (not three).
total_counts %>% filter(year >=1850)%>%ggplot()+
  geom_line(aes(x=year, y = total_volume))+
  scale_y_continuous(label = comma)


## Compute peak mentions

#For each year term, find the year where its proportion of mentions peaks (hits its highest value). 
#Store this in an intermediate dataframe.
peak <- term_with_count %>% filter(year >= term) %>% group_by(term)%>%
  filter(prop_per_year == max(prop_per_year)) %>% select(c("term","year","volume","prop_per_year"))

#after years
after_the_year <- term_with_count %>% filter(year >= term)

after_and_peak <- after_the_year %>% left_join(peak, by= "term", suffix = c("","peak_")) %>% 
  filter(year >= yearpeak_)



## Compute half-lifes

#Now, for each year term, find the minimum number of years it takes for the proportion of mentions 
#to decline from its peak value to half its peak value. Store this in an intermediate data frame.

half_life <- after_and_peak %>% group_by(term) %>% filter(prop_per_year < prop_per_yearpeak_/2)%>%
  summarize(half_life_ = min(year))


half_life2 <- half_life %>% left_join(peak,by="term")%>% mutate(period = half_life_-year)
## Plot the inset of figure 3a

#Plot the half-life of each term over time from 1850 to 2012. Each point should represent one year
#term, and add a line to show the trend using `geom_smooth()`.
half_life2 %>% ggplot() + geom_jitter(aes(x=term,y=period)) +geom_smooth(aes(x=term,y=period))
