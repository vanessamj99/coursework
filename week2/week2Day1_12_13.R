#12.2.1 #2
library(tidyverse)
table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 %>% mutate(rate = cases /population *10000)
# Compute cases per year
table1 %>% count(year,wt = cases)

# Visualise changes over time
ggplot(table1,aes(year,cases)) + geom_line(aes(group=country),colour = "grey50") + geom_point(aes(colour = country))


#2Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#1.)Extract the number of TB cases per country per year
#2.)Extract the matching population per country per year.
#3.)Divide cases by population, and multiply by 10000.
#4.)Store back in the appropriate place.
table2 <- pivot_wider(table2,names_from = type, values_from = count) %>% mutate(rate = cases /population * 10000)


#pivot_wider(table3,)
table4a <- pivot_longer(table4a,c('1999','2000'),names_to ="year" ,values_to="cases")
table4b <- pivot_longer(table4b,c('1999','2000'),names_to ="year" ,values_to="population")
table4 <- full_join(table4a,table4b)
table4 %>% mutate(rate=cases/population*10000)

#12.3.3
#1.)
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
