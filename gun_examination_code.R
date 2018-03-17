library(tidyverse)

guns <- read_csv('gun_sale_database-openrefined.csv', col_names = T)
shops <- read_csv('active_firearms_dealers.csv')
guns2 <- read_csv('gun_sale_database.csv')


#this database also may contain non-gun parts like magazines and other gun accessories. May need to split data into two sets one that has null items under barrel length one that does not have nulls in barrel length. 
#I also found through some testing that there are duplicate values in application numbers....I first detected this when I did a left join and got more observations then when I started. That will have to be dealt witjh at a later date.  I don't know why this is and will ask the state police upon getting a new version of this data. However, for the time being, I will use the distinct() operator in DPLYR to see if I can try and cut out these duplicates. 

#NOTICED THAT there are multiple values in the guns table. 
#guns_count <- guns %>%
#  group_by (`Application Number`)%>%
#  summarise(count = n()) %>%
#  arrange(desc(count))

#went back and made sure the appliocation numbers and observation values are the same before and after data cleaning. The code is noted out so it doesn't run unless needed, but saved for posterity.  
#guns <- read_csv('gun_sale_database-openrefined.csv', col_names = T)
#guns2 <- read_csv('gun_sale_database.csv')

#for both below, I found that there are 77 observations for this application number in both datasets.  
#guns_filter <- guns %>%
#  filter(`Application Number` == '2017014401')

#guns2_filter <- guns2 %>%
#  filter(`Application Number` == '2017014401')

#So now I will try and get only unique application numbers to try and cut down with my dupllicates before I do a left join, in hopes that I will get the same number of values both before and after the join. 

distinct_guns <- distinct(guns, `Application Number`, .keep_all = TRUE)
  
#ok. So now I have a distinct_guns table with 39946 obsewrvations. I will run a left join hopefully will get the same values coming out of the join. 
  
guns_and_shops <-  left_join(guns, shops, by = c("Dealer Name", "Dealer Name"))


guns_and_shops_count <- guns_and_shops %>%
  group_by (`Application Number`)%>%
  summarise(count = n()) %>%
  arrange(desc(count))





guns_count <- guns %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(jealous_contribution_type)
jealous_contribution_type[1:10,]

