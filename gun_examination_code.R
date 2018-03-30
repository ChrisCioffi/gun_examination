library(tidyverse)
library(stringr)

guns <- read_csv('gun_sale_database-openrefined.csv')

#First, I used Tabula to create a CSV from a PDF. Then, in an effort to reduce duplicates when joining the tables, which don't have this unique ID number that is included in the PDF....I'll ask about that when I hear from the State Police. In the meantime, I did a slight modification to this sheet. I removed one of the Atlantic Guns locations. They are both in Montgomery county, but were getting doubled, I also removed a duplicate in spelling J & S pawn had two separate spellings of the same address. And I changed the name of 1237 gun shop to .1237 to be consisitent with the guns table it is saved into a new spreadsheet. 
shops <- read_csv('mod_active_firearms_dealers.csv')


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

?left_join
  
#ok. So now I have a distinct_guns table with 39946 obsewrvations. I will run a left join hopefully will get the same values coming out of the join. And it does work. 

guns_and_shops <-  left_join(distinct_guns, shops, by = "Dealer Name")

# Alright, with all that annoying data cleaning out of the way, I can start looking at stuff, like where guns are sold in the state and how many. 

names(guns)

#let's look at the gun models 
gun_models <- guns_and_shops %>%
  #the colum we are grouping 
  group_by(Model, Make )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_models)
gun_models[1:10,]

#where are these guns being sold?   This is a problem, because the state gave a .pdf qwithout all the gun shop names. We need to go back and make sure all the names are there. There is a code in the gun database, but not the shop database. Will follow up with the state. 
gun_sales <- guns_and_shops %>%
  group_by(`Dealer Name`, County ) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_sales)
gun_sales[1:10,]

#This groups the counties where these gun shops are from. Hopefully new informaiton will enrich this query, whihch counts the number of sales per county. 

gun_shop_county <- guns_and_shops %>%
  group_by(County ) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_shop_county)
gun_shop_county[1:10,]


#what caliber weapons are being sold?
gun_caliber <- guns_and_shops %>%
  group_by( Caliber) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_sales_caliber)
gun_caliber[1:10,]

#what makes are being sold? 
gun_make <- guns_and_shops %>%
  group_by(Make ) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_make)
gun_make[1:10,]

#modeled from https://awakeningdatascientist.wordpress.com/2015/07/20/r-of-the-day-grep-and-grepl/ 
#Here I'm searching for guns that take big (ish) bullets. I'm trying to find weapons that have calibners that are used in assault-style weapons. I used the List of AR platform calibers page (https://en.wikipedia.org/wiki/List_of_AR_platform_calibers) from Wikipedia to build my list. I added both decimal and-non-decimal versions to make sure I was getting all the possible entries. 

big_bullets <- guns_and_shops %>%
  filter(grepl("5.45|545|5.56|556|5.7|6.5|65|6.8|68|7.62|762|223|.223", Caliber)) %>%
  group_by(Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  View(big_bullets)
  
  
  
#Now I'm looking for 9mm guns, so I use a grepl filter to grab all the values htat have a 9 in them. And just to makje sure I'm not grabbing any other calibers, I tlel the computer, hey, ignore all the calibers that could fit in the 'assault-style' category.   
  
 ninemm_guns <- guns_and_shops %>%
    filter(grepl("9", Caliber, ignore.case = T)) %>%
   filter(!grepl("5.45|545|5.56|556|5.7|6.5|65|6.8|68|7.62|762", Caliber)) %>%
    group_by(Caliber, Model) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  View( ninemm_guns)
  
  
  

  
  