library(tidyverse)
library(stringr)

guns <- read_csv('gun_sale_database-openrefined.csv')

#First, I used Tabula to create a CSV from a PDF. Then, in an effort to reduce duplicates when joining the tables, which don't have this unique ID number that is included in the PDF....I'll ask about that when I hear from the State Police. In the meantime, I did a slight modification to this sheet. I removed one of the Atlantic Guns locations. They are both in Montgomery county, but were getting doubled, I also removed a duplicate in spelling J & S pawn had two separate spellings of the same address. And I changed the name of 1237 gun shop to .1237 to be consisitent with the guns table it is saved into a new spreadsheet. 
shops <- read_csv('mod_active_firearms_dealers_new.csv')


#this database also may contain non-gun parts like magazines and other gun accessories. May need to split data into two sets one that has null items under barrel length one that does not have nulls in barrel length. 
#I also found through some testing that there are duplicate values in application numbers....I first detected this when I did a left join and got more observations then when I started. That will have to be dealt witjh at a later date.  I don't know why this is and will ask the state police upon getting a new version of this data. However, for the time being, I will use the distinct() operator in DPLYR to see if I can try and cut out these duplicates. 

#NOTICED THAT there are multiple values in the guns table. 
guns_count <- guns %>%
  group_by (`Application Number`, `Dealer Name`, Caliber )%>%
  summarise(count = n()) %>%
   arrange(desc(count))
View(guns_count)

#went back and made sure the appliocation numbers and observation values are the same before and after data cleaning. The code is noted out so it doesn't run unless needed, but saved for posterity.  
#guns <- read_csv('gun_sale_database-openrefined.csv', col_names = T)
#guns2 <- read_csv('gun_sale_database.csv')

#for both below, I found that there are 77 observations for this application number in both datasets.  
#guns_filter <- guns %>%
#  filter(`Application Number` == '2017014401')

#guns2_filter <- guns2 %>%
#  filter(`Application Number` == '2017014401')

#So first I thought I was getting duplicates, so I decided to select all discinct records.
#distinct_guns <- distinct(guns, `Application Number`, .keep_all = TRUE)
#but after talking to the MD. State Police, I was told by Capt. Barrett that the guns were likely from orders with multiple guns. 

  
#ok. So now I know I can leave in the duplicates. So I'll join the guns with the shops on dealer name. 

guns_and_shops <-  left_join(guns, shops, by = "Dealer Name")

# Alright, with all that annoying data cleaning and joining out of the way, I can start looking at stuff, like where guns are sold in the state and how many. 

names(guns)


#let's look at the gun models 
gun_models <- guns_and_shops %>%
  #the colum we are grouping 
  group_by(Model )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_models)
gun_models[1:10,]

#Remington just declared bankriuptsy. Let's see which brand was most popular.  Wow, there's a few duplicate names for Remington, but dang, they only sold like 300 guns...But don't forget. This is just handguns mostly. 

#what makes are being sold? 
gun_make <- guns_and_shops %>%
  group_by(Make ) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_make)
gun_make[1:10,]


#where are these guns being sold?   This is a problem, because the state gave a .pdf qwithout all the gun shop names. We need to go back and make sure all the names are there. There is a code in the gun database, but not the shop database. Will follow up with the state. 
gun_sales <- guns_and_shops %>%
  group_by(`Dealer Name`, County ) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_sales)
gun_sales[1:10,]

#There's a couple dealers that have a few locations. Let's just sort it by dealer name.....not much changed. 
gun_shop_sales <- guns_and_shops %>%
  group_by(`Dealer Name`, County, `Mailing & Business Address`, City) %>%
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(gun_shop_sales)
gun_shop_sales[1:10,]

#pull this out to examine closer.
write_csv(gun_shop_sales, "gun_shop_sales.csv")

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
View(gun_caliber)
gun_caliber[1:10,]



#modeled from https://awakeningdatascientist.wordpress.com/2015/07/20/r-of-the-day-grep-and-grepl/ 
#Here I'm searching for guns that take big (ish) bullets. I'm trying to find weapons that have calibners that are used in assault-style weapons. I used the List of AR platform calibers page (https://en.wikipedia.org/wiki/List_of_AR_platform_calibers) from Wikipedia to build my list. I added both decimal and-non-decimal versions to make sure I was getting all the possible entries. 

big_bullets <- guns_and_shops %>%
  filter(grepl("5.45|545|5.56|556|5.7|6.5|65|6.8|68|7.62|762|223|.223", Caliber)) %>%
  group_by(Model, Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  View(big_bullets)
  
  
  #if there's no calibner, it's just this receiver piece. It's the only part of the gun with a serial number. And it could indicate the number of ar-15-type weapons bought in MD....556 is the ruger ar-556. 
  ar_15 <- guns_and_shops %>%
    filter(grepl("ar*|ar-15|-15|*15*|556", Model) & Caliber == "NULL" ) %>%
  group_by(Model ) %>%
  summarise(count = n()) %>%
    arrange(desc(count))
  
  ar_15 <- guns_and_shops %>%
    filter(grepl("ar*|AR*|ar-15|-15|*15*|556", Model) & Caliber == "NULL" )
  

 #expands on above query. sorts the count by dealer name and adds some info about where the sales are happening  
  ar_15_shops <- guns_and_shops %>%
    filter(grepl("ar15|ar-15|-15|15|556", Model) & Caliber == "NULL" ) %>%
    group_by(`Dealer Name`, County, `Mailing & Business Address`, City) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
    write_csv(ar_15_shops, "ar_15_shops.csv")
  
  
#Now I'm looking for 9mm guns, so I use a grepl filter to grab all the values htat have a 9 in them. And just to makje sure I'm not grabbing any other calibers, I tlel the computer, hey, ignore all the calibers that could fit in the 'assault-style' category.   And Then, just count it up. 
  
 ninemm_guns <- guns_and_shops %>%
    filter(grepl("9", Caliber, ignore.case = T)) %>%
   filter(!grepl("5.45|545|5.56|556|5.7|6.5|65|6.8|68|7.62|762", Caliber)) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  View( ninemm_guns)
  
#If i Just want to see what individual giuns were most popular I'll add back in caliber and model. That way I can check my work. 
  
  
  ninemm_guns <- guns_and_shops %>%
    filter(grepl("9", Caliber, ignore.case = T)) %>%
    filter(!grepl("5.45|545|5.56|556|5.7|6.5|65|6.8|68|7.62|762", Caliber)) %>%
    group_by(Caliber, Model) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  View( ninemm_guns)
  

  
  
#looking for a different way to evaluate caliber, I dwecided to see if there was a differnet method. This technique just uses the str_extract function to pull out all nunbers, and drop all letters. 
  
# Found this from http://stla.github.io/stlapblog/posts/Numextract.html. It extracts numbers from strings. 

  
numextract <- function(string){ 
   str_extract(string, "\\-*\\d+\\.*\\d*")
 } 
 
 
caliber_digits <- numextract(guns$Caliber)
 
caliber_digits_df <- data.frame(caliber_digits)
View(caliber_digits_df)
  
#now I'm counting it up.....And it looks like It's pretty similar. I'll probably jsut got witht he ones that have characters rto make sure I'm not making  mistakes. 

caliber_digit_counts <- caliber_digits_df %>%
  #the colum we are grouping 
  group_by(caliber_digits)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
View(caliber_digit_counts)
caliber_digit_counts[1:10,]
  


library(lubridate)
crime_data <- read_csv("violent_crime_maryland.csv")
#weird. My sorting's not working. Aha it's because they're not date fields
class(crime_data$YEAR)
#ok lert's make it a date field. 
crime_Date_new <- data.frame(as.Date(crime_data$YEAR, "%m/%d/%Y")) 
#and then we'll change the name to something reasonable. 
names(crime_Date_new)[names(crime_Date_new) == 'as.Date.crime_data.YEAR....m..d..Y..'] <- 'cleaned_date'

#OK. Now let's join them together, so we can sort better. Hoperfully this works. Because otherwise my life was for naught. 
crime_data_years <- cbind(crime_data, crime_Date_new)

#and now let's pull data dated 2015 or later, the latest year there is.....I assume there's an easier way to do that, but I already know this slightly harder way  
Latest_year <- crime_data_years %>%
  filter(cleaned_date >= as.Date("2015-01-01"))

#now we want a per capita gun sales number, because it's going to make our pvalue comparisons easier. https://www.rforexcelusers.com/calculate-percent-column-r/





#OK. I've now gotten just the 
useful_crime_data <- subset(Latest_year, select = c("JURISDICTION", "POPULATION", "MURDER","VIOLENT CRIME TOTAL","PROPERTY CRIME TOTALS"))

#couldn't get my for loop to work....maybe later I'll revisit.
automated_crime_ratios <-
for(col in names(useful_crime_data)[-3]) {
  useful_crime_data[paste0(col, '_pct')] <- (useful_crime_data[col]) / useful_crime_data$POPULATION
}

#changes baltimore to baltimore city.
gun_shop_county$County[gun_shop_county$County=='Baltimore_city'] <- 'Baltimore_City'
gun_shop_county$County[gun_shop_county$County=='Baltimore City'] <- 'Baltimore_City'
#oops. Messed it up for the join.
gun_shop_county$County[gun_shop_county$County=='Baltimore_City'] <- 'Baltimore City'

#so the two sheets had slifghtly different names, and it made the join impossible. I just quikly made them the same in Excel, beccuase there were only 24 to do. 
write_csv(gun_shop_county, "gun_shop_county1.csv")

gun_shop_county_fixed <-read_csv("gun_shop_county.csv")

#joins the population table I made to the gun shop county count table 
population_and_gun_count <- left_join(gun_shop_county_fixed  , useful_crime_data, by = c("County" = "JURISDICTION"))
#changes count to guns sold

#changes the name of row "count" to row "guns sold"
colnames(population_and_gun_count)[2] <- "guns sold"
#deletes this weird row that appeared at the end of my data and I'm too lazy to ffigure out why it appeared
#population_and_gun_count <- population_and_gun_count[-25, ]
#removes the weird column that popped up. No idea why it's there, and it's ewasier to just kill it....later changed to get rid of a column i didn't need
#augmented_crime_date <- subset(augmented_crime_date, select = -cleaned_date)

write_csv(augmented_crime_date, "crime_data_all.csv")
augmented_crime_date <- mutate(population_and_gun_count, 
            murder_per_capita = MURDER / POPULATION,
            violent_crime_per_capita = `VIOLENT CRIME TOTAL` / POPULATION,
            property_crime_per_capita = `PROPERTY CRIME TOTALS` / POPULATION,
            guns_per_capita = `guns sold`/ POPULATION
            )

hist(augmented_crime_date$`guns sold`)
boxplot(augmented_crime_date$`guns sold`)
mean(augmented_crime_date$`guns sold`)
sd(augmented_crime_date$`guns sold`)

plot(augmented_crime_date$`guns sold`, augmented_crime_date$`Median income (dollars)`)

mean(augmented_crime_date$`Median income (dollars)`)
sd(augmented_crime_date$`Median income (dollars)`)


mean(augmented_crime_date$POPULATION)
sd(augmented_crime_date$POPULATION)


mean(augmented_crime_date$`VIOLENT CRIME TOTAL`)
sd(augmented_crime_date$`VIOLENT CRIME TOTAL`)

mean(augmented_crime_date$`PROPERTY CRIME TOTALS`)
sd(augmented_crime_date$`PROPERTY CRIME TOTALS`)



hist(augmented_crime_date$guns_per_capita)

plot(augmented_crime_date$`guns sold`,augmented_crime_date$`Median income (dollars)`)


cor.test(augmented_crime_date$`guns sold`, augmented_crime_date$`Median income (dollars)`)

cor.test(augmented_crime_date$`guns sold`, augmented_crime_date$POPULATION)

cor.test(augmented_crime_date$`guns sold`, augmented_crime_date$`VIOLENT CRIME TOTAL`)

cor.test(augmented_crime_date$`guns sold`, augmented_crime_date$`PROPERTY CRIME TOTALS`)
