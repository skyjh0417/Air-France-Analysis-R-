library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel('C:\\Users\\skyjh\\OneDrive\\Desktop\\Hult\\Fall 2020\\R\\Team Project\\Air France Case Spreadsheet Supplement.xls', sheet = 'DoubleClick')
kayak <- read_excel('C:\\Users\\skyjh\\OneDrive\\Desktop\\Hult\\Fall 2020\\R\\Team Project\\Air France Case Spreadsheet Supplement.xls', sheet = 'Kayak', range = 'B8:H11')


# changing col names
colnames(df) <- c("Publisher_ID","Publisher","Keyword_ID", "Keyword","Keyword_Match_Type",
  "Campaign", "Keyword_Group","Category","Bid_Strategy","Keyword_Type",
  "Status","Search_Engine_Bid","Clicks","Total_Cost","Avg_Cost_per_Click",
  "Impressions","Click_Thru","Avg_Pos","Conversion","Total_Cost_per_Bookings",
  "Total_Revenue","Total_Cost1","Bookings")

# dropping unnecessary attributes
df$Publisher_ID <- NULL #identical to publisher
df$Keyword_ID <- NULL #identical to keyword
df$Keyword <- NULL #too many keyword categories
df$Keyword_Group <- NULL #too many keyword group categories
df$Category <- NULL # too many category groups
df$Bid_Strategy <- NULL # too many missing values
df$Keyword_Type <- NULL # too man keyword_type groups
df$Status <- NULL # not important for revenues
df$Total_Cost1 <- NULL # Identical to Click_Charges

#cleaning a data
# search engine bid must be less than avg cost per click
# if there are any search engine bid less than avg_cost_per click, change the value with the avg cost per click
df[df$Search_Engine_Bid < df$Avg_Cost_per_Click, c('Search_Engine_Bid')] <- df[df$Search_Engine_Bid < df$Avg_Cost_per_Click, c('Avg_Cost_per_Click')]

# if bookings are made, number of clicks cannot be equal to zero. Otherwise delete the observations
df <- df[!(df$Clicks == 0 & df$Bookings >= 1), ]

# conversion rate higher than 50 is one time payment
df <- df[!(df$Conversion >= 50), ]

# Click Thru rate higher than 50 is one time occurence
df <- df[!(df$Click_Thru >= 50), ]

# avg_position should start from 1. 
# avg_position values less than 1: more impressions as it is close to 1. need a conversion
df$Avg_Pos <- sapply(df$Avg_Pos, function(x){if(x < 1){(1-x) + 1}else{x}})

# creating an ROA and profit column
df$ROA <- (df$Total_Revenue - df$Total_Cost) / df$Total_Cost * 100

# creating a group_by table
revenue_in_per <- df %>%
  group_by(Publisher) %>%
  summarise(Rev_sum = sum(Total_Revenue)) %>%
  ungroup() %>%
  arrange(desc(Publisher)) %>%
  mutate(Revenue = round(Rev_sum / sum(df$Total_Revenue), 4))

revenue_in_per$label <- scales::percent(revenue_in_per$Revenue)

df %>%
  group_by(Publisher) %>%
  summarise(Rev_sum = sum(Total_Revenue))

#including Kayak
revenues <- data.frame(Publisher = c("Yahoo - US","Overture - US",
                                     "Overture - Global", "MSN - US", 
                                     "MSN - Global", "Google - US", 
                                     "Google - Global", 'Kayak'),
                       Revenue= c(871936.8, 347433.2, 430084.7,
                                  181037.2, 145134.1, 1738946.2,
                                  929549.8, 233694))


#creating a pie

ggplot(data = revenues, aes( x = '', y = Revenue, fill = Publisher)) +
  geom_bar(width = 1, stat = 'identity') + 
  geom_text(aes(x = 1.3, label = paste0(round(Revenue/sum(Revenue)*100, 2), '%')), position = position_stack(vjust = 0.5)) +
  theme_classic() + 
  theme(legend.position = 'none') +
  theme(legend.position = 'right') +
  coord_polar('y', start = 0) + 
  theme(axis.line = element_blank()) +
  theme(axis.text =  element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  labs(title = "Google-US, Google_Global, and Yahoo_US
       generate 76% of the Total Revenue")

# Click through and conversion rate per publisher
click_conversion <- df %>%
  group_by(Publisher)  %>%
  summarise(click_thru = mean(Click_Thru),
            conversion = mean(Conversion))

ggplot(data = click_conversion, aes(x = Publisher, y = click_thru)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  ylab('Avg. Click Thru')

ggplot(data = click_conversion, aes(x = Publisher, y = conversion)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  ylab('Avg. Conversion')



############
df$bid_score <- sapply(df$Search_Engine_Bid, 
                          function(x){
                            if(x < 2){0.2}
                            else if(x >= 2 & x < 4){0.4}
                            else if(x >= 4 & x < 6){0.6}
                            else if(x >= 6 & x < 8){0.8}
                            else if(x >= 8 & x < 10){1}
                            else if(x >= 10 & x < 12){1.2}
                            else if(x >= 12 & x < 14){1.4}
                            else{1.6}
                          })
df$high_bid <- sapply(df$Search_Engine_Bid, 
                      function(x){if(x >= 6){'High'}
                        else if(x > 3){'Medium'}
                        else{'Low'}
  })

df$high_bid <- factor(df$high_bid, order = T, 
                               levels = c('Low','Medium','High'))
levels(df$high_bid)

opt <-df %>% 
  group_by(Publisher, high_bid) %>% 
  summarise(Avg.Bid = mean(Search_Engine_Bid),
            Impressions = mean(Impressions), 
            Clicks = mean(Clicks),
            Bookings = mean(Bookings),
            click_thru = mean(Click_Thru),
            conversion = mean(Conversion))

ggplot(data = opt, aes(x = Publisher, y = click_thru, fill = high_bid)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Bid Expense') + 
  ylab('Avg. Click Thru') + 
  ggtitle('High Bid does not generate more Click-Thru') +
  scale_fill_manual("legend", values = c("Low" = "#CC79A7", "Medium" = "#E69F00", "High" = "#56B4E9"))

# MSN-US has 0 avg. conversion. therefore doesn't make sense to invest
# overture has the highest avg conversion, which implies a better optimization. 
# but further analysis is necessary
# Again we see a similar pattern. We are overpaying

ggplot(data = opt, aes(x = Publisher, y = conversion, fill = high_bid)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Bid Expense') + 
  ylab('Avg. Conversion') + 
  ggtitle('Low and Medium expense have better conversion rate') +
  scale_fill_manual("legend", values = c("Low" = "#CC79A7", "Medium" = "#E69F00", "High" = "#56B4E9"))

# test hypothesis, which one affect more performance score? bid?
# is lower bid category have more performance score?

###############################################
# performance score from impressions
impress_above_100 <- function(x){
  weight <- (x - 100) / x
  result <- (1 + weight) * 100
  return(result)
}

impress_below_50 <- function(x){
  return(x / 2)
}

df$Impression_score <- sapply(df$Impressions,
                              function(x){
                                if(x > 100){impress_above_100(x)}
                                else if(x > 50){x}
                                else{impress_below_50(x)}})
# performance score from click thru
df$click_weight <- sapply(df$Click_Thru, 
                          function(x){
                            if(x < 5){0.2}
                            else if(x >= 5 & x < 10){0.4}
                            else if(x >= 10 & x < 15){0.6}
                            else if(x >= 15 & x < 20){0.8}
                            else if(x >= 20 & x < 25){1}
                            else if(x >= 25 & x < 30){1.2}
                            else if(x >= 30 & x < 35){1.4}
                            else if(x >= 35 & x < 40){1.6}
                            else if(x >= 40 & x < 45){1.8}
                            else{2}
                          })

#performance score 
df$performance_score <- df$Impression_score *df$click_weight

#optimized performance
df$perf_opt <- sapply(df$performance_score, 
                      function(x){if(x >= 200){'Yes'}else{'No'}})

test <- df %>%
  group_by(high_bid) %>%
  summarise(perf = mean(performance_score))


ggplot(data = test, aes(x = high_bid, y = perf, fill = high_bid)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Bid Expense') + 
  ylab('Avg. Performance') + 
  ggtitle('Low and Medium expense have higher performance score') +
  scale_fill_manual("legend", values = c("Low" = "#CC79A7", "Medium" = "#E69F00", "High" = "#56B4E9"))


################################ geo_targeted
df$Campaign_geo <- 'No'
for(i in grep('Geo Targeted', df$Campaign)){
  df$Campaign_geo[i] <- 'Yes'
}

campaign <- df %>%
  group_by(Campaign_geo, Publisher) %>%
  summarise(click_thru = mean(Click_Thru),
            conversion = mean(Conversion),
            count = n())

ggplot(campaign, aes(x = Publisher, y = click_thru, fill = Campaign_geo)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Geo Targeted CMP') + 
  ylab('Avg. Click Thru') +
  xlim(c('Google - US','Yahoo - US'))
  

ggplot(campaign, aes(x = Publisher, y = conversion, fill = Campaign_geo)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Geo Targeted CMP') + 
  ylab('Avg. Conversion') +
  xlim(c('Google - US','Yahoo - US'))

# why would geo targeted generate more click-thru & conversion
# lower avg_pos? bid?
mask1 <- df$Publisher == 'Google - US'
mask2 <- df$Publisher == 'Yahoo - US'

google_yahoo <- df[df$Publisher == 'Google - US' | df$Publisher == 'Yahoo - US', ]

cmp <- google_yahoo %>%
  group_by(Publisher, Campaign_geo, Keyword_Match_Type) %>%
  summarise(Position = mean(Avg_Pos),
            Bid = mean(Search_Engine_Bid),
            click_thru = mean(Click_Thru),
            conversion = mean(Conversion))


# different user behavior
# Google and Yahoo: cannot compare keyword match types because there is a big difference in the volumn per types
# MSN everything has broad key types

user <- df %>%
  group_by(Publisher, Keyword_Match_Type) %>%
  summarise(count = n(),
            click_thru = mean(Click_Thru),
            conversion = mean(Conversion),
            revenue = sum(Total_Revenue))

ggplot(user, aes(x = Publisher, y = count, fill = Keyword_Match_Type)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Keyword Type') + 
  ylab('Frequency') +
  ylim(0,500) +
  xlim(c('Overture - Global','Overture - US')) +
  scale_fill_discrete(breaks = c("Advanced", "Standard"))
  

ggplot(user, aes(x = Publisher, y = click_thru, fill = Keyword_Match_Type)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Keyword Type') + 
  ylab('Avg. click Thru') +
  ylim(0,4) +
  xlim(c('Overture - Global','Overture - US')) +
  scale_fill_discrete(breaks = c("Advanced", "Standard"))

ggplot(user, aes(x = Publisher, y = conversion, fill = Keyword_Match_Type)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = 'Keyword Type') + 
  ylab('Avg. Conversion') +
  ylim(0,0.4) + 
  xlim(c('Overture - Global','Overture - US')) +
  scale_fill_discrete(breaks = c("Advanced", "Standard"))
