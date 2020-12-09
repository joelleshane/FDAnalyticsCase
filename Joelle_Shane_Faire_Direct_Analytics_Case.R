# Read In Data
dataset <- read.csv("~/Documents/elevate_analytics_case_data.csv")

#Load needed Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

# some exploratory code
nrow(dataset)
length(unique(dataset$retailer_id))



# Recreating "Number of New Faire Direct Retailers Aquired" Chart
  ## checking and reformatting to dates - I included conversions on 6/16 to be consistent with 
  ## other time windows, so numbers for the last week are slightly higher
for(i in grep("_at$", colnames(dataset))){
  dataset[,i] <- as.Date(strptime(dataset[,i], "%Y-%m-%d"))
}

  ## Adding week of confirmed order and pre/post drop categories to dataset
    
    ### selecting data in target dates
data_by_confirmed_order_dates <- subset(dataset, retailer_placed_first_confirmed_order_at >= '2019-02-25' &
                                          retailer_placed_first_confirmed_order_at <=  '2019-06-16')
    
    ### creating week start dates & adding to data
week <- 1+ as.numeric(data_by_confirmed_order_dates$retailer_placed_first_confirmed_order_at - as.Date("2019-02-25")) %/% 7
data_by_confirmed_order_dates <- cbind(data_by_confirmed_order_dates, week)
sequence <- seq.Date(as.Date('2019-02-25'), as.Date('2019-06-16'), by=7)
week_start_dates <- data.frame(week = 1:16, week_start_date = sequence)
data_by_confirmed_order_dates <- left_join(data_by_confirmed_order_dates, week_start_dates, by="week")
    ### Adding pre/post drop flag
data_by_confirmed_order_dates <- data_by_confirmed_order_dates %>% mutate(Period = if_else(week_start_date < '2019-05-27', "Pre", "Post"))


  ## Summing new retailers over weeks
week_sum_confirmed_orders <- data_by_confirmed_order_dates %>% group_by(week) %>% summarise(new_retailers = length(unique(retailer_id)))
week_sum_confirmed_orders <- data.frame(week_start_date = sequence, new_retailers = week_sum_confirmed_orders$new_retailers)

  ## Plotting Retailers Week over Week with trend line 
ggplot(data = week_sum_confirmed_orders, aes(x=week_start_date, y=new_retailers)) +
  geom_bar(stat="identity", width = 5, fill = "#45D8A780") +
  labs(title="# New Faire Direct Retailers Aquired", x= "Week Start Date", y="Number of New Retailers") +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, color = "#45D8A7") +
  geom_text(aes(label=new_retailers), vjust=-0.6, size=3.5) +
  scale_x_date(date_breaks="weeks", labels = function(z) gsub("^0", "", strftime(z, "%m/%d"))) +
  theme(plot.title = element_text(color="black", size = 14, face ="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="black", size = 11, face ="bold"),
        axis.title.y = element_text(color="black", size = 11, face ="bold"),
        panel.background = element_rect(fill = "white", color = "white"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=10, face="bold", size = 7, margin = margin(t=-14)), axis.ticks.x = element_blank())

# Closer look at decline 

  ## Avg retailers acquired pre/post 5/27 WoW

    ### Adding pre/post flag
week_sum_confirmed_orders <- week_sum_confirmed_orders %>% mutate(Period = if_else(week_start_date < '2019-05-27', "Pre", "Post"))
   
    ### calculating avg retailers pre/post
pre_post_avgs <- week_sum_confirmed_orders %>% group_by(Period) %>% summarise(avg_retailers = mean(new_retailers))
    
    ### writing function for percent decline & absolute decline of metric 
perct_decline <- function(table_name, groupby, metric) {
  (({{ table_name }} %>% filter({{ groupby }} =="Post") %>% select({{ metric }})) - 
     ({{ table_name }} %>% filter({{ groupby }}=="Pre") %>% select({{ metric }}))) / 
    (table_name %>% filter({{ groupby }}=="Pre") %>% select({{ metric }}))
}

perct_decline(pre_post_avgs, Period, avg_retailers)

num_decline <- function(table_name, groupby, metric) {
  (({{ table_name }} %>% filter({{ groupby }}=="Post") %>% select({{ metric }})) - 
     ({{ table_name }} %>% filter({{ groupby }}=="Pre") %>% select({{ metric }})))
}

num_decline(pre_post_avgs, Period,avg_retailers)


# Exploring why this may have happened 

  ## Segmentation of confirmed order retailers
write_table_new_retailers_by <- function(dataset, column_name) {
  
  table_new_retailers <- {{ dataset }} %>% group_by({{ column_name }},week_start_date) %>% summarize(new_retailers = length(unique(retailer_id)))
  
  table_add_period <- table_new_retailers %>% mutate(period = if_else(week_start_date < '2019-05-27', "Pre", "Post"))
  
  result_table <<- table_add_period %>% group_by({{ column_name }}, period) %>% summarize(avg_new_retailers = mean(new_retailers))
  
}

write_table_new_retailers_by(data_by_confirmed_order_dates, normalized_referer)

write_table_new_retailers_by(data_by_confirmed_order_dates,account_owner)

write_table_new_retailers_by(data_by_confirmed_order_dates,retailer_business_type)

write_table_new_retailers_by(data_by_confirmed_order_dates,brand_stockist_count)

pre_post_avgs_retailer_gmv <- data_by_confirmed_order_dates %>% group_by(Period) %>% summarise(avg_retailer_gmv = mean(retailer_gmv))

perct_decline(pre_post_avgs_retailer_gmv, Period, avg_retailer_gmv)
num_decline(pre_post_avgs_retailer_gmv, Period, avg_retailer_gmv)

# Funnel Analysis

  ## Adding week of brand relationship created and pre/post drop categories to dataset

    ### getting target dates based on brand relationship 
data_by_click_date <- subset(dataset, brand_relationship_created_at >= '2019-02-25' & 
                 brand_relationship_created_at <= '2019-06-16')

    ### adding week to add flag for data pre and post drop
week <- 1+ as.numeric(data_by_click_date$brand_relationship_created_at - as.Date("2019-02-25")) %/% 7
data_by_click_date <- cbind(data_by_click_date, week)
data_by_click_date <- data_by_click_date %>% mutate(period_of_relationship = if_else(brand_relationship_created_at < '2019-05-27', "Pre", "Post"))
data_by_click_date <- data_by_click_date %>% mutate(period_of_conversion = if_else(retailer_placed_first_confirmed_order_at < '2019-05-27', "Pre", "Post"))
data_by_click_date <- left_join(data_by_click_date, week_start_dates, by="week")

  ## number of unique brands with email click 

brands_per_week <- data_by_click_date %>% group_by(week_start_date) %>% summarise(brands = length(unique(brand_id)))
brands_per_week <- brands_per_week %>% mutate(period_of_relationship = if_else(week_start_date < '2019-05-27', "Pre", "Post"))

pre_post_brands <- brands_per_week %>% group_by(period_of_relationship) %>% summarise(avg_brands = mean(brands))

perct_decline(pre_post_brands, period_of_relationship, avg_brands)
num_decline(pre_post_brands, period_of_relationship, avg_brands)



  ## of clicks by week

clicks_per_week <- data_by_click_date %>% group_by(period_of_relationship) %>% summarise(avg_clicks = length(brand_relationship_id)/length(unique(week)))

      ### method check
#clicks_per_week <- data_by_click_date %>% group_by(week_start_date) %>% summarise(avg_clicks = length(brand_relationship_id)/length(unique(week)))
#clicks_per_week <- clicks_per_week %>% mutate(period_of_relationship = if_else(week_start_date < '2019-05-27', "Pre", "Post"))

#pre_post_clicks <- clicks_per_week %>% group_by(period_of_relationship) %>% summarise(avg_clicks = mean(avg_clicks))

perct_decline(clicks_per_week, period_of_relationship, avg_clicks)
num_decline(clicks_per_week, period_of_relationship, avg_clicks)



  ## of clicks on emails by week

clicks_per_week_email <- data_by_click_date %>% group_by(period_of_relationship) %>% summarise(avg_clicks_email = length(which(!is.na(outgoing_email_id)))/length(unique(week)))

perct_decline(clicks_per_week_email, period_of_relationship, avg_clicks_email)
num_decline(clicks_per_week_email, period_of_relationship, avg_clicks_email)


  ## of clicks by refferer by week

write_table_clicks_by <- function(dataset, column_name) {
  
  table_new_retailers <- {{ dataset }} %>% group_by({{ column_name }},week_start_date) %>% summarise(avg_clicks = length(brand_relationship_id)/length(unique(week)))
  
  table_add_period <- table_new_retailers %>% mutate(period = if_else(week_start_date < '2019-05-27', "Pre", "Post"))
  
  result_table <<- table_add_period %>% group_by({{ column_name }}, period) %>% summarize(avg_clicks = mean(avg_clicks))
  
}

write_table_clicks_by(data_by_click_date, normalized_referer)

write_table_clicks_by(data_by_click_date, retailer_business_type)



  ## number of sign ups per week

pre_post_retailers_signup <- data_by_click_date %>% group_by(period_of_relationship) %>% summarise(avg_retailers_signup = length(which(!is.na(brand_relationship_confirmed_at)))/length(unique(week)))

perct_decline(pre_post_retailers_signup, period_of_relationship, avg_retailers_signup)
num_decline(pre_post_retailers_signup, period_of_relationship, avg_retailers_signup)


  ## click to sign up CVR WoW
pre_post_retailers_click_to_signup <- data_by_click_date %>% group_by(week_start_date) %>% summarise(retailers_click_to_signup_cvr = (length(which(!is.na(brand_relationship_confirmed_at)))/length(brand_relationship_id))*100)

    ### Plotting Sign Up CVR WoW
ggplot(data = pre_post_retailers_click_to_signup, aes(x=week_start_date, y=retailers_click_to_signup_cvr)) +
  geom_line(stat="identity", color = "#C22720") +
  labs(title = "Retailer Sign Up Conversion Rate From Click", x="Retailer SignUp CVR", y="Week Start Date") + 
  scale_x_date(date_breaks="weeks", labels = function(z) gsub("^0", "", strftime(z, "%m/%d"))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(color="black", size = 14, face ="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="black", size = 11, face ="bold"),
        axis.title.y = element_text(color="black", size = 11, face ="bold"),
        panel.background = element_rect(fill = "white", color = "white"),
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=10, face="bold", size = 7, margin = margin(t=-14)), axis.ticks.x = element_blank())


    ### Calculating decline in sign up CVR pre/post drop
pre_post_retailers_click_to_signup <- pre_post_retailers_click_to_signup %>% mutate(period_of_relationship = if_else(week_start_date < '2019-05-27', "Pre", "Post"))
pre_post_avgs_click_to_signup <- pre_post_retailers_click_to_signup %>% group_by(period_of_relationship) %>% summarise(retailers_click_to_signup_cvr = mean(retailers_click_to_signup_cvr))

perct_decline(pre_post_avgs_click_to_signup, period_of_relationship, retailers_click_to_signup_cvr)
num_decline(pre_post_avgs_click_to_signup, period_of_relationship, retailers_click_to_signup_cvr)



  ## sign up to order place CVR WoW
pre_post_retailers_signup_to_order <- data_by_click_date %>% group_by(week_start_date) %>% summarise(retailers_signup_to_order_cvr = (length(which(!is.na(retailer_placed_first_confirmed_order_at)))/length(which(!is.na(brand_relationship_confirmed_at))))*100)

    ### plotting order rate WoW
ggplot(data = pre_post_retailers_signup_to_order, aes(x=week_start_date, y=retailers_signup_to_order_cvr)) +
  geom_line(stat="identity", color = "#C22720") +
  labs(title = "Retailer Order Conversion Rate From Sign Up", x="Week Start Date", y="Retailer Order CVR") + 
  scale_x_date(date_breaks="weeks", labels = function(z) gsub("^0", "", strftime(z, "%m/%d"))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(color="black", size = 14, face ="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="black", size = 11, face ="bold"),
        axis.title.y = element_text(color="black", size = 11, face ="bold"),
        panel.background = element_rect(fill = "white", color = "white"),
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=10, face="bold", size = 7, margin = margin(t=-14)), axis.ticks.x = element_blank())


    ### Calculating pre/post order rate change 
pre_post_retailers_signup_to_order <- pre_post_retailers_signup_to_order %>% mutate(period_of_relationship = if_else(week_start_date < '2019-05-27', "Pre", "Post"))
pre_post_avg_signup_to_order <- pre_post_retailers_signup_to_order%>% group_by(period_of_relationship) %>% summarise(retailers_signup_to_order_cvr = mean(retailers_signup_to_order_cvr))

perct_decline(pre_post_avg_signup_to_order, period_of_relationship, retailers_signup_to_order_cvr)
num_decline(pre_post_avg_signup_to_order, period_of_relationship, retailers_signup_to_order_cvr)


  ## Time Between click and retailer sign up and retailer sign up to retailer order 

data_by_click_date <-data_by_click_date %>% mutate(time_from_click_to_signup = brand_relationship_confirmed_at - brand_relationship_created_at, time_from_sign_up_to_order = retailer_placed_first_confirmed_order_at - brand_relationship_created_at)


time_click_order <- data_by_click_date %>% group_by(time_from_click_to_signup) %>% summarise(retailers=length(brand_relationship_id), retailers_place_order=length(which(!is.na(retailer_placed_first_confirmed_order_at))), percent_that_order = (length(which(!is.na(retailer_placed_first_confirmed_order_at)))/length(brand_relationship_id))*100)

ggplot(data = time_click_order, aes(x=time_from_click_to_signup, y=percent_that_order)) +
  geom_line(stat="identity", color="blue", width = 0.5) +
  labs(title = "Impact of Time from Click to Signup on Retailer Order", x="Days from Retailer Click to Retailer Sign Up", y="Percent of Retailers that Place Order") +
scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(limits=c(0, 30)) +
  theme(plot.title = element_text(color="black", size = 14, face ="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="black", size = 11, face ="bold"),
        axis.title.y = element_text(color="black", size = 11, face ="bold"),
        panel.background = element_rect(color = "white"),
       axis.ticks.y = element_blank(),
        axis.text.x = element_text(face="bold", size = 7, margin = margin(t=-14)), axis.ticks.x = element_blank())



  ## Selecting Exsisting Retailers
exsisting_retailers <- data_by_click_date %>% mutate(existing = as.numeric(ifelse(brand_relationship_created_at > retailer_placed_first_confirmed_order_at, 1, 0)))
    
    ### Calculatting how many existing retailers click pre/post drop
pre_post_existing_retailers <- exsisting_retailers %>% group_by(period_of_relationship, existing) %>% summarise(avg_clicks = length(brand_relationship_id)/length(unique(week)), avg_retailers = length(unique(retailer_id))/length(unique(week)))

    ### sign up & order rates of new and existing retailers - doesn't work because we only see exsisting orders on sign up 
      





