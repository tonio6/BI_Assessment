#################################################################################
#################             Customer Segmentation            ##################
#################   antonismantzaris@gmail.com - 14 Mar 2021   ##################
#################################################################################


#########################################
# 0 - Install & load required libraries
#########################################

rm(list=ls()) # clear workspace
packages <- c("data.table","ggplot2","plyr",'lubridate')
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(c("data.table","ggplot2","plyr",'lubridate'), library, character.only = TRUE)

#############################################
# 1 - Set working directory and load raw data
#############################################

working_directory <- 'path/to/directory'
setwd(working_directory)
orders <- as.data.table(read.csv('main_assessment/csv/orders_jan_2021.csv', na.strings = 'NULL'))
options(scipen = 99999) # prevent scientific notation

######################
# 2 - Data Preparation
######################

# Summary statistics of raw data
summary(orders)

# 2.1. Data types --------
# Convert submit_dt from character to datetime
orders$submit_dt <- as.POSIXct(strptime(orders$submit_dt, '%Y-%m-%d %H:%M:%OS'))

# Convert character variable to factor
orders$brand <- as.factor(orders$brand)
orders$cuisine_parent <- as.factor(orders$cuisine_parent)
orders$city <- as.factor(orders$city)
summary(orders)

# 2.2. Look for NULL values --------
colSums(is.na(orders))

# 2.3. Duplicates --------
# Look for duplicated rows
duplicates <- orders[duplicated(orders),]
nrow(duplicates)
# Is there a unique order per row?
length(unique(orders$order_id)) == nrow(orders)

# 2.4. Create additional features  --------
# Extract features from timestamp
orders$submit_date <- as.POSIXct(strptime(orders$submit_dt, '%Y-%m-%d'))


######################################
# 3 - Data Understanding & Exploration
######################################

# 3.1. Summary statistics of January orders
summary(orders)
# Number of users
length(unique(orders$user_id))

# 3.2. Stats on different types of cuisine
t <- table(orders$cuisine_parent)
jpeg(file="main_assessment/plots/cuisine_types.jpeg")
bp <- barplot(t, xlab = 'Cuisine', ylab = 'Number of orders', ylim = c(0,160000))
text(bp,t,labels=t,col = 'darkblue', cex=1, pos =3)
dev.off()
cuisine.stats <- orders[,list(num_orders = .N, num_users = length(unique(user_id)),
                              num_cities = length(unique(city)),
                              mean_basket = round(mean(basket,na.rm = T),2)),by=list(cuisine_parent)]
summary(cuisine.stats)
write.csv(cluster.stats.breakfast, 'main_assessment/csv/cuisine_stats.csv', row.names = F)

# 3.3. Order stats per day
day.stats <- orders[,list(num_orders = .N, num_users = length(unique(user_id)),
                              num_cities = length(unique(city)), num_cuisines = length(unique(cuisine_parent)),
                              mean_basket = mean(basket,na.rm = T)),by=list(submit_date)]
summary(day.stats)
write.csv(cluster.stats.breakfast, 'main_assessment/csv/day_stats.csv', row.names = F)
# Get day of the week
day.stats$dow_l <- wday(day.stats$submit_date, label=TRUE)
day.stats$weekend_col <- ifelse(day.stats$dow_l %in% c('Sat','Fri','Sun'),'red','black')

# Plot daily orders
jpeg(file="main_assessment/plots/daily_orders.jpeg")
ggplot(day.stats, aes(submit_date, num_orders)) +
  geom_line() + geom_point(col = day.stats$weekend_col) +
  ylab("Number of orders") + xlab("Date")
dev.off()

# Plot daily users
jpeg(file="main_assessment/plots/daily_users.jpeg")
ggplot(day.stats, aes(submit_date, num_users)) +
  geom_line() + geom_point(col = day.stats$weekend_col) +
  ylab("Number of users") + xlab("Date")
dev.off()

# 3.4. Generate frequency and order value information per user (breakfast related stats for later use)
user.stats <- orders[,list(num_orders = .N,
                           num_breakfast_order = length(cuisine_parent[cuisine_parent == 'Breakfast']),
                           num_days_order = length(unique(submit_date)),
                           num_days_breakfast_order = length(unique(submit_date[cuisine_parent == 'Breakfast'])),
                           sum_basket = sum(basket,na.rm = T),
                           median_basket = median(basket, na.rm = T),
                           avg_basket = mean(basket,na.rm = T)),by=list(user_id)]
summary(user.stats)

# Distribution of orders per user
jpeg(file="main_assessment/plots/orders_per_user_distribution.jpeg")
hist(user.stats$num_orders, breaks = 30, xlab = 'Number of orders',ylab ='Number of users')
abline(v = mean(user.stats$num_orders), col = 'red')
dev.off()

# Distribution of average basket value per user
jpeg(file="main_assessment/plots/avg_basket_value_per_user_distribution.jpeg")
hist(user.stats$avg_basket, breaks = 50, xlab = 'Avg basket value',ylab ='Number of users')
abline(v = mean(user.stats$avg_basket), col = 'red')
dev.off()

# Users with very low basket value
nrow(user.stats[user.stats$avg_basket < 1,])
nrow(user.stats[user.stats$avg_basket == 0,])

############################################
# 4 - Segmentation (with k-means clustering)
############################################

# 4.1. Elbow Method to get an indication towards number of clusters to use for clustering --------
num_clusters <- 1:15
wcss.df <- NA
for (k in num_clusters){
  wcss <- kmeans(user.stats[,c('num_orders','avg_basket')], k, nstart = 50, iter.max =  20, algorithm = 'Hartigan-Wong')$tot.withinss
  wcss.df <- rbind(wcss.df,data.frame(clusters = k, wcss = wcss))
}
wcss.df <- wcss.df[complete.cases(wcss.df),]
wcss.df$lag <- c(NA,diff(wcss.df$wcss, lag = 1))


# Plot sum of squares for different clusters (and save plot) --------
jpeg(file="main_assessment/plots/elbow_plot.jpeg")
plot(wcss.df$clusters,wcss.df$wcss, type = "b",xlab = "Number of clusters",ylab = "Total within-cluster sum of squares")
dev.off()

# 4.2. Clustering for customer segmentation (k = 6)
clusters <- kmeans(user.stats[,c('num_orders','avg_basket')], 6, nstart = 50, iter.max =  20, algorithm = 'Hartigan-Wong')
user.stats$cluster <- clusters$cluster
table(user.stats$cluster)
# save clustering output to csv
write.csv(user.stats, 'main_assessment/csv/clustering_output.csv', row.names = F)

# Visualise clusters
jpeg(file="main_assessment/plots/clusters.jpeg")
ggplot(user.stats, aes(x = avg_basket, y = num_orders, color=factor(cluster), group = cluster)) +
  geom_point() +
  ylab("Number of orders") + xlab("Average order value")
dev.off()

# 4.3. Identify target segment for campaign in new Breakfast product --------
# Generate information on users that are making Breakfast orders per cluster
cluster.stats.breakfast <- user.stats[,list(num_users = .N, num_breakfast_users = length(num_days_breakfast_order[num_days_breakfast_order > 0]),
                              num_non_breakfast_users = length(num_days_breakfast_order[num_days_breakfast_order == 0]),
                              pct_non_breakfast_users = round((length(num_days_breakfast_order[num_days_breakfast_order == 0])/.N)*100,2),
                              avg_days_breakfast_orders = mean(num_days_breakfast_order, na.rm = T)),by=list(cluster)]
# Save df to csv
write.csv(cluster.stats.breakfast, 'main_assessment/csv/cluster_stats_breakfast.csv', row.names = F)

# Target group for marketing campaign on Coffee
cluster.stats.breakfast[(cluster.stats.breakfast$pct_non_breakfast_users == min(cluster.stats.breakfast$pct_non_breakfast_users)) &
              (cluster.stats.breakfast$avg_days_breakfast_orders == max(cluster.stats.breakfast$avg_days_breakfast_orders)),]

# Number of users per number of days with breakfast orders
table(cluster = user.stats$cluster, num_days_breakfast_order = user.stats$num_days_breakfast_order)

