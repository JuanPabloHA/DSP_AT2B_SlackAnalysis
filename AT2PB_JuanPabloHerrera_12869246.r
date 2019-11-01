# Assessment 2B
# MDSI Slack Analysis
# Juan Pablo Herrera - 12869246
# UTS - MDSI

# 4. Results - Slack Analysis
# On the present script you can find the code to produce the same visualisations as

library('ggplot2')
library('dplyr')

# Working directory 
setwd('/Users/juanpablo/OneDrive/UTS/DSP/AT2/PB/')

# Load CSV files
VisualisationDF <- read.csv('VisualisationDF.csv')
DateChannel <- read.csv('DateChannel.csv')

# 4.2 Data Visualisation
# 4.2.1 Evolution of Slack messagin over time
# data frame has to be organised by date first
VisualisationDF$date <- as.Date(VisualisationDF$date)

ggplot(VisualisationDF, aes(x = date, y = count)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette="Set1") +
    labs(x = 'Date', y = 'Message count', title = 'Evolution of Slack messaging over time') +
    scale_x_date(date_breaks = "1 month", date_labels = "%y/%m", element_text(angle=90))
ggsave('P421.png', width = 10, height = 5, units = c('in'))


# 4.2.2 Most popular day for messaging
# Let's organise the days of the week 
VisualisationDF$weekday <- factor(VisualisationDF$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday' ))

ggplot(VisualisationDF, aes(x = weekday, y = count, color = weekday)) +
    geom_boxplot() +
    geom_jitter() +
    theme_minimal() +
    theme(legend.position="none") +
    scale_y_continuous(breaks=seq(0,600,60)) +
    scale_color_brewer(palette="Set1") +
    labs(x = 'Weekday', y = 'Message count', title ='When are the MDSI students messaging?')
ggsave('P422.png', width = 10, height = 5, units = c('in'))

# Limited range plot
ggplot(VisualisationDF, aes(x = weekday, y = count, color = weekday)) +
    geom_boxplot() +
    geom_jitter() +
    theme_minimal() +
    theme(legend.position="none") +
    scale_y_continuous(breaks=seq(0,100,20)) +
    scale_color_brewer(palette="Set1") +
    labs(x = 'Weekday', y = 'Message count', title ='When are the MDSI students messaging?') +
    ylim(0, 100)
ggsave('P422b.png', width = 10, height = 5, units = c('in'))

# 4.2.3 Most popular chanenels over time 
DateChannel$date <- as.Date(DateChannel$date, format = '%Y-%m-%d')

PChannels <- c('dev_r', 'dam-assign-3', 'mdsi_announcements', 'spring_stds_assign2', 'mdsi_stds_proj')
DateChannel[DateChannel$channel_name %in% PChannels, ] %>% ggplot(aes(x = date, y = count, group = channel_name,color = channel_name)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette="Set1") +
    labs(x = 'Date', y = 'Message count', title = 'Evolution of most popular MDSI slack channels') +
    scale_x_date(date_breaks = "1 month", date_labels = "%y/%m", element_text(angle=90))
ggsave('P423.png', width = 10, height = 5, units = c('in'))
