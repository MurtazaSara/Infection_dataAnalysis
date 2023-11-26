install.packages("dplyr")

library(dplyr)

# Assuming the data in "Day.of.the.week" is in a character format
# Create a factor to specify the order of days
df$Day.of.the.week <- factor(df$Day.of.the.week, levels = c("M", "Tu", "W", "Th", "F", "Sa", "Su"))

# Group the data by day of the week and calculate statistics
summary_df <- df %>%
  group_by(Day.of.the.week) %>%
  summarize(
    Total_Admissions = sum(No_admitted_P_in_ED),
    Average_Admissions = mean(No_admitted_P_in_ED),
    Max_Admissions = max(No_admitted_P_in_ED),
    Min_Admissions = min(No_admitted_P_in_ED)
  )

# Display the summary data
print(summary_df)
library(ggplot2)

# Create a bar chart
ggplot(summary_df, aes(x = Day.of.the.week, y = Total_Admissions, fill = Day.of.the.week)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Admissions by Day of the Week",
       x = "Day of the Week",
       y = "Total Admissions") +
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired

###########################################
library(dplyr)
library(ggplot2)

# Divide the data into weekend and weekday
summary_df <- summary_df %>%
  mutate(Day_Type = ifelse(Day.of.the.week %in% c("Sa", "Su"), "Weekend", "Weekday"))

# Create separate data frames for weekend and weekday
weekend_data <- summary_df %>% filter(Day_Type == "Weekend")
weekday_data <- summary_df %>% filter(Day_Type == "Weekday")

# Create bar charts for weekend and weekday
weekend_plot <- ggplot(weekend_data, aes(x = Day.of.the.week, y = Total_Admissions, fill = Day.of.the.week)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Admissions on the Weekend",
       x = "Day of the Week",
       y = "Total Admissions") +
  scale_fill_brewer(palette = "Set3")

weekday_plot <- ggplot(weekday_data, aes(x = Day.of.the.week, y = Total_Admissions, fill = Day.of.the.week)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Admissions on Weekdays",
       x = "Day of the Week",
       y = "Total Admissions") +
  scale_fill_brewer(palette = "Set3")

# Display the plots
print(weekend_plot)
print(weekday_plot)


X0=data[,"No_admitted_P_in_ED"]
hist(X0)
plot(density(X0))
X1=data[data$Day.of.the.week =='M',"No_admitted_P_in_ED"]
X2=data[data$Day.of.the.week =='Tu',"No_admitted_P_in_ED"]
X3=data[data$Day.of.the.week =='W',"No_admitted_P_in_ED"]
X4=data[data$Day.of.the.week =='Th',"No_admitted_P_in_ED"]
X5=data[data$Day.of.the.week =='F',"No_admitted_P_in_ED"]
X6=data[data$Day.of.the.week =='Sa',"No_admitted_P_in_ED"]
X7=data[data$Day.of.the.week =='Su',"No_admitted_P_in_ED"]


plot(density(X0),ylim=c(0,.04),col="red",lwd=1)
lines(density(X1),col="blue",lwd=3)
lines(density(X2),col="green",lwd=3)
lines(density(X3),col="black",lwd=3)
lines(density(X4),col="yellow",lwd=3)
lines(density(X5),col="purple",lwd=3)


lines(density(X6),col="orange",lwd=3)
lines(density(X7),col="pink",lwd=3)

mean(c(X6,X7))
mean(c(X1,X2,X3,X4,X5))
hist

boxplot(X1,X2,X3,X4,X5,X6,X7)          
boxplot(c(X1,X2,X3,X4,X5),c(X6,X7))

pdata = data[!is.na(data$No_of_Infection_in_ED), c("Day.of.the.week", "No_of_Infection_in_ED")]

X0=pdata[,"No_of_Infection_in_ED"]
hist(X0)
plot(density(X0))
X1=pdata[pdata$Day.of.the.week =='M',"No_of_Infection_in_ED"]
X2=pdata[pdata$Day.of.the.week =='Tu',"No_of_Infection_in_ED"]
X3=pdata[pdata$Day.of.the.week =='W',"No_of_Infection_in_ED"]
X4=pdata[pdata$Day.of.the.week =='Th',"No_of_Infection_in_ED"]
X5=pdata[pdata$Day.of.the.week =='F',"No_of_Infection_in_ED"]
X6=pdata[pdata$Day.of.the.week =='Sa',"No_of_Infection_in_ED"]
X7=pdata[pdata$Day.of.the.week =='Su',"No_of_Infection_in_ED"]

boxplot(X1,X2,X3,X4,X5,X6,X7)          
boxplot(c(X1,X2,X3,X4,X5),c(X6,X7))

pdata = data[!is.na(data$Avg_wait_time_ED), c("Day.of.the.week", "Avg_wait_time_ED")]

X0=pdata[,"Avg_wait_time_ED"]
hist(X0)
plot(density(X0))
X1=pdata[pdata$Day.of.the.week =='M',"Avg_wait_time_ED"]
X2=pdata[pdata$Day.of.the.week =='Tu',"Avg_wait_time_ED"]
X3=pdata[pdata$Day.of.the.week =='W',"Avg_wait_time_ED"]
X4=pdata[pdata$Day.of.the.week =='Th',"Avg_wait_time_ED"]
X5=pdata[pdata$Day.of.the.week =='F',"Avg_wait_time_ED"]
X6=pdata[pdata$Day.of.the.week =='Sa',"Avg_wait_time_ED"]
X7=pdata[pdata$Day.of.the.week =='Su',"Avg_wait_time_ED"]

boxplot(X1,X2,X3,X4,X5,X6,X7)          
boxplot(c(X1,X2,X3,X4,X5),c(X6,X7))



