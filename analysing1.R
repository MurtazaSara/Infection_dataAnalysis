
excel_file_path <- "combined2.xlsx"
data <- read_excel(excel_file_path)
head(data)
str(data)
column_names <- names(data)
column_names

############################

admitted_p=data[,"No_admitted_P_in_ED"]
admitted_p=data$No_admitted_P_in_ED
hist(admitted_p)
plot(density((admitted_p)))

############################

Lwbs=data[,"LWBS"]
Lwbs=data$LWBS
hist(Lwbs)
plot(density(Lwbs))

############################

plot(data$No_of_Infection_in_ED, type="l")
plot(smooth(pdata$No_of_Infection_in_ED),type="l")
summary(data$No_of_Infection_in_ED)

############################

No_of_Infection_in_ED=data[,"No_of_Infection_in_ED"]
No_of_Infection_in_ED=data$No_of_Infection_in_ED
hist(No_of_Infection_in_ED)

pdata = data[!is.na(data$No_of_Infection_in_ED),]
plot(density(pdata$No_of_Infection_in_ED))

############################

summary(data$`Average wait time`)
hist(data$`Average wait time`)
plot(data$`Average wait time`)
plot(density('Average wait time'))

avg_wait_time <- data$"Average wait time"
avg_wait_time <- avg_wait_time[!is.na(avg_wait_time)]
density_values <- density(avg_wait_time)
plot(density(avg_wait_time))

hist(avg_wait_time, breaks = seq(0,5,0.1))


avg_waittime=data[,'Average wait time']
avg_waittime=data$Average wait time
hist(avg_waittime)
plot(density((avg_waittime)))

############################

High_volume=data[,'High volume hour']
High_volume=data$`High volume hour`
hist(High_volume)
plot(density((High_volume)))

############################

avg_LOS=data[,'avg LOS in hours']
avg_LOS=data$`avg LOS in hours`
hist(avg_LOS)
plot(density((avg_LOS)))

#################################

X0=data[,"No_admitted_P_in_ED"]
X0=data$No_admitted_P_in_ED
hist(X0)
plot(density(X0))

X1 <- data[data$`Day of the week` == 'M', 'No_admitted_P_in_ED', drop = TRUE]
X1 <- as.numeric(X1)
X2 <- data[data$`Day of the week` == 'Tu', 'No_admitted_P_in_ED', drop = TRUE]
X2 <- as.numeric(X2)
X3 <- data[data$`Day of the week` == 'W', 'No_admitted_P_in_ED', drop = TRUE]
X3 <- as.numeric(X3)
X4 <- data[data$`Day of the week` == 'Th', 'No_admitted_P_in_ED', drop = TRUE]
X4 <- as.numeric(X4)
X5 <- data[data$`Day of the week` == 'F', 'No_admitted_P_in_ED', drop = TRUE]
X5 <- as.numeric(X5)
X6 <- data[data$`Day of the week` == 'Sa', 'No_admitted_P_in_ED', drop = TRUE]
X6 <- as.numeric(X6)
X7 <- data[data$`Day of the week` == 'Su', 'No_admitted_P_in_ED', drop = TRUE]
X7 <- as.numeric(X7)

plot(density(X0),ylim=c(0,.04),col="red",lwd=1)
lines(density(X1), col = "blue", lwd = 3)
lines(density(X2),col="green",lwd=3)
lines(density(X3),col="black",lwd=3)
lines(density(X4),col="yellow",lwd=3)
lines(density(X5),col="purple",lwd=3)

lines(density(X6),col="orange",lwd=3)
lines(density(X7),col="pink",lwd=3)

legend("topleft", legend = c("X0", "M", "Tu", "W", "Th", "F", "Sa", "Su"),
       col = c("red", "blue", "green", "black", "yellow", "purple", "orange", "pink"),
       lwd = c(1, 3, 3, 3, 3, 3, 3, 3))

mean(c(X6,X7))
mean(c(X1,X2,X3,X4,X5))

boxplot(X1,X2,X3,X4,X5,X6,X7)          
boxplot(c(X1,X2,X3,X4,X5),c(X6,X7))

weekday_data <- c(X1, X2, X3, X4, X5)
weekend_data <- c(X6, X7)
boxplot(list(Weekdays = weekday_data, Weekends = weekend_data), names = c("Weekdays", "Weekends"))


############################## LWBS vs Avg wait time ###########
install.packages("hexbin")

library(hexbin)

Lwbs <- as.numeric(as.character(data$LWBS))
avg_waittime <- as.numeric(as.character(data$`Average wait time`))

valid_values <- !is.na(Lwbs) & !is.infinite(Lwbs) & !is.na(avg_waittime) & !is.infinite(avg_waittime)

hexbinplot(avg_waittime[valid_values] ~ Lwbs[valid_values], main = "Hexbin Plot",
           xlab = "LWBS", ylab = "Average wait time", colramp = heat.colors)

######################### LWBS vs Avg wait time ###########

Lwbs <- as.numeric(as.character(data$LWBS))
avg_waittime <- as.numeric(as.character(data$`Average wait time`))

valid_values <- !is.na(Lwbs) & !is.infinite(Lwbs) & !is.na(avg_waittime) & !is.infinite(avg_waittime)

cor_matrix <- cor(data[valid_values, c("LWBS", "Average wait time")])

print(cor_matrix)

######################## admission vs high volume #####################
library(hexbin)

admitted_p <- as.numeric(as.character(data$No_admitted_P_in_ED))
High_volume <- as.numeric(as.character(data$`High volume hour`))

valid_values_admitted_p <- !is.na(admitted_p) & !is.infinite(admitted_p)
valid_values_High_volume <- !is.na(High_volume) & !is.infinite(High_volume)

hexbinplot(High_volume[valid_values_High_volume] ~ admitted_p[valid_values_admitted_p], 
           main = "Hexbin Plot of No_admitted_P_in_ED vs. High volume hour",
           xlab = "No_admitted_P_in_ED", ylab = "High volume hour", colramp = heat.colors)

######################## admission vs high volume scatterplot ##########

plot(admitted_p[valid_values_admitted_p], High_volume[valid_values_High_volume],
     main = "Scatterplot of No_admitted_P_in_ED vs. High volume hour",
     xlab = "No_admitted_P_in_ED", ylab = "High volume hour", col = "blue", pch = 16)

######################## admission vs avg wait time  ##########

# Assuming admitted_p and High_volume are numeric vectors
avg_waittime <- as.numeric(as.character(data$`Average wait time`))
admitted_p <- as.numeric(as.character(data$No_admitted_P_in_ED))

# Filter out missing or infinite values
valid_values_avg_waittime <- !is.na(avg_waittime) & !is.infinite(avg_waittime)
valid_values_admitted_p <- !is.na(admitted_p) & !is.infinite(admitted_p)

# Create a scatterplot
plot(avg_waittime[valid_values_avg_waittime], admitted_p[valid_values_admitted_p],
     main = "Scatterplot of Average wait time vs. No_admitted_P_in_ED",
     xlab = "Average wait time", ylab = "No_admitted_P_in_ED", col = "blue", pch = 16)
