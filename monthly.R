excel_file_path <- "combined2.xlsx"
data <- read_excel(excel_file_path)
head(data)
str(data)
column_names <- names(data)
column_names

X0=data[,"No_admitted_P_in_ED"]
X0=data$No_admitted_P_in_ED
hist(X0)
plot(density(X0))

X1 <- data[data$`Month` == 'January', 'No_admitted_P_in_ED', drop = TRUE]
X1 <- as.numeric(X1)
X2 <- data[data$`Month` == 'February', 'No_admitted_P_in_ED', drop = TRUE]
X2 <- as.numeric(X2)
X3 <- data[data$`Month` == 'March', 'No_admitted_P_in_ED', drop = TRUE]
X3 <- as.numeric(X3)
X4 <- data[data$`Month` == 'April', 'No_admitted_P_in_ED', drop = TRUE]
X4 <- as.numeric(X4)
X5 <- data[data$`Month` == 'May', 'No_admitted_P_in_ED', drop = TRUE]
X5 <- as.numeric(X5)
X6 <- data[data$`Month` == 'June', 'No_admitted_P_in_ED', drop = TRUE]
X6 <- as.numeric(X6)
X7 <- data[data$`Month` == 'August', 'No_admitted_P_in_ED', drop = TRUE]
X7 <- as.numeric(X7)
X8 <- data[data$`Month` == 'Spetmber', 'No_admitted_P_in_ED', drop = TRUE]
X8 <- as.numeric(X8)
X9 <- data[data$`Month` == 'October', 'No_admitted_P_in_ED', drop = TRUE]
X9 <- as.numeric(X9)
X10 <- data[data$`Month` == 'November', 'No_admitted_P_in_ED', drop = TRUE]
X10 <- as.numeric(X10)
X11 <- data[data$`Month` == 'December', 'No_admitted_P_in_ED', drop = TRUE]
X11 <- as.numeric(X11)

hist(X1,X2)


# Assuming X1 and X2 are numeric vectors
combined_data <- c(X1, X2)
hist(combined_data, main = "Histogram for X1 and X2", xlab = "Values", col = c("skyblue", "lightgreen"), border = "black", legend = c("X1", "X2"))

plot(density(X0),ylim=c(0,.04),col="red",lwd=1)
lines(density(X1), col = "blue", lwd = 3)
lines(density(X2),col="green",lwd=3)
lines(density(X3),col="black",lwd=3)
lines(density(X4),col="yellow",lwd=3)
lines(density(X5),col="purple",lwd=3)

lines(density(X6),col="orange",lwd=3)
lines(density(X7),col="pink",lwd=3)
