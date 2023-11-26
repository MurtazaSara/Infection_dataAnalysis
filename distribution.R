library(readxl)

excel_file_path <- "combined2.xlsx"
data <- read_excel(excel_file_path)
head(data)
str(data)
column_names <- names(data)
column_names


X0=data[,"LWBS"]
X0=data$LWBS
hist(X0)
plot(density(X0))

summary(data$`High volume hour`)
hist(data$`High volume hour`)
data[data$`High volume hour`==24,]
data[data$`High volume hour`==0,]

summary(data$Avg_wait_time_ED)
plot(density(data$Avg_wait_time_ED))
pdata = data[!is.na(data$Avg_wait_time_ED),]

plot(density(pdata$Avg_wait_time_ED))


# Two m1, s1, m2, s2
# P = Prob of being part1
P = nrow(part1)/nrow(pdata)
P


part1 = pdata[pdata$Avg_wait_time_ED <= 7, ]
part2 = pdata[pdata$Avg_wait_time_ED > 7, ]

plot(density(part1$Avg_wait_time_ED))
plot(density(part2$Avg_wait_time_ED))

nrow(part1)
nrow(part2)


# Non-zero wait times
X = pdata$Avg_wait_time_ED
hist(X)
plot(density(X))

X = X[X > 0]
plot(density(X))
hist(X)

# Model part1
X = part1$Avg_wait_time_ED
plot(density(X))
hist(X)


# Model part 2
X = part2$Avg_wait_time_ED
plot(density(X))
summary(X)
mu = mean(X)
sigma = sd(X)

X = sort(X)
Y = dnorm(X, mean=mu, sd=sigma)
plot(X, Y)

plot(density(X), col = 'blue')
lines(X, Y, col = 'red')
