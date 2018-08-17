#lesson 3 two variable

pf <- read.csv('pseudo_facebook.tsv', sep= '\t')

#install.packages('ggplot2')
library('ggplot2')
library(ggthemes)
theme_set(theme_minimal(24)) 


#scatterplot
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point()

#ggplot syntax between age 13 & 90
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point() + 
  xlim(13, 90)

summary(pf$age)

#overplotting -> convert 20 points to 1 point
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20) + 
  xlim(13, 90)

#overplotting -> convert 20 points to 1 point --> with jitter
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_jitter(alpha = 1/20) + 
  xlim(13, 90)

#coord_trans
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20) + xlim(13,90) +
  coord_trans(y = 'sqrt')

#Alpha and Jitter
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0)) +
  coord_trans(y = 'sqrt')


#Conditional Means
install.packages('dplyr')
library(dplyr)

#filter, group_by, mutate, arrange
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups, 
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
head(pf.fc_by_age)

#for ordering on age
pf.fc_by_age <- arrange(pf.fc_by_age, age)
head(pf.fc_by_age)

#plot new dataset
ggplot(aes(age, friend_count_mean), data = pf.fc_by_age) + geom_point()

ggplot(aes(age, friend_count_mean), data = pf.fc_by_age) + geom_line()


#overlaying summaries with raw data
ggplot(aes(x = age, y = friend_count), data = pf) +
  xlim(13, 90) +
  geom_point(alpha = 0.05, position = position_jitter(h = 0), color = 'orange') +
  coord_trans(y = 'sqrt') + 
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')

#coord catesian
ggplot(aes(x = age, y = friend_count), data = pf) +
  coord_cartesian(xlim = c(13, 70), ylim = c(0, 1000)) +
  geom_point(alpha = 0.05, position = position_jitter(h = 0), color = 'orange') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')

#Correlation
cor.test(pf$age, pf$friend_count, method = 'pearson')
with(pf, cor.test(age, friend_count, method = 'pearson'))

#Correlation on subsets
with(subset(pf, age >= 70), cor.test(age, friend_count))
#default method is pearson so we dont need to say method = 'pearson

#Create scatterplots
ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')  

cor.test(pf$www_likes_received, pf$likes_received)

#More caution with correlation
library(alr3)
data("Mitchell")
?Mitchell
head(Mitchell)

ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() 
cor.test(Mitchell$Month, Mitchell$Temp)

#Making sens of data
range(Mitchell$Month)
ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 203, 12))

#Understanding Noise : Age to Age Months
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()
head(pf.fc_by_age, 10)
pf.fc_by_age[17:19,]

#Create an age_with_months variable
pf$age_with_months <- with(pf, age + (1 - dob_month / 12))
head(pf,10)

#age_with_months means
library(dplyr)
age_with_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_months <- summarise(age_with_months_groups, 
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())
pf.fc_by_age_months <- arrange(pf.fc_by_age_months, age_with_months)
head(pf.fc_by_age_months)

#plot the age_with_months variable
ggplot(aes(x = age_with_months, y = friend_count_mean), 
       data = subset(pf.fc_by_age, age_with_months < 71)) +
  geom_line()

#Smoothing conditional meaning
p1 <- ggplot(aes(x = age, y = friend_count_mean), 
       data = subset(pf.fc_by_age, age < 71)) +
  geom_line() +
  geom_smooth()

p2 <- ggplot(aes(x = age_with_months, y = friend_count_mean), 
             data = subset(pf.fc_by_age_months, age_with_months < 71)) +
  geom_line() +
  geom_smooth()

p3 <- ggplot(aes(x = round(age / 5) * 5, y = friend_count), 
             data = subset(pf, age < 71)) +
  geom_line(stat = 'summary', fun.y = mean)


library(gridExtra)

grid.arrange(p2, p1, p3, ncol = 1)


