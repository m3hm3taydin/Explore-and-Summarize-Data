

#Third Qualitative Variable
pf <- read.csv('pseudo_facebook.tsv', sep= '\t')

#install.packages('ggplot2')
library('ggplot2')
library(ggthemes)
theme_set(theme_minimal(24)) 

ggplot(aes(x = gender, y = age), 
       data = subset(pf, !is.na(gender))) + geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) + 
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)


library('dplyr')
#chain functions together
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender, 10)

#Plotting Conditional Summaries
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

#Wide and Long Format
#Resharping data
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide, 10)

#ratio plot solution
ggplot(aes(x = age, y = female / male), data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

#Third Quantitative Variable
pf$year_joined <- floor(2014 - pf$tenure / 365)
head(pf, 10)
table(pf$year_joined)

#Cut a Variable
summary(pf$year_joined)
table(pf$year_joined)

pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))
table(pf$year_joined.bucket)
table(pf$year_joined.bucket, useNA = 'ifany')


#Plotting It All Together
table(pf$year_joined.bucket, useNA = 'ifany')

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), 
            stat = 'summary', 
            fun.y = median)

#Plot the Grand Mean
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), 
            stat = 'summary', 
            fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

#Friending Rate
with(subset(pf, tenure >= 1), summary(friend_count / tenure))

#Friendships Initiated

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean)

#Bias Variance Trade off Revisited
ggplot(aes(x = round(tenure / 7) * 7, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean)

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket))

#Introducing the Yogurt Dataset
#Histograms Revisited
yo <- read.csv('yogurt.csv')
str(yo)

yo$id <- factor(yo$id)
str(yo)


ggplot(aes(x = price), data=yo) +
         geom_bar()


summary(yo)
length(unique(yo$price))
table(yo$price)       

#Number of Purchases
yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

summary(yo$all.purchases)
#alternate way
#yo$all.purchases = yo$strawberry + yo$blueberry + yo$pina.colada + yo$plain + yo$mixed.berry

#Prices Over Time
qplot(x = all.purchases, data = yo, binwidth = 1)

ggplot(aes(x = time, y = price), data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))

#Looking at Samples of Households
set.seed(4230)
sample.ids <- sample(levels(yo$id), 16)


ggplot(aes(x = time, y = price), data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

#Scatterplot Matrices
library(GGally)
theme_set(theme_minimal(20))

#set seed
set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])

#Even More Variables
nci <- read.table('nci.tsv')

#change the column names to read nicely
colnames(nci) <- c(1:64)

#Heat Maps
#melt the data to long format
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200, ]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

#make the heatmap
ggplot(aes(y = gene, x = case, fill = value), data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))


