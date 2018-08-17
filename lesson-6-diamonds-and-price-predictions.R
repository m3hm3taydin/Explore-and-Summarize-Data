#load the data
data(diamonds)


#Scatterplot Review
ggplot(diamonds, aes(x = carat, y = price)) +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99))) +
  geom_point(fill = I('#F79429'), color = I('black'), shape = 21)


#Add on linear model
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(color = '#F79429', alpha = 1/4) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99))) 

#ggpairs Function
#lets install packages
install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')

#load libraries
library(ggplot2)
library(GGally)
library(scales)
library(memisc)

set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

#The Demand of Diamonds
plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = I('#099DD9')) +
  ggtitle('Price')
plot2 <- qplot(data = diamonds, x = price, binwidth = 0.01, fill = I('#F79420')) +
  ggtitle('Price(log 10)') +
  scale_x_log10()

library(gridExtra)
library(grid)
grid.arrange(plot1, plot2, ncol = 2)

#Scatterplot Transformation
qplot(carat, price, data = diamonds) +
  scale_y_continuous(trans = log10_trans()) +
  ggtitle('Price log 10 by carat')

#create a new function to transform carat variable
cuberoot_trans = function() trans_new('cuberoot', 
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

#use the cuberoot_trans function
ggplot(aes(carat, price), data = diamonds) +
  geom_point() +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by cube root of carat')

#Overplotting Revisited
head(sort(table(diamonds$carat), decreasing = T))
head(sort(table(diamonds$price), decreasing = T))

ggplot(aes(carat, price), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by cube root of carat')

#Price vs. Carat and Clarity
ggplot(aes(x = carat, y = price, colour = clarity), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'clarity', reverse = TRUE, 
                                                        override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by cube root of carat')

#Price vs Carat and Cut

ggplot(aes(x = carat, y = price, colour = cut), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'cut', reverse = TRUE, 
                                                        override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by cube root of cut')

#Price vs Carat and Color

ggplot(aes(x = carat, y = price, colour = color), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'color', reverse = FALSE, 
                                                        override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by cube root of color')

#Building the Linear Model for price
m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

#A Bigger, Better Data Set
