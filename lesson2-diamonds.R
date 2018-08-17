library(ggplot2)
data(diamonds)
summary(diamonds)

?diamonds
diamonds$color

ggplot(aes(x = price), data = diamonds) +
  geom_histogram()

diamonds[diamonds$price < 500, ]
diamonds[diamonds$price < 250, ]
diamonds[diamonds$price >= 15000, ]


ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = 1:20)
