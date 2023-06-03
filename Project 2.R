#removing the environment variables and including the library
rm(list = ls())
library(ggplot2)


#checking the version of R
R.version.string


#reading the dataset
data <- read.csv("D:/TU Dortmund/Semesters/Summer Semester 2022/ICS/Project 2/rent_index_99.csv", header = T)


#calculating the net rent per square meter
data$rentsqm <- data$net.rent/data$living.area


# print the head of the file
head(data)

####Descriptive Statistics------------------------------------------------------

#Data Type
str(data)


#missing value check
table(is.na(data))


#location wise data summary
avgloc <- data[which(data$quality.of.location == '1'),]
summary(avgloc$rentsqm)

goodloc <- data[which(data$quality.of.location == '2'),]
summary(goodloc$rentsqm)

toploc <- data[which(data$quality.of.location == '3'),]
summary(toploc$rentsqm)


#check total sample size along with sample size for each of the three locations
nrow(data)
nrow(avgloc)
nrow(goodloc)
nrow(toploc)


#construction year
min(data$construction.year)
max(data$construction.year)


#setting the ggplot base
par(mfrow=c(1,1))
a <- ggplot(data = data, aes(x = rentsqm))



#histogram check for each of the three locations
ggplot(data = avgloc, aes(x = rentsqm)) + 
  geom_histogram(bins = 30, color = 'steelblue', fill = 'lightblue',
                 alpha=0.5, position="identity") +
  xlab("Rent per sqm") + 
  ylab("Frequency") +
  theme_grey(base_size = 20)

ggplot(data = goodloc, aes(x = rentsqm)) + 
  geom_histogram(bins = 30, color = 'steelblue', fill = 'lightblue', 
                 alpha=0.5, position="identity") +
  xlab("Rent per sqm") + 
  ylab("Frequency") +
  theme_grey(base_size = 20)

ggplot(data = toploc, aes(x = rentsqm)) + 
  geom_histogram(bins = 30, color = 'steelblue', fill = 'lightblue',
                 alpha=0.5, position="identity") +
  xlab("Rent per sqm") + 
  ylab("Frequency") +
  theme_grey(base_size = 20)


#qqplot to check for normal distribution of rentsqm
qqnorm(avgloc$rentsqm, pch = 1, frame = FALSE, ylab = 'Rent per sqm', 
       main = 'Avg location')
qqline(avgloc$rentsqm, col = "red", lwd = 2)

qqnorm(goodloc$rentsqm, pch = 1, frame = FALSE, ylab = 'Rent per sqm', 
       main = 'Good location')
qqline(goodloc$rentsqm, col = "red", lwd = 2)

qqnorm(toploc$rentsqm, pch = 1, frame = FALSE, ylab = 'Rent per sqm',
       main = 'Top location')
qqline(toploc$rentsqm, col = "red", lwd = 2)



#boxplot for homogeneity of variance check
data$facloc <- as.factor(data$quality.of.location)   

ggplot(data = data, aes(x = facloc, y = rentsqm, 
                        fill = factor(quality.of.location))) +
  geom_boxplot() +
  xlab("Quality of location") + 
  ylab("Rent per sqm") +
  theme(legend.position="none") +
  labs(subtitle = '1: Average location,  2: Good location and 3: Top location') +
  theme(axis.text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) 


#computing kruskal walis test
res.kruskal <- kruskal.test(rentsqm ~ quality.of.location, data = data)
res.kruskal


#multiple pairwise comparison between groups
before_corr <- pairwise.wilcox.test(data$rentsqm, data$quality.of.location,
                                    p.adjust.method = "none")

before_corr

####Bonferroni Adjustment
after_corr <- pairwise.wilcox.test(data$rentsqm, data$quality.of.location,
                                   p.adjust.method = "bonferroni")

after_corr