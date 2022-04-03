#loading libraries
library(dplyr)
library(ggplot2)

#reading the data
data <- read.csv('/Users/aneeshraj/Desktop/Python_Practice/Stats Prep/ab testing/WA_Marketing-Campaign.csv')

#groupping data by Promotion
SalesDist <- data %>%
  group_by(Promotion) %>%
  summarize(Sales=sum(SalesInThousands))%>%
  mutate(Percentage=round(Sales/sum(Sales)*100))

#visualizing data
options(repr.plot.width = 20, repr.plot.height = 10)
ggplot(SalesDist, aes(x="", y=Percentage, fill=Promotion)) +
  geom_bar(width=1, stat="identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Percentage), position=position_fill(vjust=0.5), color="white", size=20) +
  coord_polar("y") +
  ggtitle("Sales distribution across different promotions, in percents") +
  theme_classic() +
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20), plot.title = element_text(size = 30),
        axis.title.x = element_text(size = 25))

#groupping data by Promotion and MarketSize
Market <- data %>%
  group_by(Promotion, MarketSize) %>%
  summarize(Count=n())

#visualizing data
ggplot(Market, aes(x=Promotion, y=Count, fill=MarketSize)) +
  geom_bar(width=0.5, stat="identity", position="stack") +
  ylab("Count") +
  xlab("Promotion") +
  ggtitle("Market sizes across different promotions") +
  theme(legend.text=element_text(size=20), plot.title = element_text(size=27), axis.title = element_text(size = 25))

#groupping data by AgeOfStore
Age <- data %>%
  group_by(AgeOfStore) %>%
  summarize(Count=n())

#visualizing data
ggplot(Age, aes(x=AgeOfStore, y=Count)) +
  geom_bar(width=0.5, stat="identity", fill="orange") +
  ylab("Count") +
  xlab("Store Age") +
  ggtitle("Store Age Distribution") +
  theme(legend.text=element_text(size=20), plot.title = element_text(size=27), axis.title = element_text(size = 25))

tapply(data$AgeOfStore, data$Promotion, summary)

#We see that all three groups seem to have store age profiles. 
#The average ages (Mean) of stores for the three groups are 8-9 years old and 
#the majority of the stores are 10-12 years old or younger.

#After exploring the distribution of the variables in the three promotion groups, 
#I can verify that the sample groups are similar and 
#the A/B testing results will be meaningful and trustworthy.

promo_1 <- data[which(data$Promotion==1),]$SalesInThousands
promo_2 <- data[which(data$Promotion==2),]$SalesInThousands
promo_3 <- data[which(data$Promotion==3),]$SalesInThousands

#mean
mean_1 <- mean(promo_1)
mean_2 <- mean(promo_2)
mean_3 <- mean(promo_3)
#standard deviation
std_1 <- sd(promo_1)
std_2 <- sd(promo_2)
std_3 <- sd(promo_3)
#number of samples
n_1 <- length(promo_1)
n_2 <- length(promo_2)
n_3 <- length(promo_3)

#t-value comparing promotion 1 and promotion 2
t_value <- (
  mean_1 - mean_2
) / sqrt(
  (std_1**2/n_1 + std_2**2/n_2)
)
#computing the degrees of freedom
df_1_2 <- n_1 + n_2 - 2
#computing p-value using pt function
p_value <- 2*pt(t_value, df_1_2, lower=FALSE)

#printing t-value and p-value
print(t_value)
print(p_value)


#We got the** t-value of 6.4275** and p-value of 4.143e-10 (which is an extremely small number) 
#that suggest that there is strong evidence against the null hypothesis and that the difference 
#between promotion 1 and promotion 2 is significant and promotion 1 outperform promotion 2.

#Let's repeat the same evaluation for promotion group 1 and promotion group 3.


#t-value comparing promotion 1 and promotion 3
t_value <- (
  mean_1 - mean_3
) / sqrt(
  (std_1**2/n_1 + std_3**2/n_3)
)
#computing the degrees of freedom
df_1_3 <- n_1 + n_3 - 2
#computing p-value using pt function
p_value <- 2*pt(t_value, df_1_3, lower=FALSE)

#printing t-value, p-value and means
print(t_value)
print(p_value)
print(mean_1)
print(mean_2)
print(mean_3)


# Here we got the t-value of 1.5560 and the p-value of 0.1205 
# (which is much higher than 0.05). This result suggests that there is no 
# statistically significant difference between promotion groups 1 and 3 even 
# though the average sales from promotion group 1 (58.1) is higher than in group 3 (55.36).

# From this evaluation, I can say that promotion 1 and promotion 3 perform 
# better than promotion 2, but the difference between promotion 1 and 
# promotion 3 is not statistically significant. So the company can use 
# both 1 and 2 marketing strategies for their fast-food retail chain.


#t-test between promo 1 and promo 2
t.test(
  promo_1,
  promo_2
)

# So we got the same result using t.test. Let's check promotion 1 and promotion 3.
#t-test between promo 1 and promo 3
t.test(
  promo_1,
  promo_3
)

# Whether I use the previous approach of manually computing the t-value and 
# p-value or the approach of using t.test, I got the same 
# result - promotion 1 and 2 outperform promotion 3.

#SUMMARY

# A/B testing is a powerful technique that businesses can use to 
# evaluate new ideas and marketing strategies before fully committing 
# to one or another. In this analysis, I found that the company 
# should use promotion strategies 1 or 2 to maximize their sales