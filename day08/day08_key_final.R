################################################################################
#                                                                              #
#                      Erdos Institute Code Bootcamp for R                     #
#                                 ------------                                 #
#                 Day 8 - Statistics with Categorical Variables                #
#                                 ------------                                 #
#                 Collin McCabe | collinmichaelmccabe@gmail.com                #
#                                                                              #
################################################################################

#-----------------#
# Chi-square Test #
#-----------------#

# Statistics 101

# P value: how likely something occur by chance
# Null hypothesis: opposite of what you try to show

# Null hypothesis H_/phi: There is no relationship between your variables
# Alternative hypothesis H_A: There is a relationship between your variables
# you can have more than one, or more specific alternative hypothesis
# eg1: H_A+: A is greater than B (call one tailed test)
# eg2: H_A-: A is less than B
# why should you accept the Null or alternative hypothesis: --> P value
# P-value: probability that the Null hypothesis is correct.
# P <= 0.05: 5% possibility that your null hypothesis is correct, "significant"

#       H     T 
# Obs  31     29
# Exp  30     30

# Null Hypothesis: coin is fair
# alternative hypothesis: coin is not fair

# the following concatenation is equal to groupby + summarize
# outcome: H T T H T T H T H H H H T ...
# groupby outcome by 'T' 'H' and then summarize 
# default: the occurance of each outcome is equal

observed_coin1 <- c("heads" = 31, "tails" = 29)

chisq.test(observed_coin1)

# X-squared = ..., df = 1 ("df"="degree of freedome"), p-value =0.7963 much bigger than 0.05
# should accept Null hypothesis: fair coin

observed_coin2 <- c("heads" = 20, "tails" = 40)

coin2 <- chisq.test(observed_coin2)

coin2$expected

library(tidyverse)
as_tibble(cbind(outcome = as.integer(c(coin2$observed, coin2$expected)), 
      o_e = c("O", "O", "E", "E"),
      h_t = rep(c("H", "T"), 2))) %>%
  ggplot(aes(x = h_t, y = outcome, fill = o_e)) +
  geom_bar(stat = "identity", position = "dodge") 



# What's a p-value?

set.seed(102040)

results <- NULL

for (i in 1:200) {
  test_sample <- sample(c(0, 1), 100, replace = TRUE)
  (test_obs <- c("heads" = sum(test_sample), 
                 "tails" = length(test_sample) - sum(test_sample)))
  results[i] <- ifelse(chisq.test(test_obs)$p.value < 0.05, 1, 0)
}

total_sig <- sum(results)
total_sig / 200

### EXERCISE 1 ###

# Would you expect thee counts of dice rolls by chance?

observed_dice <- c("one" = 5, "two" = 5, "three" = 5, 
                   "four" = 5, "five" = 5, "six" = 35)

chisq.test(observed_dice)

dice_result <- chisq.test(observed_dice)
dice_result$observed
dice_result$expected

### EXERCISE 2 ###

# Do you have an equal number of manual and automatic transmissions in mtcars?
# How about an equal number of each type of cylinder?

observed_num_transmissions <- c("manual" = sum(mtcars$am), "automatic" = length(mtcars$am)-sum(mtcars$am))
chisq.test(observed_num_transmissions)

mtcars %>%
  select(cyl) %>%
  group_by(cyl) %>%
  summarize(n=n()) %>%
  select(n) %>%
  chisq.test()

res <- mtcars %>%
  select(am) %>%
  group_by(am) %>%
  summarize(n = n()) %>%
  select(n) %>%
  chisq.test()

res <- mtcars %>%
  select(am) %>%
  group_by(am) %>%
  summarize(n = n()) %>%
  select(n) %>%
  chisq.test(p=c(.8,.2))

res

res <- mtcars %>%
  select(am) %>%
  group_by(am) %>%
  summarize(n = n()) %>%
  select(n) 

c(19,13)%>%
  chisq.test(p=c(.9,.1))

res$expected

mtcars %>%
  select(cyl) %>%
  group_by(cyl) %>%
  summarize(n = n()) %>%
  select(n) %>%
  chisq.test()




#---------#
# t-tests #
#---------#

### One sample: compare a distribution to an expected mean ###

#   Round    Type    # H/30
#     1      Fair      15
#     1      UnF       28
#     2      Fair      14
#     2      UnF       25

# Null hypothesis: they are same, 
# F (expected distribution, standard normalize distribution) and 
# UnF (distribution to be tested) have two different distributions
# overlapping of two distributions below 0.05 of the total area of the curve

set.seed(42)
rnorm(200) %>%
  t.test(mu = 0)

### EXERCISE ###

# The morley dataset records many replicates of the measure of the speed of 
# light (all values are in km/sec minus 299000)- check to see if the mean is 
# different from 850
# p-value > 0.05 fali to reject the null hypothesis


t.test(morley$Speed, mu = 850)

# Add 299000 back to all of the measurements- does this change your result? Why?



t.test(morley$Speed + 299000, mu = 299850)

### What about comparing two distributions? Two sample t-test: ###

# Do 4 cylinder cars have different highway mpg than 8 cylinder ones?
four <- mpg$hwy[mpg$cyl == 4]
eight <- mpg$hwy[mpg$cyl == 8]

# t-test can not do well with two samples with quite different data sizes

# "student test", "welch 2-sample test"

t.test(four, eight)

t.test(four, eight, alternative = "greater")

t.test(four, eight, alternative = "less")

# since we can only sample a limited number of each type data
# power analysis to make sure enough sample of each group data makes you t-test robust

mpg %>%
  filter(cyl %in% c(4, 8)) %>%
  t.test(hwy ~ cyl, .)

mpg %>%
  filter(cyl %in% c(4, 8)) %>%
  ggplot(aes(hwy, colour = factor(cyl))) +
  geom_density()

# 2 tail test(5%), 1 tail test (2.5%)

### EXERCISE ###

# Compare petal length in the iris dataset between Species setosa and virginica

hist(iris$Petal.Length[iris$Species == "virginica"])

iris %>%
  filter(Species %in% c("setosa", "virginica")) %>%
  t.test(Petal.Length ~ Species, .)




# Do the same for Species virginica and versicolor





### What about if you want to compare between specific sets? Paired t-test: ###

# Example dataset and solution taken from cookbook for R: 
# http://www.cookbook-r.com/Statistical_analysis/t-test/
sleep %>%
  arrange(group, ID) %>%
  t.test(extra ~ group, ., paired = TRUE)

# if you want to pipe some thing as a second or third argument other than the first,
# you can use "."

# Basis for plot taken from a StackOverflow answer: https://stackoverflow.com/questions/31102162/r-paired-dot-plot-and-box-plot-on-same-graph-is-there-a-template-in-ggplot2
sleep %>%
  arrange(group, ID) %>%
  ggplot(aes(y = extra)) +
  geom_point(aes(x = rep(c(1, 2), each = 10)), size = 5) +
  geom_line(aes(x = rep(c(1, 2), each = 10), group = ID)) +
  scale_x_discrete(limits=c("1", "2")) +
  labs(x = "Drug Group", y = "Additional Hours of Sleep", 
       title = "Paired t-test comparing effects of two drugs on sleep time") +
  theme_classic()

# paired t-test is actually one sample test: 

### EXERCISE ###

# Compare the weights of 17 patients before and after treatment for anorexia

install.packages("PairedData"); library(PairedData)
data(Anorexia); View(Anorexia)





#-------#
# ANOVA #
#-------#
  
# What if you want to compare the distributions of more than two variables?
  
result_anova <- mpg %>%
  filter(cyl != 5) %>%
  aov(hwy ~ factor(cyl), .)

summary(result_anova)


mpg %>%
  filter(cyl != 5) %>%
  group_by(cyl) %>%
  summarize(mean_hwy = mean(hwy), sd_hwy = sd(hwy), n = n()) %>%
  ggplot(mapping = aes(x = factor(cyl), y = mean_hwy, fill = factor(cyl))) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_hwy - (sd_hwy / sqrt(n)), 
                    ymax = mean_hwy + (sd_hwy / sqrt(n)),
                width = 0.5)) +
  labs(x = "Cylinders", y = "Mean Highway MPG", title = "hwy ~ cyl ANOVA") +
  theme_minimal()

TukeyHSD(result_anova)

### EXERCISE ###

# Compare the petal lengths of all three species in the iris dataset!



result_anova2 <- iris %>%
  aov(Petal.Length ~ factor(Species), .)

summary(result_anova2)

TukeyHSD(result_anova2)
