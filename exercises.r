# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
ds_theme_set()

# observed p(Remain)
actual_p <- 0.481

# observed spread
actual_d <- (2 * actual_p) - 1

N <- 1500

# E(sum of voters choosing "Remain")
N * actual_p

# SE(sum of voters choocing "Remain")
sqrt(N * actual_p * (1 - actual_p))


# standard error of x_hat the proportion of "Remain" voters?
sqrt((actual_p * (1 - actual_p)) / N )

# standard error of d the spread between the proportion of "Remain" voters and "Leave" voters?
2 * sqrt( (actual_p * (1 - actual_p)) / N)

str(brexit_polls)

brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread + 1) / 2 )

mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

# consider this poll
brexit_polls[1,]

# confidence interval around p
interval <- c(0.52 - qnorm(0.975) * sqrt( (0.52 * (1 - 0.52)) / 4772),
              0.52 + qnorm(0.975) * sqrt( (0.52 * (1 - 0.52)) / 4772))

# First, use mutate() to calculate a plug-in estimate se_x_hat for the standard 
# error of the estimate for each poll given its sample size and value of 
# (x_hat). Second, use mutate() to calculate an estimate for the standard error
# of the spread for each poll given the value of se_x_hat. Then, use mutate() 
# to calculate upper and lower bounds for 95% confidence intervals of the 
# spread. Last, add a column hit that indicates whether the confidence interval
# for each poll covers the correct spread .
june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt( (x_hat * (1 - x_hat)) / samplesize ),
         se_d = 2 * sqrt( (x_hat * (1 - x_hat)) / samplesize),
         lower = spread - (qnorm(0.975) * se_d),
         upper = spread + (qnorm(0.975) * se_d),
         hit = lower <= -0.038 & upper >= -0.038)

# What proportion of polls have a confidence interval that covers the value 0?
june_polls %>% 
  mutate(span = lower <= 0 & upper >=0) %>%
  summarize(mean(span))

# What proportion of polls predict "Remain" 
# (confidence interval entirely above 0)?
june_polls %>% 
  mutate(span = lower >= 0 & upper >=0) %>%
  summarize(mean(span))

# What proportion of polls have a confidence interval covering the true value of 
# d?
mean(june_polls$hit)

# Group and summarize the june_polls object by pollster to find the proportion 
# of hits for each pollster and the number of polls per pollster. Use arrange() 
# to sort by hit rate.
june_polls %>% 
  group_by(pollster) %>% 
  summarize(n = n(), prop_hits = sum(hit) / n()) %>% 
  arrange(prop_hits)

june_polls %>% 
  ggplot(aes(x = poll_type, y = spread)) +
  geom_boxplot() +
  theme_tufte()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type

# Calculate the confidence intervals of the spread combined across all polls 
# in june_polls, grouping by poll type. Recall that to determine the standard 
# error of the spread, you will need to double the standard error of the
# estimate.
combined_by_type %>% 
  mutate(se = 2 * sqrt( (p_hat * (1 - p_hat)) / N),
         lower = spread - (qnorm(0.975) * se),
         upper = spread + (qnorm(0.975) * se) )

# computes the confidence intervals for all Brexit polls in 2016 and then 
# calculates whether the confidence interval covers the actual value of the 
# spread d = -0.038
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  dplyr::select(poll_type, hit)

cst <- brexit_hit %>%
  group_by(poll_type, hit) %>% 
  summarize(n = n()) %>% 
  spread(poll_type, n)
  
  
cst %>% 
  dplyr::select(-hit) %>% 
  chisq.test()

cst$p.value

# Calculate the odds that an online poll generates a confidence interval 
# that covers the actual value of the spread.
online.odds <- cst[2, 2] / cst[1, 2]

# Calculate the odds that a telephone poll generates a confidence interval 
# that covers the actual value of the spread.
phone.odds <- cst[2, 3] / cst[1, 3]

# Calculate the odds ratio to determine how many times larger the odds are for 
# online polls to hit versus telephone polls.
online.odds / phone.odds

brexit_polls %>% 
  ggplot(aes(x = enddate,
             y = spread,
             color = poll_type)) +
  geom_smooth(method = "loess",
              span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -0.038))

# Use the following code to create the object brexit_long, which has a column 
# vote containing the three possible votes on a Brexit poll ("remain", "leave", 
# "undecided") and a column proportion containing the raw proportion choosing
# that vote option on the given poll:
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% 
  ggplot(aes(x = enddate,
             y = proportion,
             color = vote)) +
  geom_smooth(method = "loess",
              span = 0.3)
