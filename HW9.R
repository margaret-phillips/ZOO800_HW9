# Load packages
library(truncnorm)
library(tidyverse)

#####################
### Objective 1 #####
#####################

##----------------------- part a ------------------------------####
#assigning values for a and b
a<- 1
b<- 10

# Generate 100 observations from N(mean=5.5, sd=2) truncated between 1 and 10
set.seed(100)
xi <- rtruncnorm(100, a = 1, b = 10, mean = 5.5, sd = 2)

# View the first few observations
#generate 100 random values for e from normal dist for sd of 1
set.seed(100)
ei_1 <- rtruncnorm(100, a= -25, b= 25, mean= 0, sd= 1)


#re-do this for sd of 10
set.seed(100)
ei_10 <- rtruncnorm(100, a= -25, b= 25, mean= 0, sd= 10)


#re-do again for sd of 25
set.seed(100)
ei_25 <- rtruncnorm(100, a= -25, b= 25, mean= 0, sd= 25)

y_1<- as.matrix(a + (b*xi) + ei_1)
y_10<- as.matrix(a + (b*xi) + ei_10)
y_25<-  as.matrix(a + (b*xi) + ei_25)

##------------ part b --------------------------------------------####
#. Using facet wrapping in ggplot, create a multipanel figure comparing plots of y vs. x in one row for
#three different values of σ: 1, 10, and 25.

#first create dataframe with x and y
xy_df <- cbind.data.frame(y_1, y_10, y_25, xi)

# Reshape to long format
xy_df_long <- xy_df %>%
  pivot_longer(
    cols = starts_with("y_"),
    names_to = "error_level",
    values_to = "y"
  )

# Clean up error level labels
xy_df_long$error_level <- recode(xy_df_long$error_level,
                              y_low = "SD = 1",
                              y_med = "SD = 10",
                              y_high = "SD = 25")

# Plot with ggplot
ggplot(xy_df_long, aes(x = xi, y = y)) +
  geom_point(color = "steelblue", alpha= 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  facet_wrap(~ error_level, ncol = 3) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of Different Error Levels on y = a + bx + ei",
    x = "x",
    y = "y"
  )

# definitely gets more difficult to see the relationship btwn x and y as error increases

######################
## Objective 2 #######
######################

##--------- parts a and b-----------------------####
# first use p= 0.55
#need to first randomly flip the coin 20 times? generate values of 0 or 1
coin_heads <-rbinom(n = 20, size= 20, p= 0.55)
coin_heads_list <- as.list(coin_heads) #putting results into a list for iterating later
#alpha= success + 1
#beta= 1 = alpha - successes
alpha_list <- list() #initializing an empty list
beta_list <- list() #initializing an empty list

for (i in coin_heads_list) {
  alpha <- i + 1
  alpha_list <- append(alpha_list, alpha)}

#beta= 1
q_list <- list() #initialize empty list to store values in loop

for (i in alpha_list) {
  q_val <- qbeta(0.55, i, 1)   #compute quantile for given alpha
  q_list <- append(q_list, q_val)  #append to list
}

#using dbinom
set.seed(100)
coin_heads <- rbinom(n = 20, size = 20, p = 0.55) #first prob of 0.55

#compute the probability of each observed value
probs <- dbinom(coin_heads, size = 20, prob = 0.55)

#view df
data.frame(successes = coin_heads, probability = probs)

#example data with pbinom
set.seed(100)
coin_heads_p <- rbinom(n = 20, size = 20, p = 0.55)

#compute the probability of each observed value
probs_p <- pbinom(coin_heads_p, size = 20, prob = 0.55)

#view df
data.frame(successes = coin_heads_p, probability = probs_p)

#define variables
set.seed(100)
n_flips <- 1:20 #list of 1 through 20, inclusive
n_sim <- 100 #100 simulations
alpha <- 0.05 #value for significance 
p_values <- c(0.55, 0.6, 0.65)  #other prob values

#Initialize a results data frame
results <- data.frame()

#Loop over each given probability
#I had to google this nested for loop part
for (p_true in p_values) {
  
  power_results <- numeric(length(n_flips))
  
  for (n in n_flips) {
    rejections <- 0
    
    for (sim in 1:n_sim) {
      # Simulate n flips
      flips <- rbinom(1, size = n, prob = p_true)
      
      # One-sided binomial test: H0: p = 0.5 vs Ha: p > 0.5
      test <- binom.test(flips, n, p = 0.5, alternative = "greater")
      
      # Check if result is significant
      if (test$p.value < alpha) {
        rejections <- rejections + 1
      }
    }
    
    # Store power (proportion of rejections)
    power_results[n] <- rejections / n_sim
  }
  
  #Add to results dataframe
  results <- rbind(
    results,
    data.frame(
      flips = n_flips,
      power = power_results,
      p_true = as.factor(p_true)
    )
  )
}

#plot results on one graph
ggplot(results, aes(x = flips, y = power, color = p_true)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "orange", "darkred")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Ability to Detect an Unfair Coin at Different Levels of Bias",
    x = "Number of Coin Flips (n)",
    y = "Probability of Detecting Unfairness",
    color = "True p(heads)"
  ) +
  ylim(0, 1) #manually setting axis limits up to 1.00 or 100% probability

