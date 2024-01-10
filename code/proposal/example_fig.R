# Simulate how disparate impact statistics can fail
# Max Griswold
# 1/9/24

# Varying population size
n <- seq(100, 500, 10)
i <- 1:100

sim_scenarios <- function(n, i){
  
  # Create population by race/ethnicity
  pop_prop <- 0.15
  m <- sample(c("black", "white"), replace = T, size = n, prob = c(pop_prop, 1 - pop_prop))
  m <- factor(m)
  m <- relevel(m, ref = "white")

  # Set up baseline outcome. For the proposal,
  # the outcome will be an offered interest rate, which will be in a 100 basis
  # point range between 2.5 and 7.5
  y <- rnorm(n)
  y <- 5*(y - min(y))/(max(y) - min(y)) + 2.5
  
  sim <- data.table("race" = m, 
                    "interest_rate" = y)
  
  # Add an evenly distributed 'latent' variable for colorism (unobserved)
  n_black <- dim(sim[race == "black",])[1]
  
  sim[race == "black", colorism := runif(n_black, 0, 1)]
  sim[race == "white", colorism := 0]
  
  # Create treatment scenarios. The average effect in the population
  # should be 25% worse in all cases:
  
  # Scenario 1: Offer a 30% worse interest rate compared to observed mean
  # Scenario 2: Offer a 0 - 60% worse interest rate, based on colorism variable
  # Scenario 3: For a random 20% subset of applicants, offer an interest rate that
  # is 150% higher than normal. 
  
  sim[, y1 := ifelse(race == "black", y*1.3, y)]
  sim[, y2 := y*(1 + colorism*0.6)]
  
  # Create baseline chance.
  sim[race == "black", bad_luck := rbinom(n_black, 1, prob = 0.2)]
  sim[race == "white", bad_luck := 0]
  
  sim[, y3 := y*ifelse(bad_luck==1, 2.5, 1)]
  
  res1 <- summary(lm(log(y1) ~ race, data = sim))$coefficients['raceblack','Pr(>|t|)']
  res2 <- summary(lm(log(y2) ~ race, data = sim))$coefficients['raceblack','Pr(>|t|)']
  res3 <- summary(lm(log(y3) ~ race, data = sim))$coefficients['raceblack','Pr(>|t|)']
  
  results <- data.table('scenario' = c(1, 2, 3),
                        'pval' = c(res1, res2, res3),
                        'sample_size' = n,
                        'iter' = i)
  
  return(results)
  
}

# Run each scenario 100 times, for each sample size\
args <- expand.grid("n" = n, "i" = i)

df <- rbindlist(mapply(sim_scenarios, n = args$n, i = args$i, SIMPLIFY = F, USE.NAMES = F))

# Take mean p-value for each scenario and sample:
df <- unique(df[, pval := mean(pval), by  = c("scenario", "sample_size")])

ggplot(df, aes(x = sample_size, y = pval, color = as.factor(scenario))) +
  geom_line(size = 1) +
  theme_bw()
