#   H1: % of "anger" responses for male faces − % for female faces
#   H2: % of "happiness" responses for female faces − % for male faces

alpha <- 0.05 / 2   # two hypotheses, so we apply Bonferroni correction
power <- 0.80 

d       <- 0.4      # target
excluded <- 0.10    # expected proportion of participants excluded

n_for <- function(d) ceiling(power.t.test(delta = d, sd = 1, sig.level = alpha, power = power, type = "paired")$n)

n <- n_for(d)
cat("Minimum N:", n, "participants\n")
cat("N to recruit (with", excluded * 100, "% excluded):", ceiling(n / (1 - excluded)), "\n\n")

ds <- c(0.2, 0.3, 0.4, 0.5)
print(data.frame(d = ds, minimum_N = sapply(ds, n_for)))
