# Stage 2: Sampling estimates on car_prices.csv
# Alexis Alberto Zuniga Alonso | EdgeHub | Statistical Methods

library(dplyr)

## 1. Load and clean the data ------------------------------------------------
df_raw <- read.csv(file = "../data/car_prices.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Drop rows with a missing sellingprice or a corrupted 'state' field
# (a known data-entry glitch where a VIN spills into the state column)
df <- df_raw %>% filter(!is.na(sellingprice), nchar(state) == 2)

## 2. Seed and sample size ----------------------------------------------------
set.seed(123)
N <- nrow(df)
n <- 50000

## 3. Simple Random Sampling ---------------------------------------------------
srs_idx <- sample(seq_len(N), size = n, replace = FALSE)
srs_sample <- df[srs_idx, ]
srs_mean <- mean(srs_sample$sellingprice)
srs_ci <- t.test(srs_sample$sellingprice)$conf.int

## 4. Systematic Sampling ------------------------------------------------------
k <- floor(N / n)
start <- sample(seq_len(k), 1)
sys_idx <- seq(from = start, to = N, by = k)[1:n]
sys_sample <- df[sys_idx, ]
sys_mean <- mean(sys_sample$sellingprice)
sys_ci <- t.test(sys_sample$sellingprice)$conf.int

## 5. Stratified Sampling (proportional allocation by state) ------------------
strata <- df %>%
  count(state, name = "Nh") %>%
  mutate(Wh = Nh / N, nh = round(Wh * n))

drift <- n - sum(strata$nh)
strata$nh[which.max(strata$Nh)] <- strata$nh[which.max(strata$Nh)] + drift

strat_sample <- df %>%
  group_by(state) %>%
  group_modify(~ {
    nh <- strata$nh[strata$state == .y$state]
    .x[sample(seq_len(nrow(.x)), size = min(nh, nrow(.x))), ]
  }) %>%
  ungroup()
strat_mean <- mean(strat_sample$sellingprice)
strat_ci <- t.test(strat_sample$sellingprice)$conf.int

## 6. Cluster Sampling (two-stage: manufacturer, then car) --------------------
df$make_clean <- tolower(trimws(df$make))
all_clusters <- sample(unique(df$make_clean))

selected <- c()
pool <- df[0, ]
for (cl in all_clusters) {
  selected <- c(selected, cl)
  pool <- df[df$make_clean %in% selected, ]
  if (nrow(pool) >= n) break
}
clus_sample <- pool[sample(seq_len(nrow(pool)), size = n), ]
clus_mean <- mean(clus_sample$sellingprice)
clus_ci <- t.test(clus_sample$sellingprice)$conf.int

## 7. Population reference -----------------------------------------------------
pop_mean <- mean(df$sellingprice)
pop_ci <- t.test(df$sellingprice)$conf.int

## 8. Comparison table ----------------------------------------------------------
comparison <- data.frame(
  Method = c("Simple Random", "Systematic", "Stratified", "Cluster (2-stage)", "Population (true)"),
  n = c(nrow(srs_sample), nrow(sys_sample), nrow(strat_sample), nrow(clus_sample), N),
  Mean = round(c(srs_mean, sys_mean, strat_mean, clus_mean, pop_mean), 2),
  CI_Lower = round(c(srs_ci[1], sys_ci[1], strat_ci[1], clus_ci[1], pop_ci[1]), 2),
  CI_Upper = round(c(srs_ci[2], sys_ci[2], strat_ci[2], clus_ci[2], pop_ci[2]), 2)
)
comparison$CI_Width <- round(comparison$CI_Upper - comparison$CI_Lower, 2)
print(comparison)

## 9. Comparison plot ------------------------------------------------------------
png("../figures/comparison_plot.png", width = 900, height = 550, res = 120)
methods <- comparison$Method[1:4]
means <- comparison$Mean[1:4]
lower <- comparison$CI_Lower[1:4]
upper <- comparison$CI_Upper[1:4]

plot(1:4, means, ylim = range(c(lower, upper, pop_mean)) * c(0.98, 1.02),
     xaxt = "n", xlab = "", ylab = "Estimated mean sellingprice (USD)",
     main = "Mean sellingprice by sampling method (95% CI)",
     pch = 19, col = "darkblue", cex = 1.4)
axis(1, at = 1:4, labels = methods, cex.axis = 0.8)
arrows(1:4, lower, 1:4, upper, angle = 90, code = 3, length = 0.08, col = "darkblue")
abline(h = pop_mean, col = "darkred", lty = 2, lwd = 2)
legend("topleft", legend = "Population mean", col = "darkred", lty = 2, lwd = 2, bty = "n")
dev.off()
