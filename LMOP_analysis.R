# =============================================================================
# LMOP Analysis Script
# Requires: LMOP_cleaning.R to have been run first (produces lmop_long)
# Preregistration specifies:
#   - Significance threshold: p < .005 (Benjamin et al., 2018)
#   - Three paired t-tests per bias type (one per difficulty level)
# =============================================================================

library(tidyverse)

# Source cleaning script to produce lmop_long
# (Comment out if you are running this in the same session as LMOP_cleaning.R)
source("LMOP_cleaning.R")

# Significance threshold per preregistration
ALPHA <- 0.005

# =============================================================================
# COMPUTED VARIABLES
# Pre-reg definitions:
#
#   Overestimation  = self_score - score
#     (weighted self-reported estimated score minus actual score)
#
#   Overplacement   = (self_score - rspp_score) - (score - mean_score_by_difficulty)
#     (self-reported superiority, corrected for actual superiority)
#
#   Overprecision   = mad_rspp - mad_self
#     (MAD of reported RSPP distribution minus MAD of actual RSPP distribution)
#     NOTE: The preregistration defines overprecision as subtracting the
#     certainty expressed in the reported RSPP distribution from the actual
#     concentration. Because the actual RSPP distribution MAD must be computed
#     from the full sample, we compute it here as:
#       actual_rspp_mad = MAD of participants' actual scores within each difficulty
#     Then: overprecision = actual_rspp_mad - mad_rspp
# =============================================================================

# Compute mean actual score per difficulty (needed for overplacement)
# and actual RSPP MAD (needed for overprecision)
lmop_long <- lmop_long %>%
  group_by(difficulty) %>%
  mutate(
    mean_actual_score = mean(score, na.rm = TRUE),

    # MAD of the actual score distribution across participants
    actual_rspp_mad = mean(abs(score - mean(score, na.rm = TRUE)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    overestimation = self_score - score,
    overplacement  = (self_score - rspp_score) - (score - mean_actual_score),
    overprecision  = actual_rspp_mad - mad_rspp
  )

# =============================================================================
# HELPER: Run a one-sample t-test of a variable against 0, return tidy output
# =============================================================================

run_ttest <- function(data, difficulty_level, variable, label) {
  x <- data %>%
    filter(difficulty == difficulty_level) %>%
    pull({{ variable }}) %>%
    na.omit()

  if (length(x) < 2) {
    return(tibble(
      bias = label, difficulty = difficulty_level,
      n = length(x), mean = NA, sd = NA,
      t = NA, df = NA, p = NA, ci_low = NA, ci_high = NA,
      sig = NA
    ))
  }

  tt <- t.test(x, mu = 0)
  tibble(
    bias       = label,
    difficulty = difficulty_level,
    n          = length(x),
    mean       = round(mean(x), 3),
    sd         = round(sd(x), 3),
    t          = round(tt$statistic, 3),
    df         = tt$parameter,
    p          = round(tt$p.value, 4),
    ci_low     = round(tt$conf.int[1], 3),
    ci_high    = round(tt$conf.int[2], 3),
    sig        = tt$p.value < ALPHA
  )
}

difficulties <- c("Easy", "Medium", "Hard")

# =============================================================================
# 1. OVERESTIMATION
#    H: On difficult tasks, people overestimate; on easy tasks, underestimate.
#    Test: Is overestimation significantly different from 0?
# =============================================================================

cat("\n========================================\n")
cat("1. OVERESTIMATION (self_score - actual score)\n")
cat("========================================\n")

overest_results <- map_dfr(difficulties, ~run_ttest(
  lmop_long, .x, overestimation, "Overestimation"
))

print(overest_results)

# =============================================================================
# 2. OVERPLACEMENT
#    H: On difficult tasks, people underplace; on easy tasks, overplace.
#    Test: Is overplacement significantly different from 0?
# =============================================================================

cat("\n========================================\n")
cat("2. OVERPLACEMENT\n")
cat("   (self_score - rspp_score) - (actual score - mean actual score)\n")
cat("========================================\n")

overplace_results <- map_dfr(difficulties, ~run_ttest(
  lmop_long, .x, overplacement, "Overplacement"
))

print(overplace_results)

# =============================================================================
# 3. OVERPRECISION
#    H: People are too certain of themselves across difficulty levels.
#    Test: Is overprecision significantly different from 0?
#    Direction: positive = reported RSPP distribution is more concentrated
#               (smaller MAD) than the actual score distribution
# =============================================================================

cat("\n========================================\n")
cat("3. OVERPRECISION\n")
cat("   (actual RSPP MAD - reported RSPP MAD)\n")
cat("========================================\n")

overprec_results <- map_dfr(difficulties, ~run_ttest(
  lmop_long, .x, overprecision, "Overprecision"
))

print(overprec_results)

# =============================================================================
# 4. COMBINED RESULTS TABLE
# =============================================================================

cat("\n========================================\n")
cat("COMBINED RESULTS TABLE\n")
cat(sprintf("Significance threshold: p < %.3f (Benjamin et al., 2018)\n", ALPHA))
cat("========================================\n")

all_results <- bind_rows(overest_results, overplace_results, overprec_results) %>%
  mutate(difficulty = factor(difficulty, levels = c("Easy", "Medium", "Hard"))) %>%
  arrange(bias, difficulty)

print(all_results, n = Inf)

# =============================================================================
# 5. DESCRIPTIVE SUMMARY TABLE
# =============================================================================

cat("\n========================================\n")
cat("DESCRIPTIVE SUMMARY\n")
cat("========================================\n")

desc_summary <- lmop_long %>%
  group_by(difficulty) %>%
  summarise(
    n                   = n(),
    mean_actual_score   = round(mean(score, na.rm = TRUE), 3),
    sd_actual_score     = round(sd(score, na.rm = TRUE), 3),
    mean_self_score     = round(mean(self_score, na.rm = TRUE), 3),
    mean_rspp_score     = round(mean(rspp_score, na.rm = TRUE), 3),
    mean_mad_self       = round(mean(mad_self, na.rm = TRUE), 3),
    mean_mad_rspp       = round(mean(mad_rspp, na.rm = TRUE), 3),
    mean_overestimation = round(mean(overestimation, na.rm = TRUE), 3),
    mean_overplacement  = round(mean(overplacement, na.rm = TRUE), 3),
    mean_overprecision  = round(mean(overprecision, na.rm = TRUE), 3),
    .groups = "drop"
  )

print(desc_summary)

# =============================================================================
# 6. OPTIONAL: PLOTS
# =============================================================================

# --- 6a. Overestimation by difficulty ---
p1 <- lmop_long %>%
  ggplot(aes(x = difficulty, y = overestimation, fill = difficulty)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("Easy" = "#4CAF50", "Medium" = "#FF9800", "Hard" = "#F44336")) +
  labs(
    title    = "Overestimation by Difficulty",
    subtitle = "Self-reported score minus actual score\n(positive = overestimate)",
    x = "Difficulty", y = "Overestimation",
    caption = sprintf("Diamond = mean. Dashed line = 0. α = %.3f", ALPHA)
  ) +
  theme_bw() +
  theme(legend.position = "none")

# --- 6b. Overplacement by difficulty ---
p2 <- lmop_long %>%
  ggplot(aes(x = difficulty, y = overplacement, fill = difficulty)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("Easy" = "#4CAF50", "Medium" = "#FF9800", "Hard" = "#F44336")) +
  labs(
    title    = "Overplacement by Difficulty",
    subtitle = "Self-reported superiority minus actual superiority\n(positive = overplace)",
    x = "Difficulty", y = "Overplacement",
    caption = sprintf("Diamond = mean. Dashed line = 0. α = %.3f", ALPHA)
  ) +
  theme_bw() +
  theme(legend.position = "none")

# --- 6c. Overprecision by difficulty ---
p3 <- lmop_long %>%
  ggplot(aes(x = difficulty, y = overprecision, fill = difficulty)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("Easy" = "#4CAF50", "Medium" = "#FF9800", "Hard" = "#F44336")) +
  labs(
    title    = "Overprecision by Difficulty",
    subtitle = "Actual RSPP MAD minus reported RSPP MAD\n(positive = overconfident)",
    x = "Difficulty", y = "Overprecision",
    caption = sprintf("Diamond = mean. Dashed line = 0. α = %.3f", ALPHA)
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Print plots
print(p1)
print(p2)
print(p3)

# Save plots
ggsave("LMOP_overestimation.png",  p1, width = 6, height = 5, dpi = 150)
ggsave("LMOP_overplacement.png",   p2, width = 6, height = 5, dpi = 150)
ggsave("LMOP_overprecision.png",   p3, width = 6, height = 5, dpi = 150)

cat("\nAnalysis complete.\n")
