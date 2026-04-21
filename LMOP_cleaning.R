

library(tidyverse)

# =============================================================================
# 0.  CONSTANTS
# =============================================================================

EASY_WEIGHTS   <- c(180, 115, 116, 146, 167, 104, 165, 138, 123, 147)
MEDIUM_WEIGHTS <- c(135, 183, 154, 174, 180, 149, 187, 165,  97, 139)
HARD_WEIGHTS   <- c(106, 128, 112, 135, 136, 214, 127, 140, 158, 121)

EASY_RADIUS   <- 30
MEDIUM_RADIUS <- 13
HARD_RADIUS   <-  3

# Score bins: _1 = 10 right (best) … _11 = 0 right (worst)
SCORE_VALUES <- 10:0   # length 11

# =============================================================================
# 1.  LOAD RAW DATA
#     Qualtrics CSVs have 2 header rows below the column names; skip them.
# =============================================================================

headers <- names(read_csv("LMOP_April+20%2C+2026_13.21.csv", n_max = 0))

raw <- read_csv(
  "LMOP_April+20%2C+2026_13.21.csv",
  skip = 3,
  col_names = headers,
  show_col_types = FALSE
)

# Quick sanity check
cat("Rows loaded:", nrow(raw), "\n")
cat("Columns:", ncol(raw), "\n")

# =============================================================================
# 2.  EXCLUSIONS
#     Per preregistration section 6: exclude incomplete responses.
# =============================================================================

raw_complete <- raw %>%
  filter(Finished == TRUE)

cat("Rows after exclusion (incomplete removed):", nrow(raw_complete), "\n")

# =============================================================================
# 3.  HELPER FUNCTIONS
# =============================================================================

# 3a. Compute actual score given a vector of 10 guesses, answer weights,
#     and an allowed radius.
compute_score <- function(guesses, weights, radius) {
  sum(abs(guesses - weights) <= radius, na.rm = TRUE)
}

# 3b. Compute weighted mean of a reported distribution.
#     `probs` is a length-11 numeric vector (sums to ~100).
#     `score_values` is 10:0.
weighted_mean_score <- function(probs, score_values = SCORE_VALUES) {
  if (all(is.na(probs))) return(NA_real_)
  sum(probs * score_values, na.rm = TRUE) / sum(probs, na.rm = TRUE)
}

# 3c. Compute Mean Absolute Deviation of a reported distribution.
#     MAD = sum( p_i * |x_i - mu| ) / sum(p_i)
compute_mad <- function(probs, score_values = SCORE_VALUES) {
  if (all(is.na(probs))) return(NA_real_)
  total <- sum(probs, na.rm = TRUE)
  if (total == 0) return(NA_real_)
  mu <- sum(probs * score_values, na.rm = TRUE) / total
  sum(probs * abs(score_values - mu), na.rm = TRUE) / total
}

# =============================================================================
# 4.  BUILD LONG-FORMAT DATA FRAME
# =============================================================================

# We process each difficulty block independently, then bind rows.

process_block <- function(df, difficulty, guess_prefix, self_prefix, rspp_prefix,
                          weights, radius) {

  guess_cols <- paste0(1:10, "_", guess_prefix)   # e.g. "1_Q195" … "10_Q195"
  self_cols  <- paste0(self_prefix, "_", 1:11)    # e.g. "Q201_1" … "Q201_11"
  rspp_cols  <- paste0(rspp_prefix, "_", 1:11)    # e.g. "Q202_1" … "Q202_11"

  df %>%
    select(subject_id = id, all_of(c(guess_cols, self_cols, rspp_cols))) %>%
    rowwise() %>%
    mutate(
      difficulty  = difficulty,

      # --- Actual score ---
      score = compute_score(
        guesses = c_across(all_of(guess_cols)) %>% as.numeric(),
        weights = weights,
        radius  = radius
      ),

      # --- Self-reported distribution: weighted mean ---
      self_score = weighted_mean_score(
        probs = c_across(all_of(self_cols)) %>% as.numeric()
      ),

      # --- RSPP distribution: weighted mean ---
      rspp_score = weighted_mean_score(
        probs = c_across(all_of(rspp_cols)) %>% as.numeric()
      ),

      # --- MAD of self distribution ---
      mad_self = compute_mad(
        probs = c_across(all_of(self_cols)) %>% as.numeric()
      ),

      # --- MAD of RSPP distribution ---
      mad_rspp = compute_mad(
        probs = c_across(all_of(rspp_cols)) %>% as.numeric()
      )
    ) %>%
    ungroup() %>%
    select(subject_id, difficulty, score, self_score, rspp_score,
           mad_self, mad_rspp)
}

easy_long   <- process_block(raw_complete, "Easy",   "Q195", "Q201", "Q202",
                              EASY_WEIGHTS,   EASY_RADIUS)
medium_long <- process_block(raw_complete, "Medium", "Q205", "Q206", "Q207",
                              MEDIUM_WEIGHTS, MEDIUM_RADIUS)
hard_long   <- process_block(raw_complete, "Hard",   "Q216", "Q212", "Q213",
                              HARD_WEIGHTS,   HARD_RADIUS)

# Combine all three blocks
lmop_long <- bind_rows(easy_long, medium_long, hard_long) %>%
  mutate(difficulty = factor(difficulty, levels = c("Easy", "Medium", "Hard")))



# For now, add a placeholder column
lmop_long <- lmop_long %>%
  mutate(test_order = NA_integer_) %>%
  relocate(test_order, .after = difficulty)

# =============================================================================
# 6.  FINAL STRUCTURE CHECK
# =============================================================================

cat("\nCleaned data: ", nrow(lmop_long), "rows (expect", nrow(raw_complete) * 3, ")\n")
cat("Columns:", paste(names(lmop_long), collapse = ", "), "\n\n")
print(head(lmop_long, 9))

# =============================================================================
# 7.  SUMMARY MEANS (for lab agenda)
# =============================================================================

means_table <- lmop_long %>%
  group_by(difficulty) %>%
  summarise(
    n               = n(),
    mean_score      = mean(score,      na.rm = TRUE),
    mean_self_score = mean(self_score, na.rm = TRUE),
    mean_rspp_score = mean(rspp_score, na.rm = TRUE),
    mean_mad_self   = mean(mad_self,   na.rm = TRUE),
    mean_mad_rspp   = mean(mad_rspp,   na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Means by Difficulty ---\n")
print(means_table)

# =============================================================================
# 8.  WRITE CLEANED FILE
# =============================================================================

write_csv(lmop_long, "LMOP_cleaned.csv")
cat("\nCleaned file written to: LMOP_cleaned.csv\n")
