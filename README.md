# LMOP: Human Confidence Calibration Across Task Difficulty

This repository contains data-cleaning and analysis code for the **LMOP** study, which investigates how well calibrated human confidence judgments are across varying levels of task difficulty. LMOP is the human companion study to [LMOC](https://aspredicted.org/9ry6nh.pdf), which tests parallel hypotheses with frontier large language models (LLMs).

## Study Overview

Participants complete three 10-item quizzes in which they estimate the weight (in pounds) of a person shown in a photo. Task difficulty is manipulated by varying the margin of error required for a correct answer:

| Difficulty | Margin of Error |
|------------|-----------------|
| Easy       | within 30 lbs   |
| Medium     | within 13 lbs   |
| Hard       | within 3 lbs    |

After each quiz, participants report two probability distributions over the 11 possible scores (0 -- 10 correct):

1. **Self distribution** -- "How likely is it you obtained each of the 11 possible scores?"
2. **RSPP distribution** -- "For a randomly selected peer, how likely is it that they obtained each of the 11 possible scores?"

These distributions are used to compute three measures of miscalibration:

- **Overestimation**: weighted self-reported score minus actual score.
- **Overplacement**: self-reported superiority (self minus peer estimate) corrected for actual superiority.
- **Overprecision**: concentration (Mean Absolute Deviation) of the actual score distribution minus that of the reported peer distribution. Positive values indicate participants believe scores are more tightly clustered than they really are.

## Hypotheses

1. On **difficult** tasks, people overestimate their own performance but underplace relative to peers.
2. On **easy** tasks, people underestimate their own performance but overplace relative to peers.
3. Across all difficulty levels, people are overprecise (too certain of their judgments).

## Pre-registration

The study is pre-registered on AsPredicted (NEED LINK). All analyses use a significance threshold of *p* < .005 (Benjamin et al., 2018).

## Repository Structure

```
LMOP/
├── AsPredicted #285824.pdf   # Pre-registration document
├── LMOP_cleaning.R           # Data cleaning & processing script
├── LMOP_analysis.R           # Hypothesis tests & visualization
├── LICENSE                   # MIT License
└── README.md
```

### `LMOP_cleaning.R`

Reads the raw Qualtrics export, excludes incomplete responses, and produces a long-format data frame (`LMOP_cleaned.csv`) with one row per participant per difficulty level. Key computed variables:

- `score` -- actual number correct on each quiz
- `self_score` -- weighted mean of the self probability distribution
- `rspp_score` -- weighted mean of the peer (RSPP) probability distribution
- `mad_self` / `mad_rspp` -- Mean Absolute Deviation of each distribution

### `LMOP_analysis.R`

Sources the cleaning script, computes the three bias variables (overestimation, overplacement, overprecision), then runs **nine one-sample *t*-tests** (3 bias types x 3 difficulty levels) testing whether each bias differs from zero. Outputs a combined results table and generates violin/box plots for each bias type.

## Requirements

- **R** (>= 4.0)
- **tidyverse**

## Usage

Place the raw Qualtrics CSV in the project root, then run:

```r
source("LMOP_analysis.R")
```

This will automatically source the cleaning script, run all hypothesis tests, print results, and save plots (`LMOP_overestimation.png`, `LMOP_overplacement.png`, `LMOP_overprecision.png`).

## License

[MIT](LICENSE)
