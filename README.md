# Sound amplitude modulates response to whistle playbacks in bottlenose dolphins

Analysis code for the manuscript **"Sound amplitude modulates response to whistle playbacks in bottlenose dolphins (*Tursiops truncatus*)"**
(Sarasota Dolphin Research Program, Sarasota Bay, Florida, USA).

> **Status:** manuscript in preparation.

## Research question

Unexpected reactions to familiar signature whistle playbacks raised the possibility that playback amplitude influences responses.
Therefore, this study assesses the effect of signature whistle playback amplitude (source and received level) on the responses of bottlenose dolphins from the Sarasota Bay community (Florida, USA).

## Study design in brief

* **Playbacks** of natural signature whistles (SW) to free-swimming dolphins, 2017–2019 and 2022–2025.
  * *Familiar SW*: the focal's own whistle, a copy of the focal's whistle, or the whistle of a relative or close associate.
  * *Unfamiliar SW*: whistles from other populations (Beaufort, NC, USA and St. Joseph Bay, FL, USA).
* **Amplitude**: both source and received levels were considered.
  * *Source level (SL)*: "high" (2017–2019; stimuli broadcast at ≥ 150 dB re 1 µPa @ 1 m) vs. "low" (2022–2025; stimuli broadcast at ≤ 140 dB re 1 µPa @ 1 m).
  * *Received level (RL)*: measured on tagged dolphins only.
* **Response direction** (first head/body turn *toward* or *away* from the speaker) was scored from drone video or dead-reckoned tracks. Trials without a response were excluded, leaving **125 trials on 70 individuals** for analysis.
* A subset of animals carried a **DTAG** (**40 trials, 23 individuals**), providing the **RL**, the **relative change in ODBA** (overall dynamic body acceleration, a proxy for energy expenditure) and the **response duration**.

---

## What this repository contains

R scripts that reproduce the statistical analyses of the article, for the full dataset and the tagged subset.
R scripts are in the `Scripts/` folder:

| Script | Purpose | Main output in the article |
|---|---|---|
| `01_response_direction.R` | Response direction (toward/away): binomial GLM `SL * Sex * Stimulus type`; Tukey post hoc tests | Fig. 1, Table 2, Table S1 |
| `02_relative_ODBA_change.R` | Relative change in ODBA: Gaussian GLM `Mean RL + Sex`; RL difference between SL categories (Student's t-test) | Table 2, Fig. 2A |
| `03_response_duration.R` | Response duration: negative binomial GLM `Mean RL + Group size + Stimulus type` | Table 2, Fig. 2B |
| `04_direction_vs_`<br>`ODBA_and_duration.R` | Welch t-test and Fisher-Pitman permutation test: response direction vs. ODBA change / response duration | Results, last section |

Each script starts with a header describing the question, the data, the model and the values to look for in the output, and is commented step by step.
You will also find all the figures presented in `Output/Figures`.

## Statistical methods (summary)

* Models were compared with AIC, AICc and BIC and with likelihood-ratio tests (`stats`, `MuMIn`). The retained models and reference levels are listed in the header of each script.
* Binomial and Gaussian GLMs: `stats::glm`. Negative binomial GLM: `MASS::glm.nb`.
* Estimated marginal means, Tukey-adjusted pairwise comparisons: `emmeans`.
* Fisher-Pitman permutation test: `coin`.
* Residual diagnostics: `car`.
* Significance threshold: α = 0.05.


## Not included
Received levels (RL), ODBA values and dead-reckoned tracks were computed from DTAG data in MATLAB; that code is not part of this repository.  
Datasets are not shared in this repository.