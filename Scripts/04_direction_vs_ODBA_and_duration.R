# =====================================================================
# 04 - RESPONSE DIRECTION vs. ODBA
# =====================================================================
# --- QUESTION ---
#   Is the first response direction associated with changes in
#   relative change in ODBA and response duration?
#
# --- DATA ---
#   data_ODBA: tagged subset, 40 trials on 23 individuals
#
# --- VARIABLE ---
#   Response_direction: Toward / Away
#   relative_ODBA_change: relative ODBA change between before and after the playback
#   resp_dur: duration of the response following playback (until a return to baseline)
#
# --- VALUES TO LOOK FOR IN THE OUTPUT (manuscript, Table 2) ---
#   > ODBA relative change:
#     Welch t-test: t = -1.99, p = 0.05
#     Away: mean = 0.96 ± 1.18
#     Toward: mean = 0.35 ± 0.54
#   > Response duration (return to baseline):
#     Fisher-Pitman permutation test: Z = -1.79, p = 0.05
#     Away: mean = 6.6 ± 9.5
#     Toward: mean = 2.7 ± 2.0
# =====================================================================



# =====================================================================
# 0. PACKAGES
# =====================================================================
library(tidyverse)
library(car) # for Levene test and oneway



# =====================================================================
# 1. RELATIVE CHANGE OF ODBA VS DIRECTION OF RESPONSE
# =====================================================================
# -------- Parametric test: Student's t-test --------
# Assumes normally distributed data and equal variances
t.test(relative_ODBA_change ~ Response_direction,
       data = data_ODBA,
       var.equal = TRUE)


# -------- Check test assumptions --------
# CONDITION 1: Normality within each group
by(data_ODBA$relative_ODBA_change,
   data_ODBA$Response_direction,
   shapiro.test)

# CONDITION 2: Homogeneity of variances
car::leveneTest(relative_ODBA_change ~ Response_direction,
                data = data_ODBA)

# -------- Welch's t-test --------
t.test(relative_ODBA_change ~ Response_direction,
       data = data_ODBA,
       var.equal = FALSE)


# -------- Descriptive statistics --------
data_ODBA %>%
  group_by(Response_direction) %>%
  summarise(mean = mean(relative_ODBA_change, na.rm = TRUE),
            sd = sd(relative_ODBA_change, na.rm = TRUE),
            median = median(relative_ODBA_change, na.rm = TRUE),
            IQR = IQR(relative_ODBA_change, na.rm = TRUE))



# =====================================================================
# 2. RESPONSE DURATION VS DIRECTION OF RESPONSE
# =====================================================================
# -------- Parametric test: Student's t-test --------
# Assumes normally distributed data and equal variances
t.test(resp_dur ~ Response_direction,
       data = data_ODBA,
       var.equal = TRUE)

# -------- Check test assumptions --------
# CONDITION 1: Normality within each group
by(data_ODBA$resp_dur,
   data_ODBA$Response_direction,
   shapiro.test)

# CONDITION 2: Homogeneity of variances
car::leveneTest(resp_dur ~ Response_direction,
                data = data_ODBA)

# -------- Non-parametric alternative: Fisher-Pitman permutation test --------
coin::oneway_test(resp_dur ~ Response_direction,
                  data = data_ODBA,
                  distribution = approximate(nresample = 100000))


# -------- Descriptive statistics --------
data_ODBA %>%
  group_by(Response_direction) %>%
  summarise(mean = mean(resp_dur, na.rm = TRUE),
            sd = sd(resp_dur, na.rm = TRUE),
            median = median(resp_dur, na.rm = TRUE),
            IQR = IQR(resp_dur, na.rm = TRUE))
