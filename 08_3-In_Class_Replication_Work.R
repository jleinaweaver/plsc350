# Load the packages
library(tidyverse)
library(readxl)
library(lubridate)

# Input the data
# Clean up the date variables for use
right_censor_delay <- as.numeric(ymd("2025-12-31") - ymd("1984-12-10"))

d <- read_excel("../../Data/DATA-CAT_VDem_Polyarchy-1980-2025.xlsx", na = "NA") |>
  rename("signed_date" = `Signature Date`, "binding_date" = `Binding Date`) |>
  mutate(
    signed_date = ymd(str_remove(signed_date, " 00:00:00")),
    binding_date = ymd(str_remove(binding_date, " 00:00:00")),
    rat_delay = as.numeric(binding_date - ymd("1984-12-10")),
    rat_delay2 = if_else(is.na(rat_delay), right_censor_delay, rat_delay),
    rat_delay_yrs = as.numeric(rat_delay2 / 365)
  )
  
# Create a "long" version of the data
d2 <- pivot_longer(d, cols = VDem_Torture1980:VDem_Torture2025, names_to = "torture_year", values_to = "torture", names_prefix = "VDem_Torture")

# Run a Cox proportional hazards model
# coxph(formula, data=, weights, subset, 
#       na.action, init, control, 
#       ties=c("efron","breslow","exact"), 
#       singular.ok=TRUE, robust, 
#       model=FALSE, x=FALSE, y=TRUE, tt, method=ties,
#       id, cluster, istate, statedata, nocenter=c(-1, 0, 1), ...)

library(survival)
library(ggeffects)

res1 <- coxph(Surv(rat_delay_yrs, Binding) ~ torture, data = d2)
summary(res1)
plot(ggpredict(res1)) + geom_hline(yintercept = 1, linetype = "dashed")

res2 <- coxph(Surv(rat_delay_yrs, Binding) ~ polyarchy_median, data = d2)
summary(res2)
plot(ggpredict(res2)) + geom_hline(yintercept = 1, linetype = "dashed")

res3 <- coxph(Surv(rat_delay_yrs, Binding) ~ polyarchy_median * torture, data = d2)
summary(res3)
plot(ggpredict(res3, terms = "torture")) + geom_hline(yintercept = 1, linetype = "dashed")
plot(ggpredict(res3, terms = "polyarchy_median")) + geom_hline(yintercept = 1, linetype = "dashed")


