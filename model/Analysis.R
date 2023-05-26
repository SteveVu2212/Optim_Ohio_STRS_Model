##################################################################
##                           Analysis                           ##
##################################################################

source("Ohio STRS master.R")

#Target columns
target_col <- c("fy", "ROA", "AAL", "MVA", "AVA", "FR_MVA", "UAL_MVA_real", "er_cont_rate", "er_cont_real", "all_in_cost_real")


#statutory funding (30 current), baseline return, no COLA
stat_baseline_noCOLA_30 <- get_funding_data() %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 30,
         return_scen = "baseline",
         cola = "no")

#statutory funding (30 current), recur recession, no COLA
stat_stress_noCOLA_30 <- get_funding_data(return_scen = "recur_recession") %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 30,
         return_scen = "recur_recession",
         cola = "no")

#statutory funding (30 current), baseline return, COLA
stat_baseline_COLA_30 <- get_funding_data(one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 30,
         return_scen = "baseline",
         cola = "yes")

#statutory funding (30 current), recur recession, COLA
stat_stress_COLA_30 <- get_funding_data(return_scen = "recur_recession", one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 30,
         return_scen = "recur_recession",
         cola = "yes")

#statutory funding (15 current), baseline return, no COLA
stat_baseline_noCOLA_15 <- get_funding_data(amo_period_current = 15) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 15,
         return_scen = "baseline",
         cola = "no")

#statutory funding (15 current), recur recession, no COLA
stat_stress_noCOLA_15 <- get_funding_data(amo_period_current = 15, return_scen = "recur_recession") %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 15,
         return_scen = "recur_recession",
         cola = "no")

#statutory funding (15 current), baseline return, COLA
stat_baseline_COLA_15 <- get_funding_data(amo_period_current = 15, one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 15,
         return_scen = "baseline",
         cola = "yes")

#statutory funding (15 current), recur recession, COLA
stat_stress_COLA_15 <- get_funding_data(amo_period_current = 15, return_scen = "recur_recession", one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "statutory",
         current_debt_amo_period = 15,
         return_scen = "recur_recession",
         cola = "yes")

#ADC funding (30 current), baseline return, no COLA
adc_baseline_noCOLA_30 <- get_funding_data(funding_policy = "ADC") %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 30,
         return_scen = "baseline",
         cola = "no")

#ADC funding (30 current), recur recession, no COLA
adc_stress_noCOLA_30 <- get_funding_data(funding_policy = "ADC", return_scen = "recur_recession") %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 30,
         return_scen = "recur_recession",
         cola = "no")

#ADC funding (30 current), baseline return, COLA
adc_baseline_COLA_30 <- get_funding_data(funding_policy = "ADC", one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 30,
         return_scen = "baseline",
         cola = "yes")

#ADC funding (30 current), recur recession, COLA
adc_stress_COLA_30 <- get_funding_data(funding_policy = "ADC", return_scen = "recur_recession", one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 30,
         return_scen = "recur_recession",
         cola = "yes")

#ADC funding (15 current), baseline return, no COLA
adc_baseline_noCOLA_15 <- get_funding_data(funding_policy = "ADC", amo_period_current = 15) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 15,
         return_scen = "baseline",
         cola = "no")

#ADC funding (15 current), recur recession, no COLA
adc_stress_noCOLA_15 <- get_funding_data(funding_policy = "ADC", amo_period_current = 15, return_scen = "recur_recession") %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 15,
         return_scen = "recur_recession",
         cola = "no")

#ADC funding (15 current), baseline return, COLA
adc_baseline_COLA_15 <- get_funding_data(funding_policy = "ADC", amo_period_current = 15, one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 15,
         return_scen = "baseline",
         cola = "yes")

#ADC funding (15 current), recur recession, COLA
adc_stress_COLA_15 <- get_funding_data(funding_policy = "ADC", amo_period_current = 15, return_scen = "recur_recession", one_time_cola = T, cola_current_retire_one = 0.01) %>% select(any_of(target_col)) %>% 
  mutate(funding_policy = "adc",
         current_debt_amo_period = 15,
         return_scen = "recur_recession",
         cola = "yes")



#Export outputs
analysis_output <- rbind(stat_baseline_noCOLA_30,
                         stat_stress_noCOLA_30,
                         stat_baseline_COLA_30,
                         stat_stress_COLA_30,
                         stat_baseline_noCOLA_15,
                         stat_stress_noCOLA_15,
                         stat_baseline_COLA_15,
                         stat_stress_COLA_15,
                         adc_baseline_noCOLA_30,
                         adc_stress_noCOLA_30,
                         adc_baseline_COLA_30,
                         adc_stress_COLA_30,
                         adc_baseline_noCOLA_15,
                         adc_stress_noCOLA_15,
                         adc_baseline_COLA_15,
                         adc_stress_COLA_15)


write.xlsx(analysis_output, "analysis_output.xlsx", overwrite = T)
