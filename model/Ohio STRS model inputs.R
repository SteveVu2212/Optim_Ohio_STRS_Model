##################################################################
##                  Model Inputs & Assumptions                  ##
##################################################################


#1. Actuarial and economic assumptions:
dr_current_ <- 0.07   #discount rate for current members
dr_new_ <- 0.07   #discount rate for new members
payroll_growth_ <- 0.03   #payroll growth assumption
pop_growth_ <- 0   #plan's active population growth assumption
inf_rate <- 0.025

#2. Benefit assumptions:
#Cost of living adjustment (COLA) assumptions:
COLA_current_active <- 0   #COLA for current active members
COLA_new_active <- 0   #COLA for new active members (note that this applies to the cash balance plan too if new members go to the CB plan)
COLA_current_retire <- 0   #COLA for current retirees
COLA_current_retire_one <- 0   #One-time COLA for current retirees
one_time_cola_ <- F   #One-time COLA or not? True means yes, False means no


#3. Funding assumptions
funding_policy_ <- "statutory"
ee_cont_rate_DB <- 0.14
ee_cont_rate_Hybrid <- 0.02
er_cont_rate_stat <- 0.14  


#Amortization policy
amo_pay_growth_ <- 0.03
amo_period_current_ <- 30
amo_period_new_ <- 30
amo_method_ <- "level %"


#4. Investment assumptions
return_scen_ <- "assumption"
model_return_ <- 0.07


#5. Plan design assumptions
db_legacy_ratio_ <- 0.94     #for those hired between 2001 and today
db_new_ratio_ <- 0.94        #for those hired after today


#6. Model assumptions 
ModelPeriod <- 30    #Projection period (typically 30 years)
MinAge <- 20          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2022     #Year of the latest val report
MinYear <- 1980       #No hard rule about this. Should get back to about 40 years from now.   
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)
RetYear <- MinYear:(YearStart + ModelPeriod)
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age


retiree_pop_current <- 156225
ben_payment_current <- 7167927000
retire_refund_ratio_ <- 0.8

#Model Calibration
cal_factor_ <- 1
nc_cal_DB <- 10.86 / 12.22036
nc_cal_Hybrid <- 4.58 / 5.754480
ben_cal <- 0.97

#Assumptions about the "remaining" accrued liability (which is not accounted for by the PVFB calculations). This is lumped together with the accrued liability for current term vested members.
PVFB_term_current <- 105264324785 - 87911316447
amo_period_term <- 50
amo_term_growth <- 0.03

#7. Import key data tables
FileName <- 'Ohio STRS data 2022.xlsx'

SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to Pub 2010 General)
MaleMP <- read_excel(FileName, sheet = 'MP-2020_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2020_Female')#Updated* (to MP-2019)

SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)
TerminationRateVested <- read_excel(FileName, sheet = 'Termination Rates Vested')#Updated*
TerminationRateNonVested <- read_excel(FileName, sheet = 'Termination Rates Non Vested')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
EarlyRetirement_Before2015 <- read_excel(FileName, sheet = 'Early Retirement before 2015')#Updated*
EarlyRetirement_After2015 <- read_excel(FileName, sheet = 'Early Retirement after 2015')#Updated*
RetirementMultipliers <- read_excel(FileName, sheet = '2015 Retirement Multipliers')#Updated*
SalaryMatrix <- read_excel(FileName, sheet = "Salary Distribution")
HeadCountMatrix <- read_excel(FileName, sheet = "HeadCount Distribution")
SalaryEntry_ <- read_excel(FileName, sheet = "Entrant Profile")

RetireeDistribution <- read_excel(FileName, sheet = "Retiree Distribution") 

funding_data <- read_excel(FileName, sheet = "Funding Data")
return_scenarios <- read_excel(FileName, sheet = "Return Scenarios")
