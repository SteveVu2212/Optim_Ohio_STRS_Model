# rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
#setwd(getwd())

FileName <- 'Ohio STRS BModel Inputs.xlsx'
YearStart <- 2021
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2015:2121    #(why 2121? Because 120 - 20 + 2021 = 2121)

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to RP-2014 General)
MaleMP <- read_excel(FileName, sheet = 'MP-2016_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2016_Female')#Updated* (to MP-2019)
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount")
TerminationRateVested <- read_excel(FileName, sheet = 'Termination Rates Vested')#Updated*
TerminationRateNonVested <- read_excel(FileName, sheet = 'Termination Rates Non Vested')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
EarlyRetirement_Current <- read_excel(FileName, sheet = 'Early Retirement Current')#Updated*
EarlyRetirement_New <- read_excel(FileName, sheet = 'Early Retirement New')#Updated*
RetirementMultipliers <- read_excel(FileName, sheet = 'Regular Retirement Multipliers')#Updated*

##############################################################################################################################
IsRetirementEligible_Regular <- function(Age, YOS, HireType){
  Check = ifelse((HireType == 'Current') & ((YOS >= 30) | (YOS >= 5 & Age >= 65)), TRUE, 
                 ifelse((HireType == 'New') & ((YOS >= 35 & Age >= 60) | (YOS >= 5 & Age >= 65)), TRUE, FALSE))
  return(Check)
}

IsRetirementEligible_Early <- function(Age, YOS, HireType){
  Check = ifelse((HireType == 'Current') & ((YOS >= 25 & Age >= 55) | (YOS >= 5 & Age >= 60)), TRUE, 
                 ifelse((HireType == 'New') & ((YOS >= 30) | (YOS >= 5 & Age >= 60)), TRUE, FALSE))
  return(Check)
}

IsRetirementEligible <- function(Age, YOS, HireType){
  Check = ifelse(IsRetirementEligible_Regular(Age, YOS, HireType) | IsRetirementEligible_Early(Age, YOS, HireType), TRUE, FALSE)
  
  return(Check)
}

RetirementType <- function(Age, YOS, HireType){
  
  Check = ifelse(IsRetirementEligible_Regular(Age, YOS, HireType), 'Regular',
                 ifelse(IsRetirementEligible_Early(Age, YOS, HireType), 'Early',
                        ifelse(YOS >= 3, 'Withdrawal','None')))
  
  return(Check)
}

##############################################################################################################################

#Custom function to calculate cumulative future values
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

LinearInterpolation <- function(Data,AgeStart,AgeEnd,Columns,Increment){
  TempMatrix <- matrix(0,nrow= (AgeEnd - AgeStart + 1),Columns)
  TempMatrix[,1] <- AgeStart:AgeEnd
  colnames(TempMatrix) <- colnames(Data)
  TempMatrix <- as.data.frame(TempMatrix)
  
  for(i in 1:nrow(Data)){
    Index <- which(TempMatrix[,1] == as.double(Data[i,1]))
    for(j in 2:Columns){
      TempMatrix[Index,j] <- Data[i,j]
    }
  }
  
  for(i in 1:(nrow(Data)-1)){
    for(k in 2:Columns){
      for(j in 1:(Increment - 1)){
        BaseRowIndex <- (i-1)*Increment + 1
        Addition <- (Data[(i+1),k] - Data[i,k]) / (Data[(i+1),1] - Data[i,1])
        TempMatrix[(BaseRowIndex+j),k] <- Data[i,k] + (Addition*j)
      }
    }
  }
  
  return(TempMatrix)
}

#Clean Data
TerminationRateVested <- LinearInterpolation(TerminationRateVested,20,60,3,5)
SalaryGrowth <- LinearInterpolation(SalaryGrowth,20,120,2,5)
SurvivalRates[is.na(SurvivalRates)] <- 0
MaleMP[is.na(MaleMP)] <- 0
FemaleMP[is.na(FemaleMP)] <- 0
AgeNames <- c("52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70")
EarlyRetirement_New <- pivot_longer(EarlyRetirement_New,cols = AgeNames, names_to = 'Age',values_to = 'EarlyRetPct')
EarlyRetirement_New$Age <- as.double(EarlyRetirement_New$Age)

##############################################################################################################################

#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable <- expand_grid(Age, Years, c('Current','New'))
colnames(MortalityTable) <- c('Age','Years','HireType')
SurvivalRates <- SurvivalRates %>% mutate_all(as.numeric)   #why do we need this step?

#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
         FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
         entry_age = Age - (Years - YearStart),
         YOS = Age - entry_age) %>% 
  group_by(HireType, Age) %>%
  
  mutate(MPcumprod_male = cumprod(1 - MaleMP_final),
         MPcumprod_female = cumprod(1 - FemaleMP_final),
         RetirementCond = IsRetirementEligible(Age, YOS, HireType),
         mort_male = ifelse(IsRetirementEligible(Age, YOS, HireType) == F, RP2014_employee_male * ScaleMultipleMaleAct, #Adding adj. facctors
                            RP2014_healthy_retiree_male * ScaleMultipleMaleRet) * MPcumprod_male,
         mort_female = ifelse(IsRetirementEligible(Age, YOS, HireType) == F, RP2014_employee_female * ScaleMultipleFemaleAct,
                              RP2014_healthy_retiree_female * ScaleMultipleFemaleRet) * MPcumprod_female,
         mort = (mort_male + mort_female)/2) %>% 
  #Recalcualting average
  filter(Years >= 2021, entry_age >= 20) %>% 
  ungroup()

#filter out the necessary variables
MortalityTable <- MortalityTable %>% select(Age, YOS, Years, HireType, RetirementCond, entry_age, mort) %>% 
  arrange(HireType, entry_age) 

##############################################################################################################################

#Separation Rates
SeparationRates <- expand_grid(Age, YOS, c('Current','New')) 
colnames(SeparationRates) <- c('Age','YOS','HireType')

SeparationRates <- SeparationRates %>%
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(HireType,entry_age, Age) %>% 
  left_join(TerminationRateVested, by = "Age") %>%
  left_join(TerminationRateNonVested, by = "YOS") %>%
  left_join(RetirementRates, by = "Age") %>%
  ### Additions ###
  #mutate_all(as.numeric) %>% 
  replace(is.na(.), 0)

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type = RetirementType(Age,YOS, HireType),
         
         SepRateMale_Current = ifelse(retirement_type == "Regular" & HireType == 'Current', MaleCurrentHire_More35YOS,
                                      ifelse(retirement_type == "Early" & HireType == 'Current', MaleCurrentHire_Less25YOS + MaleCurrentHire_2529YOS + MaleCurrentHire_3034YOS,
                                             ifelse(retirement_type == 'Withdrawal', NonTermVest_Male + TermVest_Male,0))),
         SepRateFemale_Current = ifelse(retirement_type == "Regular" & HireType == 'Current', FemaleCurrentHire_More35YOS,
                                        ifelse(retirement_type == "Early" & HireType == 'Current', FemaleCurrentHire_Less25YOS + FemaleCurrentHire_2529YOS + FemaleCurrentHire_3034YOS,
                                               ifelse(retirement_type == 'Withdrawal', NonTermVest_Female + TermVest_Female,0))),
         SepRateMale_New = ifelse(retirement_type == "Regular" & HireType == 'New', MaleNewHire_More35YOS,
                                  ifelse(retirement_type == "Early" & HireType == 'New', MaleNewHire_Less25YOS + MaleNewHire_2529YOS + MaleNewHire_3034YOS,
                                         ifelse(retirement_type == 'Withdrawal', NonTermVest_Male + TermVest_Male,0))),
         SepRateFemale_New = ifelse(retirement_type == "Regular" & HireType == 'New', FemaleNewHire_More35YOS,
                                    ifelse(retirement_type == "Early" & HireType == 'New', FemaleNewHire_Less25YOS + FemaleNewHire_2529YOS + FemaleNewHire_3034YOS,
                                           ifelse(retirement_type == 'Withdrawal', NonTermVest_Female + TermVest_Female,0))),
         SepRate_Current = (SepRateMale_Current + SepRateFemale_Current)/2,
         SepRate_New = (SepRateMale_New + SepRateFemale_New)/2) %>% 
  group_by(entry_age) %>% 
  mutate(RemainingProb_Current = cumprod(1 - lag(SepRate_Current, default = 0)),
         RemainingProb_New = cumprod(1 - lag(SepRate_New, default = 0)),
         SepProb_Current = lag(RemainingProb_Current, default = 1) - RemainingProb_Current,
         SepProb_New = lag(RemainingProb_New, default = 1) - RemainingProb_New)%>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age, YOS, entry_age, HireType, RemainingProb_Current, RemainingProb_New, SepProb_Current, SepProb_New)

##############################################################################################################################

#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(Age, YOS, c('Current','New')) 
colnames(SalaryData) <- c('Age','YOS','HireType')

SalaryData <- SalaryData %>%  
  mutate(entry_age = Age - YOS) %>%    #Add entry age
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(HireType, entry_age, Age) %>%
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowth, by = c("Age"))


SalaryData <- SalaryData %>%
  group_by(HireType, entry_age) %>% 
  mutate(Salary = start_sal*cumprod(1+lag(salary_increase,default = 0)),
         FinalAvgSalary_CurrentHire = rollmean(lag(Salary), k = FinAvgSalaryYears_Current, fill = NA, align = "right"),
         FinalAvgSalary_NewHire = rollmean(lag(Salary), k = FinAvgSalaryYears_New, fill = NA, align = "right"),
         DB_EEContrib = (DB_EE_cont + DC_DB_EE_cont)*Salary,
         DBEEBalance = ifelse(YOS < 3, cumFV(0.02, DB_EEContrib),
                              ifelse(YOS >=3 & YOS < 5, cumFV(0.03, DB_EEContrib),
                                     cumFV(0.03, 1.5*DB_EEContrib))),
         DC_EEBalance = DC_EE_cont*Salary,
         DC_ERBalance = 0.2*DC_ER_cont*Salary,
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()


#Survival Probability and Annuity Factor
AnnFactorData <- MortalityTable %>% 
  select(Age, Years, HireType, entry_age, mort) %>%
  group_by(HireType, entry_age) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1+ARR)^(Age - entry_age),
         surv_DR_COLA = surv_DR * (1+COLA)^(Age - entry_age),
         AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
  ungroup()

##############################################################################################################################

ReducedFactor <- expand_grid(Age, YOS, c('Current','New'))
colnames(ReducedFactor) <- c('Age','YOS','HireType')

ReducedFactor <- ReducedFactor %>% 
  arrange(HireType, YOS) %>%
  left_join(EarlyRetirement_Current, by = c("Age", "YOS", "HireType")) %>%
  left_join(EarlyRetirement_New, by = c("YOS","Age")) %>%
  replace(is.na(.), 0) %>% 
  group_by(HireType, YOS) %>% 
  mutate(RetType = RetirementType(Age, YOS, HireType),
         RF_Current = ifelse(RetType == "Early", Pct_Reduced*(1 - InactiveVested),
                             ifelse(RetType == "None", 0, 1)),
         RF_New = ifelse(RetType == "Early", EarlyRetPct*(1 - InactiveVested)/100,
                         ifelse(RetType == "None", 0, 1))) %>% 
  rename(RetirementAge = Age) %>% 
  ungroup() 


BenefitsTable <- expand_grid(Age, YOS, RetirementAge, c('Current','New')) 
colnames(BenefitsTable) <- c('Age','YOS','RetirementAge', 'HireType')

BenefitsTable <- BenefitsTable %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(HireType, entry_age, Age) %>% 
  left_join(SalaryData, by = c("Age", "YOS", "entry_age", "HireType")) %>% 
  left_join(RetirementMultipliers, by = c("YOS")) %>%
  left_join(ReducedFactor %>% select(RetirementAge, YOS, RF_Current, RF_New, HireType), by = c("RetirementAge", "YOS", "HireType")) %>%
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AnnuityFactor, HireType), by = c("RetirementAge" = "Age", "entry_age", "HireType")) %>%
  #Rename surv_DR and AF to make clear that these variables are at retirement
  rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor) %>% 
  #Rejoin the table to get the surv_DR for the termination age
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR), by = c("Age", "entry_age")) %>% 
  
  mutate(BaseBenefit1_Current = 0.022*FinalAvgSalary_CurrentHire*YOS,
         BaseBenefit2_Current = ifelse(YOS >= 35, Cumuative_Mult*FinalAvgSalary_CurrentHire, 0),
         BaseBenefit3_Current = 86*YOS,
         ReducedFactMult = ifelse(HireType == 'Current', RF_Current,
                                  ifelse(HireType == 'New', RF_New, 0)),
         Benefit = ifelse(HireType == 'Current', pmax(BaseBenefit1_Current,BaseBenefit2_Current,BaseBenefit3_Current),
                          ifelse(HireType == 'New', BenMult_New*FinalAvgSalary_NewHire*YOS, 0)),
         AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
         PensionBenefit = ReducedFactMult * Benefit,
         PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj)) %>%
  replace(is.na(.), 0)


#For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
OptimumBenefit <- BenefitsTable %>% 
  group_by(HireType, entry_age, Age) %>% 
  summarise(MaxBenefit = max(PresentValue)) %>%
  mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  ungroup()

##################################################################################################

####### Benefit Accrual & Normal Cost #######
#### Real Pension Wealth = Pension Wealth adjusted for inflation
#### Actuarial PV of Pension Wealth = Pension Wealth 
#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
#####################################
SalaryData <- SalaryData %>% 
  left_join(OptimumBenefit, by = c("entry_age", "HireType", "Age")) %>% 
  left_join(SeparationRates, by = c("Age", "YOS", "entry_age", "HireType")) %>%
  mutate(PenWealth = 0.5*(DBEEBalance + MaxBenefit),  #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
         PenCheck = ifelse(PenWealth == MaxBenefit, TRUE, FALSE),
         RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
         SepProb = ifelse(HireType == 'Current', SepProb_Current, SepProb_New),
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb) %>%
  replace(is.na(.), 0)


#Calculate normal cost rate for each entry age
NormalCost <- SalaryData %>% 
  group_by(HireType, entry_age) %>%
  replace(is.na(.), 0) %>%
  summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
  ungroup()

#View(NormalCost)

SalaryEntry <- left_join(SalaryEntry,NormalCost) %>% filter(HireType == 'New')
#Calculate the aggregate normal cost
NC_aggregate <- sum(SalaryEntry$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start)/
  sum(SalaryEntry$start_sal * SalaryEntry$count_start)

#Calculate the aggregate normal cost
#NC_aggregate  
################################


# ####### DC Account Balance 
# SalaryData2 <- SalaryData %>% 
#   filter(entry_age == HiringAge) %>% 
#   select(Age, YOS, start_sal, entry_age, HireType, salary_increase, Salary, RemainingProb) %>% 
#   mutate(DC_EEContrib = Salary * DC_EE_cont,
#          DC_ERContrib = Salary * DC_ER_cont,
#          DC_Contrib = DC_EEContrib + DC_ERContrib,
#          DC_balance = cumFV(DC_return, DC_Contrib),
#          RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
#   left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
#   mutate(RealHybridWealth = RealDC_balance + RealPenWealth)

## Test benefit function
# 
# NC <- benefit_cal()
# NC2 <- benefit_cal(DB_ARR = 0.06, DB_mult = 0.01)
# DB_4 <- benefit_cal(output = "DB", DB_ARR = 0.04, ea = 22)
# DB_7 <- benefit_cal(output = "DB", DB_ARR = 0.07, ea = 22)
# DC <- benefit_cal(output = "DC", DCreturn = 0.06, ea = 22)
# attri <- benefit_cal(output = "attrition", ea = 22)
# 
# 
# test <- DB_4 %>%
#   left_join(DB_7, by = "Age") %>%
#   left_join(DC, by = "Age") %>%
#   pivot_longer(cols = 2:4,
#                names_to = "type",
#                values_to = "wealth")
# 
# ggplot(test, aes(x = Age, y = wealth, col = type)) +
#   geom_line()
# 
# ggplot(attri, aes(x = Age, y = RemainingProb)) +
#   geom_line()
##################################
