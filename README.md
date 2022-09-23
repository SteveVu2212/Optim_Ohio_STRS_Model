# Ohio STRS & OPERS


## Historical

### Work Allocation:
  - ***Anil***:
      - Products: `assets_liability.csv`, `gain_loss.csv`, `interest.csv`, `net_amo.csv`, `returns.csv`, and `allocation.csv`
  - ***Swaroop***: 
      - Products: `simulation_data.csv` and `probability_table.csv`
  - ***Truong***:
      - Possible data validator (especially on debt charts)
  - ***Jordan***:
      - Format/process gain/loss and histogram data/charts
      - Integrate into dashboard

### Assets & Liabilities 
  - **Mountain of Debt / Asset & Liability Difference**
      - *File Name*: `assets_liability.csv`
      - Notes:
        - Column names: `c(year, ava, mva, aal)`

### Pension Debt
  - **Gain/Loss**
      - File Name: `gain_loss.csv`
      - Notes:
        - Column names: `c(year, name_label, loss, gain, net)`
        - The year column indicates starting year of analysis
        - The loss, gain, and net columns (which could be combined into one column) are seperated for easier processing of the data.
  - **Cumulative Interest**
      - File Name: `interest.csv`
      - Notes:
         - Column names: `c(year, net_amo, rem_ual)` 
  - **Contribution**
      - File Name: `net_amo.csv`
      - Notes:
         - Column names: `c(year, contributions, interest, neg_amo)` 

### Assets & Returns
  - **Investment Returns**
      - File Name: `returns.csv`
      - Notes:
        - Column names: `c(year, arr, mva, ava)`
  - **Asset Allocation**
      - File Name: `allocation.csv`
      - Notes:
        - Column names: `c(year, private_equity, real_estate, hedge_funds, commodities, alternatives, public_equities, fixed_income, cash, total)`
        - For these data, I do not expect column names to be the same for every plan. Broadly speaking, we tend to report: fixed income, public equities, private equities, and alternatives.

### Investment Return Analysis
  - **Histogram**
      - File Name: `simulation_data.csv`
      - Notes:
        - Column names: `c(Data, Plan Name, Mean Return)`
        - The data used here are in long form. They come directly from the investment return analysis script, but partially down.
  - **Table**
      - File Name: `table.csv`
      - Notes:
        - Column names: `c(Return, BlackRock, Research Affiliates, JP Morgan, BNY Mellon, Horizon10, Horizon20, Plan Assumption, Historical)`
        - This data come directly from the investment return analysis Excel. My only note is that often I have to rerun because the returns around the assumed return are not adjusted to the specific plan.

<hr>

## Funding


<hr>

## Benefits
 
 
