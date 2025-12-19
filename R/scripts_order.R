# Setup
rm(list=ls())

# Figure 3 and Supplements

source('R/regime_design/3.0_Calibration_regime.R')
source('R/data_processing/3.1_Apollo_calibration_processing.R') # Ran second time, no changes now
source('R/analysis/3.2_fig3&sups.R')

# Figure 4 and Supplements

source('R/regime_design/4.0_ComplexityTest_regime.R')
source('R/data_processing/4.1_measurements_processing.R') #Reran second time, no changes
source('R/data_processing/4.2_algorithmTests.R') #Reran, no changes
source('R/analysis/4.3_fig4&sups.R')
source('R/analysis/S4_spectrum.R') #ran this, no changes

# Figure 5 and Supplements

source('R/regime_design/5.0_Baseline.R') #Ran, no changes
source('R/data_processing/5.1_measurements_processing.R') #Ran, no changes
source('R/regime_design/5.2_RandomSearch_regime.R') 
source('R/data_processing/5.3_random_measurements_processing.R')
source('R/data_processing/5.4_RefinementProcessing.R')
source('R/analysis/5.5_fig5&sups.R')

# Figure 6 and Supplements

source('R/regime_design/6.0_RFR_regime.R') #Ran this, no changes
source('R/data_processing/6.1_RFR_processing.R') #Ran this, no changes
source('R/analysis/6.2_fig6.R') #Ran, no changes

# Reset

rm(list=ls())