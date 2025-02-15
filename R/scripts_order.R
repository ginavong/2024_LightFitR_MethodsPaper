# Setup
rm(list=ls())

# Figure 3 and Supplements

source('R/regime_design/3.0_Calibration_regime.R')
source('R/data_processing/3.1_Apollo_calibration_processing.R')
source('R/analysis/3.2_fig3&sups.R')

# Figure 4 and Supplements

source('R/regime_design/4.0_ComplexityTest_regime.R')
source('R/data_processing/4.1_measurements_processing.R')
source('R/data_processing/4.2_algorithmTests.R')
source('R/analysis/4.3_fig4&sups.R')

# Figure 5 and Supplements

source('R/regime_design/5.0_BaselineMSE.R')
source('R/data_processing/5.1_measurements_processing.R')
source('R/regime_design/5.2_RandomSearch_regime.R')
source('R/data_processing/5.3_random_measurements_processing.R')
source('R/data_processing/5.4_RefinementProcessing.R')
source('R/analysis/5.5_fig5&sups.R')

# Reset

rm(list=ls())