Raw data available at: https://doi.org/10.5281/zenodo.15584173

OceanView .txt files cannot be stored on GitHub because of file sizes.

# Fig 3

## `data/regimes/calibration`

Calibration regime in 3 formats to support different Heliospectra firmware versions.

## `data/heliospectra_measurements/calibration/Apollo_Calib_20240827/`

Calibration data for Apollo (name for one of our Heliospectra DYNA lights), collected on 2024 August 27 by running the regime.

- Apollo_calibration_annotated_20240827.Rda: Annotated calibration data ready for analysis. Derived by filtering raw data for the time window of the calibration and trimming the wavelengths from 300-800nm. Then annotated using the calibration regime.
- Apollo_calibration_bleedthrough_20240827.Rda: Bleedthrough data calculated from annotated calibration data.
- Apollo_calibration_medianPeaks_20240827.Rda: Median peak wavelengths calculated from annotated calibration data.
- Apollo_calibration_raw_20240827.Rda: Raw calibration data imported from OceanView's .txt output, with no filtering or annotation.
- Apollo_calibration_total_20240827.Rda: Total irradiance per event. Derived from annotated calibration data.

# Fig 4

## `data/regimes/fig4_ComplexityTest/`

Regime for testing different algorithms at various complexities, in 3 formats to support different Heliospectra firmware versions. Created with `R/regime_design/4.0_ComplexityTest_regime.R`.

## `data/heliospectra_measurements/fig4_20240905/`

Heliospectra measurements collected on 2024 September 05 by running the regime.

- 4_annotated_20240905.Rda: Annotated data ready for analysis. Derived by filtering raw data for the time window of the calibration and trimming the wavelengths from 300-800nm. Then annotated using the calibration regime.
- 4_raw_20240905.Rda: Raw calibration data imported from OceanView's .txt output, with no filtering or annotation.

## `data/algorithm_testing/fig4_algorithm_comparisons/`

- 4_algorithmTests.Rda: Different types of algorithms were run and their accuracy calculated, along with summary stats.
- 4_targetIrradiances_20240905: Target irradiances for testing the different types of algorithms. Derived from `data/heliospectra_measurements/fig4_20240905/4_annotated_20240905.Rda`

# Fig 5

## `data/regimes/fig5_Refinement/`

- 5_Baseline: Regime for baseline / algorithm intensities measurements.
- 5_RandomSearch: Regime for random search. Derived from measurements of baseline, with a random search applied (`R/regime_design/5.2_RandomSearch_regime.R`)

## `data/heliospectra_measurements/fig5/`

- baseline_20250218: Baseline / algorithm intensities measurements collected on 2025 February 18.
- RandomSearch_20250218: Measurements for the random search.
- depreciated: Old measurements that didn't work.

## `data/algorithm_testing/fig5_refinement/`

Data for analysis of fig 5 refinement process. All dataframes in this folder are in the same format.

- depreciated: From old attempts that didn't work.
- 5.0_BaselineForRefinement_20250201.Rda: Dataframe containing intensities used, in correct format for analysis. Generated from `R/regime_design/5.0_Baseline.R`
- 5_baseline_mse.Rda: Dataframes with residuals and MSE for the baseline / algorithm intensities. Derived from `data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement_20250201.Rda` and `data/heliospectra_measurements/fig5/baseline_20250218/5_baseline_annotated_20250218.Rda`
- 5_RandomRefinement.Rda: Dataframes with residuals and MSE for random search. Derived from `data/heliospectra_measurements/fig5/RandomSearch_20250218/5a_annotated_20250218.Rda`
- 5_refinementCollated.Rda: Dataframes with residuals and MSE for the entire refinement process. Collated from `5_baseline_mse.Rda` and `5_RandomRefinement.Rda`

# Fig 5b

## `data/algorithm_testing/fig5b_R_FR_demo`

Target data for figure 5b which uses Heliospectra lights to immitate the real-life R:FR observed in: https://doi.org/10.1016/j.agrformet.2020.108041

## `data/regimes/fig5b_R_FR_demo`

Regimes used on the Heliospectra lights.

## `data/heliospectra_measurements/fig5b`

Data of the spectrometer measurements from running the regime on the lights.

- 5b_raw_20260105.Rda: Raw spectrum data collected on 2026 January 05.
- fig5b_data_20260105.Rda: Processed spectrum data

