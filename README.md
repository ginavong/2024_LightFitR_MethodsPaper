# Intro

Repo for the analysis done on the LightFitR methods paper.

Find the package here: https://github.com/ginavong/LightFitR

# File structures

- data
    - regimes: Files to load into heliospectra to run the regimes
    - calibration: Data from calibrating the lights
    - light_testing: Data from testing the lights
    - outdoor_irradiance: Measurements of real-world spectra
    - hypocotyls: Hypocotyl data
- figures: Figures for the paper. Each folder corresponds to a figure in the paper
- R
    - functions: Functions which are used frequently across scripts
    - data_processing: Processing raw data and feature engineering to allow later analysis. Outputs .csv and .Rda to `data`. These scripts take a while to run.
    - analysis: Scripts for analysing data and generating figures. Outputs to `figures`
    - regime_design: Scripts which design light regimes. Outputs to `data/regimes`.
- renv: files for the `renv` package to run

# Style guide

| LED   | colour                           | HEX               |
|-------|----------------------------------|-------------------|
| 380   | mediumslateblue                  | #7b68ee           |
| 400   | navy                             | #000080           |
| 420   | royalblue                        | #4169E1           |
| 450   | skyblue                          | #87CEEB           |
| 530   | forestgreen                      | #228b22           |
| 620   | darkorange                       | #FF8C00           |
| 660   | brown1                           | #ff4040           |
| 735   | firebrick                        | #b22222           |
| 5700k | black / white depending on theme |  #000000/ #FFFFFF |

# Standard dataframe formats

## Spectrophotometer readings

The raw ascii / txt files should be processed into a dataframe with at least all these columns:

| colname | type | description |
| --- | --- | --- |
| filename | character | filename that that timepoint was taken from |
| integration_time | numeric | integration time used on spectrometer to get that reading |
| scans | character | number of scans averaged to get that reading |
| time | character | timepoint the reading was taken at |
| event | numeric | event number corresponding to the column number in the regime |
| wavelength | numeric | wavelength this row describes |
| irradiance | numeric | irradiance measured by spectrometer, in  μW cm^-2 nm^-1 |
| watts | numeric | converted to W m^-2 nm^-1 |
| mol | numeric | irradiance converted to mol m^-2 nm^-1 |
| umol | numeric | irradiance converted to μmol m^-2 nm^-1 (not same as PAR) |
| peak | boolean | does this wavelength represent the peak of the LED (at the given intensity)? |
| Additonal | columns | specific to the dataset |

## Calibration data

All calibration data should be in a dataframe with these columns:

| colname | type | description |
| --- | --- | --- |
| LED | numeric | which LED is the row about |
| intensity | numeric | intensity of the LED |
| irradiance | numeric | irradiance measured (specify units)|