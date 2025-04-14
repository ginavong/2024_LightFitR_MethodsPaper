# Intro

Repo for the analysis done on the LightFitR methods paper.

Find the package here: https://github.com/ginavong/LightFitR

# File structure

- data: Processed `.Rda` files only (github filesize limits). `x` for raw data,
    - algorithm_testing: Data from testing the programming algorithms. Subfolders named by corresponding figures
    - heliospectra_measurements: Measurements from the Heliospectra light units
        - calibration: Data from calibrating the lights
        - Other folders named by the corresponding figures
    - regimes: Regimes to feed into Heliospectra units in 3 formats to accommodate different firmware versions
- results: Figures and statistical results for the paper. Each folder corresponds to a figure in the paper
- R
    - functions: Functions which are used frequently across scripts
    - data_processing: Processing raw data and feature engineering to allow later analysis. Outputs .csv and .Rda to `data/`. These scripts take a while to run.
    - analysis: Scripts for analysing data and generating figures. Outputs to `results/`
    - regime_design: Scripts which design light regimes. Outputs to `data/regimes/`.
    - `scripts_order.R`: All the scripts to produce analysis and figs for the paper, in the correct order.
- renv: files for the `renv` package to run

## File naming

Scripts: `x.y_description_of_contents` where x=figure it corresponds to; y=step in the sequence of generating the figure. 
e.g. `4.2_etc` is the second script for fig4. 

Dataframes: `x_description_of_contents` where x=figure it corresponds to.
If the dataframe is needed for multiple figures (e.g. calibration), it will not follow this scheme.
All measurements to include date it was taken on, in format `yyyymmdd`


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

Heatmaps: low='white', high='#060038', na.value='#fa9900' 
(may swap low and high for dark mode)

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
| middle_time | boolean | is this the timepoint in the middle of the event? |
| wavelength | numeric | wavelength this row describes |
| peak | boolean | does this wavelength represent the peak of the LED (at the given intensity)? |
| irradiance | numeric | irradiance measured by spectrometer, in  μW cm^-2 nm^-1 |
| watts | numeric | converted to W m^-2 nm^-1 |
| mol | numeric | irradiance converted to mol s-1 m^-2 nm^-1 (via photobiology::as_quantum_mol|
| umol | numeric | irradiance converted to μmol s-1 m^-2 nm^-1 (**not same as PAR**) |
| Additonal | columns | specific to the dataset |

## Calibration data

All calibration data should be in a dataframe with these columns:

| colname | type | description |
| --- | --- | --- |
| LED | numeric | which LED is the row about |
| intensity | numeric | intensity of the LED |
| irradiance | numeric | irradiance measured (specify units)|
