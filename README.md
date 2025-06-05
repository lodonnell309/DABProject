# Analyzing the Influence of Clean Energy Production in Developed Countries on Developing Countries

Team 4's group project GitHub repository for MGT 6203 (Edx) Fall of 2023 semester.

## Table of Contents

* [Important Links](#important-links)
* [Folder Structure and Contents](#folder-structure-and-contents)
* [Required R Packages](#required-r-packages)
* [Instructions](#instructions)
  1. [Opening the Project](#1-opening-the-project)
  2. [Running the Data Preprocessing Script](#2-running-the-data-preprocessing-script)
  3. [Running the Generation Visualization Script](#3-running-the-generation-visualization-script)
  4. [Running the Clean Capacity by HDI Exploratory Script](#4-running-the-clean-capacity-by-hdi-exploratory-script)
  5. [Running the Clean Capacity Crosscorrelation Visualization Script](#5-running-the-clean-capacity-crosscorrelation-visualization-script)
  6. [Running the Primary Time Series Model Building Scripts](#6-running-the-primary-time-series-model-building-scripts)
  7. [Running the Final Primary Time Series Model Building Script](#7-running-the-final-primary-time-series-model-building-script)
  8. [Running the GDP vs. Clean Energy Model Building Script](#8-running-the-gdp-vs-clean-energy-model-building-script)

## Important Links

* [Proposal](https://github.com/MGT-6203-Fall-2023-Edx/Team-4/blob/main/Project%20Proposal/team4proposal.pdf)
* [Archived Original Datasets](https://1drv.ms/f/s!AidNWjxCQLvGlGqTcG3Mksmakbde?e=wEYwLk)
* [Data Dictionary](https://github.com/MGT-6203-Fall-2023-Edx/Team-4/blob/main/Data/original/Data%20Dictionary.md)

## Folder Structure and Contents

* `Code`
  * `exploratory`: Visualizations/ANOVA model for HDI code and clean energy capacity
  * `notebooks`: Exploratory and experimental Jupyter notebooks
  * `preprocessing`: Data cleaning/merging
  * `primary_modeling`: Modeling related to the primary research question
  * `gdp_vs_energy`: Modeling related to supporting research question
* `Data`
  * `original`: Project's data dictionary
* `Final Report`: Placeholder for final report deliverable
* `Gantt`: Project gantt chart/timeline.
* `Other Resources`: Miscellaneous files that do not fit in the other categories
* `Progress Report`: Team 4 Progress Report deliverable
* `Project Proposal`: Team 4 Project Proposal deliverable
* `Visualizations`
  * `exploratory`: PNG files of EDA
  * `primary_modeling`: PNG files of exported visuals related to the primary modeling
  * `visualizations_code`: R code for exploratory visualizations

## Required R Packages

* astsa
* dplyr
* forecast
* gdata
* ggplot2
* ggpubr
* scales
* tidyr

## Instructions

### 1. Opening the Project

**It is recommended to open the project in RStudio using the .RProj file at the root of the repository.** This will ensure the working directory is set correctly relative to your local copy of the checked out project.

All scripts assume that the working directory is the root of the project, unless otherwise specified. Also note that the Code folder contains a notebooks subfolder. If you would like to set up a Jupyter Server on your machine, instructions can be found [here](https://github.com/MGT-6203-Fall-2023-Edx/Team-4/blob/main/Code/notebooks/Installing%20JupyterLab.md).

### 2. Running the Data Preprocessing Script

1. Locate and open the file at `Code/preprocessing/unified_data_prep.R`.
2. Run or source the file.
3. The processed data will be output to `Data/unified/data_unified_raw.csv`.

### 3. Running the Generation Visualization Script 

1. Locate and open the file at `Visualizations/visualizations_code/usage_visualizations.R`.
3. Run or source the file. 
4. Plots are output to the RStudio plot viewer unless the `ggsave` calls are uncommented where noted in the file.

### 4. Running the Clean Capacity by HDI Exploratory Script

1. Locate and open the file at `Code/exploratory/capclean_by_hdi.R`
2. Run or source the file.
3. Plots are output to the RStudio plot viewer.

### 5. Running the Clean Capacity Crosscorrelation Visualization Script

1. Locate and open the file at `Code/primary_modeling/capcleanproportion_corr_viz.R`
2. Run or source the file.
3. Plots are output to the RStudio plot viewer.

### 6. Running the Primary Time Series Model Building Scripts

1. Open the folder `Code/primary_modeling` and locate each of the following files:
    1. `primary_africa.R`
    2. `primary_asia.R`
    3. `primary_north_america.R`
    4. `primary_oceania.R`
    5. `primary_south_america.R`
2. For each of the files listed above, run or source the file.
3. Plots are output to the RStudio plot viewer.

### 7. Running the Final Primary Time Series Model Building Script

1. Locate and open the file at `Code/primary_modeling/best_arima_models.R`
2. Run or source the file.
3. Plots are output to the RStudio plot viewer.

### 8. Running the GDP vs. Clean Energy Model Building Script

1. Locate and open the file at `Code/gdp_vs_energy/GdpAndCleanEnergy.R`
2. Run or source the file.
3. Plots are output to the RStudio plot viewer.
