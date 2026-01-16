# Life Expectancy & Healthcare Spending Dashboard (Tableau)

## Overview
This project explores the relationship between the percent of overall spending that goes towards healthcare and any effect that would have on life expectancy. Data cleaning and analysis will be done in R and an interactive visualization will be available in tableau.

## Data
This data joins two country level data sources together, one with government healthcare spending data and the other with life expectancy data.

## Methodology
### Step 1: Exploratory Data Analysis
- EDA is done in R, using tidyverse library to join the two datasets, and examine variable distributions and correlations.
- Dealt with real world missing data determining that the values were truly missing and not errors.
- Created correlation and trend plots in R to showcase direction of the target variables.
### Step 2: Data Cleaning
- Improved the column naming scheme to be more easily understood.
- Got the dataset ready for export to tableau.
### Step 3: Dashboard Creation
- Built an interactive dashboard in Tableau showcasing the following:
    - Lineplots of life expectancy and government percent spending by region
    - Scatterplot with trendline on healthcare spending and life expectancy
    - A global heatmap allowing user to switch between healthcare spending and life expectancy
    - Filtering options of yearspan, countries and regions
## Results
- Countries with higher healthcare spending had significantly improved life expectancy
- There are regional differences on both life expectancy and healthcare spending
- The interactive features allow users to explore these variables themselves on global, regional and country level scales
## DASHBOARD LINK
https://mitchellaldridge.github.io/Tableau-Dashboard-Healthcare/

For best viewing load into fullscreen when on dashboard page.
