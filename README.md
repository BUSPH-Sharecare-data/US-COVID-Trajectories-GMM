We have provided a single program and dataset that includes all variables required to generate the tables and figures, as well as separate programs, datasets and steps to allow one to recreate the full dataset (dat.RData) for reproducibility.

Programs (4):
•	COVID_for_Publish.sas: generates the analytic datasets
•	GMM_for_Publish.R: runs growth mixture models to produce covid trajectory classes
•	COVID_trajectory_analysis_for_Publish.R: generates manuscript tables and figures 
•	SDOH_for_Publish.sas: generates SDOH overall and sub-index scores

Datasets (3) (refer to “Data dictionary.xlsx” for content detail): 
•	dat.RData: used to generate the tables and figures
•	sdoh_county_2020_covid_full.xlsx: used to generate the SDOH overall and sub-index scores
•	County_stcofips_2018v3.xlsx: 2010 decentennial geographies

dat.RData dataset: N=3,142 counties based on the 2010 decentennial geography. Dataset includes COVID-19 weekly incidence rates by time period evaluated; COVID-19 IR trajectory class membership; 4 demographic variables; and overall SDOH index scores (sub-index scores, if of interest, can be generated using instructions provided below). WBI data is not provided as it is proprietary. Please contact the investigators if you would like more information about the WBI. Demographic data source: ACS, 2020. COVID-19 IR data source: JHU, 2020. SDOH data source: multiple sources (referenced in manuscript), 2019.

To generate tables and figures:
•	Run COVID_trajectory_analysis_for_Publish.R
•	Dataset: dat.RData

To recreate the COVID-19 incidence rate, IR trajectory class, and SDOH overall and sub-index score  (including psychometric analysis) variables:

1.	Generate 3 analytic datasets to create COVID measures 
•	Pull COVID-19 data from Johns Hopkins Center for Systems Science and Engineering: JHU_Centers_for_Civic_Impact_Covid-19_County_Cases__Daily_Update_11032020.csv; time_series_covid19_confirmed_US.csv
•	Subset the 3,142 counties of interest using 2010 decentennial geographies: County_stcofips_2018v3.xlsx
•	Use COVID_for_Publish.sas to generate the analytic datasets. Output files:
a.	Ir_v1_1.csv, Ir_v2_1.csv, Ir_v3_1.csv 

2.	Generate COVID-19 IR trajectory classes and accompanying model fit statistics, figures and latent class membership files
•	Use GMM_for_Publish.R to run growth mixture models for each period
•	Datasets: Ir_v1_1.csv, Ir_v2_1.csv, Ir_v3_1.csv

3.	Generate SDOH overall and sub-index scores
•	Using SDOH_for_Publish.sas:  import dataset sdoh_county_2020_covid_full.xlsx, which includes raw values of SDOH items
