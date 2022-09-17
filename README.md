# PK_Vacc_Project

1. PreRun: Source code for necessary libraries and functions that are utilized in the below files.

2. Assemble Covariates: Load the geographic shapefiles and use these as base datasets to further join both covariates and vaccination data to. In this file, UC and tehsil level data is joined with various covariate data sources.

3. Integrate MICS Covariates: In this file, further demographic and economic covariate data from MICS cluster surveys are joined with the UC and tehsil level data.

4. GetEPIData: In this file, data from EPI datasets is joined with UC and tehsil level data and utilized to attain vaccination and vaccination coverage rates. It is this data that is used to render the target variables utilized in models. Note that our target variables will consist of 1. In Clinic Vaccination Coverage Rates; 2. Outreach Vaccination Coverage Rates; 3. ratio of Outreach to Clinic Vaccinations.

5. model building: 5a. Clinics Model: Find significant features using RFE and Boruta, build and tune models to determine significant predictors of In-Clinic Vaccination Coverage. 5b. Outreach Model: Find significant features using RFE and Boruta, build and tune models to determine significant predictors of Outreach Vaccination Coverage. 5c. Proportion Model: Find significant features using RFE and Boruta, build and tune models to determine significant predictors of the ratio of Outreach to Clinic Vaccinations.
