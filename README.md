# EU Subnational Data Validation

The present repository contains the R code used by the DAU to conduct internal and external validation on the pretest and full fieldwork data from each surveyed country. In order to prepare these analyses, the DAU created a data validation plan to carry out the process.

## Files description
In this repository you will find two main folders:

1. TPS
2. CD-valid

The `TPS` directory contains all of the data cleaning functions and combined data file of the Third Party Source data collected by the DAU in preparation of the external validation process. Since each TPS source has a different data structure, each one must be cleaned in different ways. Thus, the folders under `Cleaning` are all named with a three-letter abbreviation of the TPS source and contain the unique function used to reorient, normalize, and aggregate the selected variables from that source by country. 

The `CD-valid` directory contains the functions used for analysis, including the additional contextual information matched with each indicator. The two analyses include a t-test to evaluate score change over time and a threshold difference to evaluate inconsistencies with Third Party Source scores. The `Input` directory contains a dummy file of pretest data as well as the previous GPP data and the aggregated TPS data from the `TPS` directory used for comparison. 

The data to be used in this project is micro data from each TPS database as well as the eu_merge.dta file managed by the organization. The outcomes are stored in .xlsx and .html format following this filing system: `Outcomes/Pretest/COUNTRY_NAME Validation Report.xlsx` or `Outcomes/Pretest/COUNTRY_NAME Validation Report.html`

Due to the privacy disclosures followed by the DAU (see the [DAU R Coding Handbook](https://ctoruno.quarto.pub/wjp-r-handbook/)), the contents of the `Inputs` and `Outcomes` directories are limited on the GitHub repository and their full content can only be accessed through the WJP SharePoint.

## Code logic

The code is different in each main folder:

#### TPS Directory

1. Upper level: A single file controls the entire folder's program and it is the `RunMe.R` script. This is a "_single-call_" file and it is designed to just be called alone. Once that it is called, the script will start generating all the clean and aggregated TPS data. It will also merge these files together as well as with a file containing general country information to create `TPS_data.xlsx`.

2. Cleaning Functions: The RunMe file will call several cleaning functions, one per TPS data file, applying each cleaning function to its respective TPS data file. Each TPS function can be found in the `Cleaning` directory inside its respective abbreviation. For example, to generate the clean data for the V-Dem data, RunMe will call out the `VDM_clean()` function. Given that V-Dem's abbreviation is VDM, the cleaning function can be found in the `VDM/VDM_function.R` file. All of the cleaning functions perform five steps: (1) identifying indicators, (2) subsetting the data, (3) reorienting scores so that lower numbers are worse in terms of the rule of law, (4) normalizing scores to fall between 0 and 1, and (5) aggregating scores to one score per indicator per country.

There is an auxiliary file called `setting.R`. As its name could suggest, inside this file are all the pre-settings needed by the `RunMe.R` to perform smoothly, including required libraries and local path information.

#### CD-Valid Directory

1. Upper level: A single file controls the entire folder's program and it is the `RunMe.R` script. This is a "_single-call_" file and it is designed to just be called along with a two arguments that contain the name of the country we would like to generate the report for, and the name of the person running it. Once that it is called, the script will start running the analyses and printing out two outputs: an excel with the analysis result tables and an HTML dynamic report file.

2. Analysis Functions: The RunMe file will call the two analysis functions. The t-test analysis can be found under `Code/time_changes.R` and the threshold difference analysis is under `Code/TPS.R`. The `time_changes()` function cleans the pretest data and the previous data from the given country before performing a t-test on a subset of indicators and printing out the results for each indicator in a table. The `tps_comparisson()` function cleans the pretest data and pairs up a selection of indicators with previously matched TPS indicators from `TPS/TPS_data.xlsx` to calculate the difference between both scores. These results are also printed out in a table. 

   Both sets of results include a flagging system to identify small, moderate, and large differences in our comparisons. Regarding the t-test, a red flag indicates a p-value < 0.01, a yellow flag indicates a p-value between 0.1 and 0.01, and a green flag indicates a p-value > 0.1. In terms of the threshold analysis, a red flag indicates a difference > 0.3, a yellow flag indicates a difference between 0.3 and 0.15, and a green flag indicates a difference < 0.15.

3. Report Generation: In order to further understand the results of the analyses, a validation report is created for the selected country by the RunMe calling the `Code/Country Report Template.Rmd` file. This rmarkdown is abstracted so that it does not need to be changed for each country's data, and certain text are automated to change given the data. It produces summary tables for the t-test analysis and splits the threshold analysis into two groups: comparisons with population polls and comparisons with expert surveys. The Rmd template also calls `Code/paragraph.R` which contains unique finding summaries for each country's data written with HTML formatting. This is the only part of the text that is country-specific in the report.

   The Guidelines tab describes some of the methodology and how the report will be used by the WJP. The Country Level Findings tab summarizes the overall issues and the number of each flag color in each of WJP's pillars. The appendix tab is divided into sections for each pillar, and then sub-sections for each analysis. The tables in each sub-section detail each comparison made and the result, with the row color indicating the flag color. Finally, the search tab allows the user to search for a specific indicator to view its instance in all the analyses in which it was used.

There is an auxiliary file called `setting.R`. As its name could suggest, inside this file are all the pre-settings needed by the `RunMe.R` to perform smoothly, including required libraries, local path information, and the function used to subset, reorient, and normalize our pretest data. 

## How to use

#### TPS Directory

As mentioned above, in order to generate the cleaned and merged TPS data, you just need to call the RunMe.R file. However, if you would like to change the data2update argument in the RunMe to only include specific sources, below are the abbreviations:

1. **European Social Survey**: Pass the string "_ESS_" as an argument to the data2update call in order to generate the clean data for the European Social Survey.

2. **European Working Conditions Survey**: Pass the string "_EWC_" as an argument to the data2update call in order to generate the clean data for the European Working Conditions Survey.

3. **Freedom in the World**: Pass the string "_FIW_" as an argument to the data2update call in order to generate the clean data for the Freedom in the World data.

4. **Flash Eurobarometer**: Pass the string "_FLE_" followed by an underscore and the three number code of the desired survey as an argument to the data2update call in order to generate the clean data for a Flash Eurobarometer. For example, "FLE_507" indicates Flash Eurobarometer 507.

5. **Fundamental Rights Survey**: Pass the string "_FRS_" as an argument to the data2update call in order to generate the clean data for the Fundamental Rights Survey.

6. **Global Corruption Barometer**: Pass the string "_GCB_" as an argument to the data2update call in order to generate the clean data for the Global Corruption Barometer.

7. **Government Transparency Index**: Pass the string "_GTI_" as an argument to the data2update call in order to generate the clean data for the Government Transparency Index.

8. **Organized Crime Index**: Pass the string "_OCI_" as an argument to the data2update call in order to generate the clean data for the Organized Crime Index.

9. **Public Integrity Index**: Pass the string "_PII_" as an argument to the data2update call in order to generate the clean data for the Public Integrity Index.

10. **Special Eurobarometer**: Pass the string "_SPE_" followed by an underscore and the three number code of the desired survey as an argument to the data2update call in order to generate the clean data for a Flash Eurobarometer. For example, "SPE_043" indicates Special Eurobarometer 043.

11. **World Values Survey**: Pass the string "_WVS_" as an argument to the data2update call in order to generate the clean data for the World Values Survey.

12. **Varieties of Democracy**: Pass the string "_VDM_" as an argument to the data2update call in order to generate the clean data for the Varieties of Democracy (V-Dem) survey.

In order to call the RunMe.R, open  your terminal and run the following lines:

```
$ cd PATH_TO_LOCAL_REPOSITORY
$ Rscript --vanilla RunMe.R
```


In order to call the RunMe.R with the respective country group, open  your terminal and run the following lines:

```
$ cd PATH_TO_LOCAL_REPOSITORY
$ Rscript --vanilla RunMe.R "andean"
```

The script will run entirely and you will now have the final visualizations in the `Outcomes` directory, ordered by country and figure number. 

#### CD-Valid Directory

As mentioned above, in order to generate the analysis results and the validation report, you need to call the RunMe.R file with the country and author arguments. Below is the list of country arguments available:

**"Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain",      "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia"**

## Contact
For inquiries please contact _Dalia Habiby_ (dhabiby@worldjusticeproject.org), _Carlos Toruño_ (ctoruno@worldjusticeproject.org) or _Santiago Pardo_ (spardo@worldjusticeproject.org). For general information, please contact _Ana María Montoya_ (amontoya@worldjusticeproject.org).
