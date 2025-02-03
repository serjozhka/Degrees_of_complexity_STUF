# Degrees_of_complexity_STUF

This archive contains supplementary materials for the paper entitled “Degrees of complexity in valency class systems: implications for efficiency” submitted to STUF include the following files.

## 1. Several spreadsheets containing data used as input for the analysis

### 1.1. languages_preprocessed.xlsx

This file contains basic information about the languages in the BivalTyp sample. It is based on the languges.csv file in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data) but reflects some preprocessing as discussed in the code

### 1.2. data_for_download_preprocessed.xlsx

This file contains the bulk of the BivalTyp data. It is based on the data_for_download.csv file in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data) but reflects some preprocessing as discussed in the code

### 1.3. predicates.xlsx

This file contains an overview of predicates used as BivalTyp's questionnaire. It is based on the file predicates.csv in the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data)

### 1.4. language_stats.csv
This file contains some statistic data on languages of the BivalTyp sample. It is downloaded from the BivalTyp repository (https://github.com/macleginn/bivaltyp/tree/master/data) 

### 1.5. data_published.xlsx
This file contains extended BivalTyp data from the so-called "published" languages (a subset of the sample with full annotations available). This file specifies verb lexemes used in BivalTyp entries and contains comments for every non-verbal predicate.

### 1.6. polysemy_raw_data.xlsx
This file contains additional semi-automated annotations signallin identical verb lemmas when they are found in several different entries in the dataset.

## 2. Output data
languages_everything.xlsx
This files contains a summarizing table created using an R script and containing summary information for individual languages reflexing various aspects of their valency systems. This file was used for the preparation of the Appendix in the paper

## 3. Code
Degrees_of_complexity_STUF.R
This is the R code used for data analysis, including preparing the summarizing table (languages_everything.xlsx), as well as linear mixed-effects models mentioned in the paper and also various visualizations.
