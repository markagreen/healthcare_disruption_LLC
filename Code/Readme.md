# Code

The folder here contains all of the R scripts used for generating the analyses presented in the paper. The following files have been uploaded:

* r_label_helper.R - code originally written by [LLC](https://github.com/UKLLC) which helps to load in data within the LLC Trusted Researcher Environment. Is called from clean_cohorts.R.
* r_label_functions.R - functions to help the loading of files and generation of variable names/labels. Is called from r_label_helper.R.
* clean_cohorts.R - script loads in each cohort individually, combines together waves, keeps and cleans all variables required for the analyses.
* comorbidity_scores.R - script to generate a series of comorbidity indices (Charlson and Elixhauser). The file was written by Richard Thomas of the UK LLC team.
* clean_hes.R - loads the linked electronic health record and administrative datasets, tidy and cleans their variables ready to be linked to the cohort data.
* analyses.R - loads all data in, merges datasets together, and runs all of the analyses presented in the paper.
* sensitivity_analysis.R - runs the sensitivity analysis for models only considering events after the last survey date which are presented in the appendix of the paper. Is called from analyses.R.
* sensitivity_survival.R - script fits a cox regression model to analyse time to hospitalisation as a sensitivity analysis. Is called from analyses.R.
* revised_analyses.R - script that runs all additional analyses that were requested after the first round of paper revisions. The script is standalone.
* create_plots_for_paper.R - script creates the two figures that appear in the revised paper. The script is standalone.

The workflow involves two stages: (i) Run clean_cohorts.R first, followed by comorbidity_scores.R and clean_hes.R to complete the data cleaning stage. (ii) Run analyses.R to generate all of the results in the paper and appendices, followed by revised_analyses.R for additional things as part of the review process.
