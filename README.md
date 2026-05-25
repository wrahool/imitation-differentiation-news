# Imitation and Differentiation in News Coverage in the US
Replication scripts for imitation and differentiation in news production project by Subhayan Mukerjee, Tian Yang, and Yilang Peng.

Download the derived data from the OSF repo of the original study: https://osf.io/htve7/

To run these scripts
- change the `data_folder` variable to reflect the path to the folder that contains all the `.csv` files (downloaded from the above repo)
- create a subfolder called `tables/` outside the scripts folder
- create a subfolder called `figures/` outside the scripts folder

Your working directory should have three subfolders in it:
- `scripts/` containing all the scripts in the repository
- `tables/` which will contain the results (as `.tex` tables) 
- `figures/` which will contain the plots (as `.svg` images)

Details about the scripts are as follows:

The main findings are based on the following five scripts.
- `01_estimate-intermedia-influence.R` estimates the pairwise intermedia influence between every pair of outlets in the coverage of all topics
- `02_estimate-intermedia-influence-political.R` estimates the pairwise intermedia influence between every pair of outlets in the coverage of political topics
- `03_estimate-intermedia-influence-entertainment.R` estimates the pairwise intermedia influence between every pair of outlets in the coverage of entertainment topics
- `04_LR-QAP.R`  uses results from the previous three scripts to estimate Logistic Regression QAP models to predict the likelihood of the existence of positive or negative influence between a pair of outlets, given their ideological distance.
- `05_LR-QAP-2.R` uses results from the first three scripts to estimate the Logistic Regression QAP models to predict the likelihood of the existence of positive or negative influence between a pair of lutlets, given sameness or difference in the ideological leanings of the incident nodes.

Various robustness tests are executed in scripts following the naming pattern `0[1-5]R-*.R`
A robustness test using an alternative media slant score (from Ad Fontes media) can be conducted by running the following scripts:
- `04R_LR-QAP_adfontes.R`
- `05R_LR-QAP-2_adfontes.R`

A robustness test using looser definitions of topics will require you to run the following scripts in sequence (since the intermedia influence estimation also changes, as the topics change)
- `01R_estimate-intermedia-influence_10seasons.R`
- `02R_estimate-intermedia-influence-political_10seasons.R`
- `03R_estimate-intermedia-influence-entertainment_placebo.R`
- `04R_LR-QAP_10seasons.R`
- `05R_LR_QAP-2_10seasons.R`

A placebo test using negative lag can be executed by running the following scripts in sequence:
- `01R_estimate-intermedia-influence_placebo.R`
- `02R-estimate-intermedia-influence-political_placebo.R`
- `03R_estimate-intermedia-influence-entertainment_placebo.R`
- `04R_LR-QAP_placebo.R`
- `05R_LR-QAP-2_placebo.R`

Other scripts include those used for making specific visualizations (for e.g. `06_heatmap-viz.R` and `07_scatterplot-viz.R`), auxiliary files for generating Latex table (`08_generate-latex-tables1.R` and 09_generate-latex-tables2.R`) and generating descriptive statistics that are reported in the supplementary file (`10_descriptive_media_details.R`).


