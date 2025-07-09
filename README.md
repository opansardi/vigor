# vigor
Dataset and Scripts: "Altruistic Punishment in Action: Movement Vigor in Neuroeconomic Choice"

This repository contains the primary data and analysis scripts for Study 1 and Study 2 reported in the manuscript "Altruistic punishment in action: movement vigor in neuroeconomic choice". 

## Files

- data_vigor_Study1&Study2.xlsx  
  This Excel file contains the dataset used in the analyses. 

- GLMM.R
  This R script contains all the generalized linear mixed model (GLMM) analyses for the project.  
  The models are organized one after the other within the script.  

Important:  
To run the script, you need to update the file path where the dataset is stored (where you have saved the other file on your machine)


## How to Run

1. Open `GLMM.R` in RStudio or your preferred R environment.
2. Replace the file paths in the `read_excel()` lines (once for each model) with the path to `data_vigor_Study1&Study2.xlsx` on your computer.
3. Install the required packages if prompted (e.g., `lme4`, `lmerTest`, `emmeans`, `ggplot2`, etc.).
4. Run the script section by section to replicate the analyses.

