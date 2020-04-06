# Acoustic regularities in infant-directed vocalizations across cultures
This repository contains data and code for Moser et al. (2020) "Acoustic regularities in infant-directed vocalizations across cultures". The paper is available at **XX URL**. The audio corpus is publicly available on the Open Science Framework (https://osf.io/m5yn2) and the naïve listener experiment is publicly available at https://themusiclab.org/quizzes/ids.

## Anatomy of the repo

`/data` contains the raw data, including the results of the naïve listener experiment (`IDS_naiveListeners.csv`); extracted acoustical features of each vocalization in raw (`IDS_raw.csv`) and Winsorized (`IDS_Winsor.csv`) forms; and metadata for each participant (i.e., group of vocalizations; `IDS_metadata.csv`). A codebook is in the paper.

`/analysis` contains Stata and R scripts for analysis of the naïve listener experiment, exploratory-confirmatory modeling, LASSO classification, and the convergent analyses. These scripts output tables and other data in `/results`.

`/viz` contains an R script for all visualizations in the paper, interim datasets for visualization, and the visualizations as pdfs. Some figure elements are augmented manually (e.g., adding labels) so your reproduced figures will not match those in the paper exactly.