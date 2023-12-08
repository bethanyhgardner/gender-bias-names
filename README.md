[![DOI](https://zenodo.org/badge/525525815.svg)](https://zenodo.org/doi/10.5281/zenodo.10293754)

Materials, de-identified data, and analyses for "Biased inferences about gender from names" (Bethany Gardner &amp; Sarah Brown-Schmidt, accepted at *Glossa Psycholinguistics*).

----------------

If you have questions about the contents of this repository, please get in touch with [me](mailto:bethany.gardner@vanderbilt.edu).

If you're interested in replicating or extending these results, both I and [Sarah Brown-Schmidt](mailto:sarah.brown-schmidt@vanderbilt.edu) would love to hear about it!

----------------

*  [`analysis/`](analysis/)
    - divided into sections: [norming](analysis/exp0_analysis.Rmd), [Exp1 main](analysis/exp1_analysis_main.Rmd), [Exp1 supplementary](analysis/exp1_analysis_supplementary.Rmd), [Exp2 main](analysis/exp2_analysis_main.Rmd), [Exp2 supplementary](analysis/exp2_analysis_supplementary.Rmd), [Exp 3 main](analysis/exp3_analysis_main.Rmd), [Exp3 supplementary](analysis/exp3_analysis_supplementary.Rmd), [Exp4 main](analysis/exp4_analysis_main.Rmd), [Exp4 supplementary](analysis/exp4_analysis_supplementary.Rmd)
    - each of the experiments has an RMD file rendered to MD and PDF
    - also includes [function](analysis/centerfactor.R) for weighted contrast coding
  
*  [`data/`](data/)
    - [`exp0_data_norming.csv`](data/exp0_data_norming.csv) is the norming data for the first names, and [`exp0_data_census.csv`](data/exp0_data_census.csv) is the census data that the norming data is compared to
    - [`exp1_data.csv`](data/exp1_data.csv), [`exp2_data.csv`](data/exp2_data.csv), [`exp3_data.csv`](data/exp3_data.csv), and [`exp4_data.csv`](data/exp4_data.csv) are the de-identified data files for the main experiments
    - [`exp1_data_about.txt`](data/exp1_data_about.txt), [`exp2_data.csv`](data/exp2_data_about.txt), [`exp3_data_about.txt`](data/exp3_data_about.txt), and [`exp4_data_about.txt`](data/exp4_data_about.txt) are the codebooks for each of the data files 

*  [`all_analyses.RData`](all_analyses.RData): R object with all datasets, models, and plots

*  [`materials/`](materials/):
    - [`exp1_stimuli.xlsx`](materials/exp1_stimuli.xlsx) [`exp2_stimuli.xlsx`](materials/exp2_stimuli.xlsx), and [`exp3-4_stimuli.xlsx`](materials/exp3-4_stimuli.xlsx) are the stimuli lists for each of the experiments, with information about all of the sentence/story prompts and distributions of names into lists
    - data were collected using Qualtrics, and the surveys were exported to Qualtrics' proprietary format (.QSF) and to word documents

*  [`supplement.pdf`](supplement.pdf/): supplementary materials document referenced in main paper

*  [`plots/`](plots/)
    - [`plots.R`](plots/plots.R) is the code for all of the plots included in the main manuscript and supplementary materials
    - copies of main manuscript plots (responses by first name gender rating for Exps 1-4, odds ratios across all experiments, edited manually because I couldn't get the weird axis break to work in ggplot at the time)
    - copies of supplementary materials plots (norming data compared to census data, types of "Other" responses in Experiment 1)

*  [`figures/`](figures/): figures for the example stimuli/procedure and the general discussion

*  [`extras/`](analysis/): several extra plots/tables that didn't make it into the final paper
