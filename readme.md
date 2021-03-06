## Background
This repository provides a complete set of requisite R and Python scripts that were used to produce results for two papers (aka, paper 1 and 2) published based on findings from the [Human Impacts project](https://www.eawag.ch/en/department/siam/projects/human-impacts-on-the-community-composition-of-swiss-rivers/) at Eawag:

1. Caradima, B., Schuwirth, N., & Reichert, P. (2019). [From individual to joint species distribution models: A comparison of model complexity and predictive performance](https://onlinelibrary.wiley.com/doi/abs/10.1111/jbi.13668?casa_token=_wDm-SIx2H0AAAAA:G5it16HoOoKi2D7Q83MJ2CnChqsFR_XqGQNYfDBR_D9wvOSJedsrhSBbNXiKlm9Fvgz7WwFshRIrloo7cQ). Journal of Biogeography, 46(10), 2260-2274. doi:10.1111/jbi.13668, [Institutional Repository](https://www.dora.lib4ri.ch/eawag/islandora/object/eawag:19054).
2. Caradima, B., Reichert, P., & Schuwirth, N. (2020). [Effects of site selection and taxonomic resolution on the inference of stream invertebrate responses to environmental conditions](https://www.journals.uchicago.edu/doi/abs/10.1086/709024?casa_token=8JDdR3cNHHAAAAAA:9vUT51LepXNftxx5CdWBKaiwM6eakmzLrVQzZ4XHooIulgd7jP5ItPXla0seug1JMyOxQSWmY0qekA). Freshwater Science, 39(3), 415-432. doi:10.1086/709024, [Institutional Repository](https://www.dora.lib4ri.ch/eawag/islandora/object/eawag:21111).

These published papers (along with others) are also included as chapters in the following doctoral research thesis:

Caradima, M. B. (2020). [Statistical species distribution modelling of stream invertebrate and fish communities](http://hdl.handle.net/20.500.11850/468125) (Doctoral dissertation, ETH Zurich). [Institutional Repository](http://hdl.handle.net/20.500.11850/468125).


Note that these scripts are part of a wider RStudio project and R workspace referred to as `model_dev` in the documentation below. `model_dev` includes input data, intermediary and final outputs, documentation, and additional files. I have uploaded these scripts and readme for others to review.

## Workspace Organization
`model_dev` includes the following folders:

* `/doc` contains documentation needed for frequent reference
* `/scripts` contains all scripts for supporting the entire model development process
  * `/python` Python scripts for spatial data processing using ArcPy library in ArcGIS 10.1 SP1 with 64-bit geoprocessing installed. Input data is not included for these due to disk space limitations. Scripts were used to develop predictor variables (e.g., F.EDO, FRI).
* `/inputs` contains all input data read by `model_dev` scripts (usually new pre-processed inputs were copied from Human Impacts folder on server and periodically updated based on date in filename)
* `/outputs` contains model results and processed results (e.g., plots) that are manually copied to the respective paper folder:
  * `/sampled observations` contains intermediate outputs used as observational data for the models: randomly sampled (with set.seed) stream invertebrate observations (i.e., of entire dataset and k-fold datasets) for all paper submissions as CSV files
  * `effective_parameters` example workspaces for each model in Caradima et al. (2019) that can be used in calculating effective number of parameters (see Appendix for paper 1 and script `test_pD.R`)
  * `/paper 1 accepted` results for ???paper 1 accepted??? paper
  * `/paper 2 accepted` results for ???paper 2 accepted??? paper
  
## Script Organization
Separate script files in `/scripts` are created for distinct steps in the model development process. First, input data is imported by `1-setup.R` and observational and input data ready for modelling is prepared by `3-inputs.R`. Second, user-defined custom functions are sourced from `2-functions.R` and organized using comments. Exploratory and final results are generated in `4-results_p1.R` and `5-results_P2.R`. To generate these results, the setup script must be run first (to read data, and source the inputs and functions scripts) before turning to a results_# script.

## Workflow
The scripts are run as follows for both papers:

1. `1-setup.R`:
  * source function.R to define all functions
  * install/load required packages with custom ipak() function
  * read all necessary /input datasets, with minimal or no processing
2. `2-functions.R`:
  * contains commonly used functions with full comments defining their usage
  * key functions: prepare.inputs, run.isdm, deploy.jsdm, cv.isdm, extract.jsdm, cv.jsdm, cv.jsdm.sample (see script for description and code for each function)
  * note that custom functions are used extensively in generating results, but that some functions are found in the results scripts, often because they have to be modified slightly before being called
3. `3-inputs.R`:
  * pre-process and calculate all potential influence factors for subsequent preparation for model inputs
  * prepares and samples observation data (for BDM, CFCH, and CFp datasets) for both papers
4. `variable-selection.R`:
  * given $p$ potential explanatory variables and $n$ sampled observations, performs an exhaustive search of individual models with $p$ parameters (e.g., 4-10 parameters) by doing 3-fold cross-validation for each individual model and taxon in the community
  * computes predictive performance for each model for individual taxa
  * excludes models with collinear variables from validation process
  * stores only minimum statistics required for model selection in `/outputs`
  * see paper 1 for detailed methodology
5. `model_selection.R`:
  * no longer used; most of its outputs are incorporated in the results_ scripts for specific papers
  * post-processing of output variable selection workspaces for final model selection
  * produces plots of mean predictive performance for community for each model
6. `jsdm_inference.R`:
  * prepares community and input data and calls Stan for Bayesian inference according to the hierarchical multi-species and joint models defined in Caradima et al. 2019
  * includes Stan definitions of hierarchical and joint generalized linear models
  * initial settings in script determine Stan code construction and final name of workspace file (this is important later on for processing results; see how the identify.jsdm() function works in the functions script)
  * identify.jsdm() is later called within extract.jsdm(), cv.jsdm(), cv.jsdm.sample().
7. `4-results_p1.R`:
  * generates results of `paper 1 accepted`, with comments (e.g., # Figure 1 ####) indicating code corresponding to the paper 1 submission 2 figures
8. `5-results_p2.R`:
  * generates results of `paper 2 accepted`, with comments indicating code corresponding to the paper 2 submission 1 figures
