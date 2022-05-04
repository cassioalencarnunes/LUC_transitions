[![DOI](https://zenodo.org/badge/411377128.svg)](https://zenodo.org/badge/latestdoi/411377128)
# Linking land-use and land-cover transitions to their ecological impact in the Amazon

Cássio Alencar Nunes, Erika Berenguer, Filipe França, Joice Ferreira, Alexander C. Lees, Julio Louzada, Emma J. Sayer, Ricardo Solar, Charlotte C. Smith, Luiz E. O. C. Aragão, Danielle de Lima Braga, Plinio Camargo, Carlos Eduardo Pellegrino Cerri, Raimundo Cosme, Mariana Durigan, Nárgila Moura, Victor Hugo Fonseca Oliveira, Carla Ribas, Fernando Vaz-de-Mello, Ima Vieira, Ronald Zanetti, Jos Barlow

March 2022

**e-mail**: cassioalencarnunes@gmail.com

#### Code repository for the paper: Nunes et al. Linking land-use and land-cover transitions to their ecological in the Amazon. Proceedings of the National Acaddemy of Sciences. 2022.

In this repository we included codes and data that we used to run the all the analyses presented in the paper.

1. You may find the codes to run the Linear Mixed-Effect Models (LMMs) in the script: "Script_LMMs.R". In this script we also calculated the effect sizes and the confidence interval of all land-use and land-cover (LULC) transitions. Details about the methods used for this calculation can be found in the manuscript Method's section.

2. The R scripts "Script_effect_sizes.R", "Script_graphs_transition_rates.R" and "Script_panels.R" were used to construct the figures after the analyses.

3. In the R script "Script_relationship_LULCT.R" you may find the correlation tests we ran between LULCT rates and LULCT effect sizes.

4. The R script "Script_LMMs_forest_sp.R" was used to ran the same analyses as "Script_LMMs.R", but using only forest species. It just runs models for the biodiversity ecosystem component.

5. The R script "Script_qGAM.R" provides the codes to run the validation analysis in which we used non-parametric Quantile Generalized Additive Models to validate the LMMs.
