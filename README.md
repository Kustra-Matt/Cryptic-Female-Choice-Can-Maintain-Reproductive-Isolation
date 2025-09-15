# Code from: Cryptic female choice can maintain reproductive isolation

Description of code and data files affiliated with Kustra et al. (2025) **"Cryptic female choice can maintain reproductive isolation"**. https://academic.oup.com/evolut/advance-article/doi/10.1093/evolut/qpaf156/8217261 All code files (.jl or .R) are uploaded here and all data files are uploaded on dryad (https://doi.org/10.5061/dryad.ksn02v7db) in the following zip files: **"Data-sel.zip", "RSQ.zip", "Data*n.zip", "MigrantMating.zip", "**Data_n.zip***", "***Processed_Data_2.zip***", "***Processed_Data_2H.zip***", "***Processed_Data_4.zip***", "***Processed_Data_4H.zip***", "***Processed_Data_cor.zip***", and  "***Processed_DataND.zip***".

For questions, contact: Matthew Kustra: [matthewckustra@gmail.com](mailto:matthewckustra@gmail.com)

To properly run code first unzip **"Code.zip"** files and all data zip files. Then make sure all data folders are within a directory named "Data."

Below is a description of all the code and data files broken down by the directories in which code and data files should be organized in.

**"Running_Sims":** Folder that contains all the files needed to run simulations. Only occurs in **"Code.zip"** and does not require any data.

* **"Intensity_Model_Simulation_Code.jl"**: Julia script that was used for intensity of sperm competition simulations. Currently set up  population size of 10000 and for conspecific sperm precedence only (scenario 1). Code is heavily commented and indicates what needs to be changed to run simulations.
* **"Risk_Simulation_Code.jl"**: Julia script that was used for risk of sperm competition simulations. Currently set up  population size of 10000 and for conspecific sperm precedence only (scenario 1). Code is heavily commented and indicates what needs to be changed to run simulations.
* **"migrantsucess.jl"**: Julia script that was used for running simulations of reproductive stage for Figure S9. Currently set up  population size of 10000. Code is heavily commented and indicates what needs to be changed to run simulations.
* **"Job_ibm.mpi"**: Example SLURM job script that was used to run simulations on super computer.

**"Analysis":** Folder that contains R code to generate figures/summarize data.

* **"Figures-Analysis.R"**: R script to generate all figures and supplemental figures in the paper. Uses data in all the different Data directories
* **Data directory descriptions:** with all the data from the model that were used in analysis/graphing. Different folders contain data for different analyses. File types are either csv or RDS format. RDS is a format that compresses and allows faster loading, it can be opened in R. This was done because the non compressed files are very large.
  * "**RSQ**": Folder with the saved results from the Rsquared analysis for Figures S6 and S15:
    * "**RSquares_NS_large_xm**": Files generated from and used by"**Figures-Analysis.R**" to make Figure S15. Four separate files for 1-4% migration replacing x with the number 1-4. Has correlation coefficients for Large population size when there is no initial divergence in cryptic preferences and sperm traits. Here are the descriptions of the column variables:
      * ***Gen***: Generation used for correlation
      * ***P***: Pearson correlation.
      * ***S***: Spearman's correlation.
      * ***Level***: Risk/intensity of sperm competition.
    * "**RSquares_NoE_small**": File generated from and used by"**Figures-Analysis.R**" to make Figure S6. Has correlation coefficients for small population size when there is only initial divergence in cryptic preferences and sperm traits but no divergent ecological selection. Here are the descriptions of the column variables:
      * ***Gen***: Generation used for correlation
      * ***P***: Pearson correlation.
      * ***S***: Spearman's correlation.
      * ***Level***: Risk/intensity of sperm competition.
  * "**Processed_Data_4_H**": Folder with data files from simulations with high starting variation and 4SD difference. Total number of files: 14 :
    * "**X_Y_runs.rds**" files: Files with "runs" in the name include data across all 50 replicates. First part of file name includes information about the type of scenario. "NoE" is Scenario 1, where there is no divergent ecological selection, only conspecific sperm precedence. "NS" is Scenario 2, where there is initial ecological divergence but no initial cryptic preference divergence."EE" is Scenario 3, where there is initial ecological divergence as well as initial cryptic preference divergence. If file names contain "I" this means they are for sperm competition intensity scenarios."L" means it was a longer run because populations had not stabilized yet. If a file name does not contain "I" it means it is for risk of sperm competition. "Small" if a file contains small it means that it is for runs with population sizes of 2000. Otherwise it is for population sizes of 10,000. Here are the descriptions of the column variables. "_a" refers to population a, "_b" refers to population b.
      * ***Mean.Female***: Average cryptic female choice trait (f)
      * ***SD.Female***: Standard deviation of cryptic female choice trait (f)
      * ***Mean.Male***: Average sperm trait (t)
      * ***SD.Male***: Standard deviation of sperm trait (t)
      * ***Mean.Count***: Average male sperm number (s)
      * ***SD.Count***: Standard deviation in the sperm number (s).
      * ***Mean.E***: Average ecological trait (e)
      * ***SD.E***: Standard deviation of ecological trait (e)
      * ***Mean.B***: Average neutral genome proportion from different population
      * ***SD.B***: Standard deviation in neutral genome proportion from different population
      * ***Cor.mf***: Pearson correlation coefficient between male sperm trait and cryptic female choice trait within a population.
      * ***Cor.me***: Pearson correlation coefficient between male sperm trait and ecological trait within a population.
      * ***Cor.fe***: Pearson correlation coefficient between cryptic female choice trait and ecological trait within a population.
      * ***m***: Migration rate
      * ***an***: Selection coefficient for viability selection
      * ***a***: Strength of postmating sexual selection
      * ***rsc***: risk or intensity of sperm competition
      * ***Generation:*** Generation of the simulation.
      * ***Surv_X_Y_Z:*** Number of surviving individuals. X is sex of parent (Male or Female),Y is migrant status (Mig for migrant,NMig for resident), Z is the population (a or b)
      * ***X_Off_Y_Z:*** Number of offspring produced. X is sex parent (M or F),Y is migrant status (Mig for migrant,NMig for resident), Z is the population (a or b)
      * ***X_SurvOff_Y_Z:*** Number of offspring that survived until adulthood. X is sex parent (M or F), Y is migrant status (Mig for migrant,NMig for resident), Z is the population (a or b).
      * ***X_SurvOff_Y_Z:*** Selection coefficient estimates for using surviving offspring. X is the trait (M = male sperm trait, F = cryptic choice trait, S = Sperm number, SM = sperm number * sperm trait for gamma coefficients), Y is type of selection coefficient (Beta for linear, Gamma for quadratic), Z is the population (a or b).
      * ***F_SonOff_Y_Z:*** Selection coefficient estimates for son fertilization success, only done for cryptic female choice to test for sexy sons hypothesis. Y is type of selection coefficient (Beta for linear, Gamma for quadratic), Z is the population (a or b).
      * ***X_Off_Y_Z:*** Selection coefficient estimates for using number of offspring before natural selection has occurred. X is the trait (M = male sperm trait, S = Sperm number, SM = sperm number * sperm trait for gamma coefficients), Y is type of selection coefficient (Beta for linear, Gamma for quadratic), Z is the population (a or b).
      * ***Rep:*** Population replicate of the simulation.
      * ***Tradeoff:*** Indicates whether a tradeoff between sperm number and male sperm trait existed. "true" means there was a tradeoff and "false" means there was no tradeoff.
      * ***A:*** Strength of postmating sexual selection, *a*, parameter in the paper. "1.0" is strong selection, "12.5" is moderate selection, and "50.0" is weak selection.
      * ***Level:*** Risk of sperm competition (or intensity) for that simulation run.
      * ***M:*** Migration rate
      * ***An:*** Strength of viability selection.
      * ***invest:*** is postmating investment. Sperm number for when there is no tradeoff and sperm number * sperm trait for when there is.
      * ***Mean.BP:*** Proportion of neutral genome that is from A.
      * ***DivBPA:*** Absolute divergence in neutral genome from different populations.
      * ***DivFA:*** Absolute divergence in cryptic preference trait between the two populations.
      * ***DivMA:*** Absolute divergence in male sperm trait between the two populations.
      * ***DivBPAbma:*** Divergence in neutral genome from different populations. Population B minus population A.
      * ***DivFAbma:*** Divergence in cryptic preference trait between the two populations.Population B minus population A.
      * ***DivMAbma:*** Divergence in male sperm trait between the two populations. Population B minus population A.
      * ***Surv_Tot_x_Y:*** Number of individuals that survived (females + males), X is either Migrant (Mig) or resident (NMig). Y is population.
      * ***Tot_X_Y_Z:*** Number of offspring by different groups. X is all offspring (Off) or only surviving offspring (SurvOff), Y is either Migrant (Mig) or resident (NMig). X is population (a or b)
      * ***PopSize:*** Population size of simulations.
    * "**X_Y_SUM.rds**" files: Files with "Sum" in the name include summarized data across all 50 replicates. First part of file name includes information about the type of scenario. "NoE" is Scenario 1, where there is no divergent ecological selection, only conspecific sperm precedence. "NS" is Scenario 2, where there is initial ecological divergence but no initial cryptic preference divergence."EE" is Scenario 3, where there is initial ecological divergence as well as initial cryptic preference divergence. If file names contain "I" this means they are for sperm competition intensity scenarios. If a file name does not contain "I" it means it is for risk of sperm competition."L" means it was a longer run because populations had not stabilized yet. "Small" if a file contains small it means that it is for runs with population sizes of 2000. Otherwise it is for population sizes of 10,000. Description of columns are same as above. Except data is summarized across all 50 replications. "_mean" indicates that this is the mean across of all 50 replicates. "_sd" indicates this is the standard deviation across all replicates.
  * "**Processed_Data_cor**": Folder with data files from simulations that looked at correlation between preference and neutral loci (Figure S7). Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 2
  * "**Processed_Data_2H**": Folder with data files from simulations with high starting variation and 2SD difference. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 18 :
    * "**X_Y_last.csv**": Files with "last" in the name include data across all 50 replicates but only during the last generation. First part of file name includes information about the type of scenario. "NoE" is Scenario 1, where there is no divergent ecological selection, only conspecific sperm precedence. "NS" is Scenario 2, where there is initial ecological divergence but no initial cryptic preference divergence."EE" is Scenario 3, where there is initial ecological divergence as well as initial cryptic preference divergence. If file names contain "I" this means they are for sperm competition intensity scenarios. If a file name does not contain "I" it means it is for risk of sperm competition. "Small" if a file contains small it means that it is for runs with population sizes of 2000. Otherwise it is for population sizes of 10,000. Description of columns are same as above.
  * "**Processed_Data_2**": Folder with data files from simulations with 2SD difference. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 18:
  * "**Processed_Data_4**": Folder with data files from simulations with 4SD difference. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 19 (including an .Rdata file).
  * "**Processed_DataND**": Folder with data files from simulations with no ejaculate depletion. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 7.
  * "**Data_n**": Folder with data files from simulations with the baseline starting variation. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 16
  * "**Data-sel**: Folder with data files from simulations with the baseline starting variation but rerun for correct calculation of selection coefficients. Files inside this folder follow the same naming convention and have same column names as the "Processed_Data_4_H" folder. Total number of files: 12
  * "**MigrantMating**": Folder with data files from simulations and analytical results of the reproductive stage only Used for Figure S9. Total number of files: 3.
    * "**Analytical.csv**" which contains the analytical solutions from Equations S4 and S5. Description of the columns:
      * ***Level*** is the risk/intensity of sperm competition.
      * ***Predicted*** is the predicted proportion of favorable matings at the given risk/intensity of sperm competition in the ***Level*** column
    * "**Simulation_migrantmatin-L.csv**" which contains the simulation results from large populations (N =10,000). First row is the risk/intensity of sperm competition. All other values are the number of successful migrant males in a given simulation. Simulations were run 10,000 times so there are  10,000 rows of simulation results. 
    * "**Simulation_migrantmating.csv**" which contains the simulation results from small populations (N = 2000). First row is the risk/intensity of sperm competition. All other values are the number of successful migrant males in a given simulation. Simulations were run 10,000 times so there are  10,000 rows of simulation results. 

"***SI_Web_App***": Folder that contains the code needed to generate the supplemental web app. Occurs in **"Code.zip"**.

* "**app.R**": The R code file that creates the supplemental web app. Code is not needed to run the web app but can be run locally if desired.

