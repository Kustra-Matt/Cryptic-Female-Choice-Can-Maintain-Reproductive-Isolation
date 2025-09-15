Description of code files affiliated with **"Cryptic female choice can maintain reproductive isolation"**

**[Running_Sims:]{.underline}** Folder that contains all the files needed to run simulations. Only occurs in **"Code.zip"** and does not require any data.

-   **"Intensity_Model_Simulation_Code.jl"**: Julia script that was used for intensity of sperm competition simulations. Currently set up  population size of 10000 and for conspecific sperm precedence only (scenario 1). Code is heavily commented and indicates what needs to be changed to run simulations.

-   **"Risk_Simulation_Code.jl"**: Julia script that was used for risk of sperm competition simulations. Currently set up  population size of 10000 and for conspecific sperm precedence only (scenario 1). Code is heavily commented and indicates what needs to be changed to run simulations.

-   **"migrantsucess.jl"**: Julia script that was used for running simulations of reproductive stage for Figure S9. Currently set up  population size of 10000. Code is heavily commented and indicates what needs to be changed to run simulations.

-   **"Job_ibm.mpi"**: Example SLURM job script that was used to run simulations on super computer.


**[Analysis:]{.underline}** Folder that contains R code to generate figures/summarize data. Move the **"Data"** folder in the unzipped **"Data.zip."** to the **[Analysis:]{.underline}** folder in  unzipped **"Code.zip"**. 

-   **"Figures-Analysis.R"**: R script to generate all figures and supplmental figures in the paper.Uses files in the **[Data]{.underline}** folder.

**[SI_Web_App:]{.underline}** Folder that contains the code needed to generate the supplemental web app.Occurs in **"Code.zip"**. 

-   "**app.R**": The R code file that creates the supplemental web app. Code is not needed to run the web app but can be run locally if desired.

