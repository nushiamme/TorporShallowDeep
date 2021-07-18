####  Bird torpor, shallow to deep
Associated with the manuscript describing a spectrum of shallow to deep hummingbird torpor

#### Authors
A Shankar, INH Cisneros, S Thompson, CH Graham, DR Powers

Code by: A Shankar, github/nushiamme; contact: nushiamme\<at\>gmail\<dot\>com for questions about code/datasets

#### Abstract
Daily torpor-a controlled reduction in body temperature and metabolic rate-is a key energy saving strategy for small endotherms. Birds in torpor are thought to drop their body temperature and metabolism as low as their physiology and the ambient temperatures will allow. For instance, hummingbirds are thought to exclusively use deep torpor, in which body temperature and metabolic rate drop with ambient temperature by 23-34&deg;C. However, the potential costs of this strategy include increased predation risk, inhibited immune function, and a lack of restorative sleep. A shallower form of torpor (e.g., a decrease of 3-10&deg;C) might sometimes be a better strategy for them to balance energy savings with the potential costs of deep torpor. However, birds, unlike mammals, are not thought to use both shallow and deep torpor. Using infrared imagery from three hummingbird species in Arizona under natural temperature and light cycles, we found that all three species used both deep and shallow torpor, often on the same night. Blue-throated mountain-gems (8.4g) spent an average of 25% of the night in shallow torpor, while black-chinned hummingbirds (2.9g) spent an average of only 5% of the night in shallow torpor. Rivoli's hummingbirds (7.6g) spent the most time in shallow torpor (35%) and appeared to be the most flexible at managing nighttime metabolism. Given that hummingbirds can use both shallow and deep torpor, it is possible that torpor is on a physiological spectrum with sleep. Such a spectrum would indicate that hummingbirds have much finer control of their torpid metabolism than previously thought. 

#### Code organisation

The code is organized into 3 scripts and 2 .Rmd files, for
1. Reprocessing the thermal summaries if needed (time consuming, so these are already in a file "Thermal_maxes.csv").
2. Summarizing the data and adding thermal categories and dates etc. (these are already in All_data.csv, so no need to rerun unless you'd like to re-export that file).
3. File for running all the models and producing figures. Figures not listed here were made in Illustrator (Figure 1) or Powerpoint (Figure 3).
4. Exporting model results in an easy format for many of the tables in Supplement 1.
5. Exporting individual plots of nighttime temperatures for Supplement 2.

-   **Thermal\_summaries.R** - Needs all the individual and all the *"..._summ.RDS"* files in the compressed folder *"RDS_Data.zip"*; can re-make the *"Thermal_maxes.csv"* needed below. But this script is **very** time consuming; do not run unless you want to re-process exported csv's again. The compressed file of the csvs is 216 MB; uncompressed it is about 1.7GB.

-   **Thermal\_processing.R** - Needs all the and all the *"..._summ.RDS"* files in the compressed folder *"RDS_Data.zip"*; can re-make the *"All_data.csv"* needed below. Do not run unless you want to re-process exported and want to re-make *"All_data.csv"*

-   **Thermal\_plots\_models.R** - Needs input files *"Thermal_maxes.csv"*", *"All_data.csv"*, *"Category_thresholds.csv"*, *"Interpolated_Thermal.csv"*, *"Category_percentages.csv"*, *"Bird_masses.csv"*,  contains code for running all models and producing these figures and the basis for tables:
    -   *Figure 2*: Single RIHU individual's temperatures plotted over the course of a night. Points were modified for clarity in Illustrator. 3D surface plots were constructed in ImageJ and added on in Illustrator/powerpoint
    -   *Figure 3*: Surface vs ambient temperature, with model results from the nlme model fitted (species and category)
    -   *Figure 4a*: Range of max surface temperatures per individual (or per night), colored by category
    -   *Figure 4b*: Stacked bar for proportion of nighttime spent in each category per species, original interpolated values
    -   *Figure 4b*: Stacked bar for proportion of nighttime spent in each category per species, modeled values
    
-   **ShallowTables.Rmd** - Needs input files *"All_data.csv"*, *"Prop_Duration_Categories.csv"*, produces model output tables that are easy to input into Word:
    -   *Supplementary Tables*: Tables S1, S2, S3, and S4 with model results

-   **Individuals\_Overnight.Rmd** - Needs input files *"All_data.csv"*, *"Category_thresholds.csv"*, produces individual-level plots for Supplement 2. Just knit this file to make Supplement 2 again.


#### Packages you will need for the Thermal\_summaries.R script:

    + reshape2
    
#### For the Thermal\_processing.R script:

    + here
    + plyr
    
#### Additional packages for the Thermal\_plots\_models.R script:

    + here
    + plyr
    + dplyr
    + MASS
    + ggplot2
    + scales
    + lme4
    + nlme
    + lmerTest # optional, if you believe in p-values
    + lattice
    + stringr
    + emmeans
    + lubridate
    
#### For ShallowTables.Rmd

    + here
    + nlme
    + emmeans
    + MASS

#### For Individuals\_Overnight.Rmd

    + ggplot2
    + stringr
    + summarytools
