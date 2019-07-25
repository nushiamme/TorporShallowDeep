**Paper authors: A Shankar, INH Cisneros, S Thompson, CH Graham, DR Powers**

Code by: A Shankar, github/nushiamme; contact: nushiamme\<at\>gmail\<dot\>com for questions about code/datasets

** Bird torpor, shallow to deep **
Associated with the manuscript describing a spectrum of shallow to deep hummingbird torpor


#### Abstract
Daily torpor-a controlled reduction in body temperature and metabolic rate-is a key energy saving strategy for small endotherms. Birds in torpor are thought to drop their body temperature and metabolism as low as their physiology and the ambient temperatures will allow. For instance, hummingbirds are thought to exclusively use deep torpor, in which body temperature and metabolic rate drop with ambient temperature by 23-34oC [1,2]. However, the potential costs of this strategy include increased predation risk, inhibited immune function, and a lack of restorative sleep. A shallower form of torpor (e.g., a decrease of 3-10oC) might sometimes be a better strategy for them to balance energy savings with the potential costs of deep torpor. However, birds, unlike mammals, are not thought to use both shallow and deep torpor. Using infrared imagery from three hummingbird species in Arizona under natural temperature and light cycles, we found that all three species used both deep and shallow torpor, often on the same night. Blue-throated mountain-gems (8.4g) spent an average of 25% of the night in shallow torpor, while black-chinned hummingbirds (2.9g) spent an average of only 5% of the night in shallow torpor. Rivoli's hummingbirds (7.6g) spent the most time in shallow torpor (35%) and appeared to be the most flexible at managing nighttime metabolism. Given that hummingbirds can use both shallow and deep torpor, it is possible that torpor is on a physiological spectrum with sleep. Such a spectrum would indicate that hummingbirds have much finer control of their torpid metabolism than previously thought. 

#### Code organisation

The code is organized by data type into 4 scripts, for
1. Temperature and thermoregulatory cost calculations,
2. Floral resources (floral abundance)
3. The energy budget model, and
4. A model of daily energy expenditure as a function of resources, temperature, and bird mass

Figures not listed here were conceptual figures made in powerpoint or in prism.

-   **Temp\_EnergyBudget.R** - Needs input files *"BBLH_temperatures_compiled.csv"*, *"Melted_Te_thermo.csv"*, *"Melted_Ta_thermo.csv"*; contains code for producing and compiling thermoregulatory costs for thermoregulation component of the energy budget model.
    -   *Figure 1d*: Distribution of temperatures at Harshaw and Sonoita, facetted by day and night.
    
-   **Energy\_budget\_resources.R** - Needs input file *"FloralCensusData2013.csv"*; code to plot resource availability across sites and seasons
    -   *Figure 1e*: 

-   **Energy\_budget\_BBLH\_Aug2018.R** - Needs input files *"EnergyBudget_model_values.csv"*, *"DLW_summary"*, *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water (DLW) data 
    -   *Figure 2*: Energy budget models compared against DLW measurements of daily energy expenditure. The left panel compares Harshaw DLW and model measurements, with individuals recaptured multiple times as colored points. The right panel compares Sonoita DLW and model values.
    -   *Figure 3*: Stacked bar graph showing the different modeled components of the daily energy budget 
    -   *Supplementary Figure S1*: Validation of the modified DLW method
    -   *Supplementary Figure S3*: Scholander-Irving curve for Costa's hummingbirds
    
-   **Energy\_budget\_BBLH\_models.R** - Needs input files *"FloralCensusData2013.csv"*, *"Melted_Ta_thermo.csv"*, *"DLW_summary.csv"*; Contains code to analyse daily energy expenditure in the context of environmental factors
    -   *Table S1*: 


#### Packages you will need for all three scripts:

    + reshape2 or reshape
    + ggplot2
    + dplyr
    + grid
    
#### Additional packages you will need for the temperature-thermoregulation script:

    + data.table
    + gridExtra
    + scales

#### Additional packages you will need for the resources script:
    
    + plyr
    + ggthemes
    
#### Additional packages you will need for the model script:
    
    + plyr
    + stringr
    + lme4
    
