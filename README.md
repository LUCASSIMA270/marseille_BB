# marseille_BB

🗳️ Marseille BB: Granular Electoral Mapping
Micro-Modeling the 2026 Battle: Analysis at the Polling Station (Bureau de Vote) Level

🧐 The Objective

Global trends often hide local realities. The marseille_BB project aims to predict the 2026 Municipal Elections by mapping Marseille’s political landscape at its finest grain: the Bureau de Vote (BV). By connecting each station to its specific INSEE Iris (socio-economic) data, we can identify "micro-swings" and neighborhood-specific shifts that sector-wide averages miss.
🛠️ The Data & Scientific Approach

    Granularity: 481+ Polling Stations (Bureaux de Vote) across Marseille's 16 arrondissements and 8 sectors.

    The "BB-to-Iris" Mapping: A complex data-merging process linking electoral stations to INSEE's socio-demographic blocks (Poverty, Education, Social Housing/HLM).

    Predictive Features:

        Historical BV Data: 2014, 2020 (Municipal), and 2022, 2024 (Legislative/Presidential).

        Sociological Variables: Median income per household, percentage of social housing, and demographic age groups.

    Tools: R (sf, tidyverse, Metrics) for spatial data handling and predictive modeling.

📂 Repository Structure

    cleaning_BV_marseille.R: Scripts to harmonize raw results from the Ministry of Interior with local municipal station maps.

    BB_mapping_logic.R: The core algorithm that maps Polling Station boundaries to INSEE Iris census tracts.

    master_dataset_BB.csv: The unified dataset containing the 2024 "starting point" for every single station in Marseille.

    projections_2026_BB.R: The simulation script for 2026, estimating the "swing" for each station based on current sociological trends.

    maps/: Heatmaps showing the "Left-Right-RN" dominance across the city’s micro-territories.

📈 Key Research Focus

    The "Iris" Correlation: Quantifying how the presence of specific housing projects (HLM) or luxury residences within a BV radius dictates the 2024-2026 vote shift.

    Abstention Clusters: Identifying the specific stations where mobilization is the key factor for 2026 victory.

    Dynamic Shifts: Comparing the 2020 Municipal "Printemps Marseillais" breakthrough with the 2024 Legislative surge of the RN at the street level.

🚀 How to Run

    Clone the repository.

    Process raw data: Use cleaning_BV_marseille.R to import the 2024 results.

    Generate projections: Run projections_2026_BB.R to see the simulated seat distribution for the 2026 Municipal Council.
