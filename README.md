# Do early pension withdrawals reduce labour supply? Evidence from Australia's pandemic response

### Authors: Oscar Lane and Mingji Liu

## Replication package


In order to replicate the results from this paper, you will need access to the 22nd wave of HILDA (General Release). Access can be requested from the Australian Data Archive at the following link:

<https://dataverse.ada.edu.au/dataset.xhtml?persistentId=doi:10.26193/R4IN30>

Once the Combined files have been downloaded and extracted into the same directory, you will need to alter the path in `01_clean_hilda_summary_stats.R` to reflect the location on your machine.

In order to replicate the results, you simply need to run the following scripts in order in the same R session:

1.  `01_clean_hilda.R` : cleans data (Note will need to change HILDA file path to wherever the data is saved on your system.)

2. `02_summary_stats.R`: produces summary statistics, figures

3. `03_iv_model.R` : runs core IV and OLS models 

4. `04_iv_model_extensions.R` : runs additional extension IV models

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
