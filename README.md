# StrathE2E Habitat Map Creation
This repository contains code for creating consistent StrathE2E domain habitat maps for
each model implementation.

## File structure

```code
├───data                            # folder holding bathymetry/land data and the domain/habitat files for regions
│   ├───GEBCO_2020.nc
│   └───*domain/habitat.rds files*
├───outputs                         # folder where maps will be saved
└───R_scripts
    ├───create_maps.R
    └───mapping_functions.R
```

## Required data
The repository requires `GEBCO_2020.nc` bathymetry data and any relevant `Habitats.rds` or
`Domain.rds` for mapping regions.

- `GEBCO_2020.nc` : Can be downloaded from [MA_StrathE2E_Models](https://strath.sharepoint.com/:f:/r/sites/MA_StrathE2E_Models/Shared%20Documents/Shared%20data?csf=1&web=1&e=rRxA3V).
- `Domain/Habitats .rds` : Can be downloaded from [Model domain and habitat data](https://strath.sharepoint.com/:f:/r/sites/MA_StrathE2E_Models/Shared%20Documents/Maps/Model%20domain%20and%20habitat%20data?csf=1&web=1&e=n2bBpN).

### Notes
For model implementations around the UK (such as the North_Sea and Celtic_Sea), habitat data
is processed from csv files `roberts_regions.csv` and `roberts_rock.csv`. These regions are
distinct from the later-produced regions that use finalised .rds files to define habitat 
zones. Regions using these files are defined in `roberts_regions` in `mapping_functions.R`.
These regions must be called with the specific names in this variable to distinguish them
from other .rds using regions.

For the model implementation in Senegal, habitat .rds files were created from sediment
data directly at the time of repository compiling.