# StrathE2E Habitat Map Creation
This repository contains code for creating consistent StrathE2E domain habitat maps for
each model implementation. The habitat maps are plotted for both the StrathE2E Shiny App
and for model implementation documents. Shiny App maps have white land colour, while
implementation document maps have a black land colour.

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

### Notes / Exceptions
For model implementations around the UK (such as the North_Sea and Celtic_Sea), habitat data
is processed from csv files `roberts_regions.csv` and `roberts_rock.csv`. These regions are
distinct from the later-produced regions that use finalised .rds files to define habitat 
zones. Regions using these files are defined in `roberts_regions` in `mapping_functions.R`.
These regions must be called with the specific names in this variable to distinguish them
from other .rds using regions.

For the model implementation in Senegal, habitat .rds files were created from sediment
data directly at the time of repository compiling. The resulting `Senegal_MA Habitats.rds`
file is stored along with other habitat data in the SharePoint.
If habitat creation from sediment data for Senegal needs to be performed again, the raw
sediment data can be found in the SharePoint folder /senegal_sediment/, and the habitat
creation script can be found in the archive folder.

For the model implementation in the Barents Sea, mapping the domain in a projected Coordinate
Reference System appeared most appropriate due to the domain's polar latitude. Therefore the
habitat data was projected from `EPSG:4326` to `EPSG:3035`. Raw and processed data are
available in the SharePoint folder. Due to the projected CRS of the data, the `buffer_dist`
argument for plotting must be provided in metres rather than degrees.
A similar projection process can be done for other polar model domains after identifying
a suitable PCRS [EPSG](https://epsg.io/).

## R files
- `mapping_functions.R` : Contains functions that process the domain/habitat data types and
plot out the maps for app and implementation document types. The main user facing function
is `plot_domain_map()`.
- `create_maps.R` : Contains the code that calls functions from `mapping_functions.R` and
configures the maps.

### Adding a new model region
To add a new model region for habitat map plotting, add the corresponding domain/habitat
data to the data folder and add a section of code to the `create_maps.R` file. When adding
a new implementation `plot_domain_map` arguments including buffer_dist, contour_depth,
legend_position and contour_txt_position may need adjusting by iteration to produce a 
suitable map. 