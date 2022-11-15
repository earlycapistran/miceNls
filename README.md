# Integrating local ecological knowledge, ecological monitoring, and computer simulation to evaluate conservation outcomes


## Repository overview:
- README.md
- R scripts: 
	- Prefix "analysis_01..." indicates scripts used for analyses and suggested running order. 
	- Prefix "plotting_01..." indicates scripts used to generate figures and suggested running order. 	
- results: stores results from R scripts (multiply imputed datasets, model objects, figures, etc.)
- consLettersUtils: a package that contains functions and ggplot themes. It can be installed with devtools: 

		devtools::install('/path/to/pkg')
- License

## Workflow
1. Analysis: run "analysis" scripts using suggested numerical running order from filename prefix. 
2. Plotting: run "plotting" scripts using suggested numerical running order from filename prefix.

## Data
This folder contains all standardized CPUE and morphometric data used in analyses. 

- Raw LEK data is available at <https://github.com/earlycapistran/PeerJ>


### Unprocessed data
This folder contains processed LEK and monitoring data. These datasets were used for all analyses. 

- cpue_data.csv: Mean annual CPUE values (LEK and Monitoring) standardized to one 100m net with 12hr soak time.

- morphometric_data.csv: Includes year, weight (Kg), straight carapace length (SCL), curved carapace length (CCL), and indicators for adult and juvenile age groups.

**Note**: Unpublished raw monitoring data is not included. Please contact the monitoring organizations if you are interested in their data (see Data sources below).

### Data sources

#### LEK: 
- All data (1952-1983): Early-Capistrán, M.-M., Solana-Arellano, E., Abreu-Grobois, F. A., Narchi, N. E., Garibay-Melo, G., Seminoff, J. A., Koch, V., & Saenz-Arroyo, A. (2020). Quantifying local ecological knowledge to model historical abundance of long-lived, heavily-exploited fauna. PeerJ, 8, e9494. <https://doi.org/10.7717/peerj.9494>

-  Full data and code are available at <https://github.com/earlycapistran/PeerJ>

#### Monitoring (CPUE): 
- 1995-2003: Seminoff, J. A., Jones, T. T., Resendiz, A., Nichols, W. J., & Chaloupka, M. Y. (2003). Monitoring green turtles (*Chelonia mydas*) at a coastal foraging area in Baja California, Mexico: Multiple indices to describe population status. *Journal of the Marine Biological Association of the UK*, 83(06), 1355-1362.

- 2004-2005: Seminoff, J.A. (2005). NOAA. Unpublished raw data.

- 2003-2018: [Comisión Nacional de Áreas Naturales Protegidas](https://www.gob.mx/conanp), A.P.F.F. Islas del Golfo de California & Grupo Tortuguero de Bahía de los Ángeles (2018). Unpublished raw data. 

#### Monitoring (morphometric data):

- 1995-2005: Seminoff, J.A. (2005). NOAA. Unpublished raw data.

- 2003-2018: [Comisión Nacional de Áreas Naturales Protegidas](https://www.gob.mx/conanp), A.P.F.F. Islas del Golfo de California & Grupo Tortuguero de Bahía de los Ángeles (2018). Unpublished raw data. 

## Contact
- Michelle María Early Capistrán, <earlycapistran@comunidad.unam.mx>

## Citation
Data: Early-Capistrán, M.M., Seminoff, J.A., Comisión Nacional de Áreas Naturales Protegidas, Grupo Tortuguero de Bahía de los Ángeles (2021) Dataset: Long-term abundance and morphometric data for *C. mydas* in Bahía de los Ángeles, B.C., Mexico. <https://github.com/earlycapistran/consLetters>.

Code: Early-Capistrán, M.M. (2021) R package: consLetters. <https://github.com/earlycapistran/consLetters>
