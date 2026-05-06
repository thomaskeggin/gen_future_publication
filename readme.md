# Unexpected global biodiversity responses to climate change


Thomas Keggin, Alexander Skeels, Oskar Hagen, Carlos J. Melián\*, Conor
Waldock\*

\* Shared senior authorship.

This is the code repository for the following publication:

<https://doi.org/10.64898/2026.03.03.707980>

## File structure

Each sub-directory contains a flowchart to visualise the data pipeline.
These are produced directly from script input and output paths using the
pipelinemapper package.

### 01_prepare_inputs

Scripts that prepare all simulation inputs:

- seascape objects

- species object

### 02_configure_genesis

This contains the config generator and template. The template is the
configuration file without the simulation variables (dispersal range and
adaptive rate). The configuration generator outputs each simulation’s
configuration file based on the template and parameter values.

### 03_run_genesis

This script runs the simulations.

### 04_define_ecoregions

These scripts calculate seascape metrics of the simulated seascape
across biogeographic regions as defined in Spalding et al. (2007) .

### 05_process_outputs

These scripts form a pipeline that calculates the simulation output
metrics used in the analyses.

### 06_analysis

These are the analyses scripts used to generate the results.

### functions

These scripts contain helper functions.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-spalding2007" class="csl-entry">

Spalding, Mark D., Helen E. Fox, Gerald R. Allen, Nick Davidson, Zach A.
Ferdaña, Max Finlayson, Benjamin S. Halpern, et al. 2007. “Marine
Ecoregions of the World: A Bioregionalization of Coastal and Shelf
Areas.” *BioScience* 57 (7): 573–83. <https://doi.org/10.1641/b570707>.

</div>

</div>
