# epimodFBAfunctions\_GUI

A Shiny-based graphical user interface (GUI) for generating and simulating flux-balance analysis (FBA) models using the `epimodFBAfunctions` framework.

## Features

- **Model Generation**: Build FBA models from MATLAB `.mat` files, configure per-model and global parameters, and export standardized YAML + Petri-net representations.
- **Simulation & Analysis**: Run dynamic community FBA simulations, adjust time spans and tolerances, and visualize results.
- **minMicrobiome Algorithm**: Identify minimal microbial consortia to sustain a given function.
- **Data Visualization**: Interactive plots for flux distributions, boundary conditions, and community profiles.

## Installation

### R Requirements

1. Install core dependencies:

   ```r
   install.packages(c("shiny", "shinydashboard", "shinyjs", "shinyFiles", "DT", "readr", "jsonlite", "devtools"))
   library(devtools)
   ```

2. Install the underlying FBA function libraries from GitHub:

   ```r
   # epimod_FBAfunctions
   install_github(
     "https://github.com/qBioTurin/epimod_FBAfunctions",
     ref = "unified-epimod_FBAfunctions"
   )

   # epimod core
	 install.packages("devtools")
	 library(devtools)
	 install_github("https://github.com/LorenzoChiabrando/epimod_gui", ref="main")

   library(epimodFBAfunctions)
   library(epimod)

   # Download required container images
   downloadContainers()
   ```

### Python Requirements

- **Python 3.8+**
- **COBRApy** (for FBA model handling)

Install using pip or conda:

```bash
# Using pip
pip install cobra
python3 -m pip install --user cobra

# Or with conda
conda install -c conda-forge cobra

# Only if in Rstudio gives error (missing cobra library)
sudo python3 -m pip install cobra       # global (only if the others don't works)
```

## Donwload and run epimodFBAfunctionsGUI

1. Donwload epimodFBAfunctionsGUI :
   
```
  install_github(
  "https://github.com/LorenzoChiabrando/epimodFBAfunctions_GUI",
  ref = "main")
```




2. In R (console or Rstudio), launch the Shiny app:

```
   library(epimodFBAfunctionsGUI)
   epimodFBAfunctionsGUI::run_app()
```

3. Use the sidebar to navigate:
   - **Model Generation**: Configure and export FBA model hypernodes.
   - **Simulation**: Run and analyze dynamic community simulations.
   - **minMicrobiome**: Compute minimal consortia.
   - **Data Visualization**: View interactive plots.

## Usage

1. **Model Generation**:

   - Enter a unique **Hypernode Name**.
   - Select your working and `.mat` directories.
   - Load models, adjust per-model or global parameters, pick boundary metabolites, then click **Generate Model**.

2. **Simulation**:

   - Switch to **Simulation → Analysis**.
   - Choose hypernode directory, set time span and tolerances, and click **Run Simulation**.

3. **Visualization**:

   - Navigate to **Data Visualization** to explore results.

## Project Structure

```
epimodFBAfunctions_GUI/
├── inst/app/ui.R           # Main UI definition
├── inst/app/server.R       # Main server logic
├── R/                      # Module code (modelGen, simulation, home, etc.)
├── www/                    # Static assets: CSS, JS, images
└── README.md               # This file
```

## Contributing

1. Fork the repo
2. Create a feature branch
3. Commit your changes
4. Submit a pull request

Please adhere to the existing code style and include tests where appropriate.



