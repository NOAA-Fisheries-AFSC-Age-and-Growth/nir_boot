# A framework to investigate the effects of observation error on neural network predictions of fish age

This repository contains the R scripts and data processing pipeline used to analyze the effects of ageing error on neural network model predictions of fish age. The analysis implements a Multi-Modal Convolutional Neural Network (MMCNN) and incorporates traditional ageing error through a bootstrapping simulation approach.

The code allows users to quantify how reader uncertainty (bias and precision) propagates into deep learning age predictions by simulating "Ageing Error" scenarios versus a "Known Age" (Null) control and includes an example application using FT-NIRS spectra.

## Repository Structure

To run this analysis, ensure your directory is organized as follows:

```markdown
.
├── R/
│   ├── executable.R              # MASTER SCRIPT: Prepares data and runs simulations
│   ├── Functions.R               # Core logic for data parsing, filtering, and bootstrapping
│   ├── bootstrap_MMCNN_err.R     # CNN execution script for the 'Ageing Error' scenario
│   ├── bootstrap_MMCNN_known.R   # CNN execution script for the 'Known Age' (Null) scenario
│   ├── analysis.R                # POST-PROCESSING: Aggregates results and generates figures
├── data/                         # (User input folder)
│   ├── data.csv  				  # Main dataset
│   └── Age_Error_Definitions/    # Folder containing reader precision CSVs
│       └── Results/              # Directory scanned for reader bias/SD files
└── README.md

```

## System Requirements

* **R (>= 4.0.0)**
* **Python (>= 3.8)**: Required for the TensorFlow/Keras backend.
* **Hardware**: A CUDA-enabled GPU is highly recommended for training the CNNs. The scripts include checks for GPU availability.

## Installation

### 1. R Dependencies

Run the following in R to install the required packages:

```r
install.packages(c("tidyverse", "ggplot2", "tidyr", "cowplot", "dplyr", "knitr", 
                   "signal", "FSA", "tensorflow", "keras3", "kerastuneR"))

```

### 2. Keras & TensorFlow Setup

This project uses the `keras3` interface. You must configure the Python backend within R before running the models:

```r
library(keras3)
install_keras() # Installs the necessary Python libraries via Miniconda or venv

```

## Usage Instructions

The workflow is divided into two phases: **Execution** (data prep & modeling) and **Analysis** (summarizing results).

### Phase 1: Running the Simulations (`executable.R`)

The `executable.R` script is the master controller. It performs the following steps:

1. **Data Loading:** Reads the main dataset and reader precision files.
2. **Preprocessing:** Optionally applies a Savitzky-Golay filter to the FT-NIR spectra, if the user is using a FT-NIRS model.
3. **Bootstrapping:** Generates `N` simulation folders.
* **`sims_err`**: Ages are resampled based on reader bias/SD matrices.
* **`sims_known`**: Ages are kept as the "original" control.
4. **Model Training:** Iteratively calls the external scripts `bootstrap_MMCNN_err.R` and `bootstrap_MMCNN_known.R` to train the CNNs for every simulation. These scripts are bespoke to walleye pollock FT-NIRS modeling, but can be substituted with alternative models of interest. R scripts must be run externally due to memory leak issues with Keras and tensorflow. If not run externally RAM usage balloons and causes the script to crash.

**How to Run:**

1. Open `executable.R`.
2. **Verify Paths:** Ensure `df_path` points to your main data CSV and `reader_dir` points to the folder containing your reader error CSVs.
3. **Configure:** Set `nsim` (number of simulations) and `apply_sg` (Savitzky-Golay filter toggle for FT-NIRS) as needed, refer to `Functions.R` for configuration specifics.
4. **Execute:** Run the entire script.

*Note: Depending on `nsim` and hardware, this process may take hours or days.*

### Phase 2: Analyzing Results (`analysis.R`)

Once the simulations are complete, use `analysis.R` to aggregate the metrics and produce visualizations.

**How to Run:**

1. Open `analysis.R`.
2. Run the script. It will automatically detect the number of completed simulations in the `./sims_err` folder.

**Outputs (in `./Output/`):**

* **`metrics_err.csv` / `metrics_known.csv`**: Aggregated performance metrics (R², RMSE, APE, CV) for every iteration.
* **`Violin.png`**: Violin plots comparing RMSE and R² distributions across the Null and Age Error models.
* **`Boxplot_all_scenarios.png`**: Visual comparison of predicted vs. reference age distributions.
* **Markdown Tables**: The script prints summary tables (Mean ± 95% CI) to the console for easy copying into manuscripts.

---

## Input Data Format Requirements

To use this code with your own data, ensure your inputs match the expected format:

1. **Main Data CSV (`df_path`)**:
* Must contain columns for `file_name`, `sample` (train/test labels), and `final_age`.
* **Input Features**: The code assumes features (spectra, pixel values, etc.) start at a specific column (default is column 8).
* *Note: If your features start at a different column, update the `age_col`, `train_test_col`, and `meta_cols` arguments in `executable.R`.*


2. **Reader Precision Files (`reader_dir`)**:
* The code scans this folder for **all** `.csv` files.
* Each file must follow the specific matrix format:
* **Row 4**: Standard Deviation (SD) values for each age class.
* **Row 5**: Bias values for each age class.


* *Ensure this folder contains ONLY reader files to avoid errors.*

---

## Using Custom Models (Replacing MMCNN)

You can replace the default FT-NIRS Convolutional Neural Network with **any** regression model (e.g., Random Forest, Gradient Boosting, Linear Regression) by modifying the `system()` calls in `executable.R`.

To ensure your custom model works with `analysis.R`, your script must follow these input/output specifications:

### 1. Input Arguments

Your script must accept two command-line arguments:

1. **Iteration Number (`j`)**: The current simulation index (e.g., `1`, `2`, ...).
2. **Working Directory (`wd`)**: The root path of the project.

*Example R implementation:*

```r
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])
wd <- args[2]

```

### 2. Required Outputs

For **each iteration `j`**, your script must generate the following files inside the specific simulation folder (e.g., `./sims_err/1/`):
**A. Predictions (CSV)**

* **Path:** `./Output/Data/train_predictions.csv`
* **Path:** `./Output/Data/test_predictions.csv`
* **Format:** Must contain at least two columns named `train` (Actual Age) and `pred` (Predicted Age).
```csv
train,pred
5,5.2
3,2.9
...

```



**B. Metrics (CSV)**

* **Path:** `./Output/Data/metrics{j}.csv` (e.g., `metrics1.csv`)
* **Format:** A single-row CSV with the following columns (headers must match exactly):
* `iteration`: The value of `j`.
* `train_R2`: R-squared for training set.
* `train_RMSE`: RMSE for training set.
* `test_R2`: R-squared for test set.
* `test_RMSE`: RMSE for test set.



**C. Model Output Directory Structure**
Your script is responsible for creating these subdirectories if they do not exist:

```text
./sims_err/{j}/Output/Data/
./sims_err/{j}/Output/Figures/  (Optional, but recommended for plots)

```

```markdown
.
├── sims_err/                     						# Directory containing all ageing error iterations
│   ├── 1  				  		  						# Iteration 1
│   └── 2    					  						# Iteration 2
│       └── Output/              						# Directory for output from individual iteration
│       	├── Data/              						# Directory for data output from individual iteration
│       	│	└── test_predictions.csv/              	# CSV containing test data input ages and predictions
│       	│	└── train_predictions.csv/              # CSV containing train data input ages and predictions
│       	└── Figures/              					# Directory for figures output from individual iteration
└── sims_known/                     					# Directory containing all known age iterations
    ├── 1  				  		  						# Iteration 1
    └── 2    					  						# Iteration 2
        └── Output/              						# Directory for output from individual iteration
        	├── Data/              						# Directory for data output from individual iteration
        	│	└── test_predictions.csv/              	# CSV containing test data input ages and predictions
        	│	└── train_predictions.csv/              # CSV containing train data input ages and predictions
        	└── Figures/              					# Directory for figures output from individual iteration

```


## Citation

If you use this code, please cite the associated manuscript:

> Chamberlin, D. W., Helser, T. E., Brogan, J. D., Benson, I. M., Conner, J., Chin, A. T., Gburski, C. M., Matta, M. E., Pearce, J. A., Stone, K. R., TenBrink, T. T., & Arrington, M. B. (In review). A framework to investigate the effects of observation error on neural network predictions of fish age. *Fish and Fisheries*.

Original MMCNN model adapted from Benson et al. (2023).

```

```