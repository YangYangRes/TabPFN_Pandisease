# Fine-tuned TabPFN boosts Multi-disease Diagnosis based on Transcriptomic Data

This repository contains the analysis code, example input tables, trained model artifacts, and figure-generation scripts for the manuscript:

> Fine-tuned TabPFN boosts Multi-disease Diagnosis based on Transcriptomic Data

## Overview

This study evaluates fine-tuned Tabular Prior-Data Fitted Network (TabPFN) models for transcriptome-based diagnosis across multiple diseases. A Pan-disease Cross-discipline Panel (PCP) selected representative diseases, curated public transcriptomic datasets, and supervised the modeling workflow.

The analysis includes 220 transcriptomic datasets from GEO and TCGA, covering 7,151 control samples and 13,950 disease samples. Fine-tuned TabPFN was compared with five conventional machine-learning algorithms across 22 representative diseases from 20 clinical departments.

## Models

The repository compares six diagnostic modeling approaches:

- TabPFN
- Logistic Regression (LR)
- Support Vector Machine (SVM)
- Gradient Boosting Decision Tree (GBDT)
- K-Nearest Neighbors (KNN)
- Random Forest (RF)

Conventional machine-learning models were tuned using repeated stratified 5-fold cross-validation and Bayesian optimization with the Tree-structured Parzen Estimator algorithm implemented in `hyperopt`.

## Repository Contents

| Path | Description |
| --- | --- |
| `AAA/`, `AD/`, `AR/`, `ATB/`, `BC/`, `COPD/`, `CRC/`, `EM/`, `GN/`, `HF/`, `LC/`, `MD/`, `NAFLD/`, `OA/`, `PD/`, `PE/`, `Psoriasis/`, `SCZ/`, `Sepsis/`, `SLE/`, `T2D/`, `UC/` | Disease-specific model training and external-validation workflows with training/test datasets, notebooks, and saved models. |
| `SHAP/` | SHAP-based model interpretation workflows and feature-attribution outputs. |
| `BarPlot/` | R code and input tables for model-performance bar plots. |
| `HeatMap/` | R code for heatmap visualization. |
| `APs/` | R code and data for average-precision summary visualization. |
| `Confusion Matrix/` | Confusion-matrix input tables and plotting code. |

Most disease/task folders include the following notebooks:

```text
TabPFN.ipynb
RF.ipynb
GBDT.ipynb
SVM.ipynb
KNN.ipynb
Logistic.ipynb
```

These notebooks implement the corresponding classifier workflow for each transcriptome-based diagnostic task.

## Analysis Workflow

1. Collect disease-control transcriptomic datasets from GEO and TCGA.
2. Select training datasets and independent external test datasets for each disease.
3. Process expression matrices and phenotype labels.
4. Select modeling genes using differential expression analysis, LASSO regression, and Boruta feature selection.
5. Train fine-tuned TabPFN and five conventional machine-learning classifiers.
6. Tune conventional ML models using repeated stratified cross-validation and Bayesian optimization.
7. Evaluate each model on independent external test datasets.
8. Report AUROC, average precision, accuracy, sensitivity, specificity, precision, F1 score, and confusion matrices.
9. Perform SHAP-based model interpretation using lung cancer as a representative case.
10. Generate manuscript figures using Python notebooks and R scripts.

## Requirements

The codebase uses Python notebooks and R scripts.

A typical Python environment should include:

```bash
pip install pandas numpy scikit-learn imbalanced-learn hyperopt tabpfn shap joblib matplotlib
```

The R visualization scripts use packages such as:

```r
install.packages(c("ggplot2", "RColorBrewer"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

The manuscript reports analyses performed using Python 3.11 and R 4.4.2. Package versions may need to match the original analysis environment, especially for `tabpfn`, `scikit-learn`, `hyperopt`, and `shap`.

## Usage

Open the relevant disease/task folder and run the notebook for the model of interest. The notebooks use relative paths, so run each notebook from its own folder.

```bash
jupyter lab
```

Example:

```text
LC/TabPFN.ipynb
LC/RF.ipynb
LC/Logistic.ipynb
```

For figure generation, open the corresponding R script from its own folder and confirm that the required input CSV files are present.

## Data and Outputs

The repository includes:

- CSV files for training, validation, prediction, SHAP analysis, and plotting.
- Jupyter notebooks for model training and evaluation.
- R scripts for manuscript-style visualization.
- Serialized `.pkl` and `.joblib` model files.
- Exported prediction and performance files used by downstream plotting scripts.

Some notebooks contain outputs from previous runs. For exact regeneration, rerun the notebooks in a clean environment with the required dependencies installed.

## Reproducibility Notes

- Fixed random seeds are defined in the notebooks where applicable.
- Conventional ML models use repeated stratified 5-fold cross-validation during tuning.
- Independent external test datasets are used to assess generalizability.
- Batch correction, feature selection, model training, and external validation should be performed consistently with the manuscript workflow.
- Serialized models are included for convenience, but rerunning notebooks is recommended when auditing the full pipeline.
