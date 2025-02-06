# 📌 Metaverse-Based Training: Code for Data Analysis

## 📝 Overview
This repository contains the code and data used for the analyses presented in the paper:

> **"Is Immersivity Important in Training Soft Skills in the Metaverse?"**
>
> _Authors: Bartolotta, S., Pizzolante, M., Motta, V., Garza, L., & Gaggioli, A._  
> _Published in: International Conference on Extended Reality, September 2024_  
> _Cham: Springer Nature Switzerland_  

### 🔍 Abstract
The study explores the impact of immersive versus non-immersive metaverse-based training on employees’ empathic communication abilities. A quasi-experimental design was adopted, where participants were randomly assigned to either a non-immersive desktop training condition or an immersive virtual reality (VR) training condition. The analyses focused on self-perceived communication competence, satisfaction with the training experience, embodiment, and sense of presence. The results indicate that while immersivity affects embodiment and presence, it does not significantly impact communication competence or satisfaction.

## 📂 Repository Structure
```
├── data/                     # Raw and processed datasets
│   ├── raw_data.csv          # Original data collected from the study
│   ├── cleaned_data.csv      # Processed and cleaned dataset
│
├── scripts/                  # R scripts for analysis
│   ├── normality_test.R      # Normality test using Shapiro-Wilk
│   ├── data_preprocessing.R  # Data cleaning and preparation
│   ├── statistical_analysis.R # Statistical analysis (ANOVA, regression, correlation)
│   ├── visualization.R        # Data visualization scripts
│
├── results/                  # Output files from analysis
│   ├── descriptive_stats.txt  # Summary of key statistics
│   ├── figures/              # Graphs and plots used in the paper
│
├── README.md                 # Documentation (this file)
└── requirements.txt          # List of dependencies
```

## 🚀 Getting Started

### Prerequisites
To run the analyses, install the required R packages:
```r
install.packages(c("readxl", "tidyverse", "dplyr", "ggplot2", "gtsummary", "lmerTest", "ggcorrplot"))
```

### Running the Code
1. **Test for Normality**:
   ```r
   source("scripts/normality_test.R")
   ```
   This script applies the Shapiro-Wilk test to assess normality in the dataset.

2. **Preprocess the Data**:
   ```r
   source("scripts/data_preprocessing.R")
   ```
   This script cleans the dataset and prepares it for analysis.

3. **Run Statistical Analysis**:
   ```r
   source("scripts/statistical_analysis.R")
   ```
   This script performs statistical tests (ANOVA, mixed models, regression, correlation) to analyze the effect of immersivity.

4. **Generate Visualizations**:
   ```r
   source("scripts/visualization.R")
   ```
   Outputs figures used in the paper.

## 📊 Key Analyses Included
- **Normality Test**: Shapiro-Wilk test applied to all relevant variables.
- **Descriptive Statistics**: Means, standard deviations, and group comparisons.
- **Mixed ANOVA**: Evaluating the effects of immersivity over different conditions.
- **Linear Regression**: Examining relationships between training conditions and communication competence.
- **Correlations**: Spearman correlations between embodiment, ICT, and other measures.
- **Data Visualizations**: Bar plots and correlation matrices.

## 🤝 Contribution
If you wish to contribute, feel free to fork the repository and submit a pull request.

## 📩 Contact
For inquiries, please contact Marta Pizzolante at marta.pizzolante@unicatt.it 

---
**Citation:** If you use this code, please cite our paper as follows:
```bibtex
@inproceedings{Bartolotta2024,
  author    = {Bartolotta, S., Pizzolante, M., Motta, V., Garza, L., & Gaggioli, A.},
  title     = {Is Immersivity Important in Training Soft Skills in the Metaverse?},
  booktitle = {International Conference on Extended Reality},
  pages     = {38-57},
  year      = {2024},
  publisher = {Springer Nature Switzerland},
}
```

