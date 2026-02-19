# Project Overview
This project investigates global firefly biodiversity patterns using species richness and Barcode Index Number (BIN) data. Fireflies (family Lampyridae) are bioluminescent beetles that function as important ecological bioindicators, providing insight into habitat quality and environmental change.

By analyzing global BIN distribution and species richness, this study evaluates biodiversity patterns across tropical and temperate regions and examines the influence of elevation on firefly diversity within Costa Rica.

## Hypotheses
1. Tropical regions (e.g., Costa Rica) will exhibit higher BIN richness than temperate regions (e.g., Canada, United States).

2. Within Costa Rica, firefly richness will decrease with increasing elevation due to cooler temperatures and reduced habitat complexity at higher altitudes.

# Methods
All analyses were conducted in R.

## 1. Dataset Description
The dataset was obtained from boldsystems.org under the Lampyridae[tax], and saved as a TSV file.

## 2. Data Processing
- Filtered dataset to identify the top five countries by record count
- Calculated species richness and BIN richness per country
- Subset tropical region data (Costa Rica) for elevation analysis

## 3. Statistical Analysis
- Computed species and BIN richness metrics
- Performed linear regression to assess the relationship between elevation and firefly richness

## 4. Visualization
- Double bar plot comparing species vs. BIN richness (Top 5 countries)
- Global world map displaying BIN richness distribution
- Linear regression plot illustrating elevation trends

# Key Findings
Costa Rica exhibited the highest BIN richness among the countries analyzed, yet it had one of the lowest recorded species counts. In contrast, the United States showed the highest species richness. Contrary to the original hypothesis, firefly richness in Costa Rica increased with elevation (slope = 0.067, p = 0.019), indicating a significant positive relationship between altitude and diversity. Overall, the results suggest the presence of sampling bias and uneven global representation within barcode datasets, which may influence observed biodiversity patterns.

# Limitations
This analysis is subject to several limitations. Sampling effort was unequal across countries, which may have affected richness comparisons. Tropical regions appear to be underrepresented in the dataset, potentially skewing biodiversity patterns. Additionally, incomplete metadata and possible misidentification within barcode records may introduce uncertainty into the results.

# Ecological Significance
Fireflies serve as ecological bioindicators, meaning changes in their diversity can reflect habitat degradation, pollution, artificial light exposure, and climate change impacts. Understanding global distribution patterns contributes to conservation planning and biodiversity monitoring.
