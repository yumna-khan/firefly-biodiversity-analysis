###***************************
## MBINF 6210
##
## Yumna Khan - 1094825
##
## 2025-09-27
##
##*************************** 

# Run libraries
library(tidyverse)
library(viridis)
library(VennDiagram)
library(rnaturalearth)
library(rnaturalearthdata)
library(stats)
library(sf)
library(skimr)
library(visdat)
library(DataExplorer)

# *************************************************************************
# -------------------- SECTION 1: Explore the data  -----------------------
# *************************************************************************

# This script analyzes the BIN and species richness of fireflies by country, and explores richness vs elevation in Costa Rica

# Main Question: Tropical areas (e.g. Costa Rica) will have a higher firefly species and BIN richness than temperate areas (e.g. Canada/US)

fireflies = vroom::vroom("data/fireflies_data.tsv", col_select = -c(91)) %>%
  rename(Country = 'country/ocean', 'BINs' = 'bin_uri', 'Species' = 'species')
fireflies %>% 
  problems() %>% 
  View()

# *************************************************************************
# -------------------- SECTION 2: Explore the data  -----------------------
# *************************************************************************

class(fireflies)
summary(fireflies)
skimr::skim(fireflies)

# RJ - Overview of dataset structure, variable types, and completeness.
# RJ - The intro plot shows that roughly half the dataset has missing values, suggesting the need for cleaning or feature filtering before further analysis.
plot_intro(fireflies)

# RJ - Visual summary of missing data across all columns.
# RJ - Many features show high NA proportions, so we should focus on variables with acceptable completeness to avoid biased results.
plot_missing(fireflies)

# RJ - Keep only features with 30% or less missing data.
# RJ - This threshold keeps variables with sufficient information for analysis while removing those too incomplete to interpret reliably.
fire_filtered <- fireflies %>% 
  select(where(~ mean(is.na(.)) <= 0.30))

# RJ - Check which variables were retained after filtering to confirm which are complete enough for use.
colnames(fire_filtered)

# RJ - Visualize numeric variable distributions to identify skew, outliers, or trends.
# RJ - Elevation shows noticeable variation worth exploring further. Numerical IDs (geoid, specimenid, taxid) are excluded since they’re identifiers, not true continuous data.
plot_histogram(fire_filtered)

# *************************************************************************
# ---------- SECTION 3: TOP 5 COUNTRIES — BIN AND SPECIES SUMMARY --------
# *************************************************************************

# RJ - Identify and summarize the top 5 countries based on sample count. Focusing on these reduces noise from countries with very few observations, ensuring more reliable comparisons.
top_countries <- fireflies %>%
  filter(!is.na(`Country`)) %>%
  count(`Country`, sort = TRUE, name = "Record_Count") %>%
  slice_head(n = 5)
top_countries

# RJ - Count the number of unique BINs (genetic clusters) for each of the top 5 countries. Filtering removes missing or blank BIN entries before summarizing.
bin_counts <- fireflies %>%
  filter(`Country` %in% top_countries$'Country', !is.na(BINs), BINs != "") %>%
  distinct(`Country`, BINs) %>%
  count(`Country`, name = "BINs")

# RJ - Count the number of unique species per country, using the same top 5-country filter. This provides a comparable measure of taxonomic diversity alongside BIN richness.
species_counts <- fireflies %>%
  filter(`Country` %in% top_countries$'Country', !is.na(Species), Species != "") %>%
  distinct(`Country`, Species) %>%
  count(`Country`, name = "Species")

# RJ - Merge sample count, BIN count, and species count into one summary table for downstream analysis. The resulting dataframe shows overall sampling effort and biodiversity measures per country.
summary_table <- top_countries %>% 
  left_join(bin_counts, by = "Country") %>% 
  left_join(species_counts, by = "Country")
summary_table

# *************************************************************************
# ----------------------- SECTION 4: PLOTTING ANALYSIS --------------------
# *************************************************************************

### PLOT 1: Firefly Species VS BIN Richness (Top 5 Countries Barplot) -----

# RJ - Remove raw record counts to focus on diversity metrics.
richness <- subset(summary_table, select = -Record_Count)

# Reshape the data for visualization. Pivoting BINs and Species into long format allows easy comparison of both diversity metrics in a single ggplot.
richness_long <- richness %>%
  pivot_longer(cols = c(Species, BINs), names_to = "Type", values_to = "Count")

# Plot bar graph for Species and BINs vs Country
ggplot(richness_long, aes(x = Country, y = Count, fill = Type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Count),
    position = position_dodge(width = 0.9),
    vjust = -0.3, size = 4
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(
    title = "Firefly Species vs BIN Richness (Top 5 Countries)",
    x = "Country",
    y = "Richness (Count)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

### PLOT 2 - World map to view the BIN richness of specimens collected -----
# around the world. NOTE: All countries are shown to illustrate the limited global BIN richness, rather than focusing only on the top 5.

# Calculate bin richness per country
country_data <- fireflies %>%
  filter(!is.na(BINs), BINs != "") %>%
  group_by(country_iso) %>%
  summarise(bin_richness = n_distinct(BINs))

# Load world country shapefile data for plotting geographic maps
world <- ne_countries(scale = "medium", returnclass = "sf")
world

# Join the BIN data into the world map
world_bin <- left_join(world, country_data, by = c("iso_a2_eh" = "country_iso"))

# Plot the world map
ggplot(data = world_bin) +
  geom_sf(aes(fill = bin_richness), color = "gray40", size = 0.1) +
  scale_fill_viridis(
    option = "D",
    na.value = "gray90",
    name = "BIN Richness"
  ) +
  labs(
    title = "Firefly BIN Richness by Country"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "left",
    plot.title = element_text(hjust = 0.5)
  )
## RJ - Costa Rica appears as the only country highlighted in yellow, suggesting a distinct BIN richness pattern that merits further exploration. 


#### Performance verification: -----------------------
# Confirms that the world map correctly represents BIN richness
# Compares total number of BINs across dataset vs mapped tools

# Overview of how many specimens were collected in each country
table(fireflies$country_iso)
sum(table(fireflies$country_iso))

# Find the total bins collected
total_bins <- fireflies %>%
  filter(!is.na(BINs), BINs != "") %>%
  summarise(total_bins = n_distinct(BINs))
total_bins

# Find the total number of bins plotted on the world map
mapped_bins <- sum(country_data$bin_richness, na.rm = TRUE)
mapped_bins

# Check if bins overlap with other countries
bin_country_overlap <- fireflies %>%
  filter(!is.na(BINs), BINs != "") %>%
  group_by(BINs) %>%
  summarise(num_countries = n_distinct(country_iso)) %>%
  count(num_countries)

bin_country_overlap
# Country BINs overlap! Therefore, the total bins (838) equals the total of n in bin_country_overlap. The world map has been plotted correctly

### PLOT 3: Examining Costa Rica Elevation Distribution -------

# Figure checkpoint: distribution of elevation in Costa Rica. Verifies data is reasonable with no extreme outliers or missing data

fireflies %>%
  filter(Country == "Costa Rica", !is.na(elev), elev != 999) %>%
  ggplot(aes(x = elev)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Elevation of Costa Rica", x = "Elevation (m)")

# One outlier observed at 3000 m elevation was removed to prevent skewing the regression model
# RJ- plotted histogram using ggplot (for ease in editing), and without using extra variables

#Find the elevation count for Costa Rica
# RJ - edited to not require a df_country_cr dataframe (more dynamic) 
total_specimens_cr <- fireflies %>%
  filter(Country == "Costa Rica", !is.na(elev), elev < 3000) %>%
  count(elev, sort = TRUE)
total_specimens_cr

# Check if NA removed and explore the data
sum(is.na(total_specimens_cr))
summary(fireflies$elev)
mean(fireflies$elev, na.rm = TRUE)

# Compute the X and Y coordinates for plotting
richness_elev <- total_specimens_cr$elev # X-axis
specimen_richness <- total_specimens_cr$n # Y-axis

# Linear regression calculation
model <- lm(specimen_richness ~ richness_elev, data = total_specimens_cr)
summary(model)

### PLOT 4: linear regression for elevation ----------------

ggplot(total_specimens_cr, aes(x = richness_elev, y = specimen_richness)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Firefly Richness vs Elevation in Costa Rica",
    x = "Elevation (m)",
    y = "Number of Specimens"
  ) +
  theme_minimal(base_size = 14)

### PLOT 5: Venn Diagram for Fireflies in Top 5 Countries -------
# NOTE: when plotting the venn diagram, it does not create a separate plot, but will plot on top of the current plot, therefore, grid.newpage() must be used

bin_list <- fireflies %>%
  filter(Country %in% top_countries$Country, !is.na(BINs), BINs != "") %>%
  group_by(Country) %>%
  summarise(BINs = list(unique(BINs))) %>%
  deframe()

venn.plot <- venn.diagram(
  x = bin_list,
  filename = NULL,
  fill = viridis::viridis(length(bin_list)),  
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  main = "Overlap of BINs (Top 5 Countries)"
)

grid.newpage()
grid.draw(venn.plot)

# RJ - removed hardcoding for this plot (more dynamic), changed it to viridis for accessibility 
