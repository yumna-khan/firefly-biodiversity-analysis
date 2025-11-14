###***************************
## MBINF 6210
##
## Yumna Khan - 1094825
##
## 2025-09-27
##
##*************************** 

## _ Packages used -------

# Install packages
# install.packages("VennDiagram")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sf")
# install.packages("skimr")


# Run libraries
library(stats)
library(tidyverse)
library(viridis)
library(dplyr)
library(ggplot2)
library(VennDiagram)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(skimr)

## _ OVERVIEW -------

# This script analyzes the BIN and species richness of fireflies by country, and explores richness vs elevation in Costa Rica

# Main Question: Tropical areas (e.g. Costa Rica) will have a higher firefly species and BIN richness than temperate areas (e.g. Canada/US)

getwd()

fireflies <- read_tsv("../data/fireflies_data.tsv")
fireflies

## _ Explore the data -------
class(fireflies)
summary(fireflies)
skimr::skim(fireflies)


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

# Find the BINs and Species in the top 5 countries (top 5 countries were chosen due to the lack of samples from other countries)

sort(table(fireflies$`country/ocean`), decreasing = TRUE)[1:5]
nrow(fireflies)

# Subset each country
df_country_can <- subset(fireflies, `country/ocean` == "Canada")
df_country_can

df_country_us <- subset(fireflies, `country/ocean` == "United States")
df_country_us

df_country_cr <- subset(fireflies, `country/ocean` == "Costa Rica")
df_country_cr

df_country_m <- subset(fireflies, `country/ocean` == "Malaysia")
df_country_m

df_country_p <- subset(fireflies, `country/ocean` == "Peru")
df_country_p


### 1. Find the Number of unique BINs per Country (remove NAs to prevent inflating BIN richness) -------

# Canada
bin_can <- df_country_can$bin_uri
bin_can <- bin_can[bin_can != "" & !is.na(bin_can)]
bin_can <- length(unique(bin_can))
bin_can

# United States
bin_us <- df_country_us$bin_uri
bin_us <- bin_us[bin_us != "" & !is.na(bin_us)]
bin_us <- length(unique(bin_us))
bin_us

# Costa Rica
bin_cr <- df_country_cr$bin_uri
bin_cr <- bin_cr[bin_cr != "" & !is.na(bin_cr)]
bin_cr <- length(unique(bin_cr))
bin_cr

# Malaysia
bin_m <- df_country_m$bin_uri
bin_m <- bin_m[bin_m != "" & !is.na(bin_m)]
bin_m <- length(unique(bin_m))
bin_m

# Peru
bin_p <- df_country_p$bin_uri
bin_p <- bin_p[bin_p != "" & !is.na(bin_p)]
bin_p <- length(unique(bin_p))
bin_p

# Check if all the NAs are removed for BINs
sum(is.na(bin_can))
sum(is.na(bin_us))
sum(is.na(bin_cr))
sum(is.na(bin_m))
sum(is.na(bin_p))


### 2. Find the Number of Species per Country (remove NAs to prevent inflating species richness) -------

# Canada
species_can <- df_country_can$species
species_can <- species_can[species_can != "" & !is.na(species_can)]
species_can <- length(unique(species_can))
species_can

# United States
species_us <- df_country_us$species
species_us <- species_us[species_us != "" & !is.na(species_us)]
species_us <- length(unique(species_us))
species_us

# Costa Rica
species_cr <- df_country_cr$species
species_cr <- species_cr[species_cr != "" & !is.na(species_cr)]
species_cr <- length(unique(species_cr))
species_cr

# Malaysia
species_m <- df_country_m$species
species_m <- species_m[species_m != "" & !is.na(species_m)]
species_m <- length(unique(species_m))
species_m

# Peru
species_p <- df_country_p$species
species_p <- species_p[species_p != "" & !is.na(species_p)]
species_p <- length(unique(species_p))
species_p

# Check if all the NAs are removed for BINs
sum(is.na(species_can))
sum(is.na(species_us))
sum(is.na(species_cr))
sum(is.na(species_m))
sum(is.na(species_p))


# Reproducibility checkpoint (confirms the data has been manipulated correctly)
checkpoint_summary <- data.frame(
  country = c("Canada", "United States", "Costa Rica", "Malaysia", "Peru"),
  records = c(
    nrow(df_country_can),
    nrow(df_country_us),
    nrow(df_country_cr),
    nrow(df_country_m),
    nrow(df_country_p)
  ),
  unique_bins = c(bin_can, bin_us, bin_cr, bin_m, bin_p),
  unique_species = c(species_can, species_us, species_cr, species_m, species_p)
)
checkpoint_summary


### 3. Barplot of Species and BINs vs Country -------

# Group BIN richness for top 5 countries
richness <- data.frame(
  Country = c("Canada", "United States", "Costa Rica", "Malaysia", "Peru"),
  Species = c(species_can, species_us, species_cr, species_m, species_p),
  BINs = c(bin_can, bin_us, bin_cr, bin_m, bin_p)
)

# Reshape for ggplot (used pivot longer to reshape the graph to have species and BIN side by side)
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


### 4. Plot world map to view the BIN richness of specimens collected around the world -------
# NOTE: All countries are shown to illustrate the limited global BIN richness, rather than focusing only on the top 5.

# Calculate bin richness per country
country_data <- fireflies %>%
  filter(!is.na(bin_uri), bin_uri != "") %>%
  group_by(country_iso) %>%
  summarise(bin_richness = n_distinct(bin_uri))

country_data

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


# Performance verification:
# Confirms that the world map correctly represents BIN richness
# Compares total number of BINs across dataset vs mapped tools

# Overview of how many specimens were collected in each country
table(fireflies$country_iso)
sum(table(fireflies$country_iso))

# Find the total bins collected
total_bins <- fireflies %>%
  filter(!is.na(bin_uri), bin_uri != "") %>%
  summarise(total_bins = n_distinct(bin_uri))

total_bins

# Find the total number of bins plotted on the world map
mapped_bins <- sum(country_data$bin_richness, na.rm = TRUE)
mapped_bins

# Check if bins overlap with other countries
bin_country_overlap <- fireflies %>%
  filter(!is.na(bin_uri), bin_uri != "") %>%
  group_by(bin_uri) %>%
  summarise(num_countries = n_distinct(country_iso)) %>%
  count(num_countries)

bin_country_overlap
# Country BINs overlap! Therefore, the total bins (838) equals the total of n in bin_country_overlap. The world map has been plotted correctly


### 5. Plot the linear regression model for Costa Rica (Tropical region) -------

# Figure checkpoint: distribution of elevation in Costa Rica. Verifies data is reasonable with no extreme outliers or missing data
hist(df_country_cr$elev, main = "Elevation of Costa Rica", xlab = "Elevation (m)")

# One outlier observed at 3000 m elevation was removed to prevent skewing the regression model

#Find the elevation count for Costa Rica
total_specimens_cr <- df_country_cr %>%
  filter(!is.na(elev)) %>%
  filter(elev < 3000) %>% 
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

# Plot linear regression for elevation
ggplot(total_specimens_cr, aes(x = richness_elev, y = specimen_richness)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Firefly Richness vs Elevation in Costa Rica",
    x = "Elevation (m)",
    y = "Number of Specimens"
  ) +
  theme_minimal(base_size = 14)


### 6. Venn Diagram for Fireflies in Top 5 Countries -------
# NOTE: when plotting the venn diagram, it does not create a separate plot, but will plot on top of the current plot, therefore, grid.newpage() must be used
venn.plot <- venn.diagram(
  x = list(
    Canada = unique(na.omit(df_country_can$bin_uri)),
    `United States` = unique(na.omit(df_country_us$bin_uri)),
    `Costa Rica` = unique(na.omit(df_country_cr$bin_uri)),
    `Malaysia` = unique(na.omit(df_country_m$bin_uri)),
    `Peru` = unique(na.omit(df_country_p$bin_uri))
  ),
  filename = NULL,
  fill = c("red", "blue", "green", "Yellow", "Purple"),
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  main = "Overlap of BINs (Top 5 Countries)"
)
grid.newpage()
grid.draw(venn.plot)
