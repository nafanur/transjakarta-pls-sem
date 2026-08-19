##### PLS SEM DATA KUESIONER TJ DENGAN R STUDIO #####
# Download and install the SEMinR package
# install.packages("seminr")
# Make the SEMinR library ready to use
library(seminr)

# Load the data
data_sem <- read.csv (file= "D:/kuliah/MAGANG PUSDATINHUB/HASIL PLS SEM TJ nafa/tj_sem.csv", header = TRUE, sep = ";", sheet = "")
head(data_sem) 
data_sem

##### 1. Measurement model #####
simple_mm <- constructs(
  composite("tarif_naik", multi_items("tarif_", 1:7)),
  composite("penggunaan", multi_items("minat_", 1:5))
)

##### 2. Structural model #####
simple_sm <- relationships(
  paths(from = "tarif_naik", to = "penggunaan")
)

##### 3. Estimate the model ####
simple_model <- estimate_pls(data = data_sem,
                             measurement_model = simple_mm,
                             structural_model = simple_sm,
                             inner_weights = path_weighting,
                             missing = mean_replacement,
                             missing_value = "-99")
plot(simple_model)

##### 4. Summarizing the Model #####
# Summarize the model results
summary_simple <- summary(simple_model)
summary_simple
# Inspect the model’s path coeffcients and the R^2 values
summary_simple$paths
summary_simple$validity$cross_loadings

# Inspect the construct reliability metrics
summary_simple$reliability
summary_simple$loadings
plot(summary_simple$reliability)

# output direct effect dan indirect effect
summary_simple$total_effects
summary_simple$total_indirect_effects

##### 5. Bootstrapping the Model #####
# Bootstrap the model
boot_simple <- bootstrap_model(seminr_model = simple_model,
                                        nboot = 1000,
                                        cores = NULL,
                                        seed = 123)
plot(boot_simple)

# Store the summary of the bootstrapped model
sum_boot_simple <- summary(boot_simple)
sum_boot_simple
# Inspect the bootstrapped structural paths
sum_boot_simple$bootstrapped_paths
# Inspect the bootstrapped indicator loadings
sum_boot_simple$bootstrapped_loadings
sum_boot_simple$bootstrapped_HTMT
sum_boot_simple$bootstrapped_total_paths

# Ambil nilai T Stat dari hasil bootstrapping
t_value <- sum_boot_simple$bootstrapped_paths[,"T Stat."]

# Hitung p-value (dua arah / two-tailed)
p_value <- 2 * (1 - pnorm(abs(t_value)))

# Gabungkan dengan tabel aslinya
cbind(sum_boot_simple$bootstrapped_paths, p_value)

# hasil analisis nya bisa di ekspor ke csv file
write.csv(x = summary_simple$paths, file = "paths.csv")


# Estimate the model with default settings
# simple_model <- estimate_pls(data = data_sem,measurement_model = simple_mm,structural_model = simple_sm,missing_value = "-99")


