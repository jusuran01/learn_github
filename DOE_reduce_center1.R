# ---------------------------------------------------------
# Face centered composite design
# ---------------------------------------------------------

library(rsm)
library(tidyverse)

# 1. Generate the Design
# ---------------------------------------------------------
# We use 'ccd' (Central Composite Design)
# n0 = 1 adds1 Center Points.(force run only 1)
# alpha = "faces" keeps points strictly within your -1 to +1 limits.

reduced_design <- ccd(
  ~ x1 + x2, 
  n0 = 1,         # Not recommend
  alpha = "faces", 
  randomize = FALSE
)

# Convert to data frame
design_data <- as.data.frame(reduced_design)

# 2. Decode to Real Values
# ---------------------------------------------------------
# Povidone K30 Limits: 2 to 6 (Center = 4, Radius = 2)
# Primojel Limits:     2 to 8 (Center = 5, Radius = 3)

final_reduced_experiment <- design_data %>%
  mutate(
    # Decode Independent Factors
    Povidone_K30_Real = 4 + (2 * x1),
    Primojel_Real     = 5 + (3 * x2),
    
    # Calculate Dependent Factor (Maize Starch)
    # Constraint: Sum = 29.2%
    Maize_Starch_Real = 29.2 - Povidone_K30_Real - Primojel_Real,
    
    # Label the Run Type for clarity
    Run_Type = case_when(
      x1 == 0 & x2 == 0 ~ "Center (Repeat for Error)",
      TRUE ~ "Factorial/Axial"
    )
  ) %>%
  # Randomize the run order (Crucial since we aren't blocking)
  sample_frac(1L) %>%
  mutate(Run_Order = row_number()) %>%
  select(Run_Order, Run_Type, x1, x2, Maize_Starch_Real, Povidone_K30_Real, Primojel_Real)

# 3. View and Export
# ---------------------------------------------------------
print("Reduced Optimization Design:")
print(final_reduced_experiment)

# Save to CSV
write.csv(final_reduced_experiment, "Reduced_Formulation_DOE.csv", row.names = FALSE)

# 4. Quick Visualization
# ---------------------------------------------------------
# You will see the 3 center points stacked in the middle
plot(final_reduced_experiment$Povidone_K30_Real, final_reduced_experiment$Primojel_Real, 
     main="Reduced Design Layout", 
     xlab="Povidone %", ylab="Primojel %", pch=19, col="blue", cex=1.5)
grid()
