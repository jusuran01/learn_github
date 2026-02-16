# ---------------------------------------------------------
# CODED OPTIMIZATION DESIGN (Face-Centered)
# ---------------------------------------------------------

# Load libraries
if(!require(rsm)) install.packages("rsm")
if(!require(tidyverse)) install.packages("tidyverse")
library(rsm)
library(tidyverse)

# 1. Create the Coded Design (Standard CCF for 2 factors)
# ---------------------------------------------------------
# We generate a standard table with Low (-1), High (+1), and Center (0) points.
# We treat Povidone and Primojel as the two independent factors.

# Create the base design (unique runs)
coded_design <- ccd(
  ~ x1 + x2, 
  n0 = 1,       # Number of center points per block
  alpha = "faces", # Face-centered (points stay within -1 to +1 range)
  randomize = FALSE
)

# Extract just the data frame
design_data <- as.data.frame(coded_design)

# 2. Decode: Convert Coded Units to Real %
# ---------------------------------------------------------
# Formula for Real Value: Center + (Range/2 * Coded_Value)

# Povidone K30 Limits: 2 to 6 (Center = 4, Radius = 2)
# Primojel Limits:     2 to 8 (Center = 5, Radius = 3)

design_data <- design_data %>%
  mutate(
    # Calculate Real Values for Independent Factors
    Povidone_K30_Real = 4 + (2 * x1),
    Primojel_Real     = 5 + (3 * x2),
    
    # Calculate Dependent Factor (Maize Starch)
    # Constraint: Sum must be 29.2%
    Maize_Starch_Real = 29.2 - Povidone_K30_Real - Primojel_Real
  )

# 3. Add Replication & Final Formatting
# ---------------------------------------------------------
# You requested to "repeat 2 times". 
# We will duplicate the entire set of unique runs.

final_experiment <- design_data %>%
  # Select only relevant columns
  select(x1, x2, Maize_Starch_Real, Povidone_K30_Real, Primojel_Real) %>%
  # Duplicate the entire design 2 times
  slice(rep(1:n(), times = 2)) %>%
  # Add a Block/Replicate column for tracking
  mutate(Replicate = rep(1:2, each = nrow(design_data))) %>%
  # Randomize the run order
  sample_frac(1L) %>%
  mutate(Run_Order = row_number()) %>%
  # Reorder columns for the lab technician
  select(Run_Order, Replicate, x1, x2, Maize_Starch_Real, Povidone_K30_Real, Primojel_Real)

# 4. View and Check Constraints
# ---------------------------------------------------------
print("Final Design Table (Coded and Real Values):")
print(final_experiment)

# Verification: Check if Maize Starch is within bounds (15.2 - 25.2)
print("Range check for Maize Starch:")
print(range(final_experiment$Maize_Starch_Real))

# Save to CSV
write.csv(final_experiment, "Optimizing_Formulation_Design_Coded.csv", row.names = FALSE)

# 5. Visual Check (Optional)
# ---------------------------------------------------------
plot(final_experiment$Povidone_K30_Real, final_experiment$Primojel_Real, 
     main="Design Space (Real Values)", 
     xlab="Povidone %", ylab="Primojel %", pch=19)
