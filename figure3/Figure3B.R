# ==============================================================================
# Heatmap Visualization for Pairwise McNemar P-Values
# Monochromatic Light-Red Palette (Inverse P-Value Scale with Flipped Legend)
# ==============================================================================

# 1. Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Read the raw data
raw_data <- read.csv("./figure3./mcnemar.csv", stringsAsFactors = FALSE, check.names = FALSE)

# 3. Reshape and clean the data from a wide matrix to a long format
colnames(raw_data)[1] <- "Model_A"

mcnemar_long <- raw_data %>%
  pivot_longer(
    cols = -Model_A, 
    names_to = "Model_B", 
    values_to = "p_string",
    values_transform = list(p_string = as.character)
  ) %>%
  # Filter out empty cells
  filter(!is.na(p_string) & p_string != "") %>%
  # Parse the p-value string into numeric format
  mutate(
    p_value = case_when(
      p_string == "<0.001" ~ 0.0005,
      TRUE ~ as.numeric(p_string)
    ),
    # Inverse mapping (1 - p): 
    # High significance (p close to 0) yields a value close to 1 (Dark Red)
    # Low significance (p close to 1) yields a value close to 0 (Light Pink/Red)
    inverse_p = 1 - p_value,
    
    # Cell text label is now ONLY the raw string without stars
    label = p_string
  )

# 4. Enforce strict factor ordering to ensure a clean matrix display
all_models <- unique(c(mcnemar_long$Model_A, mcnemar_long$Model_B))
model_order <- c("ChatGPT", "BiomedCLIP", "Citrus-V", "Claude", "Llama", "Monet", "SkinGPT")
model_order <- model_order[model_order %in% all_models] 

mcnemar_long <- mcnemar_long %>%
  mutate(
    Model_A = factor(Model_A, levels = rev(model_order)),
    Model_B = factor(Model_B, levels = model_order)
  )

# 5. Generate the Heatmap
p_heatmap <- ggplot(mcnemar_long, aes(x = Model_B, y = Model_A, fill = inverse_p)) +
  # White tile borders (size = 0.5)
  geom_tile(color = "white", size = 0.5) +
  # Add plain text labels inside tiles
  geom_text(aes(label = label), color = "black", size = 3.5) +
  # Single color gradient using the inverse p-value scale
  scale_fill_gradient(
    low = "#FFF0F0",   # Soft, very light red for inverse_p = 0 (p = 1)
    high = "#C02020",  # Rich, deep red for inverse_p = 1 (p = 0)
    name = "P-Value",
    breaks = c(0, 0.33, 0.67, 1),
    labels = c("1.00", "0.67", "0.33", "0.00"), # Uniform labels with 0.00 at the bottom
    limits = c(0, 1),
    guide = guide_colourbar(reverse = TRUE)     # FIX: Flips legend to go from 0 (dark red) to 1 (light red)
  ) +
  # Clean layout adjustments
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_blank(), # Removes X axis label
    axis.title.y = element_blank(), # Removes Y axis label
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
  )

# 6. Save the output plot
ggsave("mcnemar_heatmap_inverse_red.png", plot = p_heatmap, width = 7, height = 6, dpi = 300)