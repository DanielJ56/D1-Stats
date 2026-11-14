# ─────────────────────────────────────────────────────────────────────────────
# Grouped Scatter Plot: Subjective Image Quality Assessments
# Requires: ggplot2, tidyr, dplyr
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(tidyr)
library(dplyr)

# ── 1. Load data ──────────────────────────────────────────────────────────────
df <- read.csv("./figure2/tracking.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")

# ── 2. Long data transformation ───────────────────────────────────────────────
df_long <- df %>%
  pivot_longer(
    cols = -Assessment,
    names_to = c(".value", "subjective_image_quality"),
    names_pattern = "(Mean|SD)_(\\d+)"
  ) %>%
  mutate(subjective_image_quality = as.numeric(subjective_image_quality))

# ── 3. Limits & Highlighting Positions ────────────────────────────────────────
x_lo <- 0.5
x_hi <- 5.5

y_lo <- min(df_long$Mean - df_long$SD, na.rm = TRUE) - 5
y_hi <- max(df_long$Mean + df_long$SD, na.rm = TRUE) + 5

# Alternating background panel positions along the X-axis
hl_pos <- c(1, 3, 5)

# ── 4. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(df_long, aes(x = subjective_image_quality, y = Mean, color = Assessment, group = Assessment)) +
  
  # Background Shading (Alternating columns kept)
  annotate("rect",
           xmin = hl_pos - 0.5, xmax = hl_pos + 0.5,
           ymin = y_lo, ymax = y_hi,
           fill = "grey91") +
  
  # Manual Horizontal Grid Reference Lines only
  geom_hline(yintercept = seq(0, 120, by = 20), linetype = "dotted",
             colour = "grey75", linewidth = 0.4) +
  
  # Data geometries with Grouping (Dodge) effect
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD), 
    position = position_dodge(width = 0.4), 
    width = 0.15,
    linewidth = 0.65
  ) +
  
  geom_point(
    position = position_dodge(width = 0.4), 
    size = 3.5,
    shape = 15
  ) +
  
  # Scale Configuration
  scale_x_continuous(
    limits = c(x_lo, x_hi),
    breaks = 1:5,
    expand = expansion(0)
  ) +
  
  scale_y_continuous(
    limits = c(y_lo, y_hi),
    expand = expansion(0)
  ) +
  
  # Color Palette Styling
  scale_color_manual(
    values = c("BRIQUE" = "#2c3e50", "PIQUE" = "#e74c3c", "IL-NIQE" = "#3498db")
  ) +
  
  # Force legend icons to be boxes and completely remove the title
  guides(
    color = guide_legend(title = NULL, override.aes = list(shape = 15, size = 4, linetype = 0))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = "subjective image quality", 
    y = "objective image quality"
  ) +
  
  # Theme Configuration
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y        = element_text(size = 8.5, colour = "grey40"),
    axis.text.x        = element_text(colour = "grey40"),
    axis.title.x       = element_text(size = 9, colour = "grey40", margin = margin(t = 8)),
    axis.title.y       = element_text(size = 9, colour = "grey40", margin = margin(r = 8)),
    
    axis.line.x        = element_line(colour = "grey55", linewidth = 0.5),
    axis.line.y        = element_line(colour = "grey55", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    panel.background   = element_blank(),
    panel.border       = element_blank(),
    
    # Legend repositioned to bottom right without a boundary box
    legend.position    = c(0.98, -0.1),
    legend.justification = c(1, 0),
    legend.direction   = "horizontal",
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8, colour = "grey40"),
    legend.margin      = margin(6, 6, 6, 6),
    
    plot.margin        = margin(12, 12, 12, 12)
  )

# ── 5. Save ───────────────────────────────────────────────────────────────────
ggsave(
  filename = "grouped_scatter_plot.png",
  plot     = p,
  width    = 8,  
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

message("Saved: grouped_scatter_plot.png")