# ─────────────────────────────────────────────────────────────────────────────
# Forest plot: Model Architecture | Model Tier
# Requires: ggplot2, scales
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(scales)

# ── 1. Load data ──────────────────────────────────────────────────────────────
arch <- read.csv("./figure5/regression_arch.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
tier <- read.csv("./figure5/regression_tier.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")

# Rename 'models' to 'model' for script compatibility
colnames(arch)[1] <- "model"
colnames(tier)[1] <- "model"

# ── 2. Strip asterisks ────────────────────────────────────────────────────────
strip_asterisk <- function(df) {
  df$model <- trimws(gsub("\\*", "", df$model))
  df
}
arch <- strip_asterisk(arch)
tier <- strip_asterisk(tier)

# ── 3. Master row order (top → bottom) ────────────────────────────────────────
master_levels <- c(
  # Model Groups
  "CLIP", "LLM-Based", "Medical", "Dermatological",
  # Objective Quality Metrics
  "Lighting", "Camerawork", "Artifacts",
  # Subjective Quality Metrics
  "Morphology", "Size", "Demarcation", "Color", "Texture", "Distribution"
)

# ── 4. Pad every dataset to the full 13-row set (NA for absent rows) ──────────
pad_to_master <- function(df, master) {
  df <- df[, c("model", "OR", "lower", "upper")]
  missing <- setdiff(master, df$model)
  if (length(missing) > 0) {
    df <- rbind(df, data.frame(model = missing, OR = NA_real_,
                               lower = NA_real_, upper = NA_real_,
                               stringsAsFactors = FALSE))
  }
  df$model <- factor(df$model, levels = rev(master))
  df
}

arch_p <- pad_to_master(arch, master_levels)
tier_p <- pad_to_master(tier, master_levels)

# ── 5. Shared x limits (Aligned cleanly at 0.05) ──────────────────────────────
x_lo <- 0.05
x_hi <- max(c(arch_p$upper, tier_p$upper), na.rm = TRUE) * 2.5

# ── 6. Long data with panel factor ────────────────────────────────────────────
panel_levels <- c("Architecture", "Tier-Level")

all_data <- rbind(
  transform(arch_p, panel = panel_levels[1]),
  transform(tier_p, panel = panel_levels[2])
)
all_data$panel <- factor(all_data$panel, levels = panel_levels)

# ── 7. Row highlight positions ────────────────────────────────────────────────
n_rows <- length(master_levels)
hl_pos <- seq(2, n_rows, by = 2)

# Left-of-labels extension, restricted to the FIRST facet only
left_ext <- data.frame(ymin  = hl_pos - 0.5,
                       ymax  = hl_pos + 0.5,
                       panel = factor(panel_levels[1], levels = panel_levels))

# ── 8. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(all_data, aes(x = OR, y = model)) +
  
  # Background Shading
  annotate("rect",
           ymin = hl_pos - 0.5, ymax = hl_pos + 0.5,
           xmin = x_lo, xmax = x_hi,
           fill = "grey91") +
  
  geom_rect(data = left_ext,
            aes(ymin = ymin, ymax = ymax),
            xmin = x_lo * 1e-6, xmax = x_lo,
            fill = "grey91", inherit.aes = FALSE) +
  
  # Manual Y-axis line drawn on ALL panels exactly at the 0.05 left border
  geom_vline(xintercept = x_lo, colour = "grey55", linewidth = 0.5) +
  
  # Lighter vertical dotted lines
  geom_vline(xintercept = c(0.1, 0.3, 3), linetype = "dotted",
             colour = "grey75", linewidth = 0.4) +
  
  # Main reference line at OR = 1
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey55", linewidth = 0.5) +
  
  # Data geometries
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.30, linewidth = 0.65,
                 colour = "black", na.rm = TRUE) +
  
  geom_point(size = 3.5, colour = "black", shape = 15, na.rm = TRUE) +
  
  # Position titles underneath the panels
  facet_grid(. ~ panel, switch = "x") +
  
  scale_x_log10(
    limits = c(x_lo, x_hi),
    breaks = c(0.1, 0.3, 1, 3), 
    labels = label_number(drop0trailing = TRUE),
    expand = expansion(0),
    oob    = scales::oob_keep
  ) +
  scale_y_discrete(drop = FALSE) +
  coord_cartesian(clip = "off") +
  
  labs(
    x     = "Odds Ratio (log scale)",
    y     = NULL
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5, size = 13,
                                      margin = margin(b = 12)),
    
    strip.text         = element_text(face = "bold", size = 10.5, hjust = 0.5,
                                      margin = margin(t = 10, b = 5)),
    strip.background   = element_blank(),
    strip.placement    = "outside", 
    
    axis.text.y        = element_text(size = 8.5, colour = "grey40", hjust = 1),
    axis.text.x        = element_text(colour = "grey40"),
    axis.title.x       = element_text(size = 9, colour = "grey40",
                                      margin = margin(t = 8)),
    
    # Axis line settings
    axis.line.x        = element_line(colour = "grey55", linewidth = 0.5),
    axis.line.y        = element_blank(), 
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    panel.background   = element_blank(),
    panel.border       = element_blank(),
    panel.spacing.x    = unit(1.5, "lines"), 
    legend.position    = "none",
    plot.margin        = margin(12, 12, 12, 12)
  )

# ── 9. Save ───────────────────────────────────────────────────────────────────
ggsave(
  filename = "forest_plot.png",
  plot     = p,
  width    = 11,  
  height   = 8,
  dpi      = 300,
  bg       = "white"
)

message("Saved: forest_plot.png")