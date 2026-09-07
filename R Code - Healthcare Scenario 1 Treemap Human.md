# ==============================================================================
# HUMAN TREEMAP — NO DOMAIN LABELS, COLORED LEGEND ONLY
# ==============================================================================
rm(list = ls())
library(readxl)
library(treemap)
library(dplyr)

# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------
data <- read_excel(
  "Manuscript - Rubric/Qualitative Outputs/Section 1.xlsx"
)

# ------------------------------------------------------------------------------
# EXPLICIT DOMAIN LIST
# ------------------------------------------------------------------------------
domains <- c(
  "Accuracy",
  "Safety",
  "Empathetic",
  "Communicates Clearly",
  "Context Aware",
  "Completeness",
  "Referencing",
  "Recognises Uncertainty",
  "Avoid Coerciveness",
  "Follows Instructions"
)

# ------------------------------------------------------------------------------
# CLEAN DATA - HUMAN ONLY (rows 89 to end)
# ------------------------------------------------------------------------------

data_clean_human <- data %>%
  select(Name, References) %>%
  filter(!is.na(Name)) %>%
  slice(1:72) %>%
  mutate(
    Name = trimws(Name),
    References = suppressWarnings(as.numeric(References))
  )

# ------------------------------------------------------------------------------
# BUILD DATA - THEMES ONLY
# ------------------------------------------------------------------------------
treemap_data <- data.frame(
  Domain = character(),
  Name = character(),
  References = numeric(),
  stringsAsFactors = FALSE
)

current_domain <- NULL

for (i in seq_len(nrow(data_clean_human))) {  # Use data_clean_human
  row_name <- data_clean_human$Name[i]
  row_refs <- data_clean_human$References[i]
  
  # Skip root
  if (row_name == "Human") next  # Changed from "Generative AI"
  
  # Detect domain
  if (row_name %in% domains) {
    current_domain <- row_name
    next
  }
  
  # Add themes only (no domain boxes)
  if (!is.null(current_domain) &&
      !is.na(row_refs) &&
      row_refs > 0) {
    treemap_data <- rbind(
      treemap_data,
      data.frame(
        Domain = current_domain,
        Name = row_name,
        References = row_refs,
        stringsAsFactors = FALSE
      )
    )
  }
}

treemap_data <- treemap_data %>%
  mutate(Name_labeled = paste0(Name, " (n=", References, ")"))

cat("\nTheme boxes:", nrow(treemap_data), "\n")
cat("Domains:", length(unique(treemap_data$Domain)), "\n\n")

# ------------------------------------------------------------------------------
# DOMAIN COLOURS
# ------------------------------------------------------------------------------
domain_colors <- c(
  "Accuracy" = "#B3D9E6",
  "Safety" = "#F4C2A8",
  "Empathetic" = "#F4B5C4",
  "Communicates Clearly" = "#D3D3D3",
  "Context Aware" = "#B8D9B8",
  "Completeness" = "#FFE699",
  "Referencing" = "#C9D4E8",
  "Recognises Uncertainty" = "#E6C9E6",
  "Avoid Coerciveness" = "#FFB380",
  "Follows Instructions" = "#F5D9C4"
)

# ------------------------------------------------------------------------------
# ADD REFERENCE COUNTS TO DOMAIN NAMES FOR LEGEND
# ------------------------------------------------------------------------------
domain_totals <- treemap_data %>%
  group_by(Domain) %>%
  summarise(total_refs = sum(References)) %>%
  mutate(Domain_labeled = paste0(Domain, " (n=", total_refs, ")"))

# Add labeled domain names to treemap data
treemap_data <- treemap_data %>%
  left_join(domain_totals, by = "Domain") %>%
  mutate(Domain_labeled = paste0(Domain, " (n=", total_refs, ")"))

cat("Domain totals:\n")
print(domain_totals)
cat("\n")

# Create named vector with labeled domain names
domain_colors_labeled <- sapply(domain_totals$Domain, function(d) domain_colors[d])
names(domain_colors_labeled) <- domain_totals$Domain_labeled

# ------------------------------------------------------------------------------
# TREEMAP - NO DOMAIN LABELS, LEGEND WITH COUNTS AT BOTTOM
# ------------------------------------------------------------------------------
# A4 landscape dimensions: 11.69 × 8.27 inches
svg("C:/Users/corn0187/OneDrive - Flinders/R evaluation/Manuscript - Rubric/Qualitative Outputs/Treemaps/Section_1_treemap_human.svg", width = 11.69, height = 8.27) 

treemap(
  treemap_data,
  index = c("Domain_labeled", "Name_labeled"),
  vSize = "References",
  
  # Categorical coloring by domain
  type = "categorical",
  vColor = "Domain_labeled",
  palette = domain_colors_labeled,
  
  title = "Human: Assessor Feedback Themes by Domain",  # Changed title
  title.legend = "Domains",
    
  # Only show theme labels (level 2), hide domain labels (level 1)
  fontsize.labels = c(0, 10),
  fontcolor.labels = c("transparent", "black"),
  fontface.labels = c("plain", "plain"),
  
  # Label placement for themes only
  align.labels = list(
    c("center", "center"),
    c("left", "top")
  ),
  
  # FORCE ALL LABELS TO SHOW AND FILL BOXES
  force.print.labels = TRUE,
  lowerbound.cex.labels = 0.2,
  inflate.labels = FALSE,
  overlap.labels = 0.5,
  
  # Borders
  border.col = c("black", "white"),
  border.lwds = c(3, 1),
  
  # LEGEND AT BOTTOM
  position.legend = "bottom",
  fontsize.legend = 7,
  
  aspRatio = 1.6
)

dev.off()

cat("✓ Created: Section 1 treemap_human.pdf\n")
cat("  - ALL theme boxes show their names\n")
cat("  - NO domain labels inside boxes\n")
cat("  - Colored sections for each domain\n")
cat("  - Legend at BOTTOM with domain colors and reference counts (n=X)\n")
cat("  - A4 landscape size (11.69 × 8.27 inches)\n")

