# ==============================================================================
# GENERATIVE AI TREEMAP — SPLIT INTO TWO TREEMAPS (5 DOMAINS EACH)
# ==============================================================================
rm(list = ls())
library(readxl)
library(treemap)
library(dplyr)

# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------
data <- read_excel(
  "C:/Users/corn0187/OneDrive - Flinders/R evaluation/Manuscript - Rubric/Qualitative Outputs/Section 2.xlsx"
)

# ------------------------------------------------------------------------------
# EXPLICIT DOMAIN LIST - SPLIT INTO TWO GROUPS
# ------------------------------------------------------------------------------
domains_group1 <- c(
  "Accuracy",
  "Safety",
  "Empathetic",
  "Communicates Clearly",
  "Context Aware"
)

domains_group2 <- c(
  "Completeness",
  "Referencing",
  "Recognises Uncertainty",
  "Avoid Coerciveness",
  "Follows Instructions"
)

domains <- c(domains_group1, domains_group2)

# ------------------------------------------------------------------------------
# CLEAN DATA
# ------------------------------------------------------------------------------
data_clean <- data %>%
  select(Name, References) %>%
  filter(!is.na(Name)) %>%
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

for (i in seq_len(nrow(data_clean))) {
  row_name <- data_clean$Name[i]
  row_refs <- data_clean$References[i]
  
  if (row_name == "Generative AI") next
  
  if (row_name %in% domains) {
    current_domain <- row_name
    next
  }
  
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

cat("\nTotal theme boxes:", nrow(treemap_data), "\n")
cat("Total domains:", length(unique(treemap_data$Domain)), "\n\n")

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
# FUNCTION TO CREATE TREEMAP FOR A SUBSET OF DOMAINS
# ------------------------------------------------------------------------------
create_domain_treemap <- function(data, domain_list, colors, output_file, title) {
  
  # Filter data for selected domains
  subset_data <- data %>%
    filter(Domain %in% domain_list)
  
  # Calculate domain totals for legend
  domain_totals <- subset_data %>%
    group_by(Domain) %>%
    summarise(total_refs = sum(References)) %>%
    mutate(Domain_labeled = paste0(Domain, " (n=", total_refs, ")"))
  
  # Add labeled domain names
  subset_data <- subset_data %>%
    left_join(domain_totals, by = "Domain")
  
  cat("Domain totals for", title, ":\n")
  print(domain_totals)
  cat("\n")
  
  # Create color palette for this subset
  subset_colors <- sapply(domain_totals$Domain, function(d) colors[d])
  names(subset_colors) <- domain_totals$Domain_labeled
  
  # Generate PNG (instead of PDF)
  svg(output_file, width = 11.69, height = 8.27)
  
  treemap(
    subset_data,
    index = c("Domain_labeled", "Name_labeled"),
    vSize = "References",
    type = "categorical",
    vColor = "Domain_labeled",
    palette = subset_colors,
    title = title,
    title.legend = "Domains", 
    fontsize.labels = c(0, 10),
    fontcolor.labels = c("transparent", "black"),
    fontface.labels = c("plain", "plain"),
    align.labels = list(
      c("center", "center"),
      c("left", "top")
    ),
    force.print.labels = TRUE,
    lowerbound.cex.labels = 0.3,
    inflate.labels = FALSE,
    overlap.labels = 0.5,
    border.col = c("black", "white"),
    border.lwds = c(3, 1),
    position.legend = "bottom",
    fontsize.legend = 7,
    aspRatio = 1.6
  )
  
  dev.off()
  cat("✓ Created:", output_file, "\n\n")
}

# ------------------------------------------------------------------------------
# CREATE TREEMAP 1: First 5 Domains
# ------------------------------------------------------------------------------
create_domain_treemap(
  data = treemap_data,
  domain_list = domains_group1,
  colors = domain_colors,
  output_file = "C:/Users/corn0187/OneDrive - Flinders/R evaluation/Manuscript - Rubric/Qualitative Outputs/Treemaps/Section_2_treemap_AI_part1.svg",
  title = "Generative AI: Assessor Feedback Themes (Part 1)"
)

# ------------------------------------------------------------------------------
# CREATE TREEMAP 2: Second 5 Domains
# ------------------------------------------------------------------------------
create_domain_treemap(
  data = treemap_data,
  domain_list = domains_group2,
  colors = domain_colors,
  output_file = "C:/Users/corn0187/OneDrive - Flinders/R evaluation/Manuscript - Rubric/Qualitative Outputs/Treemaps/Section_2_treemap_AI_part2.svg",
  title = "Generative AI: Evaluator Feedback Themes (Part 2)"
)

cat("✓ Both treemaps created successfully!\n")
cat("  - Part 1: Accuracy, Safety, Empathetic, Communicates Clearly, Context Aware\n")
cat("  - Part 2: Completeness, Referencing, Recognises Uncertainty, Avoid Coerciveness, Follows Instructions\n")

