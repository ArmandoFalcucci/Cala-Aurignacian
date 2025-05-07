# Multiple Correspondence Analysis (MCA) Analysis of complete bladelets from La Cala AU13-AU10

# Load necessary libraries
library(factoextra)
library(FactoMineR)
library(DescTools)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RcmdrMisc)
library(forcats)
library(cowplot)
library(multcompView)


Dataset <- readr::read_csv(file = "data/Ca_Dataset_complete.csv", col_names = TRUE,
                           col_types = readr::cols(
                             Spit = readr::col_factor(levels = c("AU10", "AU11", "AU12", "AU13")
                             )))

Dataset_cores <- readr::read_csv(file = "../data/Ca_Dataset_cores.csv", col_names = TRUE,
                                 col_types = readr::cols(
                                   Spit = readr::col_factor(levels = c("AU10", "AU11", "AU12", "AU13")
                                   )))

# Define comparisons
comparisons <- c("AU13-AU12", "AU13-AU11", "AU13-AU10", "AU12-AU11", "AU12-AU10", "AU11-AU10")

# Data Preparation
MCA_bladelet.morpho <- Dataset %>%
  filter(Class == "Blank", Preservation == "Complete", Blank %in% c("Bladelet", "Flakelet")) %>%
  drop_na(Length, Width, Thickness) %>%
  mutate(Curvature = fct_lump(Curvature, prop = 0.05),
         Section = fct_lump(Section, prop = 0.05),
         Distal.end = fct_lump(Distal.end, prop = 0.05),
         Scar.Pattern.2 = fct_lump(Scar.Pattern.2, prop = 0.05))

# Binning variables (Elongation, Robustness)
MCA_bladelet.morpho$Elongation <- RcmdrMisc::binVariable(MCA_bladelet.morpho$Elongation, bins = 3, method = "natural", labels = c("low", "medium", "high"))
MCA_bladelet.morpho$Robustness <- RcmdrMisc::binVariable(MCA_bladelet.morpho$Robustness, bins = 3, method = "natural", labels = c("low", "medium", "high"))

# Rename and recode variables for clarity and consistency
MCA_bladelet.morpho <- MCA_bladelet.morpho %>%
  dplyr::select(`Sub-layer`, Elongation, Robustness, Curvature, Torsion.simplified, Distal.end, Section, Scar.Pattern.2) %>%
  na.omit() %>%
  droplevels() %>%
  dplyr::rename(SubL = `Sub-layer`) %>%
  dplyr::rename(ELO = Elongation, ROB = Robustness, CURV = Curvature, TORS = Torsion.simplified, DIST = Distal.end, CS = Section, SP = Scar.Pattern.2) %>%
  dplyr::mutate(TORS = dplyr::recode(TORS, "yes" = "Twisted", "no" = "Non-twisted"))

# Perform MCA (Multiple Correspondence Analysis)
MCA_bladelet.morpho.analysis <- MCA(MCA_bladelet.morpho, graph = FALSE, quali.sup = 1, method = "Burt")

# Plot for MCA variables (Categories)
MCA_bladelet.morpho.variable.category <- fviz_mca_var(MCA_bladelet.morpho.analysis, labelsize = 4, col.var = "contrib",
                                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                                      repel = TRUE, invisible = "quali.sup",
                                                      ggtheme = theme_gray(), title = "") +
  theme_minimal() +
  labs(y = "Dimension 2 (13%)", x = "Dimension 1 (17%)") +
  theme(axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.text.y = element_text(size = 12, family = "Times New Roman"),
        text = element_text(size = 13, family = "Times New Roman"))

# Plot for MCA individuals (colored by 'cos2' for quality of representation)
MCA_bladelet.morpho.variables <- fviz_mca_ind(MCA_bladelet.morpho.analysis, label = "none", col.ind = "cos2", gradient.cols = c("Gray70", "darkslategrey", "darkblue"),
                                              pointsize = 0.6, ggtheme = theme_grey(base_size = 11) + theme(text = element_text(family = "Times New Roman")))

# MCA scree plot (Eigenvalues)
MCA_bladelet.morpho.screeplot <- fviz_eig(MCA_bladelet.morpho.analysis, addlabels = TRUE, labelsize = 2,
                                          ggtheme = theme_grey(base_size = 11) + theme(text = element_text(family = "Times New Roman"))) + ylim(0, 18)

# MCA correlation variables plot
MCA_bladelet.morpho.variables <- fviz_mca_var(MCA_bladelet.morpho.analysis, choice = "mca.cor", repel = TRUE, labelsize = 4,
                                              ggtheme = theme_grey(base_size = 11) + theme(text = element_text(family = "Times New Roman")))

# Contribution plot for variables
MCA_bladelet.morpho.contribution <- fviz_contrib(MCA_bladelet.morpho.analysis, choice = "var", axes = 1:2, top = 8,
                                                 ggtheme = theme_grey(base_size = 11) + theme(text = element_text(family = "Times New Roman"))) +
  ggtitle("Contrib. of var. to Dim 1-2")

# MCA plot for individuals with ellipses
MCA_bladelet.morpho.individuals <- fviz_mca_ind(MCA_bladelet.morpho.analysis, addEllipses = TRUE, ellipse.type = "confidence", label = "none", pointsize = 1, habillage = 1,
                                                legend.title = "Sub-layer") +
  theme_minimal(base_size = 11) + theme(text = element_text(family = "Times New Roman"))

# MCA plot for individuals colored by contributions
MCA_bladelet.morpho.individuals.alone <- fviz_mca_ind(MCA_bladelet.morpho.analysis, label = "none", col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                                      pointsize = 0.6, ggtheme = theme_grey(base_size = 11) + theme(text = element_text(family = "Times New Roman")))

# Combine plots and save as TIFF
ggsave(filename = "./output/figures/Figure_S14.png", width = 20, height = 22, units = "cm", dpi = 300, plot = (
  ggdraw() +
    draw_plot(MCA_bladelet.morpho.individuals.alone, x = 0, y = .66, width = .5, height = .34) +
    draw_plot(MCA_bladelet.morpho.screeplot, x = .5, y = .66, width = .5, height = .34) +
    draw_plot(MCA_bladelet.morpho.variables, x = 0, y = 0.33, width = .5, height = 0.33) +
    draw_plot(MCA_bladelet.morpho.contribution, x = .5, y = 0.33, width = .5, height = 0.33) +
    draw_plot(MCA_bladelet.morpho.individuals, x = .1, y = 0, width = .8, height = 0.33) +
    draw_plot_label(label = c("a", "b", "c", "d", "e"), size = 12,
                    x = c(0, 0.5, 0, 0.5, 0.1), y = c(1, 1, 0.66, 0.66, 0.33))
))

# Prepare data for ANOVA (adapted from Cascalheira 2019)
MCA_bladelet.morpho.analysis.coord <- as.data.frame(MCA_bladelet.morpho.analysis$ind)
context <- as.data.frame(MCA_bladelet.morpho$`SubL`)

MCA_bladelet.morpho.analysis.coord <- MCA_bladelet.morpho.analysis.coord %>%
  dplyr::select(1:2)


MCA_bladelet.morpho.analysis.coord <- bind_cols(context, MCA_bladelet.morpho.analysis.coord)

MCA_bladelet.morpho.analysis.coord <- MCA_bladelet.morpho.analysis.coord %>%
  mutate(context = MCA_bladelet.morpho$`SubL` , "dim1" = coord.Dim.1, "dim2" = coord.Dim.2) %>%
  dplyr::select(context, dim1, dim2)


# Multi-comparison bootstrap
boot_dim1.bladelet.morpho <- WRS2::mcppb20(dim1 ~ context, data = MCA_bladelet.morpho.analysis.coord, nboot=5000)
boot_dim2.bladelet.morpho <- WRS2::mcppb20(dim2 ~ context, data = MCA_bladelet.morpho.analysis.coord, nboot=5000)

# Convert results to data frames
multi_comp_dim1.bladelet.morpho <- as.data.frame(boot_dim1.bladelet.morpho$comp)
multi_comp_dim2.bladelet.morpho <- as.data.frame(boot_dim2.bladelet.morpho$comp)

# Add contexts names and transform to column "context"
multi_comp_dim1.bladelet.morpho$context <- comparisons
multi_comp_dim1.bladelet.morpho <- multi_comp_dim1.bladelet.morpho[ !duplicated(names(multi_comp_dim1.bladelet.morpho)) ]
multi_comp_dim2.bladelet.morpho$context <- comparisons
multi_comp_dim2.bladelet.morpho <- multi_comp_dim2.bladelet.morpho[ !duplicated(names(multi_comp_dim2.bladelet.morpho)) ]

multi_comp.bladelet.morpho <- full_join(multi_comp_dim1.bladelet.morpho, multi_comp_dim2.bladelet.morpho, by = "context", suffix = c("dim1", "dim2"))

multi_comp.bladelet.morpho <- select(multi_comp.bladelet.morpho, context, `p-valuedim1`, `p-valuedim2`)

# Build list with pairwise p-values
labels_dim1.bladelet.morpho <- setNames(multi_comp.bladelet.morpho$`p-valuedim1`, multi_comp.bladelet.morpho$context)
labels_dim2.bladelet.morpho <- setNames(multi_comp.bladelet.morpho$`p-valuedim2`, multi_comp.bladelet.morpho$context)

# Attribute letters to each grouping based on alpha = .05
grouping_dim1.bladelet.morpho <- data.frame(multcompView::multcompLetters(labels_dim1.bladelet.morpho)["Letters"])
grouping_dim1.bladelet.morpho <- rownames_to_column(grouping_dim1.bladelet.morpho, "context")

grouping_dim2.bladelet.morpho <- data.frame(multcompView::multcompLetters(labels_dim2.bladelet.morpho)["Letters"])
grouping_dim2.bladelet.morpho <- rownames_to_column(grouping_dim2.bladelet.morpho, "context")

grouping.bladelet.morpho <- full_join(grouping_dim1.bladelet.morpho, grouping_dim2.bladelet.morpho, by = "context", suffix = c("dim1", "dim2"))

grouping.bladelet.morpho <- mutate(grouping.bladelet.morpho, Lettersdim1 = toupper(Lettersdim1), Lettersdim2 = toupper(Lettersdim2))


# Plot trimmed mean and CI with associated group belonging from bootstrap multi-comparison

mca.bladelet.morpho.summary <- MCA_bladelet.morpho.analysis.coord %>% 
  dplyr::group_by(context) %>% 
  dplyr::summarise(meandim1 = mean(dim1, trim = 0.2), meandim2 = mean(dim2, trim = 0.2), 
                   LCIdim1 = MeanCI(dim1, trim = 0.2)[2], MCIdim1 = MeanCI(dim1, trim = 0.2)[3],
                   LCIdim2 = MeanCI(dim2, trim = 0.2)[2], MCIdim2 = MeanCI(dim2, trim = 0.2)[3]) 

m1.bladelet.morpho <- max(mca.bladelet.morpho.summary$MCIdim1)
m2.bladelet.morpho <- max(mca.bladelet.morpho.summary$MCIdim2)


# For Dimension 1 Plot
dim1.bladelet.morpho <- mca.bladelet.morpho.summary %>%
  mutate(context = fct_relevel(context, "AU13", "AU12", "AU11", "AU10")) %>%
  ggplot() +
  geom_point(stat = "identity", aes(x = context, y = meandim1), size = 3) +
  geom_errorbar(stat = "identity", aes(x = context, ymin = LCIdim1, ymax = MCIdim1), width = 0) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", show.legend = FALSE) +
  theme_minimal() +
  geom_text(data = grouping.bladelet.morpho, aes(x = context, y = m1.bladelet.morpho + 0.08, label = Lettersdim1, size = 15), show.legend = FALSE) +
  xlab("") +
  ylab("Dimension 1") +
  scale_colour_discrete(l = 40) +
  theme(text = element_text(family = "Times New Roman", size = 14)) +  # Apply Times New Roman font
  coord_flip()

dim1.bladelet.morpho


# For Dimension 2 Plot
dim2.bladelet.morpho <- mca.bladelet.morpho.summary %>%
  mutate(context = fct_relevel(context, "AU13", "AU12", "AU11", "AU10")) %>%
  ggplot() +
  geom_point(stat = "identity", aes(x = context, y = meandim2), size = 3) +
  geom_errorbar(stat = "identity", aes(x = context, ymin = LCIdim2, ymax = MCIdim2), width = 0) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", show.legend = FALSE) +
  theme_minimal() +
  geom_text(data = grouping.bladelet.morpho, aes(x = context, y = m2.bladelet.morpho + 0.08, label = Lettersdim2, size = 15), show.legend = FALSE) +
  xlab("") +
  ylab("Dimension 2") +
  scale_colour_discrete(l = 40) +
  theme(text = element_text(family = "Times New Roman", size = 14)) +  # Apply Times New Roman font
  coord_flip()

dim2.bladelet.morpho

# Combine the Dimension 1 and Dimension 2 Plots
dim1.dim2.bladelet.morpho <- gridExtra::grid.arrange(dim1.bladelet.morpho, dim2.bladelet.morpho, nrow = 1)

# Combine with the other plots (e.g., variable category plot) using ggarrange
var.dim1.dim2.bladelet.morpho <- ggarrange(MCA_bladelet.morpho.variable.category, dim1.dim2.bladelet.morpho, labels = c("a", "b"), nrow = 2)

# Display the combined plot
var.dim1.dim2.bladelet.morpho

ggsave("./output/figures/Figure_8.tiff", plot = var.dim1.dim2.bladelet.morpho, width = 8, height = 8, units = "in", dpi = 300)
