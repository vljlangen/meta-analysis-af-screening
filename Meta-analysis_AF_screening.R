



############################################################################
############################################################################
###                                                                      ###
###                            LOAD LIBRARIES                            ###
###                                                                      ###
############################################################################
############################################################################



# Load pacman to install/load everything else
library(pacman)

# Load required packages
p_load(meta, metasens, gridExtra, grid)

# For paneling:
p_load(cowplot, grid, ggplotify, ggplot2)

# For flowchart
p_load(PRISMAstatement, DiagrammeRsvg, rsvg)



############################################################################
############################################################################
###                                                                      ###
###                           SET UP CONSTANTS                           ###
###                                                                      ###
############################################################################
############################################################################
 

# Distance between plot and title

seven <- 1.3
six <- 1.24
five <- 1.20
four <- 1.14


n_years <- 10



# Consistent plot colors
plot_color <- "darkblue"
square_color <- "darkred"
diamond_color <- "blue"
diamond_lines <- "blue"
square_lines <- "black"

# Consistent plot settings
plot_settings <- list(
  col.diamond = diamond_color,
  col.square = square_color,
  col.square.lines = square_lines,
  col.diamond.lines = diamond_lines,
  col.study = "black",
  xlim = c(0.1, 10),
  at = c(0.1, 0.5, 1, 2, 5, 10),
  digits = 3
)

# Stroke Analysis
meta_analysisSTROKE <- metabin(
  event.e = c(6, 67, 2, 812, 37, 673),
  n.e = c(500, 1501, 434, 13979, 5243, 13905),
  event.c = c(10, 251, 0, 874, 34, 706),
  n.c = c(501, 4503, 422, 13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("REHEARSE-AF (2017)", "LOOP (2021)", "SCREEN-AF (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisSTROKE)

# Forest plot for stroke
forest(meta_analysisSTROKE,
       main = "Forest Plot of Stroke Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",
       xlim = c(0.2, 5),
       comb.random = TRUE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

grid.text("Stroke or systemic embolism", .5, .75, gp=gpar(cex=2))


# Funnel plot for stroke
funnel(meta_analysisSTROKE,
       main = "Funnel Plot of Stroke Events",
       col = plot_color)

# Major Bleeding Analysis
meta_analysisB <- metabin(
  event.e = c(2, 65, 1431, 52, 207),
  n.e = c(500, 1501, 13979, 5243, 13905),
  event.c = c(1, 156, 1448, 60, 213),
  n.c = c(501, 4503, 13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("REHEARSE-AF (2017)", "LOOP (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisB)

# Forest plot for major bleeding
forest(meta_analysisB,
       main = "Forest Plot of Major Bleeding Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",  
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

grid.text("Major bleeding", .5, .75, gp=gpar(cex=2))



# Funnel plot for major bleeding
funnel(meta_analysisB,
       main = "Funnel Plot of Major Bleeding Events",
       col = plot_color)

# Mortality Analysis
meta_analysisM <- metabin(
  event.e = c(7, 3, 0, 168, 3177, 125, 2126),
  n.e = c(463, 500, 434, 1501, 13979, 5952, 13905),
  event.c = c(8, 5, 1, 507, 3287, 138, 2078),
  n.c = c(465, 501, 422, 4503, 13996, 5953, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("EARLY (2015)", "REHEARSE-AF (2017)", "SCREEN-AF (2021)", "LOOP (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisM)

# Forest plot for mortality
forest(meta_analysisM,
       main = "Forest Plot of All-Cause Mortality",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening", 
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

grid.text("All-cause mortality", .5, .75, gp=gpar(cex=2))


# Funnel plot for mortality
funnel(meta_analysisM,
       main = "Funnel Plot of All-Cause Mortality",
       col = plot_color)

####plots all to one pdf####

# Capture the forest plots as grobs
# Stroke forest plot
grob_stroke <- grid.grabExpr({
  forest(meta_analysisSTROKE,
         main = "Forest Plot of Stroke Events",
         xlab = "Risk Ratio (RR)",        
         label.e = "Screening",
         xlim = c(0.2, 5),
         comb.random = TRUE,
         print.tau2 = TRUE,
         print.I2 = TRUE,
         print.pval.Q = TRUE,
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Stroke or systemic embolism", .5, six, gp=gpar(cex=1.5))
})

 

# Major Bleeding forest plot
grob_bleeding <- grid.grabExpr({
  forest(meta_analysisB,
         main = "Forest Plot of Major Bleeding Events",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Major bleeding", .5, six, gp=gpar(cex=1.5))
})

 

# Mortality forest plot
grob_mortality <- grid.grabExpr({
  forest(meta_analysisM,
         main = "Forest Plot of All-Cause Mortality",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("All-cause mortality", .5, seven, gp=gpar(cex=1.5))
})
 


# Convert the "gTree" objects to grobs

grob_stroke <- as.grob(grob_stroke)

grob_bleeding <- as.grob(grob_bleeding)

grob_mortality <- as.grob(grob_mortality)


plot_grid(NULL,
          grob_stroke,
          NULL,
          grob_bleeding,
          NULL,
          grob_mortality,
          NULL,
          nrow = 7, ncol = 1, scale = 1.0,
          rel_widths = c(0.01, 1, 0.1, 1, 0.1, 1, 0.01))

 
# Save as PDF with dpi specified
ggsave("figure2.pdf", width = 10, height =15, dpi = 600)

 







###########################################################################
###########################################################################
###                                                                     ###
###                        ONLY INVASIVE METHODS                        ###
###                                                                     ###
###########################################################################
###########################################################################







# Consistent plot colors
plot_color <- "darkblue"
square_color <- "darkred"
diamond_color <- "blue"
diamond_lines <- "blue"
square_lines <- "black"

# Consistent plot settings
plot_settings <- list(
  col.diamond = diamond_color,
  col.square = square_color,
  col.square.lines = square_lines,
  col.diamond.lines = diamond_lines,
  col.study = "black",
  xlim = c(0.1, 10),
  at = c(0.1, 0.5, 1, 2, 5, 10),
  digits = 3
)

# Stroke Analysis
meta_analysisSTROKE <- metabin(
  event.e = c(6, 2, 812, 37, 673),
  n.e = c(500, 434, 13979, 5243, 13905),
  event.c = c(10, 0, 874, 34, 706),
  n.c = c(501, 422, 13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("REHEARSE-AF (2017)", "SCREEN-AF (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisSTROKE)

# Forest plot for stroke
forest(meta_analysisSTROKE,
       main = "Forest Plot of Stroke Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",
       xlim = c(0.2, 5),
       comb.random = TRUE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for stroke
funnel(meta_analysisSTROKE,
       main = "Funnel Plot of Stroke Events",
       col = plot_color)

# Major Bleeding Analysis
meta_analysisB <- metabin(
  event.e = c(2, 1431, 52, 207),
  n.e = c(500, 13979, 5243, 13905),
  event.c = c(1, 1448, 60, 213),
  n.c = c(501, 13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("REHEARSE-AF (2017)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisB)

# Forest plot for major bleeding
forest(meta_analysisB,
       main = "Forest Plot of Major Bleeding Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",  
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for major bleeding
funnel(meta_analysisB,
       main = "Funnel Plot of Major Bleeding Events",
       col = plot_color)

# Mortality Analysis
meta_analysisM <- metabin(
  event.e = c(7, 3, 0, 3177, 125, 2126),
  n.e = c(463, 500, 434, 13979, 5952, 13905),
  event.c = c(8, 5, 1, 3287, 138, 2078),
  n.c = c(465, 501, 422, 13996, 5953, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("EARLY (2015)", "REHEARSE-AF (2017)", "SCREEN-AF (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisM)

# Forest plot for mortality
forest(meta_analysisM,
       main = "Forest Plot of All-Cause Mortality",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening", 
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for mortality
funnel(meta_analysisM,
       main = "Funnel Plot of All-Cause Mortality",
       col = plot_color)

####plots all to one pdf####

# Capture the forest plots as grobs
# Stroke forest plot
grob_stroke <- grid.grabExpr({
  forest(meta_analysisSTROKE,
         main = "Forest Plot of Stroke Events",
         xlab = "Risk Ratio (RR)",        
         label.e = "Screening",
         xlim = c(0.2, 5),
         comb.random = TRUE,
         print.tau2 = TRUE,
         print.I2 = TRUE,
         print.pval.Q = TRUE,
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Stroke or systemic embolism", .5, five, gp=gpar(cex=1.5))
})

# Major Bleeding forest plot
grob_bleeding <- grid.grabExpr({
  forest(meta_analysisB,
         main = "Forest Plot of Major Bleeding Events",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Major bleeding", .5, five, gp=gpar(cex=1.5))
})

# Mortality forest plot
grob_mortality <- grid.grabExpr({
  forest(meta_analysisM,
         main = "Forest Plot of All-Cause Mortality",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("All-cause mortality", .5, six, gp=gpar(cex=1.5))
})



# Convert the "gTree" objects to grobs

grob_stroke <- as.grob(grob_stroke)

grob_bleeding <- as.grob(grob_bleeding)

grob_mortality <- as.grob(grob_mortality)


plot_grid(NULL,
          grob_stroke,
          NULL,
          grob_bleeding,
          NULL,
          grob_mortality,
          NULL,
          nrow = 7, ncol = 1, scale = 1.0,
          rel_widths = c(0.01, 1, 0.1, 1, 0.1, 1, 0.01))


# Save as PDF with dpi specified
ggsave("supplementary_figure2.pdf", width = 10, height =15, dpi = 600)







############################################################################
############################################################################
###                                                                      ###
###       ONLY STUDIES WITH CLINICAL ENDPOINTS AS PRIMARY OUTCOMES       ###
###                                                                      ###
############################################################################
############################################################################




# Consistent plot colors
plot_color <- "darkblue"
square_color <- "darkred"
diamond_color <- "blue"
diamond_lines <- "blue"
square_lines <- "black"

# Consistent plot settings
plot_settings <- list(
  col.diamond = diamond_color,
  col.square = square_color,
  col.square.lines = square_lines,
  col.diamond.lines = diamond_lines,
  col.study = "black",
  xlim = c(0.1, 10),
  at = c(0.1, 0.5, 1, 2, 5, 10),
  digits = 3
)

# Stroke Analysis
meta_analysisSTROKE <- metabin(
  event.e = c( 67, 812, 37, 673),
  n.e = c( 1501,  13979, 5243, 13905),
  event.c = c( 251,  874, 34, 706),
  n.c = c(4503,  13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("LOOP (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisSTROKE)

# Forest plot for stroke
forest(meta_analysisSTROKE,
       main = "Forest Plot of Stroke Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",
       xlim = c(0.2, 5),
       comb.random = TRUE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for stroke
funnel(meta_analysisSTROKE,
       main = "Funnel Plot of Stroke Events",
       col = plot_color)

# Major Bleeding Analysis
meta_analysisB <- metabin(
  event.e = c( 65, 1431, 52, 207),
  n.e = c( 1501, 13979, 5243, 13905),
  event.c = c( 156, 1448, 60, 213),
  n.c = c( 4503, 13996, 5277, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c("LOOP (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisB)

# Forest plot for major bleeding
forest(meta_analysisB,
       main = "Forest Plot of Major Bleeding Events",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening",  
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for major bleeding
funnel(meta_analysisB,
       main = "Funnel Plot of Major Bleeding Events",
       col = plot_color)

# Mortality Analysis
meta_analysisM <- metabin(
  event.e = c(168, 3177, 125, 2126),
  n.e = c( 1501, 13979, 5952, 13905),
  event.c = c(507, 3287, 138, 2078),
  n.c = c(4503, 13996, 5953, 13884),
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML",
  studlab = c( "LOOP (2021)", "STROKESTOP (2021)", "GUARD-AF (2024)", "STROKESTOP II (2024)")
)

summary(meta_analysisM)

# Forest plot for mortality
forest(meta_analysisM,
       main = "Forest Plot of All-Cause Mortality",
       xlab = "Risk Ratio (RR)",        
       label.e = "Screening", 
       xlim = c(0.2, 5),
       col.diamond = plot_color,
       col.square = square_color,
       col.square.lines = square_lines,
       col.diamond.lines = diamond_color,
       digits = 3)

# Funnel plot for mortality
funnel(meta_analysisM,
       main = "Funnel Plot of All-Cause Mortality",
       col = plot_color)

####plots all to one pdf####

# Capture the forest plots as grobs
# Stroke forest plot
grob_stroke <- grid.grabExpr({
  forest(meta_analysisSTROKE,
         main = "Forest Plot of Stroke Events",
         xlab = "Risk Ratio (RR)",        
         label.e = "Screening",
         xlim = c(0.2, 5),
         comb.random = TRUE,
         print.tau2 = TRUE,
         print.I2 = TRUE,
         print.pval.Q = TRUE,
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Stroke or systemic embolism", .5, four, gp=gpar(cex=1.5))
})

# Major Bleeding forest plot
grob_bleeding <- grid.grabExpr({
  forest(meta_analysisB,
         main = "Forest Plot of Major Bleeding Events",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("Major bleeding", .5, five, gp=gpar(cex=1.5))
})

# Mortality forest plot
grob_mortality <- grid.grabExpr({
  forest(meta_analysisM,
         main = "Forest Plot of All-Cause Mortality",
         xlab = "Risk Ratio (RR)",
         xlim = c(0.2, 5),
         label.e = "Screening",  
         col.diamond = plot_color,
         col.square = square_color,
         col.square.lines = square_lines,
         col.diamond.lines = diamond_color,
         digits = 3)
  grid.text("All-cause mortality", .5, four, gp=gpar(cex=1.5))
})





# Convert the "gTree" objects to grobs

grob_stroke <- as.grob(grob_stroke)

grob_bleeding <- as.grob(grob_bleeding)

grob_mortality <- as.grob(grob_mortality)


plot_grid(NULL,
          grob_stroke,
          NULL,
          grob_bleeding,
          NULL,
          grob_mortality,
          NULL,
          nrow = 7, ncol = 1, scale = 1.0,
          rel_widths = c(0.01, 1, 0.1, 1, 0.1, 1, 0.01))


# Save as PDF with dpi specified
ggsave("supplementary_figure1.pdf", width = 10, height =15, dpi = 600)







###########################################################################
###########################################################################
###                                                                     ###
###                              FLOWCHART                              ###
###                                                                     ###
###########################################################################
###########################################################################





# Generate the PRISMA flowchart with corrected values
g <- prisma(
  found = 90,                        # Total records found through database searching
  found_other = 0,                   # Additional records identified through other sources
  no_dupes = 90,                     # Records after duplicates removed
  screened = 90,                     # Records screened
  screen_exclusions = 55,            # Records excluded after screening
  full_text = 35,                    # Full-text articles assessed for eligibility
  full_text_exclusions = 28,         # Full-text articles excluded
  qualitative = 7,                    # Studies included in qualitative synthesis
  quantitative = 7,                   # Studies included in quantitative synthesis (meta-analysis)
  extra_dupes_box = TRUE,            # Do not include duplicates box

  font_size = 12,                    # Font size for the chart
  labels = list(
    found = "Records identified through database searching (n = 90)",
    found_other = "Additional records identified through other sources (n = 0)",
    no_dupes = "Records after duplicates removed (n = 90)",
    screened = "Title and abstract screened (n = 90)",
    full_text = "Full-text articles assessed for eligibility (n = 35)",
    full_text_exclusions = "Full-text articles excluded (n = 28)\nReasons:\n- Not reporting clinical endpoints\n- Not a randomized controlled trial\n- Not screening for atrial fibrillation",
    qualitative = "Studies included in qualitative synthesis (n = 7)",
    quantitative = "Studies included in meta-analysis (n = 7)"
  )
)

PRISMAstatement:::prisma_pdf(g, filename = "figure1.pdf")


##### weighter FU time#####

# Example data for follow-up times (in years) and sample sizes
follow_up_times <- c(1.84, 1, 5.34, 0.5, 6.9, 1.28, 5.1)  # Mean follow-up times for each study
sample_sizes <- c(928, 1001, 6004, 822, 28768, 11905, 28712)  # Corresponding sample sizes for each study

# Calculate the weighted mean follow-up time
weighted_mean_follow_up <- sum(follow_up_times * sample_sizes) / sum(sample_sizes)

# Print the result
cat("Weighted Mean Follow-Up Time:", weighted_mean_follow_up, "years")



#### absolute risk difference and NNT of stroke####


# Given values
RR <- 0.932                      # Risk Ratio
RR_lower <- 0.873                # Lower bound of RR CI
RR_upper <- 0.996                # Upper bound of RR CI
baseline_risk <- 0.10            # Baseline 10-year probability (10%)

# Calculate risk in treatment group for 10 years using RR and its CIs
risk_treatment <- baseline_risk * RR
risk_treatment_lower <- baseline_risk * RR_lower
risk_treatment_upper <- baseline_risk * RR_upper

# Calculate Absolute Risk Reduction (ARR) for 10 years and its 95% CI
ARR <- baseline_risk - risk_treatment
ARR_lower <- baseline_risk - risk_treatment_upper  # Lower ARR using upper RR
ARR_upper <- baseline_risk - risk_treatment_lower  # Upper ARR using lower RR

# Calculate Number Needed to Treat (NNT) for 10 years and its 95% CI
NNT <- ceiling(1 / ARR)  # NNT point estimate
NNT_lower <- ceiling(1 / ARR_upper)              # NNT lower (using upper ARR)
NNT_upper <- ceiling(1 / ARR_lower)              # NNT upper (using lower ARR)

# Print results
cat("Absolute Risk Reduction (ARR) over", n_years, "years:", ARR, "\n")
cat("95% CI for ARR:", ARR_lower, "to", ARR_upper, "\n\n")

cat("Number Needed to Treat (NNT) over", n_years, "years:", NNT, "\n")
cat("95% CI for NNT:", NNT_lower, "to", NNT_upper, "\n")
