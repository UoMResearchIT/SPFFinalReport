# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Plot results for Q1 2021
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' plot_results_q1_2021()
#' }
#'
plot_results_q1_2021 <- function() {

  # TODO: Diagnose (and possibly eliminate?) this warning:
  # Warning message:
  # Removed 5 rows containing missing values or values outside the scale range (`geom_line()`).

  # Read shapefile data
  ew_msoa <- readRDS("Data_act/Processed/Shapefiles/ew_msoa.rds")
  ew_msoa_region <- readRDS("Data_act/Processed/Shapefiles/ew_msoa_region.rds")

  # Subsetting Greater Manchester shapefiles
  mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                      'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))
  mcr_msoa_region <- subset(ew_msoa_region, area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                             'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

  # Loading exposures
  out_q12021 <- readRDS('Output_act/CaseStudy2/Analysis/DailyAverage_Q1_2021.rds')

  # Relabelling exposures
  out_q12021$nssec5[which(out_q12021$nssec5 == -1)] <- 0

  ###########################################
  ### Figure 1 - Scatter plots with lines ###
  ###########################################
  # Getting quarterly averages
  QuarterAverage_q12021 <- out_q12021 %>%
    # Note: Using the .(pop_id, sex_label, ...) syntax causes a name conflict
    # with dplyr - so use the formula syntax for the 'group by' var instead,
    # i.e. '~ pop_id + sex_label + ...' rather than '.(area_id, pop_id, ...)'
    plyr::ddply(~ pop_id + sex_label + agegr4_label,
                plyr::summarize,
                mean = mean(exposure_emep),
                lower = quantile(exposure_emep, probs = 0.025),
                upper = quantile(exposure_emep, probs = 0.975),
                outdoor = mean(pm25_emep_agg))

  # Converting labels to character
  QuarterAverage_q12021$sex_label <- as.character(QuarterAverage_q12021$sex_label)
  QuarterAverage_q12021$agegr4_label <- as.character(QuarterAverage_q12021$agegr4_label)

  # Empty datasets for storing coefficients
  coeffs_q12021 <- expand.grid(sex_label = sort(unique(QuarterAverage_q12021$sex_label)),
                               agegr4_label = sort(unique(QuarterAverage_q12021$agegr4_label)),
                               intercept = NA,
                               slope = NA)

  # Converting labels to character
  coeffs_q12021$sex_label <- as.character(coeffs_q12021$sex_label)
  coeffs_q12021$agegr4_label <- as.character(coeffs_q12021$agegr4_label)

  # Enpty dataset for predictions
  preds_q12021 <- NULL

  # Loop for each age/sex profile
  for (i in 1:nrow(coeffs_q12021)){
    # Only keeping specific age and sex
    lm_dat <- subset(QuarterAverage_q12021,
                     sex_label == coeffs_q12021$sex_label[i] &
                       agegr4_label == coeffs_q12021$agegr4_label[i])
    # Runnign linear model
    mod <- lm(mean ~ outdoor,
              data = lm_dat)
    # Storing coefficients
    coeffs_q12021$intercept[i] <- mod$coefficients[1]
    coeffs_q12021$slope[i] <- mod$coefficients[2]
    # Dataset for predictions
    preds_q12021_tmp1 <- data.frame(sex_label = coeffs_q12021$sex_label[i],
                                    agegr4_label = coeffs_q12021$agegr4_label[i],
                                    outdoor = seq(4, 16, length.out = 100))
    # Predicting from the model
    preds_q12021_tmp2 <- predict(mod,
                                 newdata = preds_q12021_tmp1,
                                 se.fit = TRUE)
    # Getting predictions
    preds_q12021_tmp1$pred <- preds_q12021_tmp2$fit
    preds_q12021_tmp1$lower <- preds_q12021_tmp2$fit - 1.96 * preds_q12021_tmp2$se.fit
    preds_q12021_tmp1$upper <- preds_q12021_tmp2$fit + 1.96 * preds_q12021_tmp2$se.fit
    # Adding outputs
    preds_q12021 <- rbind(preds_q12021, preds_q12021_tmp1)
    # Removing unecessary datasets
    rm(preds_q12021_tmp1, preds_q12021_tmp2, mod)
    # Printing index
    print(i)
  }

  # Plot with linear models
  p <- ggplot2::ggplot(QuarterAverage_q12021,
                       ggplot2::aes(x = outdoor,
                                    y = mean)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::geom_ribbon(data = preds_q12021,
                         ggplot2::aes(x = outdoor,
                                      ymin = lower,
                                      ymax = upper),
                         size = 1,
                         fill = 'skyblue',
                         colour = NA,
                         alpha = 0.5,
                         inherit.aes = FALSE) +
    ggplot2::geom_line(data = preds_q12021,
                       ggplot2::aes(x = outdoor,
                                    y = pred),
                       size = 1,
                       colour = 'skyblue') +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(limits = c(4.5,NA),
                                breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_y_continuous(limits = c(4.5,NA),
                                breaks = scales::pretty_breaks(8)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = 'red', size = 1)+
    ggplot2::facet_grid(sex_label ~ agegr4_label) +
    ggplot2::labs(x = expression('Average ambient PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
                  y = expression('Average personal exposure to PM'[2.5] * ' (' * mu *'g/m'^3 * ')'))
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig1b.pdf", plot = p, width = 14, height = 7)

  # Saving coefficients
  readr::write_csv(coeffs_q12021, file = 'Coeffs_q12021.csv')

  ########################################
  ### Figure 2 - Time series line plot ###
  ########################################
  # Case study 2
  QuarterAverage_q12021 <-
    rbind(out_q12021 %>%
            # Note: Using the .(date) syntax causes a name conflict with dplyr -
            # so use the formula syntax for the 'group by' var instead, i.e.
            # '~ date' rather than '.(date)'
            plyr::ddply(~ date,
                        plyr::summarize,
                        type = 'Personal exposure',
                        mean = mean(exposure_emep - pm25_emep_agg),
                        lower = quantile(exposure_emep - pm25_emep_agg, probs = 0.025),
                        upper = quantile(exposure_emep - pm25_emep_agg, probs = 0.975)))


  # Plotting case study 2
  p <- ggplot2::ggplot(QuarterAverage_q12021,
                       ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = mean),
                       size = 1) +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_hline(yintercept = 0, colour = 'red', size = 1, linetype = 'dotted') +
    ggplot2::scale_y_continuous( limits = c(-15, 15),
                                 breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(x = "Date",
                  y = expression('Concentration of PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
                  colour = '')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig2b_diff.pdf", plot = p, width = 10)

  #############################################
  ### Figure 3 - Maps of personal exposures ###
  #############################################
  # Getting quarterly averages
  QuarterAverage_q12021 <- out_q12021 %>%
    # Note: Using the .(area_id) syntax causes a name conflict with dplyr -
    # so use the formula syntax for the 'group by' var instead, i.e.
    # '~ area_id' rather than '.(area_id)'
    plyr::ddply(~ area_id,
                plyr::summarize,
                mean = mean(exposure_emep),
                outdoor = mean(pm25_emep_agg),
                diff = mean(exposure_emep - pm25_emep_agg))

  # Wide to long dataset
  QuarterAverage_q12021 <- reshape2::melt(QuarterAverage_q12021, id.vars=c("area_id"))

  # Merging on to shapefiles
  QuarterAverage_q12021 <- mcr_msoa %>%
    sf::st_as_sf() %>%
    dplyr::left_join(QuarterAverage_q12021) %>%
    # Note: !is.na(variable) instead of !is.na(mean) fixes error:
    #   is.na() applied to non-(list or vector) of type 'closure'
    # dplyr::filter(!is.na(mean))
    dplyr::filter(!is.na(variable))

  # Converting to SF object
  mcr_msoa_region <- sf::st_as_sf(mcr_msoa_region)

  # Colour palettes
  colourPalette1 <- rev(colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))(100))
  colourPalette2 <- colorRampPalette(c('blue', 'white', 'red'))(100)

  # Plot of personal exposures
  p1 <- ggplot2::ggplot(subset(QuarterAverage_q12021, variable == 'mean'),
                        ggplot2::aes(fill = value)) +
    ggplot2::geom_sf(colour = NA) +
    ggplot2::geom_sf(data = mcr_msoa_region,
                     inherit.aes = FALSE,
                     colour = 'black',
                     fill = NA,
                     size = 0.4) +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = 'bottom',
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(x = '',
                  y = '',
                  fill = '',
                  title = '(a)') +
    # Colour scheme
    ggplot2::scale_fill_gradientn(limits = c(6,11),
                                  breaks = seq(6, 11, length.out = 6),
                                  colours = colourPalette1,
                                  guide = ggplot2::guide_colorbar(label = TRUE,
                                                                  draw.ulim = TRUE,
                                                                  draw.llim = TRUE,
                                                                  # here comes the code change:
                                                                  frame.colour = "black",
                                                                  ticks = TRUE,
                                                                  nbin = 10,
                                                                  label.position = "bottom",
                                                                  barwidth = 13,
                                                                  barheight = 0.75,
                                                                  direction = 'horizontal'))

  # Plot of ambient concentrations
  p2 <- ggplot2::ggplot(subset(QuarterAverage_q12021, variable == 'outdoor'),
                        ggplot2::aes(fill = value)) +
    ggplot2::geom_sf(colour = NA) +
    ggplot2::geom_sf(data = mcr_msoa_region,
                     inherit.aes = FALSE,
                     colour = 'black',
                     fill = NA,
                     size = 0.4) +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = 'bottom',
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(x = '',
                  y = '',
                  fill = '',
                  title = '(b)') +
    # Colour scheme
    ggplot2::scale_fill_gradientn(limits = c(6,14),
                                  breaks = seq(6, 14, length.out = 9),
                                  colours = colourPalette1,
                                  guide = ggplot2::guide_colorbar(label = TRUE,
                                                                  draw.ulim = TRUE,
                                                                  draw.llim = TRUE,
                                                                  # here comes the code change:
                                                                  frame.colour = "black",
                                                                  ticks = TRUE,
                                                                  nbin = 10,
                                                                  label.position = "bottom",
                                                                  barwidth = 13,
                                                                  barheight = 0.75,
                                                                  direction = 'horizontal'))

  # Plot of differences
  p3 <- ggplot2::ggplot(subset(QuarterAverage_q12021, variable == 'diff'),
                        ggplot2::aes(fill = value)) +
    ggplot2::geom_sf(colour = NA) +
    ggplot2::geom_sf(data = mcr_msoa_region,
                     inherit.aes = FALSE,
                     colour = 'black',
                     fill = NA,
                     size = 0.4) +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = 'bottom',
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(x = '',
                  y = '',
                  fill = '',
                  title = '(c)') +
    # Colour scheme
    ggplot2::scale_fill_gradientn(limits = c(-3,3),
                                  breaks = seq(-3, 3, length.out = 7),
                                  colours = colourPalette2,
                                  guide = ggplot2::guide_colorbar(label = TRUE,
                                                                  draw.ulim = TRUE,
                                                                  draw.llim = TRUE,
                                                                  # here comes the code change:
                                                                  frame.colour = "black",
                                                                  ticks = TRUE,
                                                                  nbin = 10,
                                                                  label.position = "bottom",
                                                                  barwidth = 13,
                                                                  barheight = 0.75,
                                                                  direction = 'horizontal'))

  # Outputting plot
  p <- gridExtra::arrangeGrob(p1, p2, p3, ncol = 3)
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig3b.pdf", plot = p, width = 10, height = 4)


  ###############################################
  ### Figure 4  - Density plots of exposures ####
  ###############################################
  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_wrap(. ~ nssec5) +
    ggplot2::labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig4b_nssec5.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(sex_label ~ agegr4_label) +
    ggplot2::labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig4b_AgeGr_Sex.pdf", plot = p, width = 10)

  ####################################################
  ### Figure 5 - Density plots of the differences ####
  ####################################################
  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_five - exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(. ~ nssec5) +
    ggplot2::labs(x = expression('Difference in daily average exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * '; Jan-Mar 2021, EMEP)'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig5b_nssec5.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_five - exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(. ~ daytype_label) +
    ggplot2::labs(x = expression('Difference in daily average exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * '; Jan-Mar 2021, EMEP)'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig5b_DayType.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_five - exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(. ~ agegr4_label) +
    ggplot2::labs(x = expression('Difference in daily average exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * '; Jan-Mar 2021, EMEP)'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig5b_AgeGr.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_five - exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(. ~ sex_label) +
    ggplot2::labs(x = expression('Difference in daily average exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * '; Jan-Mar 2021, EMEP)'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig5b_Sex.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_q12021,
                       ggplot2::aes(x = exposure_five - exposure_emep)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(sex_label ~ agegr4_label) +
    ggplot2::labs(x = expression('Difference in daily average exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * '; Jan-Mar 2021, EMEP)'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output_act/CaseStudy2/Fig/Fig5b_AgeGr_Sex.pdf", plot = p, width = 10)

  invisible()
}
