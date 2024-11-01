# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Plot results for July 2021
#'
#' @inheritParams write_cfg_template
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' plot_results_jul_2021()
#' }
#'
plot_results_jul_2021 <- function(cfg = NULL) {

  # Read shapefile data
  ew_msoa <- readRDS("Data/Processed/Shapefiles/ew_msoa.rds")
  ew_msoa_region <- readRDS("Data/Processed/Shapefiles/ew_msoa_region.rds")

  # Subsetting Greater Manchester shapefiles
  mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                      'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

  mcr_msoa_region <- subset(ew_msoa_region, area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                             'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

  # Loading exposures
  out_july2021 <- readRDS('Output/CaseStudy2/Analysis/DailyAverage_July_2021.rds')

  # Relabelling exposures
  out_july2021$nssec5[which(out_july2021$nssec5 == -1)] <- 0

  ###########################################
  ### Figure 1 - Scatter plots with lines ###
  ###########################################
  # Getting quarterly averages
  QuarterAverage_july2021 <- out_july2021 %>%
    # Note: Using the .(pop_id, sex_label ...) syntax causes a name conflict
    # with dplyr - so use the formula syntax for the 'group by' var instead,
    # i.e. '~ pop_id + sex_label + ...' rather than '.(pop_id, sex_label, ...)'
    plyr::ddply(~ pop_id + sex_label + agegr4_label,
                plyr::summarize,
                mean = mean(exposure_gm),
                lower = quantile(exposure_gm, probs = 0.025),
                upper = quantile(exposure_gm, probs = 0.975),
                outdoor = mean(pm25_gm_near))

  # Converting labels to character
  QuarterAverage_july2021$sex_label <- as.character(QuarterAverage_july2021$sex_label)
  QuarterAverage_july2021$agegr4_label <- as.character(QuarterAverage_july2021$agegr4_label)

  # Empty datasets for storing coefficients
  coeffs_july2021 <- expand.grid(sex_label = sort(unique(QuarterAverage_july2021$sex_label)),
                                 agegr4_label = sort(unique(QuarterAverage_july2021$agegr4_label)),
                                 intercept = NA,
                                 slope = NA)

  # Converting labels to character
  coeffs_july2021$sex_label <- as.character(coeffs_july2021$sex_label)
  coeffs_july2021$agegr4_label <- as.character(coeffs_july2021$agegr4_label)

  # Enpty dataset for predictions
  preds_july2021 <- NULL

  # Loop for each age/sex profile
  for (i in 1:nrow(coeffs_july2021)){
    # Only keeping specific age and sex
    lm_dat <- subset(QuarterAverage_july2021,
                     sex_label == coeffs_july2021$sex_label[i] &
                       agegr4_label == coeffs_july2021$agegr4_label[i])
    # Runnign linear model
    mod <- lm(mean ~ outdoor,
              data = lm_dat)
    # Storing coefficients
    coeffs_july2021$intercept[i] <- mod$coefficients[1]
    coeffs_july2021$slope[i] <- mod$coefficients[2]
    # Dataset for predictions
    preds_july2021_tmp1 <- data.frame(sex_label = coeffs_july2021$sex_label[i],
                                      agegr4_label = coeffs_july2021$agegr4_label[i],
                                      outdoor = seq(4, 16, length.out = 100))
    # Predicting from the model
    preds_july2021_tmp2 <- predict(mod,
                                   newdata = preds_july2021_tmp1,
                                   se.fit = TRUE)
    # Getting predictions
    preds_july2021_tmp1$pred <- preds_july2021_tmp2$fit
    preds_july2021_tmp1$lower <- preds_july2021_tmp2$fit - 1.96 * preds_july2021_tmp2$se.fit
    preds_july2021_tmp1$upper <- preds_july2021_tmp2$fit + 1.96 * preds_july2021_tmp2$se.fit
    # Adding outputs
    preds_july2021 <- rbind(preds_july2021, preds_july2021_tmp1)
    # Removing unecessary datasets
    rm(preds_july2021_tmp1, preds_july2021_tmp2, mod)
    # Printing index
    print(i)
  }

  # Plot with linear models
  p <- ggplot2::ggplot(QuarterAverage_july2021,
                       ggplot2::aes(x = outdoor,
                                    y = mean)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::geom_ribbon(data = preds_july2021,
                         ggplot2::aes(x = outdoor,
                                      ymin = lower,
                                      ymax = upper),
                         size = 1,
                         fill = 'skyblue',
                         colour = NA,
                         alpha = 0.5,
                         inherit.aes = FALSE) +
    ggplot2::geom_line(data = preds_july2021,
                       ggplot2::aes(x = outdoor,
                                    y = pred),
                       size = 1,
                       colour = 'skyblue') +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(limits = c(0,NA),
                                breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_y_continuous(limits = c(0,NA),
                                breaks = scales::pretty_breaks(8)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = 'red', size = 1)+
    ggplot2::facet_grid(sex_label ~ agegr4_label) +
    ggplot2::labs(x = expression('Average ambient PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
                  y = expression('Average personal exposure to PM'[2.5] * ' (' * mu *'g/m'^3 * ')'))
  ggplot2::ggsave("Output/CaseStudy2/Fig/Fig1a.pdf", plot = p, width = 14, height = 7)

  # Saving coefficients
  readr::write_csv(coeffs_july2021, file = 'Output/CaseStudy2/Coeff/Coeffs_july2021.csv')

  ########################################
  ### Figure 2 - Time series line plot ###
  ########################################
  # Case study 1
  QuarterAverage_july2021 <-
    rbind(out_july2021 %>%
            # Note: Using the .(date) syntax causes a name conflict with dplyr -
            # so use the formula syntax for the 'group by' var instead, i.e.
            # '~ date' rather than '.(date)'
            plyr::ddply(~ date,
                        plyr::summarize,
                        type = 'Personal exposure',
                        mean = mean(exposure_gm - pm25_gm_near),
                        lower = quantile(exposure_gm - pm25_gm_near, probs = 0.025),
                        upper = quantile(exposure_gm - pm25_gm_near, probs = 0.975)))

  # Plotting case study 1
  p <- ggplot2::ggplot(QuarterAverage_july2021,
                       ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = mean),
                       size = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_continuous(limits = c(-4, 4),
                                breaks = scales::pretty_breaks(8)) +
    ggplot2::geom_hline(yintercept = 0, colour = 'red', size = 1, linetype = 'dotted') +
    ggplot2::labs(x = "Date",
                  y = expression('Concentration of PM'[2.5] * ' (' * mu *'g/m'^3 * ')'),
                  colour = '')
  ggplot2::ggsave("Output/CaseStudy2/Fig/Fig2a_diff.pdf", plot = p, width = 10)

  #############################################
  ### Figure 3 - Maps of personal exposures ###
  #############################################
  # Getting quarterly averages
  QuarterAverage_july2021 <- out_july2021 %>%
    # Note: Using the .(area_id) syntax causes a name conflict with dplyr -
    # so use the formula syntax for the 'group by' var instead, i.e.
    # '~area_id' rather than '.(area_id)'
    plyr::ddply(~ area_id,
                plyr::summarize,
                mean = mean(exposure_gm),
                outdoor = mean(pm25_gm_near),
                diff = mean(exposure_gm - pm25_gm_near))

  # Wide to long dataset
  QuarterAverage_july2021 <- reshape2::melt(QuarterAverage_july2021, id.vars=c("area_id"))

  # Merging on to shapefiles
  QuarterAverage_july2021 <- mcr_msoa %>%
    sf::st_as_sf() %>%
    dplyr::left_join(QuarterAverage_july2021) %>%
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
  p1 <- ggplot2::ggplot(subset(QuarterAverage_july2021, variable == 'mean'),
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
    ggplot2::scale_fill_gradientn(limits = c(3.5,7.5),
                                  breaks = seq(3.5, 7.5, length.out = 5),
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
  p2 <- ggplot2::ggplot(subset(QuarterAverage_july2021, variable == 'outdoor'),
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
    ggplot2::scale_fill_gradientn(limits = c(2,9),
                                  breaks = seq(2, 9, length.out = 8),
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
  p3 <- ggplot2::ggplot(subset(QuarterAverage_july2021, variable == 'diff'),
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
  ggplot2::ggsave("Output/CaseStudy2/Fig/Fig3a.pdf", plot = p, width = 10, height = 4)

  ###############################################
  ### Figure 4  - Density plots of exposures ####
  ###############################################
  p <- ggplot2::ggplot(out_july2021,
                       ggplot2::aes(x = exposure_gm)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_wrap(. ~ nssec5) +
    ggplot2::labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output/CaseStudy2/Fig/Fig4a_nssec5.pdf", plot = p, width = 10)

  p <- ggplot2::ggplot(out_july2021,
                       ggplot2::aes(x = exposure_gm)) +
    ggplot2::geom_density(alpha = 0.5,
                          fill = 'skyblue') +
    ggplot2::facet_grid(sex_label ~ agegr4_label) +
    ggplot2::labs(x = expression('Daily average personal exposures to PM'[2.5] * ' (in '*mu * 'g/m'^3 * ')'),
                  y = "Density",
                  fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  ggplot2::ggsave("Output/CaseStudy2/Fig/Fig4a_AgeGr_Sex.pdf", plot = p, width = 10)

  invisible()
}
