#' Themes for usage with ggplot2
#'
#' Lists of themes usage as ggplot2 templates for different
#' kinds of ggplot routines.
#' Available themes:
#' * lpj.hist_theme for histograms
#' * lpj.map_theme for maps
#' * lpj.scatter_theme for scatter plots
#' * lpj.ts_theme for timeseries
#' 
#' @author Jörg Steinkamp <joersteinkamp@yahoo.de>

#stopifnot(require(grid))

lpj.hist_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             panel.background  = element_blank(),
                             panel.border      = element_blank(),
                             axis.line         = element_line(linetype="solid", colour="black"),
                             axis.text         = element_text(size=12, colour = "black"),
                             axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             axis.ticks.length = unit(1.5, "points"),
                             axis.title        = element_text(size=12, face="bold"),
                             legend.text       = element_text(size=12),
                             legend.title      = element_text(size=12, face="bold"),
                             legend.position   = "bottom",
                             legend.key        = element_rect(colour = "black"),
                             legend.key.width  = unit(0.08, "npc"),
                             plot.background   = element_blank(),
                             plot.title        = element_text(size=22)))

lpj.map_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                            panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                            panel.background  = element_rect(fill="#cae1ff"),
                            panel.border      = element_rect(fill=NA, linetype = "solid", colour = "black"),
                            axis.line         = element_blank(),
                            axis.text         = element_text(size=10, colour = "black"),
                            axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                            axis.ticks.length = unit(1.5, "points"),
                            axis.title        = element_blank(),
                            legend.text       = element_text(size=10),
                            legend.title      = element_blank(),
                            legend.position   = "bottom",
                            legend.key        = element_rect(colour = "black"),
                            legend.key.width  = unit(0.08, "npc"),
                            plot.background   = element_blank(),
                            plot.title        = element_text(size=22)))

lpj.scatter_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                panel.background  = element_blank(),
                                panel.border      = element_blank(),
                                axis.line         = element_line(linetype="solid", colour="black"),
                                axis.text         = element_text(size=10, colour = "black"),
                                axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                axis.ticks.length = unit(1.5, "points"),
                                axis.title        = element_text(size=10, face="bold"),
                                legend.text       = element_text(size=10),
                                legend.title      = element_blank(), #element_text(size=10, face="bold"),
                                legend.position   = "bottom",
                                legend.key        = element_blank(), #element_rect(colour = "black"),
                                legend.key.width  = unit(0.08, "npc"),
                                plot.background   = element_blank(),
                                plot.title        = element_text(size=22)))

lpj.ts_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                           panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                           panel.background  = element_blank(),
                           panel.border      = element_blank(),
                           axis.line         = element_line(size=0.1, linetype="solid", colour="black"),
#                             axis.line         = element_blank(),
                           axis.text         = element_text(size=12, colour = "black"),
                           axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                           axis.ticks.length = unit(1.5, "points"),
                           axis.title        = element_text(size=12, face="bold"),
                           legend.text       = element_text(size=12),
                           legend.title      = element_text(size=12, face="bold"),
                           legend.position   = "bottom",
                           legend.key        = element_rect(colour = "black"),
#                             legend.key.width  = unit(0.08, "npc"),
                           plot.background   = element_blank(),
                           plot.title        = element_text(size=22)))
