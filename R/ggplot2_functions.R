# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# Note: A copy from Cookbook for R (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)

renderMultiPlot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
      print(plots[[1]])

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <-
          as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }





# plotErrorBar ------------------------------------------------------------

# plotErrorBar : customized ggplot2 bar plot with error bar
plotErrorBar.bi <- function(df, x, y) {
  t <- df[, .N, keyby = .(vx = get(x),
                          vy = get(y))] %>%
    dcast.data.table(vx ~ vy, value.var = "N", fill = 0L) %>%
    '['(, value := `1` / (`1` + `0`)) %>%
    '['()
  gd <-
    t[, c('lwr', 'upr') := get_binCI(`1`, `1` + `0`), by = 1:nrow(t)][]
  setnames(gd, old = 'vx', x)
  p <- gd  %>%
    ggplot(aes_string(x, 'value', fill = x)) +
    geom_col() +
    geom_errorbar(
      aes(ymin = lwr, ymax = upr),
      width = 0.3,
      size = 0.7,
      color = "gray30"
    ) +
    theme_light() +
    theme(legend.position = "none")
  list(gd, p)
}



# plotFactorVar --------------------------------------------------------------
plotFactorVar <- function(df, col) {
  g <- ggplot(df, aes_string(col, fill = col)) +
    geom_bar() +
    theme_minimal() +
    theme(legend.position = 'none')
  return(g)
}


# plotNumericVar ----------------------------------------------------------------
plotNumericVar <- function(df, col) {
  ggplot(df, aes_string(col)) +
    geom_histogram(bins = 30, color = 'grey10') +
    theme_bw()
}
# plotNumericVar(df, 'purchase')


# plotTwoVars.density ----------------------------------------------------------------
plotTwoVars.density <- function(df, x, y) {
  ggplot(df, aes_string(y, group = x)) +
    geom_density(aes(fill = x), alpha = 0.3, color = 'grey10') +
    theme_bw()
}





# plotErrorBar ------------------------------------------------------------
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data table.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
plotErrorBar <-
  function(df = NULL,
           measurevar,
           groupvars = NULL,
           na.rm = FALSE,
           conf.interval = .95,
           .drop = TRUE) {
    # # New version of length which can handle NA's: if na.rm==T, don't count them
    # length2 <- function (x, na.rm = FALSE) {
    #   if (na.rm)
    #     sum(!is.na(x))
    #   else
    #     length(x)
    # }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    gd <- df[, list(
      cnt = .N,
      N = length2(get(measurevar), na.rm = na.rm),
      avg = mean(get(measurevar), na.rm = na.rm),
      sd = sd(get(measurevar), na.rm = na.rm)
    ), keyby = list(grp = get(groupvars))]

    # Calculate standard error of the mean
    gd[, se := sd / sqrt(N)]

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    gd[, ':='(
      lwr = avg - se * qt(conf.interval / 2 + .5, N - 1),
      upr = avg + se * qt(conf.interval / 2 + .5, N - 1)
    ), keyby = list(grp)]

    # Rename the "mean" column
    setnames(gd, 'avg', measurevar)
    setnames(gd, 'grp', groupvars)
    p <- gd  %>%
      ggplot(aes_string(groupvars, measurevar, fill = groupvars)) +
      geom_col(alpha = 0.8) +
      geom_errorbar(
        aes(ymin = lwr, ymax = upr),
        width = 0.3,
        size = 0.7,
        color = "gray30"
      ) +
      theme_light() +
      theme(legend.position = "none")
    list(gd, p)
  }



# customized theme --------------------------------------------------------
theme_RYLin <-
  function(base_size = 10,
           base_family = 'YaHei Consolas Hybrid') {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
        axis.text = element_text(size = 10),
        axis.text.x = element_text(
          angle = 0,
          vjust = 0.5,
          hjust = 0.5
        ),
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fffcfc"),
        strip.background = element_rect(
          fill = "#820000",
          color = "#820000",
          size = 0.5
        ),
        strip.text = element_text(
          face = "bold",
          size = 10,
          color = "white"
        ),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_blank(),
        panel.border = element_rect(
          color = "grey30",
          fill = NA,
          size = 0.5
        )
      )
}

