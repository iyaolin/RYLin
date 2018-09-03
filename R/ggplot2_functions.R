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
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

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
plotErrorBar <- function(df, x, y) {
  t <- df[, .N, keyby = .(vx = get(x),
                          vy = get(y))] %>%
    dcast.data.table(vx ~ vy, value.var = "N") %>%
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



