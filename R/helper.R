

# length2 -----------------------------------------------------------------

# New version of length which can handle NA's: if na.rm==T, don't count them
length2 <- function (x, na.rm = FALSE) {
  if (na.rm)
    sum(!is.na(x))
  else
    length(x)
}


summarySE <-
  function(data = NULL,
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
    gd <- df[, list(cnt = .N,
                    N = length2(get(measurevar), na.rm = na.rm),
                    avg = mean(get(measurevar), na.rm = na.rm),
                    sd = sd(get(measurevar), na.rm = na.rm)
    ), keyby = list(grp = get(groupvars))]

    # # Rename the "mean" column
    # setnames(gd, 'avg', measurevar)
    # setnames(gd, 'grp', groupvars)

    # Calculate standard error of the mean
    gd[, se := sd / sqrt(N)]

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    gd[, ':='(lwr = avg - se * qt(conf.interval / 2 + .5, N - 1),
              upr = avg + se * qt(conf.interval / 2 + .5, N - 1)
    ), keyby = list(grp)]

    gd
  }



