cols <- c(
  # "#c9e2f6",
  "#423Ef9",
  "#3e7Df9",
  # "#95cbee",
  "#1dACfd",
  "#1dFDf3",
  # "#3e70f9",
  # "#49FD1d",
  "#8fF93e",
  "#f3FD1d",
  "#fdD31d",
  '#fd8D1d',
  "#FF2F00"
)

showColors <- function(cols = c('#423ef9', '#8ff93e', '#ff2f00')) {
  #cols <- c(colorRampPalette(cols[3:4])(10), colorRampPalette(cols[4:8])(55), colorRampPalette(cols[8:9])(35))
  barplot(rep(1, length(cols)), col = cols)
}

mycolors = c("#f32440", "#2185ef", "#d421ef")
