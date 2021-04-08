### Fabien Plisson 31 Mar 2021
'Short script to generate chord diagrams (using circlize) from correlation or covariance matrices'

### Working directory
setwd('...')

### Install packages 
library(tidyverse)
library(viridisLite)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(data.table)
library(magrittr)
library(dplyr)
library(tibble)

# Load dataset (correlation matrix)
corr_data <- read.csv('./Results/Pearson_corr_matrix_85muts_490seqs.csv', header=TRUE, stringsAsFactors=FALSE)
rownames(corr_data) <- corr_data[,1]
corr_data[,1] <- NULL
corr_data
colnames(corr_data) <- rownames(corr_data)
corr_mat <- as.matrix(corr_data)

corr_df <- corr_data %>%
  rownames_to_column %>%
  gather(key = 'from', value = 'to', -rowname)

# Load dataset (covariance matrix)
cov_data <- read.csv('./Results/Covariance_matrix_51muts_1552seqs_11clades.csv', header=TRUE, stringsAsFactors=FALSE)
rownames(cov_data) <- cov_data[,1]
cov_data[,1] <- NULL
colnames(cov_data) <- rownames(cov_data)
cov_mat <- as.matrix(cov_data)

cov_df <- cov_data %>%
  rownames_to_column %>%
  gather(key = 'from', value = 'to', -rowname)

#Parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# Color palette (random)
#mycolor <- viridis(315, alpha = 1, begin = 0, end = 1, option = "D")
#mycolor <- mycolor[sample(1:315)]

# Color palette (clockwise)
labls <- setNames(viridis(length(unlist(dimnames(cov_mat)))), union(rownames(cov_mat), colnames(cov_mat)))

#Chord
chordDiagram(
  x = cov_mat, 
  grid.col = labls,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  preAllocateTracks = 1,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE
  #link.decreasing = TRUE
  )

# Add text and axis
circos.trackPlotRegion(
  track.index = 2, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = ylim[1] + 1.0, 
      labels = sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE,
      adj = c(0, 0.5)
      #cex = 0.8
    )
  }
)





