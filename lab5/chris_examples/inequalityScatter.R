## Using tile to make a scatterplots of poverty reduction against
## the effective number of parties (Source: Iversen & Soskice, 2002)
##
## Chris Adolph
##
## 11 February 2016    faculty.washington.edu/cadolph

# Clear memory of all objects
rm(list=ls())

# Load libraries 
library(tile)               # For graphics
library(RColorBrewer)       # For nice colors
library(plyr)               # For mapvalues()

wd <- getwd()
setwd(paste0(wd, "/lab5/chris_examples"))

# Load data
data <- read.csv("iverRevised.csv", header=TRUE)
attach(data)

# Choose three distinct colors
col <- brewer.pal(3, "Set1")[c(2,3,1)]

# Next, recode the partySystem variable into both a set of symbols..
symbSystem <- mapvalues(partySystem,
                        from=c("Majoritarian", "Proportional", "Unanimity"),
                        to=c(17, 15, 16))
symbSystem <- as.numeric(as.character(symbSystem))

# ... and a set of colors, taking care to avoid problems with converting factors
colSystem <- mapvalues(partySystem,
                       from=c("Majoritarian", "Proportional", "Unanimity"),
                       to=col)
colSystem <- as.character(colSystem)

# First, collect all the data inputs into a series of "traces"

# The actual scattered points
trace1 <- scatter(x = effectiveParties, # X coordinate of the data
                  
                  y = povertyReduction, # Y coordinate of the data
                  
                  labels = country, # Labels for each point

                  # Plot symbol for each point
                  pch = symbSystem,

                  # Color for each point
                  col = colSystem,

                  # Offset text labels
                  labelsyoffset = -0.035,  # on npc scale

                  # Fontsize
                  fontsize = 9,

                  # Marker size
                  size = 1,  # could be vector for bubble plot

                  # Add a robust fit line and CI
                  fit = list(method="mmest", ci = 0.95),

                  # Which plot(s) to plot to
                  plot = 1
                  )

## Brian: try plotting the scatter trace alone
# tile(trace1)

# The rugs with marginal distributions
rugX1 <- rugTile(x=effectiveParties, type="lines", plot = 1)

rugY1 <- rugTile(y=povertyReduction, type="lines", plot = 1)

## Brian: try plotting the scatter trace with rug tiles
# tile(trace1, rugX1, rugY1)

# A legend
legendSymbols1 <- pointsTile(x=  c(1.8,   1.8,       1.8),
                            y=  c(78,     74,      70),
                            pch=c(17,     15,      16),
                            col=col,
                            fontsize=9,
                            size=1,
                            plot=1
                            )
# tile(trace1, rugX1, rugY1, legendSymbols1)

legendLabels1 <- textTile(labels=c("Majoritarian",
                                  "Proportional",
                                  "Unanimity"),
                         x=  c(2.05,      2.05,       2.05),
                         y=  c(78,     74,      70),
                         col=col,
                         fontsize=9,
                         plot=1
                         )
# tile(trace1, rugX1, rugY1, legendSymbols1, legendLabels1)

# Now, send that trace to be plotted with tile
tile(trace1,                # Could list as many traces here as we want
     rugX1,
     rugY1,
     legendSymbols1,
     legendLabels1,
      
     # Some generic options for tile
     RxC = c(1,1),          # the number of rows and columns of the tiling of plots
     height = list(plot="golden", plottitle=4),
     width = list(yaxistitle=3),

     limits=c(1.6, 7.5, 0, 82), # Limits of plotting region
     
     # x-axis controls
     xaxis=list(log = TRUE,
       at = c(2,3,4,5,6,7)
       ),
     
     xaxistitle=list(labels=c("Effective number of parties")),

     # y-axis controls
     yaxistitle=list(labels=c("% lifted from poverty by taxes & transfers")),

     # Plot titles
     plottitle=list(labels=("Party Systems and Redistribution")),

     # Output control
     output=list(file="iverScatter", width=8)
     )

