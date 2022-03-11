#' MyDDT
#'
#' Plots LENGTH~WEIGHT for the user-specified SPECIES.
#'
#' @param df Data set
#' @param SPECIES Species that will be left. The rest will be excluded
#'
#' @return A plot of LENGTH~WEIGHT for only the user defined SPECIES
#' @export
#'
#' @examples myddt(df=ddt, SPECIES == "LMBASS")

library(dplyr, lib.loc="C:/Users/ezeki/OneDrive/Documents/R/win-library/4.1/")
library(ggplot2, lib.loc="C:/Users/ezeki/OneDrive/Documents/R/win-library/4.1/")

globalVariables(c("%>%", "filter", "ggplot", "aes", "geom_point", "stat_smooth",
                  "ggtitle", "theme", "element_text", "write.csv"))

myddt <- function(df, cond){
  # %>% is an operator from dplyr.
  # You must use "{{}}" to tell filter to find cond
  df1 = df %>% filter({{cond}})

  LENGTH=df1$LENGTH
  WEIGHT=df1$WEIGHT
  RIVER=df1$RIVER
  # Creates a ggplot using df which maps LENGTH vs WEIGHT and colors points by RIVER
  gPlot=ggplot(df1, aes(x=LENGTH,y=WEIGHT, color = RIVER))
  # Adds points and a quadratic quadratic curve
  # Sets the title and moves it to the middle
  gPlot=gPlot+geom_point() + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + ggtitle(paste("Length Vs Weight of", df1$SPECIES), "Ezekiel House") + theme(plot.title = element_text(hjust = 0.5))

  print(gPlot)

  # Writes a new csv file based on name of SPECIES.
  # Used df1$SPECIES[1] to grab a single SPECIES string instead of all of them.
  write.csv(df1, paste("LvsWfor",df1$SPECIES[1],".csv", sep=""))

  # Creates a table of relative frequency for RIVER
  freq=table(df$RIVER)/length(df$RIVER)

  # Creates a named list of all three lists
  bigList<-list(Original=df, Filtererd=df1, Relative=freq)
  print(bigList)

}
