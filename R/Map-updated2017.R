library(rgdal) # for readOGR function
library(ggplot2) # for barplot
library(stringr) # for str_wrap function

# Data source: Table 18, DPRK Census 2008

# Use ggplot to create map
######################
#read shape file
dprk.shp <- readOGR(dsn = "../data/PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF
dprk.map <- fortify(dprk.shp) # dataframe

#read data
data <- read.csv("../data/deaths2.csv") # dataframe

# create Maternal Mortality Rate 
data$mmr <- round((data$Total.Deaths / data$Number.of.Live.Births) * 100000.0)

# make breaks in MMR
data$mmr.break <- cut(data$mmr, breaks = c(65, 70, 75, 80, 85, 90), include.lowest = TRUE, 
                      labels = c("65-69", "70-74", "75-79", "80-84", "85-90"))

# create Percentage of deaths at hospital
data$percent.hospital <- round((data$Died.at.Hospital / data$Total.Deaths) * 100)

# make breaks in Percentage of deaths at hospital
data$percenthospital.break <- cut(data$percent.hospital, breaks = c(20, 25, 30, 35, 40), 
                                  include.lowest = TRUE, labels = c("20-24", "25-29", "30-34", "35-40"))

# Merge MMR data with Map shape data
merged <- merge(dprk.map, data, by = "id") # dataframe

#read csv file that has province names and coordinates of labels
provinces <- read.csv("../data/provinces.csv")

# Map 1: MMR by Province
########################

# The sequence postscript, ggplot, and dev.off() creates a plot and saves it as EPS. 
# This creates a plot 9x9 inches. Unit is inches by default.
# postscript("mmr-cmyk.eps", width = 9, height = 9, horizontal = FALSE, 
#            onefile = FALSE, paper = "special", colormodel = "cmyk")

  # long, lat, group are columns in "merged" dataframe
gg <- ggplot(data = merged, aes(x = long, y = lat, group = group)) + 
  # "colour" specifies color of the border of the map
  geom_polygon(aes(fill = mmr.break), colour = "white") +
  # coord_fixed fixes the aspect ratio between x and y. Here y is 1.3 times x.
  coord_fixed(1.3) +
  # Specify color pallete to fill in the polygons
  scale_fill_brewer(palette = "Reds") +
  # Plot title
  ggtitle("\nMaternal Deaths Per 100,000 Live Births in DPRK by Province (2008)") +
  # No legend title. Caption is at bottom right of graph.
  labs(fill = NULL, 
       caption = "Note: Based on data of maternal deaths from DPRK Census 2008 and data of administrative boundaries from GADM.org")

gg + theme_minimal() + 
  # theme_minimal has white background. No black border around plot. Has axes, axis labels.
  # get rid of axes, axis labels, panel grid
  theme(axis.line = element_blank(), 
  # axis.line.y to specify y axis; axis.line.x for x axis
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  panel.grid = element_blank(),
  # legend text
  legend.text = element_text(size = 13),
  # title text
  plot.title = element_text(size = rel(1.5), face = "bold")
) +
  # add province names. Display is column in "provinces" dataframe
  geom_text(data = provinces, aes(long, lat, group = NULL, label = Display), size = 3.8, angle = 20) 

# dev.off()

# Save plot as a EPS, dimension 9 x 9 inches. However I'm not sure if this is CMYK encoded.
ggsave("../pics/mmr-ggsave.eps", width = 9, height = 9, units = "in")

# Map 2: Percent of maternal deaths at hospital, by province
#########################################################

# The sequence postscript, ggplot, and dev.off() creates a plot and saves it as EPS. 
# This creates a plot 9x9 inches. Unit is inches by default.
postscript("../pics/hospitals-cmyk.eps", width = 9, height = 9, horizontal = FALSE, 
           onefile = FALSE, paper = "special", colormodel = "cmyk")

# long, lat, group are columns in "merged" dataframe
gg <- ggplot(data = merged, aes(x = long, y = lat, group = group)) + 
  # "colour" specifies color of the border of the map
  geom_polygon(aes(fill = percenthospital.break), colour = "white") +
  # coord_fixed fixes the aspect ratio between x and y. Here y is 1.3 times x.
  coord_fixed(1.3) +
  # Specify color pallete to fill in the polygons. 
  # Direction = -1 reverses the color scale (first group is now darkest color). Default is 1.
  scale_fill_brewer(palette = "Reds", direction = -1) +
  # Plot title
  ggtitle("\nPercentage of Maternal Deaths at Hospitals by Province (DPRK, 2008)") +
  # No legend title. Caption is at bottom right of graph.
  labs(fill = NULL, 
       caption = "Note: Based on data of maternal deaths from DPRK Census 2008 and data of administrative boundaries from GADM.org")

gg + theme_minimal() + 
  # theme_minimal has white background. No black border around plot. Has axes, axis labels.
  # get rid of axes, axis labels, panel grid
  theme(axis.line = element_blank(), 
        # axis.line.y to specify y axis; axis.line.x for x axis
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        # legend text
        legend.text = element_text(size = 13),
        # title text
        plot.title = element_text(size = rel(1.5), face = "bold")
  ) +
  # add province names. Display is column in "provinces" dataframe
  geom_text(data = provinces, aes(long, lat, group = NULL, label = Display), size = 3.8, angle = 20) 

dev.off()


# Other themes
#####################
gg + theme_bw() # white background. Still has black border around plot, axes, axis labels.
  
gg + theme_classic() # white background. Half of black border. No panel grid. Has axes.
  