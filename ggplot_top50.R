# Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 

# Init Ggplot
g=ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(col="steelblue", size=3) + geom_smooth(method='lm', se=FALSE, col="firebrick") # area and poptotal are columns in 'midwest'
g=g+ylim(c(0,1000000))+xlim(c(0,0.1))
# Did you notice that the line of best fit became more horizontal compared to the original plot? This is because, 
# when using xlim() and ylim(), the points outside the specified range are deleted and will not be considered while drawing the 
# line of best fit (using geom_smooth(method='lm')). This feature might come in handy when you wish to know how the line of best fit
# would change when some extreme values (or outliers) are removed.

g1 = g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))
#This visually remove the outliers, but they remain in the data, thus the line of best fit remains same.
g1=g1 + labs(x="Area", y="Population", title="Area vs Population",subtitle="From midwest dataset", caption="Midwest demographics")
g1
#Another way of doing it
g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")

 


plot(g)
# Also note that aes() function is used to specify the X and Y axes. That's because, any 
# information that is part of the source dataframe has to be specified inside the aes() function.
?geom_smooth

# Suppose if we want the color to change based on another column in the source dataset (midwest), it must be specified inside the aes() function.
gg=ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(aes(col=state), size=3) + geom_smooth(method='lm', se=FALSE, col="firebrick") # area and poptotal are columns in 'midwest'
gg=gg + labs(x="Area", y="Population", title="Area vs Population",subtitle="From midwest dataset", caption="Midwest demographics")
gg = gg + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

# Now each point is colored based on the state it belongs because of aes(col=state). Not just color, but size, shape, stroke (thickness of boundary)
# and fill (fill color) can be used to discriminate groupings.


gg + theme(legend.position="None")  # remove legend
gg + scale_colour_brewer(palette = "Set1")


library(RColorBrewer)
head(brewer.pal.info, 10)
#http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html

Step 1: Set the breaks
#The breaks should be of the same scale as the X axis variable. Note that I am using scale_x_continuous because, the X axis variable is a continuous variable. 
#Had it been a date variable, scale_x_date could be used. Like scale_x_continuous() an equivalent scale_y_continuous() is available for Y axis.

gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11]) # gives you letters on the axis 

# Reverse X Axis Scale
gg + scale_x_reverse()
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) +
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})

#Show all available base themes
?theme_bw
theme_set(theme_classic())
gg

gg + theme_bw() + labs(subtitle="BW Theme")

?theme


###StartOfPart2Tutorial###
options(scipen=999)
library(ggplot2)
data("midwest", package = "ggplot2")
theme_set(theme_bw())
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Add plot components --------------------------------
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Call plot ------------------------------------------
plot(gg)

gg + theme(plot.title=element_text(size=20, 
                                   face="bold", 
                                   family="American Typewriter",
                                   color="tomato",
                                   hjust=0.75,
                                   lineheight=1.2),  # title
           plot.subtitle=element_text(size=15, 
                                      family="American Typewriter",
                                      face="bold",
                                      hjust=0.5),  # subtitle
           plot.caption=element_text(size=15),  # caption
           axis.title.x=element_text(vjust=0,  
                                     size=15),  # X axis title
           axis.title.y=element_text(size=15),  # Y axis title
           axis.text.x=element_text(size=10, 
                                    angle = 30,
                                    vjust=.5),  # X axis text
           axis.text.y=element_text(size=10))  # Y axis text
           
#face, sets the font face ("plain", "italic", "bold", "bold.italic")
#family, is used to set a new font

#There are 3 ways to change the legend title.
#Method 1: Using labs()
gg + labs(color="State", size="Density")

#Method 2: Using guides()
gg <- gg + guides(color=guide_legend("State"), size=guide_legend("Density"))  # modify legend title

#Method 3: Using scale_aesthetic_vartype() format
# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Modify Legend
gg + scale_color_discrete(name="State") + scale_size_continuous(name = "Density", guide = FALSE)  # turn off legend for size


#How to Change Legend Labels and Point Colors for Categories

gg + scale_color_manual(name="State", 
                        labels = c("Illinois", 
                                   "Indiana", 
                                   "Michigan", 
                                   "Ohio", 
                                   "Wisconsin"), 
                        values = c("IL"="blue", 
                                   "IN"="red", 
                                   "MI"="green", 
                                   "OH"="brown", 
                                   "WI"="orange"))

#Change the Order of Legend
gg + guides(colour = guide_legend(order = 1),
            size = guide_legend(order = 2))

#How to Style the Legend Title, Text and Key
gg + theme(legend.title = element_text(size=12, color = "firebrick"), 
           legend.text = element_text(size=10),
           legend.key=element_rect(fill='springgreen')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5)))


#How to Remove the Legend and Change Legend Positions

#The legend.position is the x and y axis position in chart area, where (0,0) is bottom left of the chart and 
#(1,1) is top right. Likewise, legend.justification refers to the hinge point inside the legend.

gg + theme(legend.position="None") + labs(subtitle="No Legend")

gg + theme(legend.position="left") + labs(subtitle="Legend on the Left")

gg + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="Legend at Bottom")

# legend at bottom-right, inside the plot --------------------
gg + theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
           legend.justification=c(1,0), 
           legend.position=c(0.95, 0.05),  
           legend.background = element_blank(),
           legend.key = element_blank()) + 
  labs(subtitle="Legend: Bottom-Right Inside the Plot")

# legend at top-left, inside the plot -------------------------
gg + theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
           legend.justification=c(0,1), 
           legend.position=c(0.05, 0.95),
           legend.background = element_blank(),
           legend.key = element_blank()) + 
  labs(subtitle="Legend: Top-Left Inside the Plot")

#3. Adding Text, Label and Annotation
midwest_sub <- midwest[midwest$poptotal > 300000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 300000, midwest_sub$county, "")

gg + geom_text(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggplot2::geom_text") + theme(legend.position = "None")

gg + geom_label(aes(label=large_county), size=2, data=midwest_sub, alpha=0.25) + labs(subtitle="With ggplot2::geom_label") + theme(legend.position = "None")  # label

library(ggrepel)
gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None")

gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_label_repel") + theme(legend.position = "None")   # label


library(grid)
my_text <- "This text is at x=0.7 and y=0.8!"
my_grob = grid.text(my_text, x=0.7,  y=0.8, gp=gpar(col="firebrick", fontsize=14, fontface="bold"))
gg + annotation_custom(my_grob)

#4. Flipping and Reversing X and Y Axis
#How to flip the X and Y axis?
#Just add coord_flip()

gg + coord_flip()

gg + scale_x_reverse() + scale_y_reverse()

#5. Faceting: Draw multiple plots within one figure
data(mpg, package="ggplot2")  # load data
# mpg <- read.csv("http://goo.gl/uEeRGu")  # alt data source

g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  labs(title="hwy vs displ", caption = "Source: mpg") +
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()  # apply bw theme
plot(g)

g + facet_wrap( ~ class, nrow=3) + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure") 
g + facet_wrap( ~ class, scales = "free") + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")


g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure") +
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()  # apply bw theme

# Add Facet Grid
g1 <- g + facet_grid(manufacturer ~ class)  # manufacturer in rows and class in columns
plot(g1)