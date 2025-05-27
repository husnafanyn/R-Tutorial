library(ggplot2)
diamonds
# Specify dataset and mapping
ggplot(data = diamonds,
       mapping = aes(x = carat, y = price)) +
# Add points
    geom_point()

# Make points with different color, larger, and semi-transparent
ggplot(data = diamonds,
       mapping = aes(x = carat, y = price)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 2) +
# Add a line of best fit
geom_smooth(method = "lm", se = FALSE)

# Indicate quality color of diamonds using different color
library(scales)
ggplot(data = diamonds,
       mapping = aes(x = carat, y = price, color = color)) +
  geom_point(alpha = .7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
# Modify the x and y axes
  scale_x_continuous(breaks = seq(0, 5.5, 1)) +
  scale_y_continuous(breaks = seq(0, 20000, 5000),
                     label = dollar) +
# Facets : reproduce plot for each cut variables
  facet_wrap(~cut) +
# Add informative labels
  labs(title = "Prices of Cut Diamonds",
       subtitle = "Around 54,000 Diamons",
       caption = "source: R Documentation",
       x = "Carat",
       y = "Price",
       color = "Level of Color Quality") +
# Use a minimalist theme
  theme_minimal()

# Facet for each cut using different color for cut
ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut)) + 
  geom_point() + 
  facet_wrap(~cut) + 
  theme(legend.position = "none")

# Color mapping for ggplot
ggplot(data = diamonds,
       mapping = aes(x = carat, y = price, color = color)) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)

# Color mapping for geom_point
ggplot(data = diamonds,
       mapping = aes(x = carat, y = price)) +
  geom_point(aes(color = color), alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)

# Bar Chart
ggplot(data = diamonds, mapping = aes(cut)) + 
  geom_bar()
# Using frequency table
freqtab <- as.data.frame(table(diamonds$cut))
freqtab
ggplot(data = freqtab, mapping = aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity")
# Bar chart with modified colors and labels
ggplot(data = diamonds, mapping = aes(cut)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(x = "Quality of The Cut",
       y = "Count",
       title = "Quality of The Diamonds Cut")
# Horizontal bar chart
ggplot(diamonds, aes(x = cut)) +
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(x = "",
       y = "Count",
       title = "Quality of The Diamonds Cut") +
  coord_flip()
# Bar chart with rotated labels
ggplot(diamonds, aes(x = cut)) +
  geom_bar() +
  labs(x = "",
       y = "Count",
       title = "Quality of The Diamonds Cut") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Bar chart with percentages
ggplot(diamonds, aes(x = cut, y = after_stat(count/sum(count)))) +
  geom_bar() +
  labs(x = "Quality of The Cut",
       y = "Percent",
       title = "Quality of The Diamonds Cut") +
  scale_y_continuous(labels = percent)
# Calculate number of the quality of the cut in each category
library(dplyr)
count_diamonds <- diamonds %>%
  count(cut)
# Plot the bars in ascending order
ggplot(data = count_diamonds, 
       mapping = aes(x = reorder(cut, n), y = n)) +
  geom_bar(stat="identity") +
  labs(x = "Quality of The Cut",
       y = "Count",
       title = "Quality of The Diamonds Cut") +
# Bar chart with numeric labels
  geom_text(aes(label = n), vjust=-0.5)
# Sorted bar chart with percent labels
percent_diamonds <- diamonds %>%
  group_by(cut) %>%
  summarize(n = n()) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))
ggplot(percent_diamonds, aes(x = reorder(cut, -pct), 
                             y = pct)) +
  geom_bar(stat="identity") +
  labs(x = "Quality of The Cut",
       y = "Percent",
       title = "Quality of The Diamonds Cut") +
  geom_text(aes(label = pctlabel), vjust=-0.5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     labels = percent)

# Histogram
ggplot(data = diamonds, 
       mapping = aes(x = price)) + 
  geom_histogram()
# Histogram with control bins, color, and labels
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", bins = 50) +
  labs(title="Diamonds by Price", subtitle = "number of bins = 50", x = "Price") +
# Add comma axis label
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)
# Histogram with control binwidth
ggplot(data = diamonds, 
       mapping = aes(x = price)) + 
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 2500) +
  labs(title="Diamonds by price", 
       subtitle = "number of bins = 2500", 
       x = "Price") +
  # Add comma axis label
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)

# Density
ggplot(data = diamonds, 
       mapping = aes(x = price)) + 
  geom_density() +
  labs(title="Diamonds by Price")
# Fill the density plot
ggplot(data = diamonds, 
       mapping = aes(x = price)) + 
  geom_density(fill = "indianred3") +
  labs(title="Diamonds by Price")
# default bandwidth for the price variable
bw.nrd0(diamonds$price)
# Density with control bandwidth
ggplot(data = diamonds, 
       mapping = aes(x = price)) + 
  geom_density(fill = "indianred3", bw = 50) +
  labs(title="Diamonds by Price", 
       subtitle = "bandwidth = 50")

# Stacked bar chart
ggplot(diamonds, aes(x = cut, fill = color)) +
  geom_bar(position = "stack")
# Side-by-side bar chart
ggplot(diamonds, aes(x = cut, fill = color)) +
  geom_bar(position = "dodge")
# 100% stacked bar chart / segmented bar chart
ggplot(diamonds, aes(x = cut, fill = color)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")
# 100% stacked bar chart with better labels and setting colors
library(RColorBrewer)
ggplot(diamonds, aes(x = cut, fill = color)) +
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", fill = "Level Quality of Color", x = "Quality of Cut",
       title = "Quality of The Diamonds Cut") +
  theme_minimal()
# Create a summary dataset
summary_diamonds <- diamonds %>%
  group_by(cut, color) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
# 100% stacked bar chart with adding labels to each segment
ggplot(summary_diamonds, aes(x = cut, y = pct, fill = color)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), label = percent) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
  labs(y = "Percent", fill = "Level Quality of Color", x = "Quality of Cut",
       title = "Quality of The Diamonds Cut") +
  theme_minimal()

# Line chart
library(gapminder)
?gapminder
# Select US cases
line_data = gapminder %>%
  filter(country == "United States")
# Simple line plot
ggplot(line_data, aes(x = year, 
                      y = lifeExp)) +
  geom_line() 
# Line plot with points and and improved labeling
ggplot(line_data, aes(x = year, y = lifeExp)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/") +
# Adding smoothing line
  geom_smooth(method = "lm", SE = FALSE)

# Box plot
ggplot(data = diamonds, mapping = aes(y = price)) + 
  geom_boxplot() +
  labs(title = "Distribution of Price")
# Box plot by group
ggplot(data = diamonds, mapping = aes(y = price, 
                                      x = cut)) + 
  geom_boxplot() +
  labs(title = "Distribution of Price by Quality of Cut")
# Add color for each boxplot
# Box plot by group
ggplot(data = diamonds, mapping = aes(y = price, x = cut, fill = cut)) + 
  geom_boxplot() +
  labs(title = "Distribution of Price by Quality of Cut") +
  theme(legend.position = "none")
# Box plot with notch
ggplot(data = diamonds, mapping = aes(y = price, x = cut)) + 
  geom_boxplot(notch = TRUE, fill = "cornflowerblue", alpha = 1) +
  labs(title = "Distribution of Price by Quality of Cut")

# Violin plot by group
ggplot(data = diamonds, mapping = aes(y = price, x = cut)) + 
  geom_violin() +
  labs(title = "Distribution of Price by Quality of Cut")

# Violin and boxplot
ggplot(data = diamonds, mapping = aes(y = price, x = cut)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .15, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 1) + 
  labs(title = "Distribution of Price by Quality of Cut")

# Pie chart
# Use ggpie package
library(ggpie)
ggpie(data = diamonds, group_key = "cut", 
      count_type = "full",
      label_info = "all", label_size = 4, label_pos = "in", label_split = NULL)
# Use ggplot package
maxmin_cut <- diamonds %>%
  count(cut) %>%
  mutate(Percent = n / sum(n),
         ymax = cumsum(Percent),
         ymin = ifelse(is.na(lag(ymax)), 
                       0, lag(ymax)))
ggplot(maxmin_cut) +
  geom_rect(aes(ymin = ymin, ymax = ymax, 
                fill = cut, xmin = 2, 
                xmax = 4)) +
  coord_polar(theta = "y", start = 1) +
  geom_text(aes(x = 3.5, y = Percent, 
                label = paste0(cut, "\n",
                               round(Percent*100,1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
#Change color  
  scale_fill_brewer(palette = "Set2") +
#Theme void
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Quality of The Diamonds Cut")

# Donut chart
ggplot(maxmin_cut) +
  geom_rect(aes(ymin = ymin, ymax = ymax, 
                fill = cut, 
                xmin = 2, xmax = 4)) +
  coord_polar(theta = "y", start = 1) +
  xlim(c(-1,4)) +
  geom_text(aes(x = 3.5, y = Percent, 
                label = paste0(cut, "\n",
                                                     round(Percent*100,1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  annotate('text', x = -1, y = 0.8, 
           label = paste0("Quality of The Diamonds Cut", "\n",
                          "over 50000 diamonds"), 
           size = 3) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position = "none")
  

# Tree map
library(treemapify)
ggplot(count_diamonds, aes(fill = cut, area = n)) +
  geom_treemap() + 
  labs(title = "Diamonds by Quality of Cut")
# Add label
ggplot(count_diamonds, aes(fill = cut, area = n, label = cut)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "Diamonds by Quality of Cut") +
  theme(legend.position = "none")

# Calculate summary by group
library(dplyr)
summary_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(n = n(),
            mean = mean(price),
            sd = sd(price),
            se = sd / sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))

# Ridgeline graph
library(ggridges)
ggplot(diamonds, 
       aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Price Distribution by Quality of Cut") +
  theme(legend.position = "none")

# Waffle chart
#library(waffle)
#ggplot(count_diamonds, aes(fill = cut, values=n)) +
#  geom_waffle(na.rm=TRUE)
# Create a customized caption
#cap = paste0("1 square = ", ceiling(sum(plotdata$n)/100), " case(s).")
#ggplot(plotdata, aes(fill = cut, values=n)) +
#  geom_waffle(na.rm=TRUE,
#              n_rows = 10, size = .4, color = "white") + 
#  scale_fill_brewer(palette = "Spectral") +
#  coord_equal() +
#  theme_minimal() + 
#  theme_enhance_waffle() +
#  theme(legend.title = "Quality of Cut") +
#  labs(title = "Proportion of Quality of Diamonds Cut", caption = cap)



