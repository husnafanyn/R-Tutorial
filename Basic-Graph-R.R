###### Import Data #####
# Import data from R Documentary
data = iris

# Export data from R
write.csv(iris, file = "D:\\Stat-BahanAjar\\EV Data\\iris ekspor1.csv")
library(writexl)
write_xlsx(iris, "D:\\Stat-BahanAjar\\EV Data\\iris ekspor2.xlsx")

# Import data from PC
data1 = read.csv("D:\\Stat-BahanAjar\\EV Data\\iris ekspor1.csv")
library(readxl)
data2 = read_xlsx("D:\\Stat-BahanAjar\\EV Data\\iris ekspor2.xlsx")


###### Explore data #####
View(data)
nrow(data)
ncol(data)
colnames(data)
str(data)
data$Sepal.Length
typeof(data$Sepal.Length)
mean(data$Sepal.Length)
median(data$Sepal.Length)
# modus
modus = function(x) {
  uniqx = unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}
modus(data$Sepal.Length)
var(data$Sepal.Length)
sd(data$Sepal.Length)
min(data$Sepal.Length)
max(data$Sepal.Length)
range(data$Sepal.Length)
quantile(data$Sepal.Length)
summary(data$Sepal.Length)
summary(data)

# Create tabulation
typeof(data$Species)
table(data$Species)
freq = table(data$Species)
prop.table(freq)

# Explore statistic in groups
aggregate(data$Sepal.Length ~ data$Species, FUN = mean) #explore mean
aggregate(data$Sepal.Width ~ data$Species, FUN = mean) #explore mean
aggregate(data$Petal.Length ~ data$Species, FUN = mean) #explore mean
aggregate(data$Petal.Width ~ data$Species, FUN = mean) #explore mean
# Mean for all variables based on groups
aggregate(. ~ Species, data = iris, FUN = mean)

# What is the median value of sepal length and sepal width for each species?
# Make it in one matrix.

##### Basic Visualization #####
# Barplot
?barplot
barplot(freq)
barplot(freq, col = "#00AFBB")
barplot(freq, col = c("#00AFBB", "#E7B800", "#FC4E07"))

VADeaths
par(mfrow=c(2,2))
barplot(VADeaths[, "Rural Male"])
barplot(VADeaths[, "Rural Male"], horiz=TRUE)
#library(RColorBrewer)
#coul = brewel.pal(3, "Pastel2")

# Stacked Barplot 
barplot(VADeaths, col = c("brown", "chocolate", "coral", "cornsilk"),
        legend = rownames(VADeaths))
barplot(VADeaths, col = c("brown", "chocolate", "coral", "cornsilk"),
        beside = TRUE, legend = rownames(VADeaths), ylim = c(0, 100))
legend("topleft", rownames(VADeaths),
       fill = c("brown", "chocolate", "coral", "cornsilk"))

# 100% Stacked Barplot
vade_percent = apply(VADeaths, 2, function(x){x*100/sum(x, na.rm = T)})
barplot(vade_percent, col = c("brown", "chocolate", "coral", "cornsilk"),
        ylim = c(0,130))
legend("topleft", rownames(VADeaths),
       fill = c("brown", "chocolate", "coral", "cornsilk"), cex = 0.7)

#Boxplot
boxplot(data$Sepal.Length, xlab = "Sepal Length")
boxplot(data$Sepal.Length ~ data$Species, ylab = "Sepal Length",
        xlab = "Species", col = c("#00AFBB", "#E7B800", "#FC4E07"))

# Histogram
hist(data$Sepal.Length,
     main = "Distribution of Sepal Length",
     xlab = "Sepal Length", col = "#00AFBB")

# Density
dens = density(data$Sepal.Length)
plot(dens, main = "Density of Sepal Length")
polygon(dens, col = "#00AFBB")

# Combine histogram and density
hist(data$Sepal.Length,
     main = "Distribution of Sepal Length",
     xlab = "Sepal Length",
     col = "#00AFBB",
     prob = TRUE)
lines(density(data$Sepal.Length), lwd = 2)
boxplot(data$Sepal.Length, main = "Boxplot of Sepal Length", col = "#00AFBB",
        horizontal = TRUE)

# Scatterplot
plot(data$Sepal.Length, data$Sepal.Width,
     xlab = "Sepal Length", ylab = "Sepal Width") #plot in default type
plot(data$Sepal.Length, data$Sepal.Width,
     xlab = "Sepal Length", ylab = "Sepal Width",
     pch = 20) #pch available from 0 until 25
plot(data$Sepal.Length, data$Sepal.Width,
     xlab = "Sepal Length", ylab = "Sepal Width",
     pch = 20, col = "#00AFBB")

# Matrix plot
plot(data)
par(mfrow = c(2,2))
plot(lm(data$Sepal.Length ~ data$Sepal.Width))
pairs(data)
pairs(data, lower.panel = NULL)
pairs(data, lower.panel = NULL, col = 1:3)
pairs(data, lower.panel = NULL, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# panel correlation
correlation = cor(data$Sepal.Length, data$Sepal.Width)
correlation
lower     = function(x, y) {
  usr     = par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r       = round(cor(x, y), digits=2)
  txt     = paste0("R = ", r)
  text(0.5, 0.5, txt)
}
colors_3 = c("red", "blue", "yellow")
upper = function(x, y) {
  points(x,y, col = colors_3[data$Species])
}
pairs(data,
      lower.panel= lower,
      upper.panel= upper)

# Looping for type of plot
?plot
x = c(1:10)
y = x^2
par(mfrow = c(3,3))
type <- c("p","l","b","o","h","s","n")
for (i in type) {
  plot(x, y, type = i,
       xlab = "x", ylab = "y",
       main = paste("type = ", i))
}

# Line chart
set.seed(123)
x = 1:100
y = rnorm(100, mean = 0, sd = 1)
plot(x,y, type = "l")

##### Multiple line chart #####
library(quantmod)
list_company  = c("FREN.JK", "BUMI.JK", "BBKP.JK",
                  "BBRI.JK", "BRMS.JK", "BBCA.JK",
                  "HMSP.JK", "TLKM.JK", "BRPT.JK",
                  "BMRI.JK")
start_date    = "2023-01-01"
end_date      = "2024-04-30"
example       = getSymbols(list_company[1], src = "yahoo", 
                    from = start_date, to = end_date, 
                    auto.assign = FALSE)
time_points   = nrow(as.data.frame(example))
n             = length(list_company)
price_company = matrix(0, time_points, n)
for (i in 1:n) {
  closed = getSymbols(list_company[i], src = "yahoo", 
                      from = start_date, to = end_date, 
                      auto.assign = FALSE)
  price_company[, i] = as.matrix(closed[, 4])
}
colnames(price_company) = list_company
Date_1             = strptime(as.character(rownames(as.data.frame(example))), "%Y-%m-%d")
Date_2             = format(Date_1, "%d/%m/%Y")
Date               = as.data.frame(Date_2)
names(Date)        = "Date"
pc_date            = cbind(Date, as.data.frame(price_company))

summary(pc_date)
Date_asDate = as.Date(pc_date$Date,"%d/%m/%Y")
plot(Date_asDate, pc_date[,2], type = 'l', col = 1,
     xlab = "Date", ylab = "Harga Saham", font.lab = 2, ylim = c(0,11500))
for (i in 3:n+1) {
  lines(Date_asDate, pc_date[,i], type = "l", col = (i-1))
}
legend("topleft", list_company,
       lty = 1, col = 1:n, inset = 0, cex = 0.5)
