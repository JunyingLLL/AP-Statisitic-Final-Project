attach(Part_1_Basics_Stats)

sd(Regression)
sd(`Confidence Interval`)
sd(`Hypothesis Testing`)
sd(Slopes)
sd(`Chi-Square`)
sd(`Analyse Data`)
sd(Correlation)
sd(Probability)

TotalScore <- c(Regression + `Confidence Interval`+`Hypothesis Testing`+Slopes+`Chi-Square`+`Analyse Data`+Correlation+Probability)
TotalScore
library(ggplot2)
data <- data.frame(x=`STUDENT NAME`,y=TotalScore)
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("Student Name") +
  ylab("Total Score") +
  ggtitle("Lollipop Chart for Students' Total Score")

myScore <- c(50, 42, 70, 27, 47, 73, 90, 51)
library(fmsb)
max_min <- data.frame("Regression"=c(100, 0), "Confidence Interval"=c(100,0), "Hypothesis Test"=c(100,0), "Slopes"=c(100, 0), "Chi-Square"=c(100, 0), "Analyse Data"=c(100, 0), "Correlation"=c(100,0), "Probability"=c(100,0))
dat.A <- data.frame("Regression"=50, "Confidence Interval"=42, "Hypothesis Test"=70, "Slopes"=27, "Chi-Square"=47, "Analyse Data"=73, "Correlation"=90, "Probability"=51)
dat.A2 <- rbind(max_min, dat.A)
radarchart(dat.A2, axistype=1 , 
  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
  vlcex=0.6, title="Radarchart for My Performance")


boxplot(Part_1_Basics_Stats, main="Boxplot for Overall Distrubition of Each Test", col=c('red','orange','yellow', 'green','skyblue','blue','pink','white'))



x=`Country ID`
y1=`Ice Cream Consumption`
y2=`Shark Attack Frequency`
y3=`Murder Rate`
y4=`Average Life Expectancy`
y5=`Number of Hamburger Related Deaths`
plot(x,y1, xlab="Countries‘ ID", ylab ="Data", main="Correlation of All Events",pch =19, col="red", ylim = c(0, 1000))
points(x,y2, pch =15, col="orange")
points(x,y3, pch =17, col="green")
points(x,y4, pch =18, col="purple")
points(x,y5, pch =23, col="blue")
legend("topright", c("Ice Cream Consumption", "Shark Attack Frequency", "Murder Rate", "Average Life Expectancy", "Number of Hamburger Related Deaths"),pch = c(19, 15, 17, 18, 23), bg ="white")

lm(`Ice Cream Consumption`~`Shark Attack Frequency`)
plot(`Ice Cream Consumption`~`Shark Attack Frequency`, main="Correlation Between Ice Cream Consumption and Shark Attack Frequency", pch=19, col="Lightgreen")
lm(formula = `Ice Cream Consumption` ~ `Shark Attack Frequency`)
cor.test(`Shark Attack Frequency`, `Ice Cream Consumption`)
plot(`Number of Hamburger Related Deaths`~`Ice Cream Consumption`, main="Correlation Between Number of Hamburger Related Deaths and Ice Cream Cunsuption", pch=15, col="darkgreen")
lm(`Number of Hamburger Related Deaths`~`Ice Cream Consumption`)
cor.test(`Ice Cream Consumption`,`Number of Hamburger Related Deaths`)
