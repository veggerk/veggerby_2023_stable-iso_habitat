# Function for processing data from NACHO (EA-IRMS)
# Jessica Diallo, jodiallo@uw.edu

library(tidyverse)
library(ggpubr)
library(gridExtra)


# Function ------------------------------------
# This function uses GA1 and GA2 areas for the scaling correction for C, 
# and "Cap Del" method for scaling correction for N
# also checks whether run drift regression slope confidence interval contains 0 
# and only corrects for run drift if the 3 LM slope values don't contain 0 and 
# are the same sign.
NACHO.data.process <- function(RawEAfile, sequenceID,
                               plot = "YES", # do you want the diagnostic plots to be displayed?
                               save.file = "NO", # do you want the data file to be saved at the end?
                               seq.eval = "NO") # do you want the sequence evaluation data to be saved to the sequence evaluation file?
{
# read in data
seq <- read.csv(file = RawEAfile, header = TRUE) 
orig.col.names <- colnames(seq)
colnames(seq) <- c("Row", orig.col.names[2:49])

# Subsetting data -----------------------------
# this section adapted from Beka Stiling's code
# It's important that the sample ID's follow the convention for standards, including
# "Std", "QTY", "GA", "SAL", "Dummy", etc
keep.all <- c("Row", "Analysis", "Identifier.1", "Comment")
keep.N <- c("Area.28", "d.15N.14N")
keep.C <- c("Area.44", "d.13C.12C")
# sample N2
sampN2 <-
  seq[which(seq$Gasconfiguration == "N2" &
              seq$Is.Ref._ == 0), c("Analysis", keep.N)] 
# sample CO2
sampCO2 <-
  seq[which(seq$Gasconfiguration == "CO2" &
              seq$Is.Ref._ == 0), c(keep.all, keep.C)]
# combine CO2 and N2
sampCN <-
  merge(x = sampCO2, sampN2, by = "Analysis")

# subset samples
samples <- sampCN[(-grep("GA", sampCN$Identifier.1)),]
samples <- samples[(-grep("SAL", samples$Identifier.1)),]
samples <- samples[(-grep("Dummy", samples$Identifier.1)),]

# subset standards
stdCN <- rbind(sampCN[(grep("GA1", sampCN$Identifier.1)),],
               sampCN[(grep("GA2", sampCN$Identifier.1)),],
               sampCN[(grep("SAL", sampCN$Identifier.1)),])
stdCN$group <- substr(stdCN$Identifier.1, 1, 3)
stdCN$type <- substr(stdCN$Identifier.1, 5, 7)
stdCN$no <- substr(stdCN$Identifier.1, 9,10)
# calculating amount of C and N in ALL standards
Amt.N <- rep(NA, times = nrow(stdCN))
Amt.C <- rep(NA, times = nrow(stdCN))
N.mg <- rep(NA, times = nrow(stdCN))
C.mg <- rep(NA, times = nrow(stdCN))
stdCN <- cbind(stdCN, Amt.N, Amt.C)
stdCN[stdCN$group == "GA1",]$Amt.N <- 0.0952
stdCN[stdCN$group == "GA1",]$Amt.C <- 0.408168
stdCN[stdCN$group == "GA2",]$Amt.N <- 0.0952
stdCN[stdCN$group == "GA2",]$Amt.C <- 0.408168
stdCN[stdCN$group == "SAL",]$Amt.N <- 0.1183
stdCN[stdCN$group == "SAL",]$Amt.C <- 0.457
stdCN$N.mg <- stdCN$Amt.N * stdCN$Comment
stdCN$C.mg <- stdCN$Amt.C * stdCN$Comment
         
# all the standard subsets
GA1.data <- stdCN[stdCN$group == "GA1",]
GA1.Std.data <- GA1.data[GA1.data$type == "Std",]
GA1.QTY.data <- GA1.data[GA1.data$type == "QTY",]
GA2.data <- stdCN[stdCN$group == "GA2",]
GA2.Std.data <- GA2.data[GA2.data$type == "Std",]
GA2.QTY.data <- GA2.data[GA2.data$type == "QTY",]
SAL.data <- stdCN[stdCN$group == "SAL",]

# PLOT 1 -----------------------------
# Area Response vs. Measured Element Mass for ALL standards
if (plot == "YES") {
p1 <- ggplot(stdCN, aes(x = N.mg, y = Area.28, group = group, color = group)) + 
  geom_point() + 
  labs(x = "mass (mg)", title = "Area Response vs. Input N") +
  geom_smooth(method=lm, formula = y ~ x, se = FALSE) + 
  theme(legend.position = "none") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y.npc = "center") +
  stat_regline_equation()
p2 <- ggplot(stdCN, aes(x = C.mg, y = Area.44, group = group, color = group)) + 
  geom_point() + 
  labs(x = "mass (mg)", title = "Area Response vs. Input C") +
  geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y.npc = "center") +
  stat_regline_equation()
grid.arrange(p1, p2, ncol = 2, top = sequenceID)
}

# Check for Drift -----------------------------
# Calculate the confidence interval of the slope. 
# If all 3 slope CIs exclude 0 and the slopes share the same sign, 
# then the samples will be corrected for drift. 
# This is done separately for C and N.

# for N
# LM for delta vs. row number for STD standards (drift)
lm.drift.GA1.N <- lm(data = GA1.Std.data, formula = d.15N.14N ~ Row)
lm.drift.GA2.N <- lm(data = GA2.Std.data, formula = d.15N.14N ~ Row)
lm.drift.SAL.N <- lm(data = SAL.data, formula = d.15N.14N ~ Row)
# Does the confidence interval for all of the LM slope values exclude zero?
if (confint(lm.drift.GA1.N, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.GA1.N, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.N.GA1 <- "ZERO"
} else {
  run.drift.conf.N.GA1 <- "NONZERO"
}
if (confint(lm.drift.GA2.N, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.GA2.N, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.N.GA2 <- "ZERO" 
} else {
  run.drift.conf.N.GA2 <- "NONZERO"
}
if (confint(lm.drift.SAL.N, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.SAL.N, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.N.SAL <- "ZERO"
} else {
  run.drift.conf.N.SAL <- "NONZERO"
}
if(run.drift.conf.N.GA1 == "NONZERO" 
   & run.drift.conf.N.GA2 == "NONZERO" 
   & run.drift.conf.N.SAL == "NONZERO") {
  run.drift.conf.N <- "NONZERO"
} else {
  run.drift.conf.N <- "ZERO"
}
# Are the LM slope values all the same sign?
if(sign(lm.drift.GA1.N$coefficients[2]) == sign(lm.drift.GA2.N$coefficients[2])
   & sign(lm.drift.GA1.N$coefficients[2]) == sign(lm.drift.SAL.N$coefficients[2])
   & sign(lm.drift.GA2.N$coefficients[2]) == sign(lm.drift.SAL.N$coefficients[2])) {
  run.drift.sign.N <- "SAME"
} else {
  run.drift.sign.N <- "DIFF"
}
# Combining this information
if(run.drift.conf.N == "NONZERO" & run.drift.sign.N == "SAME" ){
  run.drift.N <- "YES"
} else {
  run.drift.N <- "NO"
}

# for C
# LM for delta vs. row number for STD standards (drift)
lm.drift.GA1.C <- lm(data = GA1.Std.data, formula = d.13C.12C ~ Row)
lm.drift.GA2.C <- lm(data = GA2.Std.data, formula = d.13C.12C ~ Row)
lm.drift.SAL.C <- lm(data = SAL.data, formula = d.13C.12C ~ Row)
# Does the confidence interval for all of the LM slope values exclude zero?
if (confint(lm.drift.GA1.C, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.GA1.C, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.C.GA1 <- "ZERO"
} else {
  run.drift.conf.C.GA1 <- "NONZERO"
}
if (confint(lm.drift.GA2.C, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.GA2.C, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.C.GA2 <- "ZERO" 
} else {
  run.drift.conf.C.GA2 <- "NONZERO"
}
if (confint(lm.drift.SAL.C, "Row", level = 0.95)[1] < 0 
    & confint(lm.drift.SAL.C, "Row", level = 0.95)[2] > 0) {
  run.drift.conf.C.SAL <- "ZERO"
} else {
  run.drift.conf.C.SAL <- "NONZERO"
}
if(run.drift.conf.C.GA1 == "NONZERO" 
   & run.drift.conf.C.GA2 == "NONZERO" 
   & run.drift.conf.C.SAL == "NONZERO") {
  run.drift.conf.C <- "NONZERO"
} else {
  run.drift.conf.C <- "ZERO"
}
# Are the LM slope values all the same sign?
if(sign(lm.drift.GA1.C$coefficients[2]) == sign(lm.drift.GA2.C$coefficients[2])
   & sign(lm.drift.GA1.C$coefficients[2]) == sign(lm.drift.SAL.C$coefficients[2])
   & sign(lm.drift.GA2.C$coefficients[2]) == sign(lm.drift.SAL.C$coefficients[2])) {
  run.drift.sign.C <- "SAME"
} else {
  run.drift.sign.C <- "DIFF"
}
# Combining this information
if(run.drift.conf.C == "NONZERO" & run.drift.sign.C == "SAME" ){
  run.drift.C <- "YES"
} else {
  run.drift.C <- "NO"
}

print("Will data be corrected for run drift (N, C)?")
print(matrix(c(run.drift.N, run.drift.C), nrow = 1))

## PLOT 2 -----------------------------
# Drift of STD standards
if (plot == "YES") {
p3 <- ggplot(stdCN[stdCN$type == "Std",], aes(x = Row, y = d.15N.14N, group = group, color = group)) + 
  geom_point() + 
  labs(y = "d15N", title = "N15 Drift") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 30, label.y.npc = "center") +
  stat_regline_equation(label.y.npc = "center") +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE)+ 
  theme(legend.position = "none")
p4 <- ggplot(stdCN[stdCN$type == "Std",], aes(x = Row, y = d.13C.12C, group = group, color = group)) + 
  geom_point() + 
  labs(y = "d13C", title = "C13 Drift") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 30, label.y = c(-5, -5.5, -6)) +
  stat_regline_equation(label.y = c(-5, -5.5, -6)) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) 
grid.arrange(p3, p4, ncol = 2, top = sequenceID)
}

## CORRECTION for drift -----------------------------
# N
if (run.drift.N == "YES") {
  drift.slope.N <- mean(c(lm.drift.GA1.N$coefficients[2],
                          lm.drift.GA2.N$coefficients[2],
                          lm.drift.SAL.N$coefficients[2]))
  # sample correction
  d.15N.14N.drift <- samples$d.15N.14N - drift.slope.N * samples$Row
  samples <- cbind(samples, d.15N.14N.drift)
  # standard correction
  d.15N.14N.drift <- stdCN$d.15N.14N - drift.slope.N * stdCN$Row
  stdCN <- cbind(stdCN, d.15N.14N.drift)
}

# C
if (run.drift.C == "YES") {
  drift.slope.C <- mean(c(lm.drift.GA1.C$coefficients[2],
                          lm.drift.GA2.C$coefficients[2],
                          lm.drift.SAL.C$coefficients[2]))
  # sample correction
  d.13C.12C.drift <- samples$d.13C.12C - drift.slope.C * samples$Row
  samples <- cbind(samples, d.13C.12C.drift)
  # standard correction
  d.13C.12C.drift <- stdCN$d.13C.12C - drift.slope.C * stdCN$Row
  stdCN <- cbind(stdCN, d.13C.12C.drift)
}

# PLOT 3 -----------------------------
# Delta vs. Area of QTY standards 
# including all points in plot, but only QTY points in calculations for GA1 and GA2
# I haven't used this plot very much in evaluating the sequence
if (plot == "YES") {
if (run.drift.N == "YES") {
p5 <- ggplot(stdCN, aes(x = Area.28, y = d.15N.14N.drift, group = group, color = group)) +
  geom_point() + 
  labs(y = "d15N", title = "d15N vs Area Response (drift corrected)") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y.npc = "center") +
  stat_regline_equation(label.y.npc = "top") +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  theme(legend.position = "none")
} else if (run.drift.N == "NO") {
 p5 <-  ggplot(stdCN, aes(x = Area.28, y = d.15N.14N, group = group, color = group)) +
    geom_point() + 
    labs(y = "d15N", title = "d15N vs Area Response") +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             label.y.npc = "center") +
    stat_regline_equation(label.y.npc = "top") +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    theme(legend.position = "none")
}
if (run.drift.C == "YES") {
 p6 <-  ggplot(stdCN, aes(x = Area.44, y = d.13C.12C.drift, group = group, color = group)) +
    geom_point() + 
    labs(y = "d13C", title = "d13C vs Area Response (drift corrected)") +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             label.y.npc = "center") +
    stat_regline_equation(label.y.npc = "top") +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE)
} else if (run.drift.C == "NO") {
 p6 <-  ggplot(stdCN, aes(x = Area.44, y = d.13C.12C, group = group, color = group)) +
    geom_point() + 
    labs(y = "d13C", title = "d13C vs Area Response") +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             label.y.npc = "center") +
    stat_regline_equation(label.y.npc = "top") +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE)
}
  grid.arrange(p5, p6, ncol = 2, top = sequenceID)
  
}

# Rename delta cols -------------------------------
# moving forward, I don't want to have to reference drift or non-drift 
# corrected values, so I'm going to label both as d.13C.12C.1 & d.15N.14N.1
# N
if (run.drift.N == "YES") {
  d.15N.14N.1 <- samples$d.15N.14N.drift
  samples <- cbind(samples, d.15N.14N.1)
  d.15N.14N.1 <- stdCN$d.15N.14N.drift
  stdCN <- cbind(stdCN, d.15N.14N.1)
} else if (run.drift.N == "NO") {
  d.15N.14N.1 <- samples$d.15N.14N
  samples <- cbind(samples, d.15N.14N.1)
  d.15N.14N.1 <- stdCN$d.15N.14N
  stdCN <- cbind(stdCN, d.15N.14N.1)
}
# C
if (run.drift.C == "YES") {
  d.13C.12C.1 <- samples$d.13C.12C.drift
  samples <- cbind(samples, d.13C.12C.1)
  d.13C.12C.1 <- stdCN$d.13C.12C.drift
  stdCN <- cbind(stdCN, d.13C.12C.1)
} else if (run.drift.C == "NO") {
  d.13C.12C.1 <- samples$d.13C.12C
  samples <- cbind(samples, d.13C.12C.1)
  d.13C.12C.1 <- stdCN$d.13C.12C
  stdCN <- cbind(stdCN, d.13C.12C.1)
}

# CORRECTION for scale ---------------------------
# N
# verify that mass (area) does not have impact on d15N
# using GA1 and GA2 here b/c SAL does not have QTY points far apart and 
# therefore the slope could be different from 0 without actually meaning anything.
lm.area.GA1.N <- lm(data = stdCN[stdCN$group == "GA1",], 
                    formula = d.15N.14N.1 ~ Area.28)
lm.area.GA2.N <- lm(data = stdCN[stdCN$group == "GA2",], 
                    formula = d.15N.14N.1 ~ Area.28)
if (confint(lm.area.GA1.N, "Area.28", level = 0.95)[1] < 0 
    & confint(lm.area.GA1.N, "Area.28", level = 0.95)[2] > 0
    & confint(lm.area.GA2.N, "Area.28", level = 0.95)[1] < 0 
    & confint(lm.area.GA2.N, "Area.28", level = 0.95)[2] > 0) {
  delta.area.N <- "NO"
} else {
  delta.area.N <- "YES"
}
print("Does area response impact d15N?")
print(delta.area.N)
# measured vs. known for standards
known.N <- rep(NA, times = nrow(stdCN))
stdCN <- cbind(stdCN, known.N)
stdCN[stdCN$group == "GA1",]$known.N <- -4.6
stdCN[stdCN$group == "GA2",]$known.N <- -5.7
stdCN[stdCN$group == "SAL",]$known.N <- 11.3
stdCN$meas.known.N <- stdCN$d.15N.14N.1-stdCN$known.N
# measured  - known difference vs. area response
if (plot == "YES") {
p7 <- ggplot(stdCN[stdCN$type == "Std",], aes(x = Area.28, y = meas.known.N, group = group, color = group)) + 
  geom_point() + 
  labs(y = "meas-known d15N", title = "N15 Residuals vs. Area Response")
grid.arrange(p7, ncol = 1, top = sequenceID)
}
# using GA2 and SAL
# find average difference from measured - known d15N value
diff.N <- mean(rbind(stdCN[stdCN$group == "GA2",],
                     stdCN[stdCN$group == "SAL",])$meas.known.N)
# now diff.N is subtracted from all d15N sample values
samples$d.15N.14N.2 <- samples$d.15N.14N.1 - diff.N
# N precision (1 sigma precision) is the standard deviation of the GA2 and SAL meas - known
prec.N <- abs(sd(rbind(stdCN[stdCN$group == "GA2",],
                     stdCN[stdCN$group == "SAL",])$meas.known.N))
print("N scale correction")
print(c("Average measured - known for GA2 & SAL =", round(diff.N, digits = 3)))
print(c("N precision =", round(prec.N, digits = 3)))

# calculate final GA1 d15N values using this method
# N accuracy is the difference of the average corrected d15N - known d15N for GA1
# final measured - known for GA1 based on GA2 and SAL difference
accur.N <- abs(mean(stdCN[stdCN$group == "GA1",]$d.15N.14N.1 - diff.N) - -4.6)
print(c("N accuracy =", round(accur.N, digits = 3)))
if (2*prec.N > accur.N) {
  good.accur <- "YES"
} else {
  good.accur <- "NO" 
}
print("Is N accuracy for the run less than 2 x precision?")
print(good.accur)
# correction for samples
samples$d.15N.14N.2 <- samples$d.15N.14N.1 - diff.N

# C
# LM for delta vs. Area 
# for only QTY points of GA1 and GA2 and Std points of SAL
lm.area.GA1.C <- lm(data = stdCN[stdCN$group == "GA1" & stdCN$type == "QTY",], 
                    formula = d.13C.12C.1 ~ Area.44)
lm.area.GA2.C <- lm(data = stdCN[stdCN$group == "GA2" & stdCN$type == "QTY",], 
                    formula = d.13C.12C.1 ~ Area.44)
# add GA1 and GA2 columns to samples data frame
GA1.C.val <- samples$Area.44*lm.area.GA1.C$coefficients[2] + lm.area.GA1.C$coefficients[1]
GA2.C.val <- samples$Area.44*lm.area.GA2.C$coefficients[2] + lm.area.GA2.C$coefficients[1]
samples <- cbind(samples, GA1.C.val, GA2.C.val)
# correction 
scale.int.C <- rep(NA, times = nrow(samples))
scale.slope.C <- rep(NA, times = nrow(samples))
for(i in 1:nrow(samples)){
  Std.measured <- c(samples$GA1.C.val[i], samples$GA2.C.val[i])
  Std.known <- c(-28.3, -13.7)
  df <- as.data.frame(cbind(Std.measured, Std.known))
  scale.int.C[i] <- lm(data = df, formula = Std.measured ~ Std.known)$coefficients[1]
  scale.slope.C[i] <- lm(data = df, formula = Std.measured ~ Std.known)$coefficients[2]
}
# add to samples 
samples$d.13C.12C.2 <- (samples$d.13C.12C.1 - scale.int.C)/scale.slope.C
# verify that SAL delta values computed by GA1 and GA2 are close to true values of C
# SAL d13C = -21.3
SAL.results <- stdCN[stdCN$group == "SAL",]
# add GA1 and GA2 columns to SAL.results data frame
GA1.C.val.4SAL <- SAL.results$Area.44*lm.area.GA1.C$coefficients[2] + lm.area.GA1.C$coefficients[1]
GA2.C.val.4SAL <- SAL.results$Area.44*lm.area.GA2.C$coefficients[2] + lm.area.GA2.C$coefficients[1]
SAL.results <- cbind(SAL.results, GA1.C.val.4SAL, GA2.C.val.4SAL)
# SAL final scale correction for C
scale.int.C.SAL <- rep(NA, times = nrow(SAL.results))
scale.slope.C.SAL <- rep(NA, times = nrow(SAL.results))
for(i in 1:nrow(SAL.results)) {
  Std.measured <- c(SAL.results$GA1.C.val.4SAL[i], SAL.results$GA2.C.val.4SAL[i])
  Std.known <- c(-28.3, -13.7)
  df <- as.data.frame(cbind(Std.measured, Std.known))
  scale.int.C.SAL[i] <- lm(data = df, formula = Std.measured ~ Std.known)$coefficients[1]
  scale.slope.C.SAL[i] <- lm(data = df, formula = Std.measured ~ Std.known)$coefficients[2]
}
d13C.scale.corr.SAL <- (SAL.results$d.13C.12C.1 - scale.int.C.SAL)/scale.slope.C.SAL
# combine with SAL results data frame
SAL.results <- cbind(SAL.results, d13C.scale.corr.SAL)
# precision is variability of drift corrected d13C vs. area response residuals
prec.C <- sd(c(resid(lm.area.GA1.C), resid(lm.area.GA2.C)))
# accuracy is the average difference between the scale corrected SAL and the known SAL d13C
accur.C <- abs(mean(SAL.results$d13C.scale.corr.SAL+21.3))
print("C scale correction")
print(c("C precision =", round(prec.C, digits = 3)))
print(c("C accuracy =", round(accur.C, digits = 3)))

# Mass of N & C -------------------------------------
# combining data for LM 
lm.mass.N <- lm(data = stdCN, formula = Area.28 ~ N.mg)
lm.mass.C <- lm(data = stdCN, formula = Area.44 ~ C.mg)

# compute mass of C and N in samples
samples$Amt.N <- (samples$Area.28 - lm.mass.N$coefficients[1])/lm.mass.N$coefficients[2]
samples$percent.N <- samples$Amt.N/samples$Comment
samples$Amt.C <- (samples$Area.44 - lm.mass.C$coefficients[1])/lm.mass.C$coefficients[2]
samples$percent.C <- samples$Amt.C/samples$Comment

# simplify data
simple.samples <- samples %>%
  select(Identifier.1, Comment, d.15N.14N.2, d.13C.12C.2, Amt.N, percent.N, Amt.C, percent.C)
colnames(simple.samples) <- c("sampleID", "sample.mass", "d15N", "d13C", "Amt.N", "per.N", "Amt.C", "per.C")
row.names(simple.samples) <- NULL

if (save.file == "YES") {
  write.csv(x = simple.samples, file = paste0("processed_data/", sequenceID, ".csv"))
}

# Evaluation of Run ----------------------------------
# PRECISION
# N
prec.N
# C
prec.C
# ACCURACY
# N
accur.N
# C
accur.C
# REPRODUCIBILITY --------------------
# N
mean.GA1.N <- mean(stdCN[stdCN$group == "GA1",]$d.15N.14N.1 - diff.N)
# C
mean.SAL.C <- mean(SAL.results$d13C.scale.corr.SAL)

date <- as.character(as.Date(paste0("20", substr(RawEAfile, 10, 11), "-", substr(RawEAfile, 12, 13), "-", substr(RawEAfile, 14, 15))))
sample.no <- nrow(samples)
seq.eval.row <- cbind(date, sequenceID, sample.no, round(prec.N, digits = 5), round(prec.C, digits = 5), round(accur.N, digits = 5), round(accur.C, digits = 5), round(mean.GA1.N, digits = 5), round(mean.SAL.C, digits = 5))

colnames(seq.eval.row) <- c("date", "sequenceID", "samples", "N.prec", "C.prec", "N.accur", 
                             "C.accur", "GA1.N", "SAL.C")

if (seq.eval == "YES") {
  old.seq.eval <- read.csv(file = "Sequence_Evaluation.csv", header = TRUE)
  new.seq.eval <- rbind(old.seq.eval, seq.eval.row)
  write_csv(x = new.seq.eval, file = "Sequence_Evaluation.csv", append = FALSE)
}

} # end of function

