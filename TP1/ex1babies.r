setwd("/mnt/F8660197660157B0/Dropbox/UVs/SY09/TP1")
babies <- read.table("babies23.txt", header = T)

babies <- babies[c(7, 5, 8, 10, 12, 13, 21, 11)]
names(babies) <- c("bwt", "gestation", "parity", "age", "height", "weight",
                  "smoke", "education")
babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 5] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- AN

babies$smoke <- factor(c("NonSmoking", "Smoking", "NonSmoking", "NonSmoking")[babies$smoke + 1])
babies$education <- factor(babies$education, ordered = T)

# Question 1
# SELECT bwt FROM babies WHERE smoke == 'Smoking';
a = babies$bwt[babies$smoke == 'Smoking']
b = babies$bwt[babies$smoke == 'NonSmoking']
summary(a);
summary(b);
png("bwt.png", width = 400, height = 400)
boxplot(babies[babies$smoke == "Smoking", ]$bwt,
        babies[babies$smoke == "NonSmoking", ]$bwt,
#         notch = T, 
        names = c("Fumeuses", "Non fumeuses"),
        ylim = c(50, 200),
		    col = c("gray","aliceblue"));
title("Poids des nouveaux-nés en oncess")
dev.off()
a <- as.numeric(as.character(a))
b <- as.numeric(as.character(b))
mean(a,na.rm=T)-mean(b,na.rm=T)
summary(e)
# Question 2
# SELECT gestation FROM babies WHERE smoke == 'Smoking'
c = babies$gestation[babies$smoke == 'Smoking']
d = babies$gestation[babies$smoke == 'NonSmoking']
boxplot(babies[babies$smoke == "Smoking", ]$gestation,
        babies[babies$smoke == "NonSmoking", ]$gestation,
                notch = T, 
        outline=F,
        names = c("Fumeuses", "Non fumeuses"),
        ylim = c(250, 313),
        col = c("gray","aliceblue"));
title("Jours en gestation")
summary(c);
summary(d);

# Question 3

e = (babies$education[babies$smoke == 'Smoking'])
f = (babies$education[babies$smoke == 'NonSmoking'])
summary(e);
summary(f);

table <- table(babies$smoke, babies$education)
educationCategories = c("<8th grade", "8th<grade<12th", "High School",
                        "HS + trade", "HS + Some college", "College", 
                        "Trade school")
png("barplotEducation.png", width = 650, height = 400)
barplot(table, main = "Repartition des meres fumeuses et non fumeuses
        en fonction de leur niveau d'education", 
#         col = c("aliceblue", "gray"),
        legend = c("Meres non fumeuses", "Meres fumeuses"),
        names.arg = educationCategories,
#         cex.names = 0.8,

        ylim = c(0,275), 
#         beside = T,
        horiz = T)
# ,
#         font.lab = 2)
colnames(table)<-(educationCategories)
prop <- round(table/sum(table)*100,2)
bp <- barplot(table, main = "Niveau d'education des meres",
              horiz=T,
              col = c("aliceblue", "gray"),
              las=1,
              names.arg = educationCategories,
              cex.names = .505,
              cex.axis = .8,
              xlab = "Effectif de meres",
              xlim=c(0,max(apply(table,2,sum))+250),
              border="gray");
legend("topright",c("Meres non fumeuses", "Meres fumeuses"),,pch=rep(19,2),col = c("aliceblue", "gray"),bty="y")
labs <- paste(paste(as.character(prop[1,]),"%",sep=""),
              paste(as.character(prop[2,]),"%",sep=""),sep=" / ")
text(apply(table,2,sum),bp,labs,pos=4,cex=.8)

