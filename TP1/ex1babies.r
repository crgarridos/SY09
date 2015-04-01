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
babies[babies$education == 9, 8] <- NA

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
        notch = T, 
        names = c("FumeuseS", "Non fumeuseS"),
        ylim = c(50, 200)),
		colors = c("red","grey")
title("Poids des nouveaux-nés en oncess")
dev.off()

# Question 2
# SELECT gestation FROM babies WHERE smoke == 'Smoking'
c = babies$gestation[babies$smoke == 'Smoking']
d = babies$gestation[babies$smoke == 'NonSmoking']
summary(c);
summary(d);

# Question 3

e = (babies$education[babies$smoke == 'Smoking'])
f = (babies$education[babies$smoke == 'NonSmoking'])


smokers = matrix(cbind(a,c,e),length(a));
colnames(smokers) <- c("bwt","gestation","education")
summary(smokers);



NonSmoke = summary(babies$education[babies$smoke == 'NonSmoking'])
Smoke = summary(babies$education[babies$smoke == 'Smoking'])
