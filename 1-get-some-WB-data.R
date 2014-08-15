library(WDI)
library(countrycode)
library(car)
library(lattice)

WBD <- WDI(country="all", indicator=c("NY.GNP.PCAP.CD","EG.USE.PCAP.KG.OE","SL.UEM.TOTL.ZS","SH.DYN.MORT"), start=1950, end=2013)

WBD <- rename(WBD, c(NY.GNP.PCAP.CD="gnipc"))
WBD <- rename(WBD, c(EG.USE.PCAP.KG.OE="pec"))
WBD <- rename(WBD, c(SL.UEM.TOTL.ZS="unemploymentrate"))
WBD <- rename(WBD, c(SH.DYN.MORT="cmr"))

WBD$ccode <- countrycode(WBD$iso2c, "iso2c", "cown")
WBD$ccode[WBD$iso2c == "RS"] <- 345

WBD <- subset(WBD, !is.na(ccode))

meangnipc <- mean(WBD$gnipc, na.rm = TRUE)
mediangnipc <- median(WBD$gnipc, na.rm = TRUE)
meangnipc
mediangnipc
histogram(WBD$gnipc)

WBD$log.gnipc <- log(WBD$gnipc)
meanloggnipc <- mean(WBD$log.gnipc, na.rm = TRUE)
medianloggnipc <- median(WBD$log.gnipc, na.rm = TRUE)
meanloggnipc
medianloggnipc
histogram(WBD$log.gnipc)

histogram(WBD$pec)
histogram(WBD$unemploymentrate)

WBD$log.pec <- log(WBD$pec)
WBD$log.ur <- log(WBD$unemploymentrate)
histogram(WBD$log.pec)
histogram(WBD$log.ur)


## Subset the data to just Turkmenistan (ccode == 701), Tajikistan (702), Kyrgyzstan (703), Uzbekistan (704), and Kazakhstan (705).
CAWBD <- subset(WBD, ccode >= 701 & ccode <= 705)
meancmr.ca <- mean(CAWBD$cmr, na.rm = TRUE)
mediancmr.ca <- median(CAWBD$cmr, na.rm = TRUE)
meancmr.ca
mediancmr.ca
histogram(CAWBD$cmr)
