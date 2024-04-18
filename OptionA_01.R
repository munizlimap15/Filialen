library(lubridate)

setwd("C:/Users/pedro/Desktop/JO_bonus")

del <- read_excel("Zeitreihen_2Artikel.xlsx")
del$year =  lubridate::year(del$DATUM)
del$month =  lubridate::month(del$DATUM)

ggplot(del) +
  aes(x = DATUM) +
  geom_point(aes(y = Artikel3), shape = 1, size = 1.5, color = "#112446") +
  geom_point(aes(y = Artikel5), shape = 1, size = 1.5, color = "#D55E00") +
  theme_minimal() +
  #facet_wrap(~year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(del) +
  aes(x = DATUM) +
  geom_point(aes(y = Artikel3), shape = 1, size = 1.5, color = "#112446") +
  geom_point(aes(y = Artikel5), shape = 1, size = 1.5, color = "#D55E00") +
  theme_minimal() +
  facet_wrap(~year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
