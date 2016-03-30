source("check-gss-and-maybe-download.R")

GSS <- foreign::read.dta("./data/GSS7214_R4.DTA",
                         convert.factors = FALSE)

table(GSS$nateduc, GSS$natsoc, exclude = NULL)

cor(GSS$nateduc,
    GSS$natsoc,
    use="complete.obs")

table(GSS$race)

table(GSS$black)

GSS$black <- with(GSS, ifelse(race == 2, TRUE, FALSE))
table(GSS$black)

cor(GSS$nateduc[GSS$black == TRUE],
    GSS$natsoc[GSS$black == TRUE],
    use="complete.obs")

suppressPackageStartupMessages(library(dplyr))

with(filter(GSS, black == TRUE),
      cor(nateduc, natsoc,
          use = "complete.obs"))

GSS %>%
  filter(black == TRUE) %>%
  summarize(mycor =
      cor(nateduc, natsoc,
      use = "complete.obs"))

GSS <- GSS %>%
  filter(year != 1972)

table(GSS$sex)
GSS <- GSS %>%
  mutate(sex = factor(sex,
                      levels = c(1,2),
                      labels = c("M","F")))

table(GSS$sex)

thecors <- GSS %>%
  group_by(sex, black) %>%
  summarize(thecor = cor(nateduc, natsoc,
                       use = "complete.obs"),
            n = n())

print(thecors)

gss_yearly <- GSS %>%
  group_by(year) %>%
  summarize(educ = mean(nateduc,
                        na.rm=TRUE),
            soc = mean(natsoc,
                       na.rm=TRUE))

head(gss_yearly)

netsupport <- function(thedata){
  prop_more <- mean(thedata == 1, na.rm = TRUE)
  prop_less <- mean(thedata == 3, na.rm = TRUE)
  prop_more - prop_less
}

GSS %>%
  group_by(year) %>%
  summarize(support_educ=netsupport(nateduc),
            support_soc=netsupport(natsoc))

library(ggplot2)
head(economics)

economics <- economics %>%
  mutate(unemp_rate = unemploy / pop)

economics_yearly <- economics %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(unemp = mean(unemp_rate))

head(economics_yearly)

gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by = "year")

class(gss_yearly$year)

class(economics_yearly$year)

economics_yearly$year <- as.integer(economics_yearly$year)

gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by="year")

head(gss_yearly)

readr::write_csv(gss_yearly, "data/gss-yearly-data.csv")
