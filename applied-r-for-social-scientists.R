## ------------------------------------------------------------------------
source("check-gss-and-maybe-download.R")

## ------------------------------------------------------------------------
GSS <- foreign::read.dta("./data/GSS7214_R4.DTA",
                         convert.factors = FALSE)

## ------------------------------------------------------------------------
table(GSS$nateduc, GSS$natsoc, exclude = NULL)

## ------------------------------------------------------------------------
cor(GSS$nateduc,
    GSS$natsoc,
    use = "complete.obs")

## ------------------------------------------------------------------------
table(GSS$race)

## ------------------------------------------------------------------------
table(GSS$black)

## ------------------------------------------------------------------------
GSS$black <- ifelse(GSS$race == 2, TRUE, FALSE)
table(GSS$black)

## ------------------------------------------------------------------------
cor(GSS$nateduc[GSS$black == TRUE],
    GSS$natsoc[GSS$black == TRUE],
    use="complete.obs")

## ------------------------------------------------------------------------
library(tidyr)
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
with(filter(GSS, black == TRUE),
     cor(nateduc, natsoc,
         use = "complete.obs"))

## ------------------------------------------------------------------------
GSS %>%
  filter(black == TRUE) %>%
  summarize(mycor =
              cor(nateduc, natsoc,
                  use = "complete.obs"))

## ------------------------------------------------------------------------
GSS <- GSS %>%
  filter(year != 1972)

## ------------------------------------------------------------------------
table(GSS$sex)

## ------------------------------------------------------------------------
GSS <- GSS %>%
  mutate(sex = factor(sex,
                      levels = c(1, 2),
                      labels = c("M", "F")))

table(GSS$sex)

## ------------------------------------------------------------------------
thecors <- GSS %>%
  group_by(sex, black) %>%
  summarize(thecor = cor(nateduc, natsoc,
                         use = "complete.obs"),
            n = n())

## ------------------------------------------------------------------------
print(thecors)

## ------------------------------------------------------------------------
gss_yearly <- GSS %>%
  group_by(year) %>%
  summarize(educ = mean(nateduc,
                        na.rm = TRUE),
            soc = mean(natsoc,
                       na.rm = TRUE))

## ------------------------------------------------------------------------
head(gss_yearly)

## ------------------------------------------------------------------------
netsupport <- function(thedata){
  prop_more <- mean(thedata == 1, na.rm = TRUE)
  prop_less <- mean(thedata == 3, na.rm = TRUE)
  prop_more - prop_less
}

## ------------------------------------------------------------------------
GSS %>%
  group_by(year) %>%
  summarize(support_educ = netsupport(nateduc),
            support_soc = netsupport(natsoc))

## ------------------------------------------------------------------------
library(ggplot2)
head(economics, 3)

## ------------------------------------------------------------------------
economics <- economics %>%
  mutate(unemp_rate = unemploy / pop)

## ------------------------------------------------------------------------
economics$unemp_rate <- economics$unemploy / economics$pop

## ------------------------------------------------------------------------
economics_yearly <- economics %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(unemp = mean(unemp_rate))

## ------------------------------------------------------------------------
head(economics_yearly)

## ---- error = TRUE-------------------------------------------------------
gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by = "year")

## ------------------------------------------------------------------------
class(gss_yearly$year)

## ------------------------------------------------------------------------
class(economics_yearly$year)

## ------------------------------------------------------------------------
economics_yearly$year <- as.integer(economics_yearly$year)

gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by="year")

## ------------------------------------------------------------------------
head(gss_yearly)

## ------------------------------------------------------------------------
readr::write_csv(gss_yearly, "data/gss-yearly-data.csv")

## ------------------------------------------------------------------------
messy1 <- data_frame(
  country = c("Afghanistan", "Albania", "Algeria"), 
  "2007" = c(43.82, 76.42, 72.30), 
  "2002" = c(42.13, 75.65, 70.99))

## ------------------------------------------------------------------------
print(messy1)

## ------------------------------------------------------------------------
gather(messy1, "year", "life_expect", 2:3)

## ------------------------------------------------------------------------
messy2 <- data.frame(
  country = c(rep("Afghanistan", 4), rep("Albania", 4), rep("Algeria", 4)), 
  year = c(rep(2002, 2), rep(2007, 2)), 
  variable = c("life_expect", "pop"), 
  value = c(42.12, 25268405, 43.82, 31889923,
            75.65, 3508512, 76.42, 3600523,
            70.99, 31287142, 72.30, 33333216)
)

## ------------------------------------------------------------------------
head(messy2)

## ------------------------------------------------------------------------
spread(messy2, key = variable, value)

## ------------------------------------------------------------------------
thedata <- data_frame(
  one = rnorm(100), two = rnorm(100), 
  three = rnorm(100), four = rnorm(100)
)

## ------------------------------------------------------------------------
output <- list()
output[[1]] <- median(thedata$one)
output[[2]] <- median(thedata$two)
output[[3]] <- median(thedata$three)
output[[4]] <- median(thedata$four)
print(output); rm(output)

## ------------------------------------------------------------------------
output <- list()
for (i in 1:4) {
  output[[i]] <- median(thedata[[i]])
}
print(output); rm(output)

## ------------------------------------------------------------------------
purrr::map(thedata, median)
purrr::map_dbl(thedata, median)

## ------------------------------------------------------------------------
library(gapminder); library(ggplot2)
head(gapminder, 3)

## ---- eval = FALSE-------------------------------------------------------
## ggplot(gapminder, aes(x = year, y = lifeExp,
##                       color = continent, by = country)) +
##   geom_line()

## ---- echo = FALSE-------------------------------------------------------
ggplot(gapminder, aes(x = year, y = lifeExp,
                      color = continent, by = country)) +
  geom_line()

## ------------------------------------------------------------------------
by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

## ------------------------------------------------------------------------
head(by_country,3)

## ------------------------------------------------------------------------
by_country$data[[1]]

## ------------------------------------------------------------------------
by_country <- by_country %>%
  mutate(model = purrr::map(data, 
                            ~ lm(lifeExp ~ year, data = .)))

## ------------------------------------------------------------------------
head(by_country, 3)

## ---- eval = FALSE-------------------------------------------------------
## by_country %>% unnest(model %>% purrr::map(broom::augment)) %>%
##   select(continent, country, year, .fitted) %>%
##   ggplot(aes(x = year, y = .fitted,
##              by = country, color = continent)) +
##   geom_line()

## ---- echo = FALSE-------------------------------------------------------
by_country %>% unnest(model %>% purrr::map(broom::augment)) %>%
  select(continent, country, year, .fitted) %>%
  ggplot(aes(x = year, y = .fitted,
             by = country, color = continent)) +
  geom_line()

