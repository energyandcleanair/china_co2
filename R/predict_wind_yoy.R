predict_wind_yoy <- function(){

source('load_package.R')


pwr_data <- read_power_generation()
top <- c("Inner Mongolia", "Gansu", "Xinjiang", "Hebei", "Shandong", "Shanxi")

# Get data
weather <- get_weather_for_wind_prediction(avg_jan_feb = T, use_cache = T) %>%
  spread(variable, value)
gen_cap <- get_wind_solar_gen_cap(use_cache = T)
curtailment <- get_wind_curtailment_rates() %>%
  select(prov=province_en, date, curtailment=value) %>%
  mutate(date = date %>% 'day<-'(lubridate::days_in_month(date)))

alldata <- gen_cap %>%
  inner_join(weather) %>%
  inner_join(curtailment) %>%
  mutate(cf_corrected = Utilization / (1-curtailment) / lubridate::days_in_month(date) / 24) %>%
  # mutate(is_sane=Utilization>30*24*0.03 & Utilization<30*24*0.35) %>%
  mutate(is_sane = cf_corrected > 0.03 & cf_corrected < 0.5) %>%
  filter(is_sane) %>%
  # Standardize wind_power_density
  mutate(
    wind_power_density = wind_power_density / max(wind_power_density),
  )


# Add prediction
alldata_pred <- alldata %>%
  # filter(prov %in% top) %>%
  group_by(prov) %>%
  group_map(function(x, group){
    model <- lm(cf_corrected~wind_power_density + date + log(wind_power_density), data=x)
    x$predicted <- predict(model, x)
    x$residual <- x$cf_corrected - x$predicted
    x$prov <- group$prov
    x
  }) %>%
  bind_rows()


alldata_pred %>%
  ggplot(aes(cf_corrected, predicted)) +
  geom_point(aes(col=factor(year(date)))) +
  geom_abline(slope=1, intercept=0) +
  scale_color_brewer(palette="RdYlBu") +
  facet_wrap(~prov) +
  labs(
    title="Predicted vs observed",
    subtitle="Formula: wind_power_density + date",
  ) +
  theme_dark()


# Investigate
x=alldata_pred %>% filter(prov=="Zhejiang")
x$wind_power_density <- pmin(0.8, x$wind_power_density)
summary(lm(cf_corrected~ log(wind_power_density), data=x))
summary(lm(cf_corrected~ wind_power_density, data=x))

summary(lm(cf_corrected~ date + log(wind_power_density), data=x))
summary(model)
model <- lm(cf_corrected~ date + wind_power_density, data=x)
summary(model)
ggplot(x, aes(date, wind_power_density)) + geom_point()
ggplot(x, aes(date, Utilization)) + geom_point()


ggplot(x) +
  geom_line(aes(date, cf_corrected), col='blue') +
  geom_line(aes(date, predicted), col='red')




# See model results





# Better understand outliers
alldata %>%
  filter(prov=="Xinjiang") %>%
  arrange(cf_corrected) %>%
  select(date, prov, cf_corrected, wind_power_density)





# Some verification
alldata %>%
  filter(source=='Wind', is_sane) %>%
  select(prov, date, Utilization, Utilization_corrected) %>%
  gather(key='var', value='value', -prov, -date) %>%
  ggplot(aes(date, value, col=var)) +
  geom_point() +
  geom_line() +
  facet_wrap(~prov, scales='free_y')


# alldata %>% filter(source=='Solar', is_sane) %>%
#   lm(Utilization~(solar_radiation*temperature):prov, data=.) %>% summary
#
# alldata %>% filter(source=='Solar', is_sane) %>%
#   ggplot(aes(solar_radiation, Utilization)) + geom_point()
#
# # Original
# alldata %>% filter(source=='Wind', is_sane) %>%
#   lm(Utilization~wind_speed:prov+temperature, data=.) %>% summary
#
# alldata %>% filter(source=='Wind', is_sane) %>%
#   ggplot(aes(wind_speed, Utilization)) + geom_point()
#
# # Hubert attempts ---------------------------------------------------------
alldata %>% filter(source=='Wind', is_sane) %>%
  lm(Utilization~wind_speed_cubed_div_temp:prov + prov, data=.) %>% summary

alldata %>% filter(source=='Wind', is_sane) %>%
  lm(Utilization_corrected~wind_power_density:prov + prov, data=.) %>% summary


alldata %>% filter(source=='Wind', is_sane) %>%
  filter(prov %in% top) %>%
  # filter(month(date)!=2) %>%
  ggplot(aes(wind_power_density,
             Utilization_corrected,
  )) +
  geom_point(aes(
    col=factor(year(date))
  ), show.legend = T) +
  geom_smooth(method='lm') +
  facet_wrap(~prov, scales='free') +
  scale_color_brewer(palette="RdYlBu") +
  theme_dark()


alldata %>% filter(source=='Wind', is_sane) %>%
  filter(prov %in% top) %>%
  # filter(year(date)>=2018) %>%
  ggplot(aes(wind_power_density,
             Utilization_corrected,
  )) +
  geom_point(aes(
    col=factor(month(date))
  ), show.legend = T) +
  geom_smooth(method='lm') +
  facet_wrap(~prov, scales='free') +
  scale_color_brewer(palette="RdYlBu") +
  theme_dark()


alldata %>% filter(source=='Wind', is_sane) %>%
  filter(year(date)>=2019) %>%
  # filter(wind_speed > cutin_speed) %>%
  ggplot(aes(wind_power_density,
             Utilization_corrected,
  )) +
  geom_point(aes(
    # col=wind_speed
    col=factor(year(date))
  ), show.legend = T) +
  geom_smooth(method='lm') +
  facet_wrap(~prov, scales='free') +
  # scale_color_distiller(palette="RdYlBu") +
  scale_color_brewer(palette="RdYlBu") +
  # make it dark
  theme_dark()



# Plot comparison
alldata %>% filter(source=='Wind', is_sane)  %>%
  filter(prov %in% top) %>%
  group_by(prov) %>%
  group_map(function(x, group){
    model <- lm(Utilization_corrected~wind_power_density + date, data=x)
    x$predicted <- predict(model, x)
    x$residual <- x$Utilization_corrected - x$predicted
    x$prov <- group$prov
    x
  }) %>%
  bind_rows() %>%
  ggplot(aes(Utilization_corrected, predicted)) +
  geom_point(aes(col=factor(month(date)))) +
  # Force square coords
  # coord_cartesian(xlim=c(1, 10), ylim=c(1, 10)) +
  # geom_smooth(method = "lm") +
  geom_abline(slope=1, intercept=0) +
  scale_color_brewer(palette="RdYlBu") +
  facet_wrap(~prov) +
  labs(
    title="Predicted vs observed",
    subtitle="Formula: Utilization_corrected~wind_speed_cubed_div_temp",
  )


# Time series predicted vs observed
alldata %>% filter(source=='Wind', is_sane)  %>%
  filter(prov %in% "Xinjiang") %>%
  group_by(prov) %>%
  group_map(function(x, group){
    model <- lm(Utilization_corrected~wind_power_density , data=x)
    x$predicted <- predict(model, x)
    x$residual <- x$Utilization_corrected - x$predicted
    x$prov <- group$prov
    x
  }) %>%
  bind_rows() %>%
  select(date, prov, Utilization_corrected, predicted) %>%
  tidyr::gather(key='var', value='value', -date, -prov) %>%
  ggplot(aes(date, value, col=var)) +
  geom_line() +
  geom_point() +
  # Force square coords
  # coord_cartesian(xlim=c(1, 10), ylim=c(1, 10)) +
  # geom_smooth(method = "lm") +
  # scale_color_brewer(palette="RdYlBu") +
  facet_wrap(~prov) +
  labs(
    title="Predicted vs observed",
    subtitle="Formula: Utilization_corrected~wind_power_density + date",
  ) -> plt

library(plotly)

ggplotly(plt)


# See if there is a trend by province within residuals
alldata %>% filter(source=='Wind', is_sane)  %>%
  group_by(prov) %>%
  group_map(function(x, group){
    model <- lm(Utilization_corrected~wind_power_density, data=x)
    x$predicted <- predict(model, x)
    x$residual <- x$Utilization_corrected - x$predicted
    x$prov <- group$prov
    x
  }) %>%
  bind_rows() %>%
  ggplot(aes(date, residual)) +
  geom_point(aes(col=factor(month(date)))) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette="RdYlBu") +
  facet_wrap(~prov, scales='free_y') +
  labs(
    title="Residuals of wind utilization model",
    subtitle="Formula: Utilization_corrected~wind_speed_cubed_div_temp",
  )


alldata %>% filter(source=='Wind', is_sane) %>%
  filter(prov %in% top) %>%
  group_by(prov) %>%
  group_map(function(x, group){
    model <- lm(Utiliszation~wind_speed_cubed_div_temp, data=x)
    x$predicted <- predict(model, x)
    x$residual <- x$Utilization_corrected - x$predicted
    x$prov <- group$prov
    x
  }) %>%
  bind_rows() %>%
  ggplot(aes(date, residual)) +
  geom_point(aes(col=factor(month(date)))) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette="RdYlBu") +
  facet_wrap(~prov, scales='free_y') +
  labs(
    title="Residuals of wind utilization model",
    subtitle="Formula: Utilization_corrected~wind_speed_cubed_div_temp",
  )



# One model per province
alldata %>% filter(source=='Wind', is_sane) %>%
  # filter(year(date)>=2019) %>%
  group_by(prov) %>%
  # do by group
  group_map(function(x, prov){

    x <- x %>% filter(!is.na(wind_speed), !is.na(wind_power_density))
    # Model 1 wind date
    model <- lm(Utilization~wind_speed, data=x)
    R2 <- summary(model)$r.squared
    capacity <- mean(x$Capacity)
    res1 <- tibble(prov=prov$prov, R2=R2, capacity=capacity, formula="Utilization ~ wind_speed")

    # Model 2 wind date
    model <- lm(Utilization_corrected~wind_power_density, data=x)
    R2 <- summary(model)$r.squared
    capacity <- mean(x$Capacity)
    res2 <- tibble(prov=prov$prov, R2=R2, capacity=capacity, formula="Utilization_corrected ~ wind_power_density")


    # Model 2
    # model <- lm(Utilization_corrected~wind_power_density + date, data=x)
    # R2 <- summary(model)$r.squared
    # capacity <- mean(x$Capacity)
    # res3 <- tibble(prov=prov$prov, R2=R2, capacity=capacity, formula="Utilization ~ wind_power_density")

    res <- bind_rows(res1, res2)
    return(res)
  }) %>%
  bind_rows() %>%
  ggplot(aes(capacity, R2, col=formula)) +
  geom_point() +
  # Add a line between points by province
  geom_line(aes(group=prov), col="grey60") +
  ggrepel::geom_text_repel(data=function(x) x %>% group_by(prov) %>% filter(R2==max(R2)),
                           aes(label=prov))






# Let's predict provincial curtailment rate instead from residual
alldata %>%
  filter(source=='Wind', is_sane) %>%
  group_by(prov) %>%
  # do by group
  group_map(function(x, group){
    model <- lm(Utilization_corrected~wind_power_density, data=x)
    x$predicted <- predict(model, x)
    x$curtailment_normalised <- 1 - (x$Utilization_corrected / x$predicted)
    x$prov <- group$prov
    return(x)
  }) %>%
  bind_rows() %>%
  group_by(year=year(date)) %>%
  summarise(curtailment_normalised=weighted.mean(curtailment_normalised, Capacity)) %>%
  ggplot(aes(year, curtailment_normalised)) +
  geom_line() +
  scale_x_continuous(breaks=seq(2015, 2024, 1)) +
  labs(title='"Curtailement rate" derived from model')




# Original ----------------------------------------------------------------

alldata %>%
  arrange(date) %>%
  group_by(source, prov) %>%
  fill(Capacity, .direction='down') %>%
  group_by(source, date) %>%
  summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
  select(-Capacity, -Utilization) ->
  national_data

pwr_data$monthly %>% filter(source %in% c('Wind','Solar'), var=='Utilization') %>%
  ungroup %>% select(date, source, Utilization=Value1m) %>%
  inner_join(national_data) ->
  national_data


national_data %>% filter(source=='Solar') %>%
  lm(Utilization~solar_radiation*temperature+date, data=.) ->
  m_solar

m_solar %>% summary

national_data %>% filter(source=='Wind') %>%
  lm(Utilization~wind_speed_cubed+temperature+date, data=.) -> m_wind

m_wind %>% summary

national_data %>% filter(source=='Wind') %>%
  ggplot(aes(wind_speed_cubed, Utilization)) + geom_point()


national_data %<>% ungroup %>%
  mutate(Utilization_predicted = case_when(source=='Wind'~predict(m_wind, national_data),
                                           source=='Solar'~predict(m_solar, national_data))) %>%
  group_by(month(date), source) %>%
  mutate(Utilization_YoY = Utilization/lag(Utilization)-1,
         Utilization_predicted_YoY = Utilization_predicted/lag(Utilization_predicted)-1)


national_data %>% filter(Utilization_YoY<.35) %>%
  ggplot(aes(Utilization_YoY, Utilization_predicted_YoY)) + geom_point() + facet_wrap(~source) +
  geom_smooth(method='lm') + geom_abline()



# YOY Provincial ------------------------------------------------------------------
predicted_yoy_prov <- alldata %>%
  # filter(year(date) >= 2018) %>%
  filter(source=='Wind', is_sane) %>%
  group_map(
    function(x, group){
      model <- lm(Utilization_corrected ~ wind_power_density:prov + date:prov + prov, data=x)
      summary(model)
      x$predicted <- predict(model, x)
      # x$prov <- group$prov
      x$Capacity <- x$Capacity

      x %>%
        mutate(month = month(date),
               year = year(date)) %>%
        select(date, month, year, prov, Capacity, predicted) %>%
        ungroup()
    }) %>%
  bind_rows() %>%
  group_by(date, prov, year, month) %>%
  summarise(predicted = weighted.mean(predicted, Capacity, na.rm=T)) %>%
  ungroup() %>%
  select(prov, year, month, predicted) %>%
  tidyr::complete(prov, year, month=seq(2,12), fill=list(predicted=NA)) %>%
  group_by(prov, month) %>%
  arrange(year) %>%
  mutate(predicted_yoy = (predicted/lag(predicted)-1)) %>%
  ungroup()


actual_yoy_prov <- alldata %>%
  # filter(year(date) >= 2018) %>%
  filter(source=='Wind', is_sane) %>%
  mutate(month = month(date), year = year(date)) %>%
  select(date, month, year, prov, Capacity, Utilization_corrected) %>%
  group_by(date, prov, year, month) %>%
  summarise(Utilization_corrected = weighted.mean(Utilization_corrected, Capacity, na.rm=T)) %>%
  ungroup() %>%
  select(prov, year, month, Utilization_corrected) %>%
  tidyr::complete(prov, year, month=seq(2,12), fill=list(Utilization_corrected=NA)) %>%
  group_by(prov, month) %>%
  arrange(year) %>%
  mutate(actual_yoy = (Utilization_corrected/lag(Utilization_corrected)-1)) %>%
  ungroup()



actual_yoy_prov %>%
  filter(year >= 2018) %>%
  filter(prov %in% top) %>%
  left_join(predicted_yoy_prov) %>%
  filter(!is.na(actual_yoy),
         !is.na(predicted_yoy)) %>%
  ggplot(aes(actual_yoy, predicted_yoy)) +
  geom_point(
    aes(
      col=factor(month)
    )
  ) +
  geom_smooth(method='lm') +
  geom_abline(slope=1, intercept=0) +
  scale_color_brewer(palette="RdYlBu") +
  theme_dark() +
  facet_wrap(~prov, scales='free_y')


# Explore worst points
actual_yoy_prov %>%
  filter(prov %in% top) %>%
  left_join(predicted_yoy_prov) %>%
  filter(!is.na(actual_yoy),
         !is.na(predicted_yoy)) %>%
  filter(prov=="Xinjiang") %>%
  arrange(desc(actual_yoy))

alldata %>%
  filter(prov=="Shanxi", source=='Wind', is_sane) %>%
  filter(month(date)==12) %>%
  arrange(desc(date))



# YOY National ------------------------------------------------------------------
predicted_yoy <- alldata %>%
  # filter(year(date) >= 2018) %>%
  filter(source=='Wind', is_sane) %>%
  group_map(
    function(x, group){
      model <- lm(Utilization_corrected ~ wind_power_density:prov + date:prov + prov, data=x)
      summary(model)
      x$predicted <- predict(model, x)
      # x$prov <- group$prov
      x$Capacity <- x$Capacity

      x %>%
        mutate(month = month(date),
               year = year(date)) %>%
        select(date, month, year, prov, Capacity, predicted) %>%
        ungroup()
    }) %>%
  bind_rows() %>%
  group_by(year, month) %>%
  summarise(predicted = weighted.mean(predicted, Capacity, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(year, month=seq(2,12), fill=list(predicted=NA)) %>%
  group_by(month) %>%
  arrange(year) %>%
  mutate(predicted_yoy = (predicted/lag(predicted)-1)) %>%
  ungroup()%>%
  select(year, month, predicted_yoy)

actual_yoy <- pwr_data$monthly %>%
  ungroup() %>%
  filter(source %in% c('Wind'), var=='Utilization') %>%
  select(date, source, Utilization=Value1m) %>%
  # Correct for curtailment
  left_join(
    get_wind_curtailment_rates() %>%
      filter(province_en == "China") %>%
      rename(curtailment=value,
             source=type) %>%
      mutate(date = date %>% 'day<-'(lubridate::days_in_month(date)))
  ) %>%
  mutate(Utilization_corrected = Utilization / (1-curtailment)) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  ungroup() %>%
  select(year, month, Utilization_corrected) %>%
  tidyr::complete(year, month=seq(2,12), fill=list(Utilization_corrected=NA)) %>%
  group_by(month) %>%
  arrange(year) %>%
  mutate(actual_yoy = (Utilization_corrected/lag(Utilization_corrected)-1)) %>%
  ungroup() %>%
  select(year, month, Utilization_corrected, actual_yoy)


actual_yoy %>%
  filter(year>= 2018) %>%
  filter(month!=2) %>%
  left_join(predicted_yoy) %>%
  filter(!is.na(actual_yoy),
         !is.na(predicted_yoy)) %>%
  ggplot(aes(actual_yoy, predicted_yoy)) +
  geom_point(
    aes(
      col=factor(year)
    )
  ) +
  geom_smooth(method='lm') +
  geom_abline(slope=1, intercept=0) +
  scale_color_brewer(palette="RdYlBu") +
  theme_dark()


# MOM National ------------------------------------------------------------------
predicted_mom <- alldata %>%
  # filter(year(date) >= 2018) %>%
  filter(source=='Wind', is_sane) %>%
  group_map(
    function(x, group){
      model <- lm(Utilization_corrected ~ wind_power_density:prov + date:prov + prov, data=x)
      summary(model)
      x$predicted <- predict(model, x)
      # x$prov <- group$prov
      x$Capacity <- x$Capacity

      x %>%
        mutate(month = month(date),
               year = year(date)) %>%
        select(date, month, year, prov, Capacity, predicted) %>%
        ungroup()
    }) %>%
  bind_rows() %>%
  group_by(year, month) %>%
  summarise(predicted = weighted.mean(predicted, Capacity, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(year, month=seq(2,12), fill=list(predicted=NA)) %>%
  mutate(date=as.Date(paste0(year, "-", month, "-01"))) %>%
  arrange(date) %>%
  mutate(predicted_mom = (predicted/lag(predicted)-1)) %>%
  ungroup()%>%
  select(year, month, predicted_mom)

actual_mom <- pwr_data$monthly %>%
  ungroup() %>%
  filter(source %in% c('Wind'), var=='Utilization') %>%
  select(date, source, Utilization=Value1m) %>%
  # Correct for curtailment
  left_join(
    get_wind_curtailment_rates() %>%
      filter(province_en == "China") %>%
      rename(curtailment=value,
             source=type) %>%
      mutate(date = date %>% 'day<-'(lubridate::days_in_month(date)))
  ) %>%
  mutate(Utilization_corrected = Utilization / (1-curtailment)) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  ungroup() %>%
  select(year, month, Utilization_corrected) %>%
  tidyr::complete(year, month=seq(2,12), fill=list(Utilization_corrected=NA)) %>%
  mutate(date=as.Date(paste0(year, "-", month, "-01"))) %>%
  arrange(date) %>%
  mutate(actual_mom = (Utilization_corrected/lag(Utilization_corrected)-1)) %>%
  ungroup() %>%
  select(year, month, Utilization_corrected, actual_mom)


actual_mom %>%
  filter(year>= 2018) %>%
  filter(month!=2) %>%
  left_join(predicted_mom) %>%
  filter(!is.na(actual_mom),
         !is.na(predicted_mom)) %>%
  ggplot(aes(actual_mom, predicted_mom)) +
  geom_point(
    aes(
      col=factor(year)
    )
  ) +
  geom_smooth(method='lm') +
  geom_abline(slope=1, intercept=0) +
  scale_color_brewer(palette="RdYlBu") +
  theme_dark()

}



