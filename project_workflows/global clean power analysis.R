install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))

require(ggflags)

change_base_year = 2019

ember <- get_ember_monthly_data()
#ember <- read_csv(get_data_file("monthly_full_release_long_format-4.csv"))

ember %<>% set_names(make.names(names(.)))

ember %>% saveRDS('data/ember_monthly.RDS')
ember <- readRDS('data/ember_monthly.RDS')

gem_nuclear_path <- "~/../Downloads/Global-Nuclear-Power-Tracker-July-2024.xlsx"

download.file("https://globalenergymonitor.org/wp-content/uploads/2024/07/Global-Nuclear-Power-Tracker-July-2024.xlsx",
              gem_nuclear_path,
              mode="wb")

read_xlsx(gem_nuclear_path, sheet='Data') %>%
  set_names(make.names(names(.))) %>%
  mutate(Construction.Start.Year = Construction.Start.Date %>%
           substr(1,4) %>% as.numeric) ->
  gem_nuclear

gem_nuclear %<>% mutate(iso3 = countrycode(Country.Area, 'country.name', 'iso3c'))

gem_nuclear %>%
  mutate(Status =
           paste0('nuclear_mw_',
                  case_when(grepl('pre-constr|announced', Status)~'planned',
                            grepl('oper', Status) &
                              Start.Year > change_base_year~'operating_new',
                            grepl('oper', Status) ~'operating_old',
                            grepl('retired', Status) &
                              Retirement.Year > change_base_year &
                              Retirement.Year <= year(today()) ~'newly_retired',
                              T~Status))) %>%
  filter(grepl('oper|_construction|planned|newly_retired', Status)) %>%
  group_by(iso3, Status) %>%
  summarise(across(Capacity..MW., sum)) %>%
  spread(Status, Capacity..MW., fill=0) ->
  gem_bycountry


if_null <- function(x, replacement=NA) {
  if(length(x)==0) x[1]<-replacement
  x
}



ember %>% ungroup %>% filter(Unit == 'TWh') %>%
  complete(nesting(Area, Country.code, Unit, Area.type, Date, OECD, EU),
           nesting(Variable, Category, Subcategory),
           fill=list(Value=0)) %>%
  group_by(Area, Unit, Category, Subcategory, Variable) %>%
  mutate(Value_12m=rollapplyr(Value, 12, sum, fill=NA)) ->
  ember_12m

ember_12m %<>% group_by(Area, Date) %>%
  mutate(Value_12m = Value_12m / Value_12m[Variable=='Total Generation'],
         Unit='%') %>%
  bind_rows(ember_12m)

ember_12m %>% ungroup %>%
  select(Area, iso3 = Country.code, Unit, Area.type, OECD, EU, Date, Variable, Value_12m) %>%
  filter(Variable %in% c('Wind', 'Solar', 'Total Generation', 'Nuclear', 'Fossil', 'Clean', 'Coal')) %>%
  mutate(across(Variable, make_names)) %>%
  spread(Variable, Value_12m) %>%
  mutate(wind_solar_total=wind+solar,
         #nuclear=Value_12m[Variable=='Nuclear'],
         #fossil=Value_12m[Variable=='Fossil'],
         #clean=Value_12m[Variable=='Clean'],
         #total_generation=Value_12m[Variable=='Total Generation'],
         coal_share_in_thermal=coal/fossil) %>%
  group_by(Area, Unit, Area.type) %>%
  mutate(across(where(is.numeric),
                list(change_12m=~.x-lag(.x, 12),
                     change_long=~if_null(.x[Date==max(Date)]) -
                       if_null(.x[Date==ymd(paste(change_base_year, 12, 1))]))),
         across(ends_with('change_long'),
                list(relative=~.x/if_null(total_generation[Date==max(Date)]))),
         across(contains('nuclear_change'),
                list(category=~case_when(.x<0~'Reduced nuclear',
                                         .x==0~'No change in nuclear',
                                         .x>0~'Increased nuclear')))) %>%
  filter(month(Date)==12 | Date==max(Date),
         Area.type=='Country') -> marginal_thermal_data


marginal_thermal_data %<>%
  left_join(gem_bycountry) %>%
  group_by(Area, Date) %>%
  mutate(across(starts_with('nuclear_mw_'),
                ~replace_na(.x, 0)*8760/1e6*.85/if_null(total_generation[Unit=='TWh']))) %>%
  ungroup %>%
  mutate(OECD=ifelse(OECD==0, 'non-OECD', 'OECD'),
         EU=ifelse(EU==0, 'non-EU', 'EU'))



###W&S effect on coal

marginal_thermal_data %>% filter(Unit=='%') %>%
  ggplot(aes(wind_solar_total_change_12m, coal_share_in_thermal_change_12m)) +
  geom_point() + geom_smooth(method='lm')


marginal_thermal_data %>% filter(Date==max(Date), Unit=='%') %>%
  ggplot(aes(wind_solar_total_change_long, coal_share_in_thermal_change_long)) +
  geom_point() + geom_smooth(method='lm')


marginal_thermal_data %>% filter(Unit=='%') %>%
  lm(wind_solar_total_change_12m~coal_share_in_thermal_change_12m,
     data=.) %>% summary

marginal_thermal_data %>% filter(Date==max(Date), Unit=='%') %>%
  lm(wind_solar_total_change_long~coal_share_in_thermal_change_long,
     data=.) %>% summary


###nuclear effect on clean: models
marginal_thermal_data %>% group_by(Area) %>% filter(Date==max(Date), Unit=='TWh') %>%
  lm(clean_change_long_relative~nuclear_change_long_relative:OECD, data=.) %>%
  summary

marginal_thermal_data %>% filter(Date==max(Date), Unit=='TWh') %>%
  lm(wind_solar_total_change_long_relative~nuclear_change_long_relative:OECD, data=.) %>%
  summary




marginal_thermal_data %>%
  group_by(Area) %>%
  filter(Date==max(Date), Unit=='TWh') %>%
  lm(clean_change_long_relative~
       (nuclear_mw_construction+nuclear_mw_newly_retired+
          nuclear_mw_operating_new+nuclear_mw_operating_old+
          nuclear_mw_planned), data=.) %>%
  summary


marginal_thermal_data %>%
  group_by(Area) %>%
  filter(Date==max(Date), Unit=='TWh') %>%
  mutate(across(starts_with('nuclear_mw_'), ~.x>0)) %>%
  lm(clean_change_long_relative~
    (nuclear_mw_construction+nuclear_mw_newly_retired+
    nuclear_mw_operating_new+nuclear_mw_operating_old+
    nuclear_mw_planned), data=.) %>%
    summary




##plots
marginal_thermal_data %>% group_by(Area) %>%
  filter(Date==max(Date), Unit=='TWh') %>%
  ggplot(aes(nuclear_change_long_relative, clean_change_long_relative)) +
  geom_point(aes(size=total_generation)) + geom_smooth(method='lm')

marginal_thermal_data %>% group_by(Area) %>%
  filter(Date==max(Date), Unit=='TWh', !is.na(nuclear_change_long_category)) %>%
  ggplot(aes(nuclear_change_long_category, clean_change_long_relative)) +
  geom_boxplot()






marginal_thermal_data %>% group_by(Area) %>%
  filter(Date==max(Date), Unit=='%') %>%
  ggplot(aes(nuclear_mw_operating_new, clean_change_long)) +
  geom_flag(aes(country=iso3 %>% countrycode('iso3c', 'iso2c') %>% tolower))


marginal_thermal_data %>% group_by(Area) %>%
  filter(Date==max(Date), Unit=='%') %>%
  ggplot(aes(nuclear_mw_construction>0, clean_change_long)) +
  geom_boxplot()




marginal_thermal_data %>% group_by(Area, Unit) %>%
  filter(Date==max(Date), Unit=='%') %>%
  ggplot(aes(Area, clean_change_long)) + geom_col() + coord_flip()


#old stuff




ember_12m %<>%
  group_by(Area, Unit, Category, Subcategory, Variable) %>%
  mutate(Value_12m=rollapplyr(Value, 12, sum, fill=NA),
         change_12m=Value_12m-lag(Value_12m, 12))

ember_12m %>% group_by(Area) %>%
  filter(Date==max(Date), Subcategory=='Fuel',
         !grepl('Coal|Gas|Bio|Fossil', Variable),
         Area.type=='Country') ->
  clean_growth


clean_growth %>%
  group_by(Category, Unit) %>%
  mutate(change_12m = ifelse(Area=='World' & Variable=='Hydro',
                             change_12m - change_12m[Area=='China' & Variable=='Hydro'],
                             change_12m)) %>%
  filter(Area=='World', grepl('generation', Category), Unit=='TWh') %>%
  select(Variable, change_12m) %>%
  mutate(share=change_12m/sum(change_12m)) %>%
  ggplot(aes(Variable, change_12m)) + geom_col(aes(fill=Variable)) +
  theme_crea() +
  x_at_zero(headroom=.15) +
  scale_fill_crea_d(guide='none') +
  geom_label(aes(label=scales::percent(share)), vjust=-.2) +
  labs(title='Increase in global carbon-free power generation by source',
       subtitle='past 12 months', y='TWh', x='')


clean_growth %>% filter(grepl('Wind|Solar', Variable)) %>%
  group_by(Area) %>% summarise(across(change_12m, sum)) %>%
  arrange(desc(change_12m)) %>% mutate(ranking=seq_along(Area)) ->
  ws_ranking

clean_growth %>%
  ggplot(aes(Area, change_12m, fill=Variable)) +
  geom_col() + coord_flip() +
  scale_x_discrete(limits=ws_ranking$Area[20:1])
