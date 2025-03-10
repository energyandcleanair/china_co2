#required:
#apparent consumption of fossils.xlsx
#generation-consumption-utilization-capacity.xlsx
#Electricity consumption by sector YTD.xlsx
#monthly industry stats with YoY.xlsx
require(chinatracker)

source('scripts/load_package.R')

output_dir='outputs/2024Q4'
dir.create(output_dir)

last_month = ymd('2024-12-31')

in_file = get_data_file("apparent consumption of fossils.xlsx")


rw <- function(...) readwindEN(..., read_vardata = T, zero_as_NA = T, skip=3)

rw(in_file, c('var', 'prod'), columnFilter = "YTD", columnExclude = "Heat Supply") -> d1

rw(in_file, c('var', 'prod', 'area', 'sector'), columnFilter = "Consump.*Steam") %>%
  mutate(type='M') -> d2
#rw(in_file, c('var', 'prod'), columnFilter = "(Imports|Exports).*(Oil Products)") -> d3
rw(in_file, c('var', 'prod'), columnFilter = "Consumption: Coking Coal$") %>%
  mutate(type='M', sector='Metallurgy Industry') -> d4

rw(in_file, c('var'), columnFilter = "Heat Supply") %>%
  mutate(prod='Steam Coal', var='Consumption', sector='Heating System Industry', Source='NEA') ->
  d5

d = bind_rows(d1, d2, d4, d5) %>% arrange(date) %>%
  mutate(YoY=!is.na(YoY),
         Value=Value*ifelse(Unit=="ton", 1e-4,1),
         Unit=recode(Unit, ton="10000 tons"),
         Value=ifelse(Value==0, NA, Value))

d %<>%
  group_by(var, prod, sector, type, Source) %>%
  unYoY %>% filter(!YoY) %>%
  unYTD


#filter out WIND data for heating sector when NEA data available
is.na.or.missing <- function(x) length(x)==0 || is.na(x)
d %<>% filter(grepl('Heating', sector)) %>%
  group_by(date, var, sector, prod) %>%
  filter((Source=='NEA') == (!is.na.or.missing(Value1m[Source=='NEA']))) %>%
  ungroup %>%
  bind_rows(d %>% filter(!grepl('Heating', sector)))


#update last month natgas imports manually
ind=d$var=='Volume of Imports' & d$prod=='Natural Gas'
last_month_last_year <- last_month %>% 'year<-'(year(.)-1)
d$Value1m[d$date==last_month & ind & is.na(d$Value1m)] <-
  d$Value1m[d$date==last_month_last_year & ind] * 1155.8/1264.9

#correct old oddities in data
dates_to_correct <- c('2022-06-30', '2022-07-31') %>% ymd
dates_to_correct_last_year <- dates_to_correct %>% 'year<-'(year(.)-1)
yoy = sum(d$Value1m[d$date %in% dates_to_correct & ind]) /
  sum(d$Value1m[d$date %in% dates_to_correct_last_year & ind])

d$Value1m[d$date %in% dates_to_correct & ind] <-
  d$Value1m[d$date %in% dates_to_correct_last_year & ind] * yoy


ind=d$prod=='Steam Coal' & d$var=='Consumption' & d$sector=='Total'
d$Value1m[d$date=='2014-01-31' & ind] <- d$Value1m[d$date=='2013-01-31' & ind] * .9

d$Value1m[year(d$date)==2021 & month(d$date) %in% 7:9] %<>% multiply_by(.98)
d$Value1m[year(d$date)==2022 & month(d$date) %in% 1:3 & d$prod=='Oil Products'] %<>% multiply_by(.96)




d %<>% mutate(var = ifelse(var=='Apparent Consumption' & prod=='Natural Gas',
                           'Apparent Consumption WIND', var),
              Value1m=Value1m*ifelse(Unit %in% c('10k ton', '10000 tons') & prod=='Natural Gas', 1e4/0.657e-3/1e8, 1),
              Unit=ifelse(prod=='Natural Gas', '100M cu.m', Unit))


d %<>% filter(grepl('Crude Oil', prod)) %>% group_by(date) %>% summarise_at('Value1m', sum) %>%
  mutate(var='Apparent Consumption', prod='Crude Oil') %>% bind_rows(d, .)

d %<>% filter(grepl('Processing of Petroleum|Finished Oil Products', prod)) %>%
  mutate(prod='Oil Products', Value1m = Value1m * ifelse(grepl('Exports', var), -1, 1)) %>%
  group_by(prod, date) %>% summarise(across(Value1m, sum)) %>%
  mutate(var='Apparent Consumption') %>% bind_rows(d, .)

#1 m3 = 0.76e-3 t

d %<>% filter(prod=='Natural Gas', grepl('Output|Imports', var)) %>%
  group_by(prod, date) %>% summarise(across(Value1m, sum)) %>%
  mutate(var='Apparent Consumption') %>%
  bind_rows(d, .)

d %>% filter(prod=='Natural Gas') %>% ggplot(aes(date, Value1m, col=var)) + geom_line() + facet_wrap(~var)
d %>% filter(grepl('Cons', var), !grepl('WIND', var), year(date)>=2021) %>%
  ggplot(aes(date, Value1m)) +
  geom_col() + facet_wrap(~prod+sector+Source, scales='free_y')


#get bottom up estimates for last month when it is missing from data
#refresh_data=T
refresh_data=F
source('project_workflows/coal use bottom-up, 2024Q4.R')

coaluse_fill %>% mutate(sector=case_when(sector_coal=='Heating'~'Heating System Industry',
                                         sector_coal=='Other'~'Other Sectors',
                                         sector_coal=='Coking'~'Metallurgy Industry',
                                         grepl('Metal|Chemical', sector_coal)~paste(sector_coal, 'Industry'),
                                         T~sector_coal),
                        prod=case_when(sector_coal=='Coking'~'Coking Coal',T~'Steam Coal'),
                        basis='reported') %>%
  group_by(sector_coal, date) %>%
  filter(name=='reported' | 'reported' %notin% name) %>%
  ungroup %>% select(-name) %>% distinct() %>%
  full_join(d) %>%
  group_by(sector, prod) %>%
  fill(var) %>%
  group_by(sector, prod, var) %>%
  group_modify(function(df, group) {
    last_year_date <- df$date %>% 'year<-'(year(df$date)-1)
    last_year_value <- df$Value1m[match(last_year_date, df$date)]
    ind <- is.na(df$Value1m) & !is.na(df$YoY_pred)

    message('filling ', sum(ind), ' values for ', group$prod, ': ', group$sector)

    df$Value1m[ind] <- df$YoY_pred[ind] * last_year_value[ind]
    df$basis[ind] <- 'predicted based on output'

    return(df)
  }) -> d




CO2.factor <- c(Cement = 858.200/233035.7, #Cement = 769.300/231624.90 #2020 data https://zenodo.org/record/5126601
                'Steam Coal' = .7*29.3*85/1e3/100, #Emission factor from China's new inventory
                'Coking Coal' = 24*94.6/1e3/100,
                'Crude Oil' = 42.3*20.0*11/3/1e3/100,
                Gasoline = 45.8*19.88*11/3/1e3/100,
                Kerosene = 46.3*19.5*11/3/1e3/100,
                'Diesel Oil' = 45.3*20.0727*11/3/1e3/100,
                'Fuel Oil' = 40.5*21.4909*11/3/1e3/100,
                'Natural Gas' = 36.0*55.04/100/100,
                'Oil Products' = NA) %>%
  tibble(cat=names(.), ef=.)

d %>% filter(grepl('Diesel|Fuel Oil|Gasoline|Kerosene', prod), grepl('Consumption', var),
             year(date)==2021, month(date)==11) %>% ungroup %>%
  left_join(CO2.factor %>% rename(prod=cat)) %>% summarise(ef=weighted.mean(ef, Value)) %>% unlist ->
  CO2.factor$ef[CO2.factor$cat=='Oil Products']

d$cat = disambiguate(d$prod, CO2.factor$cat)
d$cat[d$prod == 'Kerosene'] <- 'Oil Products'
d$cat[d$prod == 'Petroleum Coke'] <- 'Oil Products'

oilprod = 'Oil Products' #c('Gasoline', 'Diesel Oil', 'Fuel Oil')
d %<>% group_by(var, sector, date) %>%
  group_modify(function(df, ...) {
    df$Value1m[df$prod=='Crude Oil'] %<>% subtract(sum(df$Value1m[df$prod %in% oilprod]))
    df
  })

d %<>% left_join(CO2.factor) %>% mutate(CO2 = Value1m * ef)


#adjustment to BP numbers
read_bp('G:/Shared drives/CREA-data/BP Statistical Review of World Energy/Statistical Review of World Energy Data.xlsx',
        'Coal Cons.*EJ|Oil Cons.*EJ|Gas Cons.*EJ', read_multiple = T, year=2023) %>%
  bind_rows() %>% filter(country=='China') %>% mutate(prod_bp=gsub(' .*', '', variable)) -> bp

d %<>% mutate(prod_bp=disambiguate(prod, c(Coal='Coal', Oil='Oil', Gas=' Gas')), year=year(date))

d %>% filter(grepl('Cons', var), is.na(sector) | sector=='Total' | prod=='Coking Coal', grepl('Oil Prod|Coal| Gas', prod)) %>%
  group_by(prod_bp, year) %>%
  summarise(across(CO2, sum)) ->
  d_yr

d_yr %>% inner_join(bp %>% rename(EJ_BP=value)) %>% na.omit %>%
  group_by(prod_bp) %>%
  mutate(adjustment=EJ_BP * CO2[year==max(bp$year)] / EJ_BP[year==max(bp$year)] / CO2) %>%
  select(prod_bp, year, adjustment) ->
  adj

d %>% left_join(adj) %>% group_by(prod_bp) %>%
  fill(adjustment, .direction='downup') %>%
  mutate(adjustment=na.cover(adjustment, 1),
         CO2=CO2*adjustment) ->
  d.adj

d.adj %<>%
  group_by(var, prod, sector) %>%
  fill(sector_coal, basis, .direction='downup') %>%
  roll12m(months=12, outcol='CO2_12m', incol='CO2') %>%
  roll12m(months=3, outcol='CO2_3m', incol='CO2')


bp %>% filter(year<=2011) %>% group_by(prod_bp) %>%
  mutate(bp_adj_hist=value/value[year==2011]) %>%
  select(year, prod_bp, bp_adj_hist) ->
  bp_hist

d.adj %>% filter(date=='2011-12-31',
                 var != 'Apparent Consumption WIND',
                 grepl('Consumpt', var), is.na(sector) | sector == 'Total' | prod=='Coking Coal',
                 !grepl('Cement', prod)) %>%
  select(-date, -year) %>%
  right_join(bp_hist) %>%
  mutate(CO2_12m = CO2_12m * bp_adj_hist,
         CO2=CO2_12m,
         date=ymd(paste0(year, '-12-31'))) %>%
  bind_rows(d.adj %>% filter(date>'2012-01-01' | (grepl('Cement', prod) & month(date)==12))) ->
  d.adj2


#quarterly
d.adj2 %>% filter(date>'2000-12-01',date<=last_month,
                  #month(date) %in% c(3,6,9,11),
                  prod %in% c('Cement', 'Natural Gas', 'Steam Coal', 'Coking Coal', oilprod), #, 'Crude Oil'
                  var != 'Apparent Consumption WIND',
                  grepl('Consumpt', var) | prod=='Cement') %>%
  mutate(sector=na.cover(sector, 'Total'),
         include_in_totals = prod %notin% c('Steam Coal', 'Total') | sector != 'Total') ->
  d.quarter


#adjust for bottom-up estimates
coaluse_plot %>% filter(sector_coal %in% c('Power Industry',
                                           #'Building Materials',
                                           #'Chemical',
                                           'Coking',
                                           'Heating',
                                           #'Other',
                                           'Metallurgy')) %>%
  group_by(sector_coal, date) %>%
  mutate(adj = if_null(value - value[name=='reported'], 0)) %>%
  select(name, sector_coal, date, value, adj) %>%
  left_join(d.quarter %>% ungroup %>% distinct(sector_coal, prod)) ->
  bottomup_adj

#fill adjustments forward
bottomup_adj %<>%
  group_by(name, prod, sector_coal, month=month(date)) %>%
  fill(adj, .direction = 'down') %>% ungroup %>%
  select(-month)

d.quarter %<>% cross_join(bottomup_adj %>% ungroup %>% distinct(name)) %>%
  left_join(bottomup_adj)

#add year-on-year changes to energy mix reported by NBS
d.quarter %<>% mutate(broad_prod=disambiguate(prod, c('Coal', 'Oil', 'Gas')))

if(file.exists('project_workflows/2024Q4 NBS reported vs Lauri dataset.R')) {
  source('project_workflows/2024Q4 NBS reported vs Lauri dataset.R')

  energy_mix_yoy_adj$nbs_adj[energy_mix_yoy_adj$broad_prod=='Oil'] <-
    mean(energy_mix_yoy_adj$nbs_adj[energy_mix_yoy_adj$broad_prod=='Oil'])

  d.quarter %>% filter(grepl('predicted', name)) %>%
    mutate(quarter=date %>% quarter(with_year=T) %>% as.character) %>%
    left_join(energy_mix_yoy_adj) %>%
    mutate(name='adjusted to NBS numbers') %>%
    select(-quarter) %>%
    bind_rows(d.quarter) ->
    d.quarter
} else {
  if(T) {
    read_xlsx(file.path('outputs/2024Q3', '2024Q1-3 energy sudoku.xlsx'), skip = 1) %>%
      filter(grepl('^Q[1-4]$', period)) %>%
      mutate(quarter=paste0('2024.', force_numeric(period))) %>%
      select(quarter, broad_prod, nbs_adj=adjustment) ->
      energy_mix_yoy_adj

    d.quarter %>% filter(grepl('predicted', name)) %>%
      mutate(quarter=date %>% quarter(with_year=T) %>% as.character) %>%
      left_join(energy_mix_yoy_adj) %>%
      group_by(broad_prod) %>% fill(nbs_adj, .direction='down') %>%
      mutate(name='adjusted to NBS numbers') %>%
      select(-quarter) %>%
      bind_rows(d.quarter) ->
      d.quarter
  } else {
    d.quarter$nbs_adj <- 1
  }
}





#apply adjustments
d.quarter %<>%
  replace_na(list(adj=0, nbs_adj=1)) %>%
  mutate(CO2 = (CO2 + adj*ef)*nbs_adj) #the adjustment to predicted values (adj) has not been applied to CO2 earlier


#add total CO2
d.quarter %<>% filter(prod!='Total') %>%
  filter(include_in_totals) %>%
  group_by(name, date) %>% summarise(across(c(starts_with('CO2')), sum)) %>%
  mutate(include_in_totals=F, prod='Total') %>%
  bind_rows(d.quarter)

#add steam coal total
d.quarter %<>% filter(prod=='Steam Coal', sector!='Total') %>%
  group_by(name, prod, broad_prod, date) %>% summarise(across(c(starts_with('CO2')), sum)) %>%
  mutate(include_in_totals=F, sector='Total') %>%
  bind_rows(d.quarter %>% filter(!(prod=='Steam Coal' & sector=='Total')))

#add gas use breakdown to power and others
pwr_data$monthly %>% filter(subtype=='Gas', var=='Generation, hybrid') %>%
  mutate(CO2=Value1m/.5*3.6*55/1000/10) %>%
  select(date, CO2) %>%
  mutate(prod='Natural Gas', broad_prod='Gas', sector='Power Industry') ->
  gas_power_CO2

gas_power_CO2 %<>% cross_join(d.quarter %>% ungroup %>% distinct(name))

gas_power_CO2 %>%
  bind_rows(d.quarter %>% filter(prod=='Natural Gas', sector=='Total')) %>%
  group_by(name, prod, broad_prod, date) %>%
  summarise(CO2=CO2[sector=='Total']-CO2[sector=='Power Industry']) %>%
  mutate(sector='Other Sectors') %>%
  bind_rows(gas_power_CO2) %>%
  mutate(include_in_totals=F) %>%
  bind_rows(d.quarter %>% filter(prod!='Natural Gas' | sector=='Total')) ->
  d.quarter

#add rolling means
d.quarter %<>%
  group_by(name, var, prod, sector) %>%
  roll12m(months=12, outcol='CO2_12m', incol='CO2', verbose=T) %>%
  roll12m(months=3, outcol='CO2_3m', incol='CO2') %>%
  filter(!is.na(CO2_12m)) %>%
#add year-on-year changes
  mutate(YoY_change_3m = get.yoy(CO2_3m, date, 'absolute'),
         YoY_3m = get.yoy(CO2_3m, date, 'relative'),
         YoY_change_1m = get.yoy(CO2, date, 'absolute'),
         YoY_1m = get.yoy(CO2, date, 'relative'),
         YoY_12m = get.yoy(CO2_12m, date, 'relative')) %>%
  ungroup



d.quarter %>% filter(prod=='Total', year(date)>=2019) %>% ggplot(aes(date, CO2_12m, col=name)) + geom_line()

variant_to_highlight = 'NBS' #'predicted'


d.quarter %>% filter(date==last_month,
                     grepl(variant_to_highlight, name),
                     prod=='Total') %>%
  mutate(across(starts_with('YoY_'), ~scales::percent(.x, accuracy=.1))) %>%
  select(name, prod, sector, date, CO2, CO2_3m, YoY_1m, YoY_3m, YoY_12m) %T>% copy.xl %>% View


d.quarter %>% ungroup %>%
  dplyr::select(basis_of_estimate=name, date,
                product=prod, product_category=broad_prod,
                sector,
                CO2, CO2_3m_mean=CO2_3m, CO2_12m_mean=CO2_12m,
                YoY_change_percent_1m=YoY_1m,
                YoY_change_percent_3m_mean=YoY_3m,
                YoY_change_absolute_1m=YoY_change_1m,
                YoY_change_absolute_3m_mean=YoY_change_3m,
                include_in_totals) %>%
  mutate(unit='MtCO2/month') %>%
  write_csv(file.path('outputs', 'CO2.csv'))

system('git add outputs/CO2.csv')
system(paste0('git commit -m "CO2 data until ',last_month,'"'))
system('git push')


d.quarter %<>% mutate(include_in_plots = sector=='Total' | prod=='Coking Coal')

d.quarter %>% filter(prod=='Total' | include_in_plots, name != 'reported') %>% ungroup %>%
  dplyr::select(name, date, prod, CO2_12m) %>% spread(prod, CO2_12m) %>%
  mutate(across(where(is.numeric), multiply_by, 12)) %>%
  write_csv(file.path(output_dir, '12-month CO2.csv'))


prodcols = c('black', crea_palettes$dramatic[c(6,5,5,3,3,2)], 'gray30', 'gray30') #
names(prodcols) = c('Crude Oil','Cement','Natural Gas', 'Gas', 'Oil Products', 'Oil','Coking Coal','Steam Coal', 'Coal')




###PLOTS
d.quarter %>%
  filter(include_in_plots | prod=='Total', grepl(variant_to_highlight, name), date>='2017-01-01') %>%
  mutate(broad_prod=ifelse(prod=='Total', prod, broad_prod)) %>%
  group_by(broad_prod, date) %>%
  summarise(across(CO2_12m, sum)) %>%
  mutate(prod=factor(broad_prod, levels=c(names(prodcols), 'Total'))) ->
  plotdata_12m


plotdata_12m %>%
  select(product=broad_prod, date, CO2_12m) %>%
  mutate(CO2_12m=CO2_12m*12) %>%
  write_csv(file.path(output_dir, 'CO2 by product, 12-month mean.csv')) %>%
  ggplot(aes(date, CO2_12m)) +
  geom_line(aes(col=product), linewidth=1) +
  facet_wrap(~product, scales='free_y') +
  x_at_zero() +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(c(0,.05)), labels = scales::comma) +
  theme_crea() +
  scale_color_manual(values=prodcols, name='Product', guide='none') +
  scale_linetype_manual(name='', values='dashed') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="12-month moving sum", y='Mt CO2 / year', x='') +
  theme(plot.title = element_text(size=rel(1.8)), legend.position = 'bottom') -> plt
quicksave(file.path(output_dir, 'CO2 12m sum, lineplot.png'), plot=plt)




period_name_short='Mar-Dec'
period_name_long="in March-December 2024, compared with 2023"


d.quarter %>% filter(!(sector == 'Total' & prod %in% c('Steam Coal', 'Natural Gas')),
                     grepl(variant_to_highlight, name)) %>%
  mutate(sector = case_when(prod=='Cement'~'Building Materials',
                            prod %in% c('Crude Oil', oilprod)~'Oil Consumption',
                            is.na(sector)&prod!='Total'~paste(prod,'Consumption'),
                            prod=='Total'~'Total',
                            T~sector)) %>%
  group_by(broad_prod, sector, date) %>%
  summarise(across(matches('^YoY_change|CO2'), sum)) %>%
  mutate(sector=recode(sector, Total='All Sectors', 'Oil Consumption'='All Sectors', 'Natural Gas Consumption'='All Sectors')) ->
  d.changes


d.changes %>% filter(broad_prod != 'Total', year(date)==2024, month(date) %in% 3:12) %>%
  group_by(broad_prod, sector) %>%
  summarise(across(c(YoY_change=YoY_change_1m), sum),
            across(date, list(min=min, max=max))) %>%
  mutate(unit='MtCO2') -> change_plot



change_plot %>%
  select(sector, broad_prod, starts_with('date'), YoY_change, unit) %>%
  write_csv(file.path(output_dir, paste0('Contributions to emissions growth, by sector, ',period_name_short,'.csv'))) %>%
  ggplot(aes(sector, YoY_change, fill=broad_prod)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  labs(title="Contributions to changes in emissions",
       subtitle=period_name_long, y='Mt CO2', x='') +
  coord_flip() +
  #x_at_zero() +
  scale_x_discrete(limits=rev) -> plt
quicksave(file.path(output_dir, paste0('Contributions to emissions growth, by sector, ',period_name_short,'.png')),
          plot=plt, scale=1, logo=F)




d.changes %>% filter(broad_prod != 'Total', date==last_month) %>%
  mutate(prod=factor(broad_prod, levels=names(prodcols)), Unit='Mt CO2',
         YoY_change_3m=3*YoY_change_3m) %>%
  select(sector, broad_prod, date, YoY_change_3m, Unit) %>%
  write_csv(file.path(output_dir, 'Contributions to emissions growth, by sector, Q4.csv')) %>%
  ggplot(aes(sector, YoY_change_3m, fill=broad_prod)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  labs(title="Contributions to changes in emissions",
       subtitle="in Q4 2024, compared with 2023", y='Mt CO2', x='') +
  coord_flip() +
  #x_at_zero() +
  scale_x_discrete(limits=rev) -> plt
quicksave(file.path(output_dir, 'Contributions to emissions growth, by sector, Q4.png'),
          plot=plt, scale=1, logo=F)




pwr_data <- read_power_generation(predict_solar_wind = T)

paste.xl() -> energy

energy %>%
  pivot_longer(starts_with('X'), names_to='year', values_to = 'growth') %>%
  mutate(unit=ifelse(grepl('%', growth), '%', 'Mtce/year'),
         across(c(year, growth), force_numeric),
         growth=growth/ifelse(unit=='%', 100, 1)) %>% na.omit ->
  energy_long

energy_long %>% write_csv(file.path(output_dir, 'total energy demand growth.csv'))

energy_long <- read_csv(file.path(output_dir, 'total energy demand growth.csv'))

ember_annual <- read_csv('https://storage.googleapis.com/emb-prod-bkt-publicdata/public-downloads/yearly_full_release_long_format.csv')

ember_annual %>%
  filter(Area=='China', Subcategory=='Fuel', Category=='Electricity generation', Unit=='TWh') %>%
  rename(year=Year) %>%
  mutate(source=Variable %>% recode(Bioenergy='Biomass')) %>%
  group_by(source) %>%
  mutate(growth=Value-lag(Value)) ->
  ember_annual_growth

pwr_data$monthly %>% filter(var=='Generation, hybrid') %>%
  group_by(var, source, subtype, year=year(date)) %>%
  summarise(across(Value1m, ~sum(.x, na.rm=F))) %>%
  mutate(growth=(Value1m-lag(Value1m))/10) %>%
  ungroup %>% mutate(source=na.cover(subtype, source)) ->
  crea_annual_growth

bind_rows(ember_annual_growth,
          crea_annual_growth %>% filter(year %notin% ember_annual_growth$year)) %>%
  select(source, year, growth) %>%
  filter(source %in% c('Hydro', 'Nuclear', 'Wind', 'Solar', 'Biomass'), year>=2015) %>%
  mutate(source=recode(source, Thermal='Biomass'),
         growth=growth/(29.3/3.6)/.303,
         unit='Mtce/year') ->
  plotdata

plotdata %<>% rename(variable=source) %>%
  mutate(unit='Mtce/year') %>%
  bind_rows(energy_long %>% filter(!grepl('Non-fossil', variable))) %>%
  filter(year>=2015) %>% ungroup %>%
  select(variable, year, growth, unit) %>% arrange(year) %>%
  write_csv(file.path(output_dir, 'Annual growth in energy demand and non-fossil energy supply.csv'))

plotdata %>% filter(!grepl('increase|growth', variable)) %>%
  ggplot(aes(year, growth)) + geom_col(aes(fill=variable)) +
  geom_line(data=plotdata %>% filter(grepl('demand', variable), unit=='Mtce/year', year>=2015),
            aes(linetype='Energy consumption'),
            color='darkgray', linewidth=1) +
  #geom_line(data=plotdata %>% filter(grepl('GDP', variable), unit=='%') %>%
  #            filter(year>=2015) %>% mutate(growth=growth*50*100),
  #          aes(linetype='GDP growth (right axis)'),
  #          color='darkred', linewidth=1) +
  theme_crea() + scale_fill_crea_d() +
  scale_x_continuous(breaks=function(x) seq(2015,2024,2),
                     expand=expansion(mult=c(.02,.02))) +
  scale_y_continuous(sec.axis = sec_axis(trans=~./5000, name="GDP growth", labels = scales::percent)) +
  labs(linetype='', x='', y='Mtce/year',
       title='Annual growth in energy demand and non-fossil energy supply')
quicksave(file.path(output_dir, 'Annual growth in energy demand and non-fossil energy supply.png'))


plotdata %>% filter(grepl('growth', variable), unit=='%') %>%
  ggplot(aes(year, growth)) + geom_line(aes(col=variable), linewidth=1) +
  theme_crea() + scale_color_crea_d() +
  scale_x_continuous(breaks=function(x) seq(2015,2024,2),
                     expand=expansion(mult=c(.02,.02))) +
  x_at_zero(labels=scales::percent) +
  labs(linetype='', x='', y='',
       title='Annual growth in energy demand and GDP')
quicksave(file.path(output_dir, 'Annual growth in energy demand and GDP.png'))




