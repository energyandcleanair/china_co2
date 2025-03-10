#total energy +5.2%, coal -1.1pp, oil -0.2pp, gas +0.5pp, non-fossil +0.8pp
#https://www.stats.gov.cn/sj/sjjd/202404/t20240417_1954647.html
#2023 full year shares coal, oil, gas, non-fossil 55.5, 18.3, 8.5, 17.7

pwr_data$monthly %>% filter(grepl('Wind|Solar|Hydro|Nuclear', source) | subtype=='Biomass') %>%
  group_by(date) %>% summarise(across(Value1m, ~sum(.x, na.rm=T))) %>%
  mutate(Mtce=Value1m * .305/1e3, broad_prod='Non-fossil') -> non.fossil

d.quarter %>% filter(include_in_totals, prod!='Cement', grepl('predicted', name)) %>%
  mutate(Mtce=CO2/case_when(broad_prod=='Coal'~85, broad_prod=='Oil'~72, broad_prod=='Gas'~55)/29.3*1e3) %>%
  group_by(broad_prod, date) %>% summarise(across(c(CO2, Mtce), sum)) %>%
  bind_rows(non.fossil) -> energy_mix

tibble(broad_prod=c('Coal', 'Oil', 'Gas', 'Non-fossil'),
       reported_Mtce=5720 * c(55.5, 18.3, 8.5, 17.7)/100) ->
  reported_mix

energy_mix %>% filter(year(date)==2023) %>%
  group_by(broad_prod) %>%
  summarise(across(c(Mtce, CO2), sum)) %>%
  left_join(reported_mix) %>%
  mutate(adj=reported_Mtce/Mtce) ->
  energy_mix_adj

energy_mix %<>%
  left_join(energy_mix_adj %>% select(broad_prod, adj)) %>%
  mutate(Mtce=Mtce*adj)

energy_mix %>%
  ggplot(aes(date, Mtce, fill=broad_prod)) + geom_area()

tibble(broad_prod=c('Coal', 'Oil', 'Gas', 'Non-fossil'),
       reported_mix_change=c(-1.1, -.2, .5, .8)/100,
       year=2024) ->
  reported_mix_change

reported_yoy=5.2e-2

energy_mix %>% group_by(year=year(date), broad_prod) %>%
  filter(year(date)>=2023, month(date) %in% 1:3) %>%
  summarise(across(Mtce, sum)) %>%
  mutate(share=Mtce/sum(Mtce)) %>%
  left_join(reported_mix_change) %>% replace_na(list(reported_mix_change=0)) %>%
  group_by(broad_prod) %>%
  mutate(share=share[year==2023]+reported_mix_change) %>%
  ungroup %>%
  mutate(reported_yoy=ifelse(year==2023, 0, reported_yoy),
         reported_Mtce=sum(Mtce[year==2023])*(1+reported_yoy)*share,
         adj=reported_Mtce/Mtce) %>%
  filter(year==2024) ->
  energy_mix_yoy_adj
