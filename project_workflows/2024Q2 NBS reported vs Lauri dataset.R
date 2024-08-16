#Q1: total energy +5.2%, coal -1.1pp, oil -0.2pp, gas +0.5pp, non-fossil +0.8pp
#https://www.stats.gov.cn/sj/sjjd/202404/t20240417_1954647.html
#2023 full year shares coal, oil, gas, non-fossil 55.5, 18.3, 8.5, 17.7
#H1: total energy +4.7%, non-fossil +2.2pp


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

#output data needed to compute adjustments in Excel
energy_mix %>%
  group_by(year=year(date), period=paste0('Q', quarter(date)), broad_prod) %>%
  filter(year(date)>=2023, month(date) %in% 1:6) %>%
  summarise(across(Mtce, sum)) %>%
  (function(df) {
    df %>% group_by(year, broad_prod) %>% summarise(across(Mtce, sum)) %>%
      mutate(period='H1') %>% bind_rows(df)
  }) %>%
  group_by(year, period) %>% mutate(share=Mtce/sum(Mtce)) %>%
  (function(df) {
    df %>% group_by(year, period) %>% summarise(across(Mtce, sum)) %>%
      mutate(broad_prod='Total') %>% bind_rows(df)
  }) %>%
  pivot_wider(names_from=year, values_from=c(Mtce, share)) %>%
  arrange(period, broad_prod) %>% #copy.xl()
  write_csv(file.path(output_dir, '2024H1 energy sudoku.csv'))

read_xlsx(file.path(output_dir, '2024H1 energy sudoku.xlsx'), skip = 1) %>%
  filter(grepl('^Q', period)) %>%
  mutate(quarter=paste0('2024.', force_numeric(period))) %>%
  select(quarter, broad_prod, nbs_adj=adjustment) ->
  energy_mix_yoy_adj

energy_mix %>%
  mutate(quarter=date %>% quarter(with_year=T) %>% as.character) %>%
  left_join(energy_mix_yoy_adj) %>%
  replace_na(list(nbs_adj=1)) %>%
  group_by(broad_prod) %>%
  mutate(across(c(Mtce, CO2), ~.x*nbs_adj)) %>%
  (function(df) {
    df %>% group_by(date) %>% summarise(across(c(Mtce, CO2), sum)) %>%
      mutate(broad_prod='Total') %>% bind_rows(df)
  }) %>% group_by(broad_prod) %>%
  mutate(date=date %>% 'day<-'(1),
         Mtce_3m=zoo::rollapplyr(Mtce, 3, mean, fill=NA),
         Mtce_12m=zoo::rollapplyr(Mtce, 12, mean, fill=NA),
         yoy=get_yoy(Mtce, date),
         yoy_3m=get_yoy(Mtce_3m, date)) %>%
  filter(date>='2018-01-01') %T>% View %>%
  ggplot(aes(date, yoy_3m, col=broad_prod)) +
  geom_line() + facet_wrap(~broad_prod)

