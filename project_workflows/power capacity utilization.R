pwr %>% mutate(source=prod %>% gsub(' Power| Generating| Capacity', '', .)) %>% 
  group_by(source) %>% unYTD %>% 
  select(source, date, generation=Value1m) %>% 
  full_join(cap %>% filter(!grepl('New', var), fuel=='All') %>% select(source, date, capacity=Value)) %>% 
  group_by(source) %>% mutate(capacity=na.approx(capacity, na.rm=F)) %>% 
  na.omit ->
  pwr_plot

pwr_plot %>% 
  group_by(source, year=year(date)) %>% 
  summarise(utilization=mean(generation/capacity)) %>% 
  ggplot(aes(year, utilization)) + geom_col() + facet_wrap(~source)

pwr_plot %>% 
  filter(month(date)<=11, year(date)>=2017) %>% 
  group_by(source, year=year(date)==2022) %>% 
  summarise(utilization=mean(generation/capacity))
