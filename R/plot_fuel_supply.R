fuel_supply_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1),
                              lang=parent.frame()$lang,
                              output_dir=get('output_dir', envir=.GlobalEnv)) {
  in_file = get_data_file("fuel supply.xlsx")

  readwindEN(in_file, c('var', 'prod'), read_vardata = T, zero_as_NA = T) %>%
    replace_na(list(type='M')) -> fuelsupply

  data_summary <<- data_summary %>% bind_rows(check_dates(data = fuelsupply,
                                                          file_name = "fuel supply.xlsx"))

  fuelsupply %<>% arrange(date) %>%
    mutate(Value=Value*ifelse(Unit=="ton", 1e-4,1),
           Unit=recode(Unit, ton="10000 tons"),
           type=case_when(V4=='YTD'~'YTD', T~type))

  fuelsupply %<>% group_by(var, prod, type) %>% unYTD %>% roll12m()

  fuelsupply %<>% mutate(var = ifelse(var=='Apparent Consumption' & prod=='Natural Gas',
                                      'Apparent Consumption WIND', var),
                         Value1m=Value1m*ifelse(Unit %in% c('10k ton', '10000 tons') & prod=='Natural Gas', 1e4/0.657e-3/1e8, 1),
                         Unit=ifelse(prod=='Natural Gas', '100M cu.m', Unit))

  fuelsupply %<>% filter(grepl('Finished Oil Products', prod)) %>%
    mutate(prod='Oil Products', across(starts_with('Value'), ~.x * ifelse(grepl('Exports', var), -1, 1))) %>%
    group_by(prod, date, Unit) %>% summarise(across(starts_with('Value'), sum)) %>%
    mutate(var='Net Imports') %>% bind_rows(fuelsupply %>% filter(!grepl('Imports',var) | !grepl('Oil Products', prod)))

  fuelsupply %>%
    filter(!grepl('Exports', var)) %>%
    mutate(prod = case_when(grepl('Coal', prod)~'Coal',
                            grepl('Natural Gas', prod)~'Fossil Gas',
                            grepl('Crude Oil', prod)~'Crude Oil',
                            grepl('Oil Products|Processing of Petroleum', prod)~'Oil Products',
                            T~prod),
           prod_group = case_when(grepl('Diesel|Gasoline|Kerosene', prod)~'Oil Products', T~prod),
           var = case_when(grepl('Imports', var)~'Net Imports', T~var)) ->
    fuelsupply_plotdata

  fuelsupply_plotdata %<>% group_by(date, prod_group, var, Unit) %>%
    filter(prod_group == 'Oil Products', var=='Output') %>%
    summarise(across(starts_with('Value'), ~.x[prod=='Oil Products'] - sum(.x[grepl('Diesel|Gasoline|Kerosene', prod)]))) %>%
    mutate(prod='Other Oil Products') %>%
    bind_rows(fuelsupply_plotdata)

  fuelsupply_plotdata %<>%
    filter(!grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>%
    group_by(prod_group, date, Unit) %>%
    summarise(across(starts_with('Value'), sum)) %>%
    mutate(var='Total Supply') %>%
    bind_rows(fuelsupply_plotdata)

  fuelsupply_plotdata %<>% ungroup %>%
    mutate(across(starts_with('Value'), ~convert_value(.x, Unit))*ifelse(Unit=='10000 tons' & lang=='ZH', 1e-4, 1),
           Unit = unit_label(ifelse(Unit=='10000 tons' & lang=='ZH', "100Mt", Unit), lang=lang),
           prod_group = paste0(trans(prod_group), ', ', Unit))

  if(lang=='EN') fuelsupply_plotdata %>% write_csv(file.path(output_dir, 'fossil fuel supply.csv'))

  p <- fuelsupply_plotdata %>%
    filter(year(date) >= 2017, !grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(date, Value12m * 12, col = trans(var))) +
    geom_line(aes(linewidth = var == 'Total Supply')) +
    facet_wrap(~prod_group, scales = 'free_y') +
    theme_crea_new() +
    lang_theme(lang = lang) +
    labs(title = trans('Fossil fuel supply'),
         subtitle = trans('12-month moving sum'),
         y = '', x = '') +
    scale_linewidth_discrete(range = c(1, 2), guide = 'none') +
    scale_color_crea_d('dramatic', name = '') +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, .05)), labels = yearlab)
  quicksave(file.path(output_dir, paste0('fossil fuel supply, ',lang,'.png')),
            plot = p)

  p <- fuelsupply_plotdata %>%
    filter(year(date) >= 2017, grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>%
    write_csv(file.path(output_dir, 'oil products output.csv')) %>%
    mutate(prod = factor(prod)) %>%
    ggplot(aes(date, Value12m * 12, col = trans(prod))) +
    geom_line(linewidth = 1.5) +
    theme_crea_new() +
    lang_theme(lang = lang) +
    labs(title = trans('Output of different oil products'),
         subtitle = trans('12-month moving sum'),
         y = ifelse(lang == 'ZH', unit_label('100Mt', lang = lang), 'Mt'),
         x = '') +
    scale_color_crea_d('dramatic', name = '') +
    expand_limits(y = 0) +
    x_at_zero() +
    scale_x_date(expand = expansion(mult = c(0, .05)), labels = yearlab)
  quicksave(file.path(output_dir, paste0('oil products output, ', lang,'.png')),
            plot = p)


  p <- fuelsupply_plotdata %>%
    filter(year(date) >= 2010, !grepl('Diesel|Gasoline|Kerosene|Other', prod),
           var == 'Net Imports') %>%
    group_by(prod_group = ifelse(grepl('Oil', prod_group), 'Oil, Mt', prod_group),
             date) %>%
    summarise(across(Value12m, sum)) %>%
    mutate(YoY_12m = get.yoy(Value12m, date,
                             fun = (function(x1, x0) {(x1 / x0 - 1) * sign(x0)})) %>%
             pmax(-.2) %>%
             pmin(.2)) %>%
    ggplot(aes(date, Value12m * 12, col = YoY_12m)) +
    geom_line(linewidth = 1.5) +
    facet_wrap(~prod_group, scales = 'free_y') +
    theme_crea_new() +
    lang_theme(lang = lang) +
    labs(title = trans('Fossil fuel imports'),
         subtitle = trans('12-month moving sum'),
         y = '', x = '') +
    scale_color_crea_c('change', labels = scales::percent,
                       name = trans('Year-on-year'),
                       guide = guide_colorbar(direction = 'horizontal',
                                              barwidth = 10)) +
    scale_x_date(expand = expansion(mult = c(0, .05)), labels = yearlab) +
    x_at_zero()
  quicksave(file.path(output_dir, paste0('Fossil fuel imports, ', lang,'.png')), plot=p)
}
