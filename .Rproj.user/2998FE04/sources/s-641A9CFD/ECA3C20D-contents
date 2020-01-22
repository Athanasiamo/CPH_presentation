
synth_group_data <- function(size, mean, sd){
  
  mapply()  
}

se <- function(x) (sd(x)/sqrt(length(x)))*1.96

group_means <- function(data, x, y, funs = list(Mean = mean, SE = se)){
  data %>% 
    gather(measure, value, {{x}}, {{y}} ) %>% 
    group_by(measure, add = TRUE) %>% 
    summarise_at(vars(value), .funs = funs) %>% 
    gather(stat, value, one_of(names(funs))) %>% 
    unite(measure, c(measure, stat)) %>% 
    spread(measure, value)
}


plot_groups <- function(data, x, y, group){
  
  means <- group_means(data, {{x}}, {{y}}, {{group}}, 
                       list(Mean = mean, SE = se))
  
  data %>% 
    ggplot(aes(x = {{x}}, y = {{y}}) ) +
    geom_point(aes(colour = {{group}}), alpha=.7 ) +
    geom_errorbar(data = means, 
                  alpha = .8, inherit.aes = FALSE,
                  width=0, size = 2, 
                  aes(x = !!sym(names(means)[2]),
                      ymin = !!sym(names(means)[4]) - !!sym(names(means)[5])*1.96, 
                      ymax = !!sym(names(means)[4]) + !!sym(names(means)[5])*1.96)) +
    geom_line(data = means , size = 2, alpha = .8, 
              aes(x = !!sym(names(means)[2]), y = !!sym(names(means)[4]))) +
    geom_point(data = means, size = 6, alpha = .8, shape = 21,
               aes(x = !!sym(names(means)[2]), y = !!sym(names(means)[4]), 
                   fill = {{group}})) 
}
