# functions
library(patchwork)

# add shaded areas to indicate blood sampling----
# * with text labels
add_sampling <- function(text_height){
  list(
    annotate("rect", alpha = 0.1, ymin = -Inf, ymax = Inf,
             xmin = ymd("2020-02-28"), 
             xmax = ymd("2020-04-17")) ,
    
    annotate("rect", alpha = 0.1,ymin = -Inf, ymax = Inf,
             xmin = ymd("2021-02-15"), 
             xmax = ymd("2021-03-02")) ,
    
    annotate("rect", alpha = 0.1, ymin = -Inf, ymax = Inf,
             xmin = ymd("2020-10-18")-3.5, 
             xmax = ymd("2020-11-29")+3.5,
             ) ,
    
    
    annotate("text", x = ymd("2020-03-22"), y = {{ text_height }}, label = "annual sampling",  hjust = 0.5) ,
    annotate("text", x = ymd("2020-11-05"), y = {{ text_height }}, label = "midyear sampling", hjust = 0.75) , 
    annotate("text", x = ymd("2021-02-28"), y = {{ text_height }}, label = "annual sampling",  hjust = 1) 
    
    #annotate("text", x = ymd("2021-05-02"), y = {{ text_height }}, label = "period comparing severity in primary/secondary",  hjust = 0) 
    
  )
  
  
}
# * without text labels
add_sampling_notext <- function( text_height){
  list(
    annotate("rect", alpha = 0.1, ymin = -Inf, ymax = Inf,
             xmin = ymd("2020-02-28"), 
             xmax = ymd("2020-04-17")) ,
    
    annotate("rect", alpha = 0.1,ymin = -Inf, ymax = Inf,
             xmin = ymd("2021-02-15"), 
             xmax = ymd("2021-03-02")) ,
    
    annotate("rect", alpha = 0.1, ymin = -Inf, ymax = Inf,
             xmin = ymd("2020-10-18")-3.5, 
             xmax = ymd("2020-11-29")+3.5,
    ) 
  )
  
  
}


# format date range and labels----
date_labels <- function(n, date_labels){
  scale_x_date(date_labels = {{ date_labels }},
               breaks = scales::pretty_breaks(n= {{ n }}), 
               limits = c(ymd("2020-02-21", ymd("2021-10-31"))))  
}


# define severity colors----
colors_severity <- c( # severe first
  "#49006a", # severe
  "#ae017e", # moderate
  "#f768a1", # mild
  "#fde0dd", # subclinical
  "grey60" # missing
)


theme_hm <- function(plot){
    theme_bw(base_size = 12) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.background = element_blank()) 
    
}

# plots ----
plot_sev_group <- function(data, group_var, n_or_percent){
  counts <- data %>% 
    group_by({{ group_var }}) %>% 
    count(severity_clinic_imp) %>% 
    mutate(percent = n / sum(n)*100) 
  
  ggplot(counts, aes(x    = {{ group_var }},
                     y    = {{ n_or_percent }},
                     fill = severity_clinic_imp)) +
    geom_col() +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()) +
    scale_fill_manual(values = colors_severity) +
    labs(x = NULL)
}


model_sev <- function(data, severity){
  severity <- ((data[[severity]]))
  
  glm(
    severity ~ infection_n_char,
    family = poisson,
    data = {{ data }}
  ) 
}

tidy_rr <- function(mod){# tidy & add CI's
  tidy(mod) %>% 
    mutate(l   = exp(estimate - 1.96*std.error),
           u   = exp(estimate + 1.96*std.error),
           
           across(where(is.numeric), round_half_up, digits = 2),
           ratio = ( exp(estimate)),
           CI  = paste0("(", 
                        sprintf("%.2f", round_half_up(l,2)), 
                        ", ", 
                        sprintf("%.2f", round_half_up(u,2)), 
                        ")") 
    ) %>% filter(term != "(Intercept)") %>% 
    select(term, ratio, CI, p.value, l, u)
  
}

add_model <- function(outcome){
  nest %>% 
    mutate(model = map(data, model_sev, {{ outcome }}),
           outcome = {{ outcome }},
           map_df(model, tidy_rr)
           
    )
}


add_model_age <- function(outcome){
  nest_age %>% 
    mutate(model = map(data, model_sev, {{ outcome }}),
           outcome = {{ outcome }},
           map_df(model, tidy_rr) 
           
    ) %>% 
    select(-term)
}

