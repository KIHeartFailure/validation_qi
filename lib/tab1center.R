tab1center <- function(var) {
  cl <- class(rsdata %>% pull(!!sym(var)))
  
  if (cl == "numeric") {
    ds <- rsdata %>%
      group_by(shf_centre) %>%
      summarise(
        ds = median(!!sym(var), na.rm = TRUE),
        n = n()
      ) %>%
      ungroup() %>%
      filter(n >= 10)
    
    
    out <- ds %>%
      summarise(
        med = fn(median(ds, na.rm = T), dig = 1),
        q1 = fn(quantile(ds, probs = 0.25, na.rm = T), dig = 1),
        q3 = fn(quantile(ds, probs = 0.75, na.rm = T), dig = 1)
      ) %>%
      mutate(out = paste0(med, " [", q1, "-", q3, "]")) %>%
      pull(out)
    tab1[tab1$var == var, "hosp"] <<- out
  }
  
  if (cl %in% c("factor", "character")) {
    if (cl == "character") {
      ds <- rsdata %>%
        mutate(!!sym(var) := factor(!!sym(var)))
    }
    
    ds <- rsdata %>%
      filter(!is.na(!!sym(var))) %>%
      group_by(shf_centre) %>%
      count(!!sym(var), .drop = FALSE) %>%
      mutate(
        ntot = sum(n),
        ds = n / ntot * 100
      ) %>%
      ungroup() %>%
      filter(ntot >= 10)
    
    lev <- levels(rsdata %>% pull(!!sym(var)))
    
    if (length(lev) == 2) {
      ds <- ds %>%
        filter(!!sym(var) == lev[2])
      
      out <- ds %>%
        summarise(
          med = fn(median(ds, na.rm = T), dig = 1),
          q1 = fn(quantile(ds, probs = 0.25, na.rm = T), dig = 1),
          q3 = fn(quantile(ds, probs = 0.75, na.rm = T), dig = 1)
        ) %>%
        mutate(out = paste0(med, " [", q1, "-", q3, "]")) %>%
        pull(out)
      tab1[tab1$var == paste0(var, " = ", lev[2]), "hosp"] <<- out
    } else {
      out <- ds %>%
        group_by(!!sym(var)) %>%
        summarise(
          med = fn(median(ds, na.rm = T), dig = 1),
          q1 = fn(quantile(ds, probs = 0.25, na.rm = T), dig = 1),
          q3 = fn(quantile(ds, probs = 0.75, na.rm = T), dig = 1)
        ) %>%
        mutate(out = paste0(med, " [", q1, "-", q3, "]")) %>%
        pull(out)
      
      tab1[tab1$var %in% paste0("   ", lev), "hosp"] <<- out
    }
  }
}