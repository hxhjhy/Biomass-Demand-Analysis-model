## Model selection--------------------------------------------------

## option 1-6

mdoel.selection.straw <- function(formula_num){

    formula_1 <- y ~ H_pop + H_inc  + P_land + P_forest + 
      P_road + P_coal + P_straw + year

    formula_2 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_road + P_coal + P_straw + year

    formula_3 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_road + P_coal + P_straw + pro

    formula_4 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_road + P_coal + P_straw + pro +
      f(pro, model = "besagproper", graph = wood.nb.rs) 

    formula_5 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_road + P_coal + P_straw +
      f(year, model = "ar",order = 2) +
      f(pro, model = "besagproper", graph = wood.nb.rs) 

    formula_6 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_road + P_coal + P_straw +
      f(year, model = "ar",order = 2) +
      f(pro, model = "besagproper", graph = wood.nb.rs) +
      + f(pro2, year, model="iid", constr=TRUE)

    formula <- switch(formula_num, formula_1, formula_2, formula_3, formula_4,
                       formula_5, formula_6)
}
  
mdoel.selection.wood <- function(formula_num){

    formula_1 <- y ~ H_pop + H_inc  + P_land + P_forest + 
       P_coal + year

    formula_2 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_coal + year

    formula_3 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_coal + pro

    formula_4 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_coal + pro +
      f(pro, model = "besagproper", graph = wood.nb.rs) 

    formula_5 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_coal + f(year, model = "ar",order = 2) +
      f(pro, model = "besagproper", graph = wood.nb.rs) 

    formula_6 <- y ~ H_pop + I(H_pop^2) + I(H_pop^3) + 
      H_inc + I(H_inc^2) + I(H_inc^3) +
      P_land + I(P_land^2) + I(P_land^3) +
      P_forest + P_coal + f(year, model = "ar",order = 2) +
      f(pro, model = "besagproper", graph = wood.nb.rs) +
      + f(pro2, year, model="iid", constr=TRUE)

    formula <- switch(formula_num, formula_1, formula_2, formula_3, formula_4,
                       formula_5, formula_6)
}
  
