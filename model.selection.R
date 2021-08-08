## Model selection--------------------------------------------------

## option 1-6

formula_1 <- y ~ f_pop +  f_size  + f_inc  + f_land + Ex_forest + 
  Ex_road + Ex_coal + cr + year

formula_2 <- y ~ f_pop + I(f_pop^2) + I(f_pop^3) + 
  f_size + I(f_size^2) + I(f_size^3) +
  f_inc + I(f_inc^2) + I(f_inc^3) +
  f_land + I(f_land^2) + I(f_land^3) +
  Ex_forest + Ex_road + Ex_coal + cr + year

formula_3 <- y ~ f_pop + I(f_pop^2) + I(f_pop^3) + 
  f_size + I(f_size^2) + I(f_size^3) +
  f_inc + I(f_inc^2) + I(f_inc^3) +
  f_land + I(f_land^2) + I(f_land^3) + pro

formula_4 <- y ~ f_pop + I(f_pop^2) + I(f_pop^3) + 
  f_size + I(f_size^2) + I(f_size^3) +
  f_inc + I(f_inc^2) + I(f_inc^3) +
  f_land + I(f_land^2) + I(f_land^3) +
  Ex_forest + Ex_road + Ex_coal + cr +
  f(pro, model = "besagproper", graph = wood.nb.rs) 

formula_5 <- y ~ f_pop + I(f_pop^2) + I(f_pop^3) + 
  f_size + I(f_size^2) + I(f_size^3) +
  f_inc + I(f_inc^2) + I(f_inc^3) +
  f_land + I(f_land^2) + I(f_land^3) +
  Ex_forest + Ex_road + Ex_coal + cr +
  f(year, model = "ar",order = 2) +
  f(pro, model = "besagproper", graph = wood.nb.rs) 

formula_6 <- y ~ f_pop + I(f_pop^2) + I(f_pop^3) + 
  f_size + I(f_size^2) + I(f_size^3) +
  f_inc + I(f_inc^2) + I(f_inc^3) +
  f_land + I(f_land^2) + I(f_land^3) +
  Ex_forest + Ex_road + Ex_coal + cr +
  f(year, model = "ar",order = 2) +
  f(pro, model = "besagproper", graph = wood.nb.rs) +
  + f(pro2, year, model="iid", constr=TRUE)