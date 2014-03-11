
# a function which converts season letters to years
# x is a vector containing the season letters
season_to_year <- function(x, n_seasons=19, last_year=2014){
  sy = data.frame(season=LETTERS[1:n_seasons], 
                  year=seq(to=last_year,length=n_seasons))
  
  sy$year[match(x,sy$season)]  
}
