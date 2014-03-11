The files in this directory attempt to estimate a Bayesian version of the models used in [Sagarin ratings](http://www.usatoday.com/sports/ncaab/sagarin/). The two <model>s are 

- elo_chess (PURE_ELO on the website)
- predictor

The main files are

  - <model>_model.txt             the model written in Stan
  - <model>_mcmc.R                runs the mcmc for all seasons
  - <model>_team_statistics.R     summary statistics from mcmc
  - <modal>_matchup_statistics.R  matchup statistics from mcmc 
  
The remaining files are functions or scripts called from these files.
