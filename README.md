# [Super Gomoku](https://super-gomoku.herokuapp.com/)
## Web-based UI for Gomoku, implemented in Haskell.

### Demo
## You can play the current version of Super Gomoku [here](https://super-gomoku.herokuapp.com/)

### Main Goals
When Completed, this project will let users:

* Play Gomoku in three modes:

  - [x] Human X Human (Local)
  - [x] Human X Bot
  - [x] Bot X Bot

* Define the board dimensions and number of aligned pieces needed to win

### Strech Goals
Time and knowledge permitting, this project will:

*  Save Browser Session (Configs, Score, Players)
*  Persist Bot Stats (Wins, Ties, Losses)
*  Include Remote Human X Human

### Local Installation
If you would like to deploy this project on your local machine (and maybe come up with your own bot strategy), you can following these steps:

* Clone/Download the project
* Run `stack install` in the project's root directory
* Run `gomoku` (You may have to add the bin directory to your shell path)
* Open a browser window and visit `localhost:4000` or `127.0.0.1:4000`
