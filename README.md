# Super Gomoku
## Web-based UI for Gomoku, implemented in Haskell.

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

### Installation
Because this project is currently in early development, it's not deployed on a public server. If you would like to try it out, you can deploy it locally by following these steps:

* Clone/Download the project
* Run `stack install` in the project's root directory
* Run `gomoku` (You may have to add the bin directory to your shell path)
* Open a browser window and visit `localhost:3000` or `127.0.0.1:3000`
