# [Super Gomoku](https://super-gomoku.herokuapp.com/)
## Web-based UI for Gomoku, implemented in Haskell.

### Demo
You can play the current version of Super Gomoku [here](https://super-gomoku.herokuapp.com/)

### Main Goals
When Completed, this project will let users:

- [x] Play Gomoku in three modes:

  * Human X Human (Local)
  * Human X Bot
  * Bot X Bot

- [x] Define the board dimensions and number of aligned pieces needed to win

### Strech Goals
Time and knowledge permitting, this project will:

- [x]  Save Browser Session (Configs, Score, Players)
- [x]  Persist Bot Stats (Wins, Ties, Losses)
- [ ]  Include Remote Human X Human

### Local Installation
If you would like to deploy this project on your local machine (and maybe come up with your own bot strategy), you can following these steps:

* Clone/Download the project
* install `postgres` and set local DB credentials under the environment variable `DATABASE_URL`
* Run `stack install` in the project's root directory
* Run `gomoku` (You may have to add the bin directory to your shell path)
* Open a browser window and visit `localhost:4000` or `127.0.0.1:4000`

### Text-based terminal version, implemented entirely in Haskell.
The text-based version is run in the terminal, using the (mostly) untouched original code written by [Niki Vazou](https://github.com/nikivazou). This version may be more useful for future students who are writing bots, since the entire implementation is in Haskell. In the web version, checking who wins and such is done in Javascript.

#### Local Installation
Installation is the same as for the web version, though the executable is `gomoku-text` instead of `gomoku`.

However, if you want to avoid installing `postgres`, you should be able to comment out the lines related to the `gomoku` executable in [gomoku.cabal](https://github.com/payros/super-gomoku/blob/master/gomoku.cabal). Then you should be able to run `stack install` and run `gomoku-text` without installing `postgres`. I haven't tested this, however.
