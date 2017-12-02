CREATE DATABASE gomoku;

ALTER DATABASE gomoku OWNER TO gomoku;

CREATE TABLE bot_stats (
  id SERIAL CONSTRAINT bot_stats_id_pkey PRIMARY KEY,
  name TEXT UNIQUE NOT NULL,
  wins_vs_humans BIGINT NOT NULL,
  wins_vs_bots BIGINT NOT NULL,
  losses_vs_humans BIGINT NOT NULL,
  losses_vs_bots BIGINT NOT NULL
);

ALTER TABLE bot_stats OWNER TO gomoku;

INSERT INTO bot_stats (name, wins_vs_humans, wins_vs_bots, losses_vs_humans, losses_vs_bots) VALUES
  ('BestNext', 0, 0, 0, 0),
  ('BlindGuess', 0, 0, 0, 0),
  ('Computer', 0, 0, 0, 0),
  ('HeadlessChicken', 0, 0, 0, 0),
  ('Hybrid', 0, 0, 0, 0),
  ('Kevin', 0, 0, 0, 0),
  ('KunkelOwen', 0, 0, 0, 0),
  ('LazyMinMax', 0, 0, 0, 0),
  ('Mikey', 0, 0, 0, 0),
  ('Mugatu', 0, 0, 0, 0),
  ('Notemotives', 0, 0, 0, 0),
  ('Rooster', 0, 0, 0, 0),
  ('SashankMichael', 0, 0, 0, 0),
  ('Sigma', 0, 0, 0, 0),
  ('TeamSinister', 0, 0, 0, 0),
  ('Theta', 0, 0, 0, 0);
