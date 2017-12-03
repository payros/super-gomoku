CREATE DATABASE gomoku;

ALTER DATABASE gomoku OWNER TO gomoku;

CREATE TABLE bot_stats (
  id SERIAL CONSTRAINT bot_stats_id_pkey PRIMARY KEY,
  name TEXT UNIQUE NOT NULL,
  author TEXT NOT NULL,
  wins_vs_humans BIGINT NOT NULL,
  wins_vs_bots BIGINT NOT NULL,
  losses_vs_humans BIGINT NOT NULL,
  losses_vs_bots BIGINT NOT NULL,
  ties_vs_humans BIGINT NOT NULL,
  ties_vs_bots BIGINT NOT NULL
);

ALTER TABLE bot_stats OWNER TO gomoku;

INSERT INTO bot_stats (name, author, wins_vs_humans, wins_vs_bots, losses_vs_humans, losses_vs_bots, ties_vs_humans, ties_vs_bots) VALUES
  ('BestNext', 'nikivazou', 0, 0, 0, 0, 0, 0),
  ('BlindGuess', 'bmollot', 0, 0, 0, 0, 0, 0),
  ('Computer', 'nikivazou', 0, 0, 0, 0, 0, 0),
  ('HeadlessChicken', 'aputlock', 0, 0, 0, 0, 0, 0),
  ('Hybrid', 'jackastner', 0, 0, 0, 0, 0, 0),
  ('Kevin', 'kevchn', 0, 0, 0, 0, 0, 0),
  ('KunkelOwen', 'wkunkel', 0, 0, 0, 0, 0, 0),
  ('LazyMinMax', 'payros', 0, 0, 0, 0, 0, 0),
  ('Mikey', 'misaugstad', 0, 0, 0, 0, 0, 0),
  ('Mugatu', 'phicklephrodo', 0, 0, 0, 0, 0, 0),
  ('Notemotives', 'nikivazou', 0, 0, 0, 0, 0, 0),
  ('Rooster', 'rexledesma', 0, 0, 0, 0, 0, 0),
  ('SashankMichael', 'helloworld', 0, 0, 0, 0, 0, 0),
  ('Sigma', 'sw3dish', 0, 0, 0, 0, 0, 0),
  ('TeamSinister', 'sinistersnare', 0, 0, 0, 0, 0, 0),
  ('Theta', 'suteerth1', 0, 0, 0, 0, 0, 0);
