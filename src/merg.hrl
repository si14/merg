-record(mod, {name     :: atom(),
              path     :: string(),
              body     :: binary(),
              last_mod :: calendar:datetime()}).

-record(app, {name    :: atom(),
              app_src :: string(),
              mods    :: [string()]}).
