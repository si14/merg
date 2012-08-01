-record(mod, {name     :: atom(),
              app      :: atom(),
              file     :: string(),
              body     :: binary(),
              last_mod :: integer()}).

-record(app, {name         :: atom(),
              dir          :: string(),
              file         :: string(),
              description  :: string(),
              applications :: [atom()],
              version      :: string(),
              last_mod     :: integer()}).

-record(merg, {watch=false        :: boolean(),
               serve=false        :: boolean(),
               serve_port=8081    :: pos_integer(),
               doc_dir="doc_merg" :: string()}).
