-record(qoi,
  {width :: pos_integer()
  ,height :: pos_integer()
  ,format :: rgb|rgba
  ,color_space :: alpha_linear|all_linear
  ,pixels :: binary()
  }).

-opaque qoi() :: #qoi{}.
-export_type([qoi/0]).
