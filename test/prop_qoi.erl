-module(prop_qoi).

-include_lib("proper/include/proper.hrl").

-export([ prop_qoi/1
        ]). % NOT auto-exported by PropEr, we must do it ourselves

prop_qoi(doc) ->
  %% Docs are shown when the test property fails
  "only properties that return `true' are seen as passing";
prop_qoi(opts) ->
  %% Override CLI and rebar.config option for `numtests' only
  [{numtests, 1000}].

prop_qoi() -> % auto-exported by Proper
  ?FORALL({W, H, F, CS, Img}, image_generator(),
    begin
      Qoi = erl_qoi:new(W, H, F, CS, Img),
      {ok, Encoded} = erl_qoi:encode(Qoi),
      {ok, Decoded} = erl_qoi:decode(Encoded),
      Qoi =:= Decoded
    end
  ).

image_generator() ->
  ?LET( {W, H, F, CS}
      , { pos_integer()
        , pos_integer()
        , noshrink(oneof([rgb, rgba]))
        , noshrink(oneof([alpha_linear, all_linear]))
        }
      , {W, H, F, CS, binary(H*W* case F of rgb -> 3; rgba -> 4 end)}
      ).
