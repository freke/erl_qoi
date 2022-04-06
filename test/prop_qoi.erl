-module(prop_qoi).
-include_lib("proper/include/proper.hrl").
-export([prop_qoi_rgb/1
        ,prop_qoi_rgba/1
        ]). % NOT auto-exported by PropEr, we must do it ourselves

prop_qoi_rgb(doc) ->
  %% Docs are shown when the test property fails
  "only properties that return `true' are seen as passing";
prop_qoi_rgb(opts) ->
  %% Override CLI and rebar.config option for `numtests' only
  [{numtests, 500}].

prop_qoi_rgba(doc) ->
  %% Docs are shown when the test property fails
  "only properties that return `true' are seen as passing";
prop_qoi_rgba(opts) ->
  %% Override CLI and rebar.config option for `numtests' only
  [{numtests, 500}].

prop_qoi_rgb() -> % auto-exported by Proper
  C = rgb,
  ?FORALL({W,H,Img}, image_generator(C),
    begin
      {ok, Qoi} = erl_qoi:encode(#{w => W, h => H, c => C, cs => 0, pixels => Img}),
      {ok, #{w := Wd, h := Hd, c := C, cs := 0, pixels := ImgD}} = erl_qoi:decode(Qoi),
      (Wd =:= W) and (Hd =:= H) and (Img =:= ImgD)
    end
  ).

prop_qoi_rgba() -> % auto-exported by Proper
  C = rgba,
  ?FORALL({W,H,Img}, image_generator(C),
    begin
      {ok, Qoi} = erl_qoi:encode(#{w => W, h => H, c => C, cs => 0, pixels => Img}),
      {ok, #{w := Wd, h := Hd, c := C, cs := 0, pixels := ImgD}} = erl_qoi:decode(Qoi),
      (Wd =:= W) and (Hd =:= H) and (Img =:= ImgD)
    end
  ).

image_generator(C) ->
  ?SIZED(W, ?SIZED(H, {W,H,binary(H*W* case C of rgb -> 3; rgba -> 4 end)})).
