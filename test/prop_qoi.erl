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
  F = rgb,
  CS = alpha_linear,
  ?FORALL({W,H,Img}, image_generator(F),
    begin
      {ok, Qoi} = erl_qoi:encode(#{width => W, height => H, format => F, color_space => CS, pixels => Img}),
      {ok, #{width := Wd, height := Hd, format := F, color_space := CS, pixels := ImgD}} = erl_qoi:decode(Qoi),
      (Wd =:= W) and (Hd =:= H) and (Img =:= ImgD)
    end
  ).

prop_qoi_rgba() -> % auto-exported by Proper
  F = rgba,
  CS = alpha_linear,
  ?FORALL({W,H,Img}, image_generator(F),
    begin
      {ok, Qoi} = erl_qoi:encode(#{width => W, height => H, format => F, color_space => CS, pixels => Img}),
      {ok, #{width := Wd, height := Hd, format := F, color_space := CS, pixels := ImgD}} = erl_qoi:decode(Qoi),
      (Wd =:= W) and (Hd =:= H) and (Img =:= ImgD)
    end
  ).

image_generator(Format) ->
  ?SIZED(W, ?SIZED(H, {W,H,binary(H*W* case Format of rgb -> 3; rgba -> 4 end)})).
