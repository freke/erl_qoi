-module(erl_qoi).

-export([new/5
        ,encode/1
        ,decode/1
        ]).

-include("erl_qoi.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(in_range_2(Val), (-2 =< Val) and (Val =< 1)).
-define(in_range_4(Val), (-8 =< Val) and (Val =< 7)).
-define(in_range_6(Val), (-32 =< Val) and (Val =< 31)).
-define(diff_op(Rd, Gd, Bd), ?in_range_2(Rd) and ?in_range_2(Gd) and ?in_range_2(Bd)).
-define(luma_op(Rd, Gd, Bd), ?in_range_6(Gd) and ?in_range_4(Rd - Gd) and ?in_range_4(Bd - Gd)).

-spec new(Width::pos_integer(), Height::pos_integer(), Format::rgb|rgba, ColorSpace::alpha_linear|all_linear, Pixels::binary()) -> qoi().
new(Width, Height, Format, ColorSpace, Pixels) ->
  #qoi{width=Width, height=Height, format=Format, color_space=ColorSpace, pixels=Pixels}.

-spec encode(QOI::qoi()) -> {ok, binary()}.
encode(#qoi{width=Width, height=Height, format=Format, color_space=ColorSpace, pixels= Pixels}) ->
  Chunks = iolist_to_binary(encode_pixels(Pixels, Format)),
  F = case Format of rgb -> 3; rgba -> 4 end,
  CS = case ColorSpace of alpha_linear -> 0; all_linear -> 1 end,
  {ok, <<"qoif", Width:32, Height:32, F:8, CS:8, Chunks/binary, 0:(7*8),1:8>>}.

-spec decode(Binary::binary()) -> {ok, qoi()}.
decode(<<"qoif", Width:32, Height:32, Format:8, ColorSpace:8, Pixels/binary>>) ->
  F = case Format of 3 -> rgb; 4 -> rgba end,
  CS = case ColorSpace of 0 -> alpha_linear; 1 -> all_linear end,
  {ok
  , #qoi{width = Width
        ,height = Height
        ,format = F
        ,color_space = CS
        ,pixels = iolist_to_binary(decode_chunks(Pixels, F))
    }
  }.

encode_pixels(Pixels, Format) ->
  Prev = <<0, 0, 0, 255>>,
  RunLength = 0,
  Lut = #{},
  Acc = [],
  lists:reverse(do_encode(Pixels, Format, Prev, RunLength, Lut, Acc)).

do_encode(Pixels, Format, Prev, RunLength, Lut, Acc) when RunLength >= 62 ->
  logger:debug("RunLength reset"),
  RL = bias_run(RunLength),
  do_encode(Pixels, Format, Prev, 0, Lut, [<<3:2, RL:6>> |Acc]);

do_encode(<<Pixel:32, Rest/binary>>, rgba = F, <<Pixel:32>>, RunLength, Lut, Acc) ->
  logger:debug("Repet RGBA"),
  do_encode(Rest, F, <<Pixel:32>>, RunLength + 1, Lut, Acc);

do_encode(<<Pixel:24, Rest/binary>>, rgb = F, <<Pixel:24>>, RunLength, Lut, Acc) ->
  logger:debug("Repet RGB"),
  do_encode(Rest, F, <<Pixel:24>>, RunLength + 1, Lut, Acc);

do_encode(<<Pixel:32, Rest/binary>>, rgba = F, Prev, 0, Lut, Acc) ->
  logger:debug("New RGBA"),
  {Chunk, NewLut} = handle_non_running_pixel(<<Pixel:32>>, Prev, Lut),
  do_encode(Rest, F, <<Pixel:32>>, 0, NewLut, [Chunk | Acc]);

do_encode(<<RGB:24, Rest/binary>>, rgb = F, Prev, 0, Lut, Acc) ->
  logger:debug("New RGB"),
  Pixel = <<RGB:24, 255:8>>,
  {Chunk, NewLut} = handle_non_running_pixel(Pixel, Prev, Lut),
  do_encode(Rest, F, Pixel, 0, NewLut, [Chunk | Acc]);

do_encode(Pixels, Format, Prev, RunLength, Lut, Acc) when RunLength > 0 ->
  logger:debug("End of run"),
  RL = bias_run(RunLength),
  do_encode(Pixels, Format, Prev, 0, Lut, [<<3:2, RL:6>> | Acc]);

do_encode(<<>>, _Format, _Prev, 0, _Lut, Acc) ->
  logger:debug("Done no current run"),
  Acc;

do_encode(<<>>, _Format, _Prev, RunLength, _Lut, Acc) ->
  logger:debug("Done end current run"),
  RL = bias_run(RunLength),
  [<<3:2, RL:6>> | Acc].


decode_chunks(Pixels, Format) ->
  Prev = <<0, 0, 0, 255>>,
  Lut = #{},
  Acc = [],
  lists:reverse(do_decode(Pixels, Format, Prev, Lut, Acc)).

do_decode(<<0:(7*8),1:8>>, _Format, _Prev, _Lut, Acc) ->
  logger:debug("End marker"),
  Acc;

do_decode(<<254:8, R:8, G:8, B:8, Rest/binary>>, Format, Prev, Lut, Acc) ->
  logger:debug("rgb"),
  <<_:24, A:8>> = Prev,
  Pixel = <<R, G, B, A>>,
  do_decode(Rest, Format, Pixel, update_lut(Lut, Pixel), [maybe_drop_alpha(Pixel, Format) | Acc]);

do_decode(<<255:8, R:8, G:8, B:8, A:8, Rest/binary>>, Format, _Prev, Lut, Acc) ->
  logger:debug("rgba"),
  Pixel = <<R, G, B, A>>,
  do_decode(Rest, Format, Pixel, update_lut(Lut, Pixel), [maybe_drop_alpha(Pixel, Format) | Acc]);

do_decode(<<0:2, Index:6, Rest/binary>>, Format, _Prev, Lut, Acc) ->
  logger:debug("Index ~p", [Index]),
  Pixel = maps:get(Index, Lut, <<0:32>>),
  do_decode(Rest, Format, Pixel, Lut, [maybe_drop_alpha(Pixel, Format) | Acc]);

do_decode(<<1:2, Rd:2, Gd:2, Bd:2, Rest/binary>>, Format, <<Rp:8, Gp:8, Bp:8, A:8>>, Lut, Acc) ->
  logger:debug("Diff Rd:~p Rp:~p Gd:~p Gp:~p Bd:~p Bp:~p",[Rd,Rp,Gd,Gp,Bd,Bp]),
  R = Rp + unbias_diff(Rd),
  G = Gp + unbias_diff(Gd),
  B = Bp + unbias_diff(Bd),
  Pixel = <<R:8, G:8, B:8, A:8>>,
  do_decode(Rest, Format, Pixel, update_lut(Lut, Pixel), [maybe_drop_alpha(Pixel, Format) | Acc]);

do_decode(<<2:2, B_Gd:6, Rd_Gd:4, Bd_Gd:4, Rest/binary>>, Format, <<Rp:8, Gp:8, Bp:8, A:8>>, Lut, Acc) ->
  logger:debug("Luma B_Gd:~p Rd_Gd:~p Bd_Gd:~p",[B_Gd,Rd_Gd,Bd_Gd]),
  Gd = unbias_luma_dg(B_Gd),
  R = Rp + unbias_luma_dr_db(Rd_Gd) + Gd,
  G = Gp + Gd,
  B = Bp + unbias_luma_dr_db(Bd_Gd) + Gd,
  Pixel = <<R:8, G:8, B:8, A:8>>,
  do_decode(Rest, Format, Pixel, update_lut(Lut, Pixel), [maybe_drop_alpha(Pixel, Format) | Acc]);

do_decode(<<3:2, Count:6, Rest/binary>>, Format, Prev, Lut, Acc) ->
  logger:debug("Run"),
  Pixels = binary:copy(maybe_drop_alpha(Prev, Format), unbias_run(Count)),
  do_decode(Rest, Format, Prev, Lut, [Pixels | Acc]).

maybe_drop_alpha(Pixel, rgba) -> Pixel;
maybe_drop_alpha(<<R:8, G:8, B:8, _:8>>, rgb) -> <<R:8,G:8,B:8>>.

-ifdef(EUNIT).
maybe_drop_alpha_test_() ->
  W = <<255,255,255,255>>,
  B = <<0,0,0,0>>,
  [?_assertEqual(W, maybe_drop_alpha(W,rgba))
  ,?_assertEqual(B, maybe_drop_alpha(B,rgba))
  ,?_assertEqual(<<255,255,255>>, maybe_drop_alpha(W,rgb))
  ,?_assertEqual(<<0,0,0>>, maybe_drop_alpha(B,rgb))
  ].
-endif.

handle_non_running_pixel(Pixel, Prev, Lut) ->
  Index = index(Pixel),
  case maps:get(Index, Lut, <<0:32>>) of
    Pixel ->
      logger:debug("Pixel in index"),
      {<<0:2, Index:6>>, Lut};
    _ ->
      logger:debug("Add pixel to index"),
      Chunk = diff_luma_color(Pixel, Prev),
      NewLut = maps:put(Index, Pixel, Lut),
      {Chunk, NewLut}
  end.

diff_luma_color(<<R:8, G:8, B:8, A:8>>, <<Rp:8, Gp:8, Bp:8, A:8>>) when ?diff_op((R-Rp), (G-Gp), (B-Bp)) ->
  R_Rp = bias_diff(R - Rp),
  G_Gp = bias_diff(G - Gp),
  B_Bp = bias_diff(B - Bp),
  <<1:2, R_Rp:2, G_Gp:2, B_Bp:2>>;

diff_luma_color(<<R:8, G:8, B:8, A:8>>, <<Rp:8, Gp:8, Bp:8, A:8>>) when ?luma_op((R-Rp), (G-Gp), (B-Bp)) ->
  Gd = G - Gp,
  Rd_Gd = R - Rp - Gd,
  Bd_Gd = B - Bp - Gd,
  Gd_Luma = bias_luma_dg(Gd),
  Rd_Gd_Luma = bias_luma_dr_db(Rd_Gd),
  Bd_Gd_Luma = bias_luma_dr_db(Bd_Gd),
  <<2:2, Gd_Luma:6, Rd_Gd_Luma:4, Bd_Gd_Luma:4>>;

diff_luma_color(<<R:8, G:8, B:8, A:8>>, <<_RGB:24, A:8>>) ->
  <<254:8, R:8, G:8, B:8>>;

diff_luma_color(<<RGBA:32>>, _Prev) ->
  <<255:8, RGBA:32>>.

-ifdef(EUNIT).
diff_luma_color_test_() ->
  [?_assertMatch(<<1:2, 3:2, 1:2, 3:2>>, diff_luma_color(<<1,1,1,0>>, <<0,2,0,0>>))
  ,?_assertMatch(<<2:2, 34:6, 6:4, 7:4>>, diff_luma_color(<<0,4,1,0>>, <<0,2,0,0>>))
  ,?_assertMatch(<<254, 124, 124, 124>>, diff_luma_color(<<124,124,124,0>>, <<0,2,0,0>>))
  ,?_assertMatch(<<254, 0, 0, 6>>, diff_luma_color(<<0, 0, 6, 0>>, <<0, 2, 0, 0>>))
  ,?_assertMatch(<<255, 124, 124, 124, 54>>, diff_luma_color(<<124,124,124,54>>, <<0,2,0,0>>))
  ].
-endif.

update_lut(Lut, Pixel) ->
  LutIndex = index(Pixel),
  maps:put(LutIndex, Pixel, Lut).

index(<<R:8, G:8, B:8, A:8>>) ->
  (R * 3 + G * 5 + B * 7 + A * 11) rem 64.

bias_run(Val) -> Val - 1.
unbias_run(Val) -> Val + 1.

bias_diff(Val) -> Val + 2.
unbias_diff(Val) -> Val - 2.

bias_luma_dg(Val) -> Val + 32.
unbias_luma_dg(Val) -> Val - 32.

bias_luma_dr_db(Val) -> Val + 8.
unbias_luma_dr_db(Val) -> Val - 8.
