%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by  <>
%%%-------------------------------------------------------------------
-module(erl_qoi_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("erl_qoi.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
  [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() ->
  [decode_qoi_logo_image_test_case
  ,decode_dice_image_test_case
  ,decode_kodim10_image_test_case
  ,decode_kodim23_image_test_case
  ,decode_testcard_image_test_case
  ,decode_testcard_rgba_image_test_case
  ,decode_wikipedia_008_image_test_case
  ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Test case info function - returns list of tuples to set
%%  properties for the test case.
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%
%% @spec TestCase() -> Info
%% @end
%%--------------------------------------------------------------------
decode_qoi_logo_image_test_case() ->
  [].

decode_dice_image_test_case() ->
  [].

decode_kodim10_image_test_case() ->
  [].

decode_kodim23_image_test_case() ->
  [].

decode_testcard_image_test_case() ->
  [].

decode_testcard_rgba_image_test_case() ->
  [].

decode_wikipedia_008_image_test_case() ->
  [].

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
decode_qoi_logo_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"qoi_logo.qoi").

decode_dice_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"dice.qoi").

decode_kodim10_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"kodim10.qoi").

decode_kodim23_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"kodim23.qoi").

decode_testcard_rgba_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"testcard_rgba.qoi").

decode_testcard_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"testcard.qoi").

decode_wikipedia_008_image_test_case(Config) ->
  logger:set_primary_config(level, info),
  DataDir = ?config(data_dir, Config),
  test_image(DataDir,"wikipedia_008.qoi").

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
test_image(DataDir,FileName) ->
  FilePath = filename:join(DataDir, FileName),
  {ok, Binary} = file:read_file(FilePath),
  ct:log("Decode ~s",[FilePath]),
  {ok, Img} = erl_qoi:decode(Binary),
  ct:log("Encode ~s",[FilePath]),
  {ok, Encoded} = erl_qoi:encode(Img),
  <<"qoif", Width:32, Height:32, Channels:8, CSpace:8, _Rest/binary>> = Binary,
  ct:log("~s W:~p H:~p C:~p CS:~p",[FilePath, Width, Height, Channels, CSpace]),
  PixList = to_bit_array(Img,Channels),
  ppm:write({bitmap,rgb,PixList,{Width, Height}}, iolist_to_binary(filename:basename(FileName, ".qoi")++".ppm")),
  ct:log("~s~nsize:         ~p~nsize erl_qoi: ~p~nsize qoi:     ~p",[FilePath, size(Img#qoi.pixels), size(Encoded), size(Binary)]),
  Binary = Encoded.

to_bit_array(#qoi{pixels=Pixels}, 4) ->
  array:from_list(
    lists:map(fun(<<R,G,B,_>>) -> <<R,G,B>> end
             ,split_packet(4, Pixels)
             )
  );
to_bit_array(#qoi{pixels=Pixels}, 3) ->
  array:from_list(split_packet(3, Pixels)).

split_packet(Size, P) when byte_size(P) >= Size ->
  {Chunk, Rest} = split_binary(P, Size),
  [Chunk|split_packet(Size, Rest)];
split_packet(_Size, <<>>) ->
  [];
split_packet(_Size, P) ->
  [P].
