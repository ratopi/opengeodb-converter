%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%% Code is under MIT License
%%% @end
%%% Created : 31. Jan 2022 21:35
%%%-------------------------------------------------------------------
-module(oc).
-author("RalfThomasPietsch").

%% API
-export([main/1]).

%%%===================================================================
%%% API
%%%===================================================================

main([]) ->
	io:fwrite("Usage: oc [<database file> ..]~n");

main(Files) ->
	lists:foreach(
		fun(File) -> convert(File) end,
		Files
	).

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert(File) ->
	case file:open(File, [read, binary]) of

		{error, enoent} ->
			io:fwrite("ERROR: ~p NOT FOUND~n", [File]);

		{error, Reason} ->
			io:fwrite("ERROR: ~p ~p~n", [File, Reason]);

		{ok, IN} ->
			io:fwrite("converting ~p~n", [File]),

			case file:read_line(IN) of

				{ok, <<$#, HeaderLine/binary>>} ->
					Headers = split_line(HeaderLine),
					{ok, OUT} = file:open(File ++ ".json", [write, binary]),
					file:write(OUT, <<"[">>),
					convert(IN, OUT, Headers, false),
					file:write(OUT, <<"]">>),
					file:close(OUT);

				_ ->
					io:fwrite("ERROR: ~p HAS NO HEADER LINE~n", [File])

			end,

			file:close(IN)

	end.


convert(IN, OUT, Headers, PrintComma) ->
	case file:read_line(IN) of
		eof ->
			ok;
		{ok, Line} ->
			case PrintComma of
				false -> ok;
				_ -> file:write(OUT, <<",">>)
			end,
			Values = split_line(Line),
			Map = convert_to_map(Headers, Values, #{}),
			file:write(OUT, jsx:encode(Map)),
			convert(IN, OUT, Headers, true)
	end.


split_line(Line) ->
	string:split(
		case string:split(Line, "\n") of
			[X, _] -> X;
			[X] -> X
		end,
		"\t",
		all
	).


% we have to be tolerant in converting lines.  Some lines missing the last entry.
convert_to_map([], _, Map) ->
	Map;
convert_to_map(_, [], Map) ->
	Map;
convert_to_map([K | KT], [V | VT], Map) ->
	convert_to_map(KT, VT, maps:put(K, V, Map)).
