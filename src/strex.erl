%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc String support and parsers library

-module(strex).
-export([split_str/2, extract_meta/3, parse/2]).

%% @spec split_str(string(), string()) -> {First::string(), Second::string()} | string()
%% @doc Split Str::string() by Delim one time.
%%
%%      Returns the two parts around Delim or the original string itself.
split_str(Delim, Str) ->
    N = string:str(Str, Delim),
    if
        N > 0 ->
            {First, Second} = lists:split(N - 1, Str),
            Second1 = lists:nthtail(length(Delim), Second),
            {First, Second1};
        true -> Str
    end.

%% @spec extract_meta(string(), string(), string()) -> {Meta::string(), Before::string(), After::string()} | {none, Str::string(), []}
%% @doc Extract one string Meta between Start and End tokens.
%%
%%      Return Meta and the two parts of Str around Meta and delimiters.
%%      <br/>Function is not greedy.
extract_meta(Start, End, Str) ->
    
    case split_str(Start, Str) of
        {BeforeMeta, Rest} ->
            {Meta, AfterMeta} = split_str(End, Rest),
            {Meta, BeforeMeta, AfterMeta};
        _ ->
            {none, Str, ""}
    end.

%% @spec parse(string(), string()) -> list()
%% @doc Retunrns Str, splitted by Delim down to the list()
parse(Delim, Str) ->
    parse(Delim, Str, []).

parse(Delim, Str, Acc) ->
    N = string:str(Str, Delim),
    if
        N > 0 ->
            parse(Delim, lists:nthtail(N + length(Delim) - 1, Str), [lists:sublist(Str, N - 1) | Acc]);
        true ->
            lists:reverse([Str | Acc])
    end.

