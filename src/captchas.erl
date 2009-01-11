-module(captchas).
-compile(export_all).

password(Secret, Random, Alphabet, Letters) ->
    Request = [Secret, Random], % [Secret, Random, ":", Alphabet, ":", Letters],
    MD5 = erlang:md5(Request),
    {HeadLetters, _Tail} = split_binary(MD5, Letters),
    AbLength = length(Alphabet),
    %L = lists:map(fun(X) -> X rem AbLength end, binary_to_list(HeadLetters)),
    %io:format("~p - ~p - ~p - ~p - ~p - ~p~n",[Random, Request, MD5, HeadLetters, AbLength, L]),
    lists:map(fun(X) -> lists:nth(X rem AbLength + 1, Alphabet) end, binary_to_list(HeadLetters)).

make_href(User, Password, Width) ->
    Random = enge:guid(),
    Pw = password(Password, Random ,"abcdefghijklmnopqrstuvwxyz", 6),
    Link = ["http://image.captchas.net/?client=", User, "&random=", Random, "&width=", io_lib:format("~p", [Width])],
    {lists:flatten(Link), Pw}.

test_captchas() ->
    make_href("demo", "secret", 200).
