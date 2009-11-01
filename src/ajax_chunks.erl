-module(ajax_chunks).
-compile(export_all).

load(Struct, _Session, _Req) ->
    Base = enge2_conf:option(www_root),
    Chunks = collect_chunks(Struct, Base, []),
    case Chunks of
        {ok, Collected} ->
            {{ok, Collected}, []};
        {error, Reason} ->
            {{fail, list_to_binary(["Chunks collect error: ", Reason])}, []}
    end.

sane_filename(File) ->
    case re:run(File, "^[A-Za-z_0-9/]+\.?[A-Za-z_0-9]+$") of
        {match, _Captured} -> ok;
        _ -> false
    end.

collect_chunks([ChunkReq | T], Base, A) ->
    [Id, File] = engejson:get_values(["id", "file"], ChunkReq),
    io:format("collect_chunks: ~p:~p~n", [Id, File]),
    Chunk = case sane_filename(File) of
        ok ->
            case file:read_file(Base ++ binary_to_list(File)) of
                {ok, Data} -> Data;
                {error, _Reason} -> <<"">>
            end;
        _ -> <<"">>
    end,
    collect_chunks(T, Base, [[{id, Id}, {chunk, base64:encode(Chunk)}] | A]);

collect_chunks([], _Base, A) -> {ok, lists:reverse(A)}.
