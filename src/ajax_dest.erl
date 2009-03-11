-module(ajax_dest).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").
-include("destination.hrl").

new(Struct, Session, _Req) ->
    [Parent, Title, Anno] = obj:get_values(["parent", "title", "anno"], Struct),
    io:format("ajax_dest:new: ~p, ~p, ~p~n", [Parent, Title, Anno]),
    #session{opaque = #authkey{user = User = {U, D}}} = Session,
    UserCheck = list_to_binary([U, "@", D]),
    Parent1 = case Parent of
        UserCheck -> User;
        _ -> Parent
    end,
    io:format("Parent1: ~p~n", [Parent1]),
    case destination:new(User, Parent1, Title, Anno, #destprops{}) of
        {ok, Id} -> {{ok, {obj, [{"id", Id},
                                 {"parent", Parent},
                                 {"title", Title},
                                 {"anno", Anno}]}}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []};
        Err -> 
            io:format("ERROR: ~p~n", [Err]),
            {{fail, <<"ERROR">>}, []}
    end.

update(Struct, Session, _Req) ->
    Keys = ["id", "parent", "title", "annotation", "sub_allow", "read_apvd", "post_apvd", "sub_apvd", "sub_type"],
    [Id, Parent, Title, Anno, SubAllow, ReadApproved, PostApproved, SubApproved, SubType] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = User = {U, D}}} = Session,
    UserCheck = list_to_binary([U, "@", D]),
    Parent1 = case Parent of
        UserCheck -> User;
        _ -> Parent
    end,
    case destination:update(User, Id, Parent1, Title, Anno,
                            #destprops{sub_allow = SubAllow,
                                       read_apvd = ReadApproved,
                                       post_apvd = PostApproved,
                                       sub_apvd = SubApproved,
                                       sub_type = SubType})
    of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

remove(Struct, Session, _Req) ->
    Id = obj:get_value(<<"id">>, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:remove(U, Id) of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

% options: by owner, by name
destinations(_Struct, Session, _Req) ->
    #session{opaque = #authkey{user = U}} = Session,
    List = destination:destinations(U),
    {{ok, List}, []}.
