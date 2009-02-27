-module(ajax_dest).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

new(Struct, Session, _Req) ->
    [Parent, Title, Anno] = obj:get_values(["parent", "title", "annotation"], Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:new(U, Parent, Title, Anno, #destprops{}) of
        {ok, Id} -> {{ok, Id}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

save(Struct, Session, _Req) ->
    Keys = ["id", "parent", "title", "annotation", "sub_allow", "read_apvd", "post_apvd", "sub_apvd", "sub_type"],
    [Id, Parent, Title, Anno, SubAllow, ReadApproved, PostApproved, SubApproved, SubType] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:save({U, Id}, #desprops{ sub_allow = SubAllow, read_apvd = ReadApproved, post_apvd = PostApproved, sub_apvd = SubApproved, sub_type = SubType}) of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

remove(Struct, Session, _Req) ->
    Id = obj:get_value(<<"id">>, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:remove({U, Id}) of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

% options: by owner, by name
destinations(Struct, Session, _Req) ->
    Order = obj:get_value(<<"order">>, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:destinations(U, Order) of
        {ok, List} -> {{ok, List}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.