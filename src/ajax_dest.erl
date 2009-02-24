-module(ajax_dest).
-compile(export_all).
-include("session.hrl").
-include("authdb.hrl").

% XXX restrict export
new_dest(Struct, Session, _Req) ->
    Keys = ["name", "parent"],
    [Title, ParentId] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:new_dest(U, ParentId, Title) of % {ok, id()} | {fail, reason()}
        {ok, Id} -> {{ok, list_to_binary(Id)}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

remove_dest(Struct, Session, _Req) ->
    Id = obj:get_value(<<"id">>, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:remove_dest(U, Id) of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

save_dest(Struct, Session, _Req) ->
    Keys = ["id", "name", "annotation", "subscribe_type", "read_approved", "post_approved", "subscr_approved", "subscr_type"],
    [Id, Title, Anno, SubscrType, ReadApproved, PostApproved, SubscrApproved, SubscrType] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:save_dest(U, Id, Title, Anno, SubscrType, ReadApproved, PostApproved, SubscrApproved, SubscrType) of
        ok -> {{ok, []}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.

% options: by owner, by name
list_dests(Struct, Session, _Req) ->
    Order = obj:get_value(<<"order">>, Struct),
    #session{opaque = #authkey{user = U}} = Session,
    case destination:list_dests(U, Order) of
        {ok, List} -> {{ok, List}, []};
        {error, Reason} -> {{fail, list_to_binary(Reason)}, []}
    end.
