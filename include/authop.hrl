%% @type authop() = {authdb, uid(), tokens(), realm(), name(), recovery(), roles()}
%% @type realm() = term(). Security realm for realmdb.erl
%% @type name() = term(). User Name of any form.
%% @type recovery() = {recotype(), term()}. Fallback type.
%% @type recotype() = email. Email address. Can be improved.
%% @type roles() = [term()]. Roles, assigned to the account.

-record(authop, {realm, name, recovery, roles}).
