%% session storage for accounts
-record(authkey, {user, realm, opaque}).
-define(AUTHKEY_TIMEOUT, 3600).
-define(CAPTCHA_TIMEOUT, 600).
