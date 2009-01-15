#!/bin/sh
cd `dirname $0`
# pretty dirty ;-) TODO: implement OTP logging
erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -sname enge2@localhost -s enge2 -noinput -mnesia dir db >>$PWD/log/enge2.log 2>&1
