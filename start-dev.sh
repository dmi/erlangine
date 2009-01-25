#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s enge2 -mnesia dir db -sname enge2@localhost
#exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -mnesia dir db -sname enge2@localhost
