#!/bin/sh

node="enge2@localhost"

enge_cmd="erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -mnesia dir db -sname $node"

cd `dirname $0`
case $1 in
	prod) 
		# production mode (background, pretty dirty ;-) TODO: implement OTP logging
		nohup $enge_cmd -s enge2 -noinput >>$PWD/log/enge2.log 2>&1 &
	;;
	ctl)
		# attach to working instance (exit: c-c c-c)
		erl -sname ctl -remsh $node
	;;
	maint)
		# maintenance mode (not starting enge2)
		exec $enge_cmd
	;;
	dev)
		# development mode (foreground)
		exec $enge_cmd -s enge2
	;;
	*)
		echo usage: $0 [prod ctl maint dev]
		exit 1
	;;
esac
