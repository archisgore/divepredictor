#!/bin/bash

if [[ "$DATABASE_URL" == "" ]]; then
	echo "Please export DATABASE_URL to test locally."
	echo "I typically run mine like this:"
	echo "export DATABASE_URL=postgres://test@localhost/test"
	exit 1
fi

rebar3 release
./_build/default/rel/divepredictor/bin/divepredictor -noshell -noinput
