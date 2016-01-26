#!/bin/bash
rebar3 compile
rebar3 release
./_build/default/rel/divepredictor/bin/divepredictor -noshell -noinput
