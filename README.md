divepredictor
=============

DivePredictor is an Erlang system that predicts when particular dive sites are safe to dive in.

This repo automatically deploys to Heroku from now on. It uses rebar3, and no longer requires a custom mad build chain.

To generate a release, run:
$rebar3 release, 

and then,
$./_build/default/rel/divepredictor/bin/divepredictor
