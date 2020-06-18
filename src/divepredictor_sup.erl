%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%    This file is part of DivePredictor.
%
%    DivePredictor is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    DivePredictor is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-module(divepredictor_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case cowboy:start_http(http, 100, [{port, wf:config(n2o,port,port())}],
            [
                {env, [{dispatch, dispatch_rules()}]},
                {onrequest, fun ?MODULE:https_redirection_hook/1}
            ]) of
        {ok, _} -> ok;
        {error,{{_,{_,_,{X,_}}},_}} -> io:format("Can't start Web Server: ~p\r\n",[X]), halt(abort,[]);
        X -> io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[]) end,
    Children=[
        {tidesandcurrents_sup, {tidesandcurrents_sup, start_link, []}, permanent, infinity, supervisor, [tidesandcurrents]},
        {database_sup, {database_sup, start_link, []}, permanent, infinity, supervisor, [database]}
        ],
    {ok, {{one_for_one, 5, 10}, Children}}.

mime() -> [{mimetypes,cow_mimetypes,all}].

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", n2o_dynalo, {dir, "priv/static", mime()}},
            {"/n2o/[...]", n2o_dynalo, {dir, "deps/n2o/priv", mime()}},
            {"/rest/:resource", rest_cowboy, []},
            {"/rest/:resource/:id", rest_cowboy, []},
            {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
            {'_', n2o_cowboy, []}
    ]}]).

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.

https_redirection_hook(Req) ->
    {FwdProto, Req2} = cowboy_req:header(<<"x-forwarded-proto">>, Req),
    case FwdProto of
        <<"https">> -> Req2;
        _ -> io:fwrite("NOT HTTPS: ~p~n", [FwdProto]),
            {OrigUrl, Req3} = cowboy_req:url(Req),
            io:fwrite("OrigUrl: ~p~n", [OrigUrl]),
            TargetUrl = binary:replace(OrigUrl, <<"http://">>, <<"https://">>),
            io:fwrite("TargetUrl: ~p~n", [TargetUrl]),
            Req4 = cowboy_req:reply(301, [
                {<<"location">>, TargetUrl}
            ], Req3),
            Req4
    end.