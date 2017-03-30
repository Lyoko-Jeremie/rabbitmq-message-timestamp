%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Message Timestamp.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_timestamp_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-import(rabbit_misc, [format/2, protocol_error/3]).

-rabbit_boot_step({?MODULE,
                   [{description, "timestamp interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"timestamp interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"timestamp interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
    undefined.

description() ->
    [{description,
      <<"Adds current timestamp to messages as they enter RabbitMQ, And check them must have user_id.">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    case filter_exchange(DecodedContent, Method) of
        {ok, Content2} ->
            {Method, Content2};
        {error, Err} ->
            Err
    end;

intercept(Method, Content, _VHost) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
%%precondition_failed

% if exchange not start with 'cs-', bypass it
% otherwise, to check user_id
filter_exchange(Content, Method)
  when Method#'basic.publish'.exchange == <<'cs-'/utf8, _/binary>> ->
    check_user_id(Content, Method);
filter_exchange(Content, _Method) ->
    {ok, Content}.

% if user_id not set, failed the message
% otherwise, to add timestamp
check_user_id(#content{properties = Props} = _Content, Method)
  when Props#'P_basic'.user_id == undefined ->
    {error, precondition_failed("Error checking user_id in: ~p~n~p.", [Props, Method])};
check_user_id(Content, _Method) ->
    set_content_timestamp(Content).

% add timestamp to message
set_content_timestamp(#content{properties = Props} = Content) ->
    %% we need to reset properties_bin = none so the new properties
    %% get serialized when deliverying the message.
    Timestamp = os:system_time(seconds),
    {ok, Content#content{properties = Props#'P_basic'{timestamp = Timestamp},properties_bin = none}}.

precondition_failed(Format, QName) ->
    protocol_error(precondition_failed, Format, QName).
