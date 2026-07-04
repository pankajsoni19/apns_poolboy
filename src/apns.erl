-module(apns).

-author("Pankaj Soni <pankajsoni19@live.com>").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("logger.hrl").
%% API
-export([ push/2 ]).

%% poolboy_worker callback
-export([start_link/1]).

%% gen_server callbacks
-export([
  init/1,
  terminate/2,
  code_change/3,

  handle_cast/2,
  handle_call/3,
  handle_info/2,
  handle_continue/2
]).


% --------------------
% api
% --------------------
push(Pid, Input) ->
  gen_server:call(Pid, {push, Input}, infinity).

% --------------------
% gen_server
% --------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Opts) ->
  process_flag(trap_exit, true),
  Env = proplists:get_value(env, Opts, development),
  Keyfile = proplists:get_value(token_keyfile, Opts),
  KeyId = to_binary(proplists:get_value(token_kid, Opts, "")),
  TeamId = to_binary(proplists:get_value(team_id, Opts, "")),
  Feedback = proplists:get_value(feedback, Opts, undefined),
  RequestTimeout = proplists:get_value(request_timeout, Opts, 5000),

  Headers0 = proplists:get_value(headers, Opts, []),

  Headers1 = maps:from_list(lists:map(fun({K, V}) -> {K, to_binary(V)} end, Headers0)),

  {ok, #{
    environment => Env,
    keyfile     => Keyfile,
    keyid       => KeyId,
    teamid      => TeamId,
    feedback    => Feedback,
    request_timeout => RequestTimeout,
    connected   => false,
    headers     => Headers1,
    jwt         => undefined
  }, {continue, connect}}.

terminate(_, #{connected := true} = State) ->
  #{conn := ConnPid, monitor := MRef} = State,
  gun:close(ConnPid),
  demonitor(MRef);
terminate(_, _) -> ok.

code_change(_, State, _) ->
  {ok, State}.

handle_continue(connect, State0) ->
  Host = host(State0),
  {ok, ConnPid} = gun:open(Host, 443, #{
    connect_timeout   => 5000,
    protocols         => [http2],
    retry             => 0,
    % retry_fun         => retry_fun,
    http2_opts        => #{
      max_concurrent_streams  => 1
    },
    tls_opts          => [
      {server_name_indication, Host},
      {verify, verify_peer},
      {cacerts, public_key:cacerts_get()},
      {customize_hostname_check, [
        {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
      ]}
    ]
  }),
  MRef = monitor(process, ConnPid),
  {ok, http2} = gun:await_up(ConnPid, 5000, MRef),
  {noreply, State0#{
    conn        => ConnPid,
    connected   => true,
    monitor     => MRef
  }, {continue, refresh_token}};

handle_continue(refresh_token, 
  #{
    keyid       := KeyId,
    teamid      := TeamId,
    keyfile     := Keyfile
  } = State0) ->
  Token = apns_utils:generate_token(KeyId, TeamId, Keyfile),

  erlang:send_after(3000 * 1000, self(), refresh_token),

  {noreply, State0#{
    jwt     => Token
  }};
handle_continue(Request, State) ->
  ?INFO_MSG("~p, handle_continue unhandled: ~p~n", [?MODULE, Request]),
  {noreply, State}.

handle_info(refresh_token, State) ->
  {noreply, State, {continue, refresh_token}};
handle_info({'DOWN', MRef, process, _ConnPid, Reason}, State0) ->
  demonitor(MRef),
  {stop, Reason, State0#{connected => false}};
handle_info(Info, State) ->
  ?INFO_MSG("~p, handle_info unhandled: ~p~n", [?MODULE, Info]),
  {noreply, State}.

handle_cast(Request, State) ->
  ?INFO_MSG("~p, handle_cast unhandled: ~p~n", [?MODULE, Request]),
  {noreply, State}.

handle_call({push, Input}, _From, #{
    conn      := ConnPid,
    headers   := DefHeaders,
    monitor   := MRef,
    jwt       := Token,
    request_timeout := RequestTimeout
  } = State) ->
  
    #{
    device_id   := DeviceId,
    headers     := Headers0,
    payload     := Payload
  } = Input,


  Headers1 = maps:merge(DefHeaders, Headers0),
  Headers2 = add_apns_push_type(add_apns_id(Headers1)),
  Headers3 = maps:map(fun(_K, V) -> to_binary(V) end, Headers2),
  WHeaders0 = wire_headers(Headers3),

  WHeaders1 = WHeaders0#{
    <<"authorization">> => <<"bearer ", Token/binary>>,
    <<"content-type">>  => <<"application/json">>
  },

  Path = get_device_path(DeviceId),

  StreamRef = gun:post(ConnPid, Path, WHeaders1, jsx:encode(Payload)),
  
  case wait_for_response(ConnPid, StreamRef, MRef, RequestTimeout) of
    {ok, _} = OK ->
      {reply, OK, State};
    {error, _} ->
      gun:cancel(ConnPid, StreamRef),
      {reply, retry, State}
  end;
handle_call(Request, _From, State) ->
  ?INFO_MSG("~p, handle_call unhandled: ~p~n", [?MODULE, Request]),
  {reply, {error, {unsupported_call, Request}}, State}.
% --------------------
% internal
% --------------------

wait_for_response(ConnPid, StreamRef, MRef, Timeout) ->
  case gun:await(ConnPid, StreamRef, Timeout, MRef) of
    {response, fin, Status, Headers} -> 
        process_response(Status, Headers, no_body);
    {response, nofin, Status, Headers} ->
        case gun:await_body(ConnPid, StreamRef, Timeout, MRef) of
          {ok, Body} -> process_response(Status, Headers, Body);
          {error, _} = E -> E
          end;      
    E -> E
  end.

parse_body(no_body) -> no_body;
parse_body(Body) when is_binary(Body) andalso (Body /= <<"">>) ->
  try jsx:decode(Body, [return_maps]) of
    Decoded when is_map(Decoded) ->
      normalize_response_body(Decoded);
    Decoded ->
      #{raw_body => Body, decoded_body => Decoded}
  catch
    _:_ -> #{raw_body => Body}
  end;
parse_body(_) -> no_body.

normalize_response_body(Body0) ->
  Body1 = case maps:take(<<"reason">>, Body0) of
    {Reason, Rest1} -> Rest1#{reason => Reason};
    error -> Body0
  end,
  case maps:take(<<"timestamp">>, Body1) of
    {Timestamp, Rest2} -> Rest2#{timestamp => Timestamp};
    error -> Body1
  end.

parse_headers(Headers) ->
  M0 = maps:from_list(Headers),
  M1 = maps:with([<<"apns-id">>, <<"apns-unique-id">>], M0),
  #{
    apns_id         => maps:get(<<"apns-id">>, M1, <<"">>),
    apns_unique_id  => maps:get(<<"apns-unique-id">>, M1, undefined)
  }.

process_response(Status, Headers, Body) ->
  {ok, {
    Status, 
    parse_headers(Headers), 
    parse_body(Body)
  }}.

host(#{environment := sandbox}) ->
  "api.sandbox.push.apple.com";
host(#{environment := development}) ->
  "api.sandbox.push.apple.com";
host(_) -> 
  "api.push.apple.com".

get_device_path(DeviceId) ->
  <<"/3/device/", DeviceId/binary>>.

wire_headers(Headers) ->
  List = [ {<<"apns-id">>, apns_id}
         , {<<"apns-expiration">>, apns_expiration}
         , {<<"apns-priority">>, apns_priority}
         , {<<"apns-topic">>, apns_topic}
         , {<<"apns-collapse-id">>, apns_collapse_id}
         , {<<"apns-push-type">>, apns_push_type}
         ],
  F = fun({ActualHeader, Key}) ->
    case maps:find(Key, Headers) of
      error -> [];
      {ok, Value} -> [{ActualHeader, Value}]
    end
  end,
  lists:flatmap(F, List).

add_apns_id(Headers) ->
  case maps:is_key(apns_id, Headers) of
    false -> 
      Headers#{
          apns_id => uuid:uuid_to_string(uuid:get_v4(), binary_standard)
      };
    _ -> 
      Headers
  end.

add_apns_push_type(Headers) ->
  case maps:is_key(apns_push_type, Headers) of
    false -> 
      Headers#{
          apns_push_type => alert
      };
    _ -> 
      Headers
  end.

to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A);
to_binary(B) -> B.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

malformed_response_body_test() ->
  ?assertEqual(#{raw_body => <<"not json">>}, parse_body(<<"not json">>)).

response_body_keys_test() ->
  ?assertEqual(
    #{reason => <<"BadDeviceToken">>, timestamp => 123},
    parse_body(<<"{\"reason\":\"BadDeviceToken\",\"timestamp\":123}">>)
  ).

default_push_type_is_preserved_test() ->
  Defaults = #{apns_push_type => <<"background">>},
  Headers = add_apns_push_type(add_apns_id(maps:merge(Defaults, #{}))),
  ?assertEqual(<<"background">>, maps:get(apns_push_type, Headers)).

request_push_type_overrides_default_test() ->
  Defaults = #{apns_push_type => <<"background">>},
  Request = #{apns_push_type => alert},
  Headers = add_apns_push_type(add_apns_id(maps:merge(Defaults, Request))),
  ?assertEqual(alert, maps:get(apns_push_type, Headers)).

production_host_test() ->
  ?assertEqual("api.push.apple.com", host(#{environment => production})).

development_host_test() ->
  ?assertEqual("api.sandbox.push.apple.com", host(#{environment => development})).

token_keyfile_config_test() ->
  {ok, State, {continue, connect}} = init([
    {env, production},
    {token_keyfile, "AuthKey_TEST.p8"}
  ]),
  ?assertEqual("AuthKey_TEST.p8", maps:get(keyfile, State)).

-endif.
