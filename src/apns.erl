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
  gen_server:call(Pid, {push, Input}).

% --------------------
% gen_server
% --------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Opts) ->
   process_flag(trap_exit, true),
  Env = proplists:get_value(env, Opts, development),
  Keyfile = proplists:get_value(env, Opts, token_keyfile),
  KeyId = erlang:list_to_binary(proplists:get_value(token_kid, Opts, "")),
  TeamId = erlang:list_to_binary(proplists:get_value(team_id, Opts, "")),
  Feedback = proplists:get_value(feedback, Opts, undefined),

  Headers0 = proplists:get_value(headers, Opts, []),

  Headers1 = maps:from_list(lists:map(fun({K, V}) -> {K, to_binary(V)} end, Headers0)),

  {ok, #{
    environment => Env,
    keyfile     => Keyfile,
    keyid       => KeyId,
    teamid      => TeamId,
    feedback    => Feedback,
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
      {server_name_indication, disable},
      {crl_check, false},
      {verify, verify_none}
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
    jwt       := Token
  } = State) ->
  
    #{
    device_id   := DeviceId,
    headers     := Headers0,
    payload     := Payload
  } = Input,


  Headers1 = add_apns_push_type(add_apns_id(Headers0)),
  Headers2 = maps:map(fun(_K, V) -> to_binary(V) end, Headers1),
  Headers3 = maps:merge(DefHeaders, Headers2),
  WHeaders0 = wire_headers(Headers3),

  WHeaders1 = WHeaders0#{
    <<"authorization">>   => <<"bearer ", Token/binary>>
  },

  Path = get_device_path(DeviceId),

  StreamRef = gun:post(ConnPid, Path, WHeaders1, jsx:encode(Payload)),
  
  case wait_for_response(ConnPid, StreamRef, MRef) of
    {ok, _} = OK ->
      {reply, OK, State};
    {error, _} = Err ->
      {stop, Err, retry, State}
  end;
handle_call(Request, _From, State) ->
  ?INFO_MSG("~p, handle_call unhandled: ~p~n", [?MODULE, Request]),
  {noreply, State}.
% --------------------
% internal
% --------------------

wait_for_response(ConnPid, StreamRef, MRef) ->
  case gun:await(ConnPid, StreamRef, 5000, MRef) of
    {response, fin, Status, Headers} -> 
        process_response(Status, Headers, no_body);
    {response, nofin, Status, Headers} ->
        case gun:await_body(ConnPid, StreamRef, 5000, MRef) of
          {ok, Body} -> process_response(Status, Headers, Body);
          {error, _} = E -> E
          end;      
    E -> E
  end.

parse_body(no_body) -> no_body;
parse_body(Body) when is_binary(Body) andalso (Body /= <<"">>) ->
  jsx:decode(Body, [return_maps, {labels, atom}]);
parse_body(_) -> no_body.

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