-module(vmq_util).

-export([is_online/1, is_online_ql/1,
         is_register/1,
         get_online_amount/0, get_all_amount/0,
         get_registers_info/0, 
         disconnect/1,
         sub_topic/2, sub_topics/2,
         unsub_topic/2, unsub_topics/2,
         get_subed_topics/1]).

-export([get_register_queue_pid_/1,
         get_register_queue_pid/1,
         do_sub_topics/2, do_unsub_topics/2
        ]).


-spec is_online(Id :: binary() | tuple()) -> true | false.
is_online(Id) ->
    case get_register_queue_pid(Id) of
        undefined ->
            false;
        QPid ->
            case vmq_queue:status(QPid) of
                {online, _Mode, _TotalStoredMsgs, _Sessions, _IsPlugin} -> true;
                _ -> false
            end
    end.


get_register_queue_pid(ClientId) when is_binary(ClientId) ->
    get_register_queue_pid(get_nodes(), {[], ClientId});
get_register_queue_pid(SubscriberId) when is_tuple(SubscriberId) ->
    get_register_queue_pid(get_nodes(), SubscriberId).

get_register_queue_pid([], _SubscriberId) ->
    undefined;
get_register_queue_pid([Node | Nodes], SubscriberId) when Node == node() ->
    case get_register_queue_pid_(SubscriberId) of
        undefined -> get_register_queue_pid(Nodes, SubscriberId);
        Pid -> Pid
    end;
get_register_queue_pid([Node | Nodes], SubscriberId) ->
    case rpc:call(Node, ?MODULE, get_register_queue_pid_, [SubscriberId]) of
        undefined -> get_register_queue_pid(Nodes, SubscriberId);
        Pid -> Pid
    end.

get_register_queue_pid_(SubscriberId) ->
    case vmq_queue_sup_sup:get_queue_pid(SubscriberId) of
        not_found -> undefined;
        QPid -> QPid
    end.


-spec is_register(Id :: binary() | tuple()) -> true | false.
%is_register(Id) ->
%    case get_register_queue_pid(Id) of
%        undefined -> false;
%        _ -> true
%    end.
is_register(ClientId) when is_binary(ClientId) ->
    is_register({[], ClientId});
is_register(SubscriberId) when is_tuple(SubscriberId) ->
    case vmq_subscriber_db:read(SubscriberId) of
        undefined -> false;
        _ -> true
    end.


-spec is_online_ql(binary()) -> true | false.
is_online_ql(ClientId) ->
    Q = <<"SELECT client_id FROM sessions WHERE is_online=true AND client_id=\"", ClientId/binary, "\"">>,
    Fun = fun(Row, Acc) ->
                  case Row of
                      #{client_id := ClientId} -> [ClientId | Acc];
                      _ -> Acc
                  end
          end,
    case vmq_ql_query_mgr:fold_query(Fun, [], Q) of
        [] -> false;
        [ClientId] -> true
    end.


-spec get_online_amount() -> integer().
get_online_amount() ->
    Q = "SELECT client_id FROM sessions WHERE is_online=true",
    Fun = fun(_Row, Acc) ->
                  Acc + 1
          end,
    vmq_ql_query_mgr:fold_query(Fun, 0, Q).


-spec get_all_amount() -> integer().
get_all_amount() ->
    Q = "SELECT client_id FROM sessions",
    Fun = fun(_Row, Acc) ->
                  Acc + 1
          end,
    vmq_ql_query_mgr:fold_query(Fun, 0, Q).


-type register_info() :: #{all => integer(), online => integer()}.
-spec get_registers_info() -> register_info().
get_registers_info() ->
    Q = "SELECT client_id, is_online FROM sessions",
    Fun = fun(#{is_online := false}, {AllAcc, OnlineAcc}) ->
                  {AllAcc+1, OnlineAcc};
             (#{is_online := true}, {AllAcc, OnlineAcc}) ->
                  {AllAcc+1, OnlineAcc+1}
          end,
    {All, Online} = vmq_ql_query_mgr:fold_query(Fun, {0, 0}, Q),
    #{all => All, online => Online}.


-spec disconnect(Id :: binary() | tuple()) -> ok | ignore.
disconnect(ClientId) when is_binary(ClientId) ->
    disconnect({[], ClientId});
disconnect(SubscriberId) when is_tuple(SubscriberId) ->
    case get_register_queue_pid(SubscriberId) of
        undefined ->
            ignore;
        QPid ->
            SessionPids = vmq_queue:get_sessions(QPid),
            [Pid ! disconnect || Pid <- SessionPids],
            ok
    end.


-type topic() :: {binary(), integer()}.
-type topics() :: list(topic()).
-spec sub_topic(Id :: binary() | tuple(), Topic :: topic()) -> ok | {error, any()}.
%%sub_topic(<<"test1">>, {<<"inbox/test1">>, 1}).
sub_topic(Id, Topic) when is_tuple(Topic) ->
    sub_topics(Id, [Topic]).


-spec sub_topics(Id :: binary() | tuple(), Topics :: topics()) -> ok | {error, any()}.
%%sub_topics(<<"test1">>, [{<<"inbox/test1">>, 1}]).
sub_topics(ClientId, Topics) when is_binary(ClientId) ->
    sub_topics({[], ClientId}, Topics);
sub_topics(SubscriberId, Topics) when is_tuple(SubscriberId) andalso is_list(Topics) ->
    case validate_sub_topics(Topics) of
        {error, Reason} -> {error, Reason};
        {ok, ValidatedTopics} -> handle_sub_topics(SubscriberId, ValidatedTopics)
    end.


validate_sub_topics(Topics) ->
    validate_sub_topics(Topics, []).

validate_sub_topics([], AccTopics) ->
    {ok, AccTopics};
validate_sub_topics([{Topic, QoS} | Topics], AccTopics) when is_binary(Topic) ->
    case vmq_topic:validate_topic(subscribe, Topic) of
        {ok, ValidatedTopic} ->
            validate_sub_topics(Topics, [{ValidatedTopic, QoS} | AccTopics]);
        {error, Reason} ->
            {error, Reason}
    end.


validate_unsub_topics(Topics) ->
    validate_unsub_topics(Topics, []).

validate_unsub_topics([], AccTopics) ->
    {ok, AccTopics};
validate_unsub_topics([Topic | Topics], AccTopics) when is_binary(Topic) ->
    case vmq_topic:validate_topic(publish, Topic) of
        {ok, ValidatedTopic} ->
            validate_unsub_topics(Topics, [ValidatedTopic | AccTopics]);
        {error, Reason} ->
            {error, Reason}
    end.


handle_sub_topics(SubscriberId, Topics) ->
    case vmq_subscriber_db:read(SubscriberId) of
        undefined ->
            no_the_subscriber_id;
        [{Node, _, _}] ->
            case Node == node() of
                true ->
                    do_sub_topics(SubscriberId, Topics);
                false ->
                    case rpc:call(Node, ?MODULE, do_sub_topics, [SubscriberId, Topics]) of
                        subscriber_node_changed -> handle_sub_topics(SubscriberId, Topics);
                        ok -> ok
                    end
            end
    end.


do_sub_topics({_, ClientId} = SubscriberId, Topics) ->
    [{Node, _, _}] = vmq_subscriber_db:read(SubscriberId),
    case Node == node() of
        true ->
            vmq_reg:subscribe(false, ClientId, SubscriberId, Topics),
            ok;
        false ->
            subscriber_node_changed
    end.


do_unsub_topics({_, ClientId} = SubscriberId, Topics) ->
    [{Node, _, _}] = vmq_subscriber_db:read(SubscriberId),
    case Node == node() of
        true ->
            vmq_reg:unsubscribe(false, ClientId, SubscriberId, Topics),
            ok;
        false ->
            subscriber_node_changed
    end.


-spec unsub_topic(Id :: binary() | tuple(), Topic :: binary()) -> ok | {error, any()}.
%%unsub_topic(<<"test1">>, <<"inbox/test">>).
unsub_topic(Id, Topic) when is_binary(Topic) ->
    unsub_topics(Id, [Topic]).


-spec unsub_topics(Id :: binary() | tuple(), Topics :: topics()) -> ok | {error, any()}.
unsub_topics(ClientId, Topics) when is_binary(ClientId) ->
    unsub_topics({[], ClientId}, Topics);
unsub_topics(SubscriberId, Topics) when is_tuple(SubscriberId) andalso is_list(Topics) ->
    case validate_unsub_topics(Topics) of
        {error, Reason} -> {error, Reason};
        {ok, ValidatedTopics} -> handle_unsub_topics(SubscriberId, ValidatedTopics)
    end.


handle_unsub_topics(SubscriberId, Topics) ->
    case vmq_subscriber_db:read(SubscriberId) of
        undefined ->
            no_the_subscriber_id;
        [{Node, _, _}] ->
            case Node == node() of
                true ->
                    do_unsub_topics(SubscriberId, Topics);
                false ->
                    case rpc:call(Node, ?MODULE, do_unsub_topics, [SubscriberId, Topics]) of
                        subscriber_node_changed -> handle_unsub_topics(SubscriberId, Topics);
                        ok -> ok
                    end
            end
    end.

-spec get_subed_topics(Id :: binary() | tuple()) -> list({topic(), integer()}).
get_subed_topics(ClientId) when is_binary(ClientId) ->
    get_subed_topics({[], ClientId});
get_subed_topics(SubscriberId) when is_tuple(SubscriberId) ->
    case vmq_subscriber_db:read(SubscriberId) of
        undefined -> [];
        [{_, _, Topics}] -> Topics
    end.


get_nodes() ->
    LocalNode = node(),
    [LocalNode | (vmq_cluster:nodes() -- [LocalNode])].
