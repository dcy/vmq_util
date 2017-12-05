-module(vmq_util).

-export([is_online/1, %is_online_/1,
         is_online_ql/1,
         is_register/1,
         get_register_queue_pid_/1,
         get_online_amount/0, get_all_amount/0,
         get_registers_info/0,
         sub_topics/2,
         %get_online_client_ids/0,
         unsub_topics/2,
         get_nodes/0]).


get_nodes() ->
    LocalNode = node(),
    [LocalNode | (vmq_cluster:nodes() -- [LocalNode])].

is_online(ClientId) ->
    is_online(get_nodes(), {[], ClientId}).

is_online([], _SubscriberId) ->
    false;
is_online([Node | Nodes], SubscriberId) when Node == node() ->
    case is_online_(SubscriberId) of
        true -> true;
        false -> is_online(Nodes, SubscriberId)
    end;
is_online([Node | Nodes], SubscriberId) ->
    case rpc:call(Node, ?MODULE, is_online_, [SubscriberId]) of
        true -> true;
        false -> is_online(Nodes, SubscriberId)
    end.

is_online_(SubscriberId) ->
    case vmq_queue_sup_sup:get_queue_pid(SubscriberId) of
        not_found ->
            false;
        QPid ->
            case vmq_queue:status(QPid) of
                {online, _Mode, _TotalStoredMsgs, _Sessions, _IsPlugin} -> true;
                _ -> false
            end
    end.

get_register_queue_pid(ClientId) ->
    get_register_queue_pid(get_nodes(), {[], ClientId}).

get_register_queue_pid([], _SubscriberId) ->
    undefined;
get_register_queue_pid([Node | Nodes], SubscriberId) when Node == node() ->
    case get_register_queue_pid_(SubscriberId) of
        undefined -> get_register_queue_pid(Nodes, SubscriberId);
        Pid -> Pid
    end;
get_register_queue_pid([Node | Nodes], SubscriberId) ->
    case rcp:call(Node, ?MODULE, get_register_queue_pid_, [SubscriberId]) of
        undefined -> get_register_queue_pid(Nodes, SubscriberId);
        Pid -> Pid
    end.

get_register_queue_pid_(SubscriberId) ->
    case vmq_queue_sup_sup:get_queue_pid(SubscriberId) of
        not_found -> undefined;
        QPid -> QPid
    end.

is_register(ClientId) ->
    case get_register_queue_pid(ClientId) of
        undefined -> false;
        _ -> true
    end.



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

get_online_amount() ->
    Q = "SELECT client_id FROM sessions WHERE is_online=true",
    Fun = fun(_Row, Acc) ->
                  Acc + 1
          end,
    vmq_ql_query_mgr:fold_query(Fun, 0, Q).

get_all_amount() ->
    Q = "SELECT client_id FROM sessions",
    Fun = fun(_Row, Acc) ->
                  Acc + 1
          end,
    vmq_ql_query_mgr:fold_query(Fun, 0, Q).

get_registers_info() ->
    Q = "SELECT client_id, is_online FROM sessions",
    Fun = fun(#{is_online := false}, {AllAcc, OnlineAcc}) ->
                  {AllAcc+1, OnlineAcc};
             (#{is_online := true}, {AllAcc, OnlineAcc}) ->
                  {AllAcc+1, OnlineAcc+1}
          end,
    {All, Online} = vmq_ql_query_mgr:fold_query(Fun, {0, 0}, Q),
    #{all => All, online => Online}.


%sub_topics(<<"test1">>, [{[<<"chat">>, <<"test1">>], 1}])
sub_topics(ClientId, Topics) ->
    case binary:match(ClientId, [<<"+">>, <<"#">>]) of
        nomatch ->
            SubscriberId = {[], ClientId},
            vmq_reg:subscribe(false, ClientId, SubscriberId, Topics);
        _ ->
            {error, contain_wildcards, ClientId}
    end.

%Topics = [[<<"chat">>, <<"test">>]]
unsub_topics(ClientId, Topics) ->
    vmq_reg:unsubscribe(false, ClientId, {[], ClientId}, Topics).
