vmq_util
=====

## Doc
- [is_register/1](#is_register)
- [is_online/1](#is_online)
- [disconnect/1](#disconnect)
- [get_online_amount/0](#get_online_amount)
- [get_all_amount/0](#get_all_amount)
- [get_registers_info/0](#get_registers_info)
- [sub_topic/2](#sub_topic)
- [sub_topics/2](#sub_topics)
- [unsub_topic/2](#unsub_topics)
- [unsub_topics/2](#unsub_topics)
- [get_subed_topics/1](#get_subed_topics)

## is_register
``` 
-spec is_register(Id :: binary() | tuple()) -> true | false.

(dcy_vmq@192.168.3.236)38> vmq_util:is_register(<<"test1">>).
true
(dcy_vmq@192.168.3.236)39> vmq_util:is_register({[], <<"test100">>}).
false
```

## is_online
```
-spec is_online(Id :: binary() | tuple()) -> true | false.

(dcy_vmq@192.168.3.236)50> vmq_util:is_online(<<"test1">>).
false
(dcy_vmq@192.168.3.236)51> vmq_util:is_online({[], <<"test6">>}).
true
```

## disconnect
```
-spec disconnect(Id :: binary() | tuple()) -> ok | ignore.

(dcy_vmq@192.168.3.236)56> vmq_util:disconnect({[], <<"test6">>}).
ok
(dcy_vmq@192.168.3.236)57> vmq_util:disconnect(<<"test1">>).      
ok
```

## get_online_amount
```
-spec get_online_amount() -> integer().

(dcy_vmq@192.168.3.236)58> vmq_util:get_online_amount().
1
```

## get_all_amount
```
-spec get_all_amount() -> integer().

(dcy_vmq@192.168.3.236)59> vmq_util:get_all_amount().   
3
```

## get_registers_info
```
-type register_info() :: #{all => integer(), online => integer()}.
-spec get_registers_info() -> register_info().

(dcy_vmq@192.168.3.236)60> vmq_util:get_registers_info().
#{all => 3,online => 1}
```

## sub_topic
```
-type topic() :: {binary(), integer()}.
-type topics() :: list(topic()).
-spec sub_topic(Id :: binary() | tuple(), Topic :: topic()) -> ok | {error, any()}.
%%sub_topic(<<"test1">>, {<<"inbox/test1">>, 1}).

(dcy_vmq@192.168.3.236)61> vmq_util:sub_topic(<<"test6">>, {<<"inbox/test6">>, 1}).
ok
(dcy_vmq@192.168.3.236)62> vmq_util:sub_topic({[], <<"test8">>}, {<<"inbox/test8">>, 1}).
no_the_subscriber_id
(dcy_vmq@192.168.3.236)63> vmq_util:sub_topic({[], <<"test6">>}, {<<"inbox/test8">>, 1}).
ok
```

## sub_topics
```
-spec sub_topics(Id :: binary() | tuple(), Topics :: topics()) -> ok | {error, any()}.
%%sub_topics(<<"test1">>, [{<<"inbox/test1">>, 1}]).

(dcy_vmq@192.168.3.236)65> vmq_util:sub_topics(<<"test6">>, [{<<"inbox/test1">>, 1}, {<<"inbox/test2">>, 1}]).
ok
(dcy_vmq@192.168.3.236)66> vmq_util:sub_topics({[], <<"test6">>}, [{<<"inbox/test1">>, 1}, {<<"inbox/test2">>, 1}]).
ok
```

## unsub_topic
```
-spec unsub_topic(Id :: binary() | tuple(), Topic :: binary()) -> ok | {error, any()}.
%%unsub_topic(<<"test1">>, <<"inbox/test">>).

(dcy_vmq@192.168.3.236)68> vmq_util:unsub_topic(<<"test6">>, <<"inbox/test1">>).
ok
(dcy_vmq@192.168.3.236)69> vmq_util:unsub_topic({[], <<"test6">>}, <<"inbox/test1">>).
ok
```

## unsub_topics
```
-spec unsub_topics(Id :: binary() | tuple(), Topics :: topics()) -> ok | {error, any()}.

(dcy_vmq@192.168.3.236)70> vmq_util:unsub_topics({[], <<"test6">>}, [<<"inbox/test1">>, <<"inbox/test2">>]).
ok
(dcy_vmq@192.168.3.236)71> vmq_util:unsub_topics(<<"test6">>, [<<"inbox/test1">>, <<"inbox/test2">>]).      
ok
```

# get_subed_topics
```
-spec get_subed_topics(Id :: binary() | tuple()) -> list({topic(), integer()}).

(dcy_vmq@192.168.3.236)72> vmq_util:get_subed_topics(<<"test6">>).
[{[<<"inbox">>,<<"test6">>],1},
 {[<<"inbox">>,<<"test8">>],1}]
(dcy_vmq@192.168.3.236)73> vmq_util:get_subed_topics({[], <<"test6">>}).
[{[<<"inbox">>,<<"test6">>],1},
 {[<<"inbox">>,<<"test8">>],1}]
```
