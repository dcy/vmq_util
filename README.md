vmq_util
=====

## Doc
- [is_register/1] (#is_register)
- [is_online/1] (#is_online)
- [disconnect/1] (#disconnect)
- [get_online_amount/0] (#get_online_amount)
- [get_all_amount/0] (#get_all_amount)
- [get_registers_info/0] (#get_registers_info)
- [sub_topic/2] (#sub_topic)
- [sub_topics/2] (#sub_topics)
- [unsub_topic/2] (#unsub_topics)
- [unsub_topics/2] (#unsub_topics)
- [get_subed_topics/1] (#get_subed_topics)

## is_register
``` 
-spec is_register(Id :: binary() | tuple()) -> true | false.

(xlg_vmq@192.168.3.236)48> vmq_util:is_register(<<"test1">>).
true
(xlg_vmq@192.168.3.236)49> vmq_util:is_register({[], <<"test100">>}).
false
```

## is_online
```
-spec is_online(Id :: binary() | tuple()) -> true | false.

(xlg_vmq@192.168.3.236)50> vmq_util:is_online(<<"test1">>).
false
(xlg_vmq@192.168.3.236)51> vmq_util:is_online({[], <<"test6">>}).
true
```

## disconnect
```
-spec disconnect(Id :: binary() | tuple()) -> ok | ignore.

```
