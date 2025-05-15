
# Apns Erlang

Provider for Apple Push Notificaion services (APNs). Uses poolboy for groups of connection pools to apns.

PR's welcome.

## Supports

- OTP 27
- Apple Keyfile P8 format.

### Add to project

You can use `apns_poolboy` as a dependency in your rebar.config:

    {deps , [
        {apns_poolboy, ".*", {git, "https://github.com/pankajsoni19/apns_poolboy", {tag, "1.0.0"}}}
    ]}.

### Configure

In your sys.config file, add `apns` block.

```erlang
[
...
    {apns_poolboy, [
        {pools, [
            {pool_name, {
                [
                    {size, 2}, 
                    {max_overflow, 0}
                ],
                [
                    {env, development | production},
                    {token_keyfile,    "p8 file path"},
                    {token_kid,        "team_kid"},
                    {team_id,          "team_id"},

                    {headers, [
                        {apns_id, "val"},
                        {apns_expiration, 1747322684},
                        {apns_priority, 10},
                        {apns_topic, "mytopic.com"},
                        {apns_push_type, alert},
                        {apns_collapse_id, "collapse-key"}
                    ]}
                ]
            }
        ]}   
    ]}
...
]
```

### How to use

```erlang
{ok, {Status, Headers, Response}} | retry =
apns_poolboy:push(pool_name, 
    #{
        device_id    => <<"device_id">>,
        headers     => #{
            apns_id             => <<"uuid">>,
            apns_expiration     => 1747322684,
            apns_priority       => 10,
            apns_topic          => <<"app bundle id">>,
            apns_push_type      => alert,
            apns_collapse_id    => <<"collapse-key">>
        },
        payload     => #{
            aps     => #{
                alert       => #{
                    title   => <<"hi">>
                }
            }
        }
    }).
= apns:push_notification(ProcName, DeviceId, payload(), default_headers()).

Status      -> integer()
Headers     -> #{ apns_id := binary(), apns_unique_id := binary() | undefined}
Response    -> no_body | #{ reason := binary() }
```

For `status` & `reason` definitions check [Apns Status Response Codes](https://developer.apple.com/documentation/usernotifications/handling-notification-responses-from-apns)

### Links

- [handling-notification-responses-from-apns](https://developer.apple.com/documentation/usernotifications/handling-notification-responses-from-apns)