
# Apns Erlang

Provider for Apple Push Notification service (APNs). Uses poolboy for groups of APNs connection pools.

PR's welcome.

## Supports

- OTP 27
- Apple Keyfile P8 format.

### Add to project

You can use `apns_poolboy` as a dependency in your rebar.config. The `1.0.0`
tag contains an authentication configuration bug and must not be used:

    {deps , [
        {apns_poolboy, ".*", {git, "https://github.com/pankajsoni19/apns_poolboy", {branch, "master"}}}
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
                    {request_timeout,  5000},

                    {headers, [
                        {apns_priority, 10},
                        {apns_topic, "mytopic.com"},
                        {apns_push_type, alert}
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
            apns_priority       => 10,
            apns_topic          => <<"app bundle id">>,
            apns_push_type      => alert
        },
        payload     => #{
            aps     => #{
                alert       => #{
                    title   => <<"hi">>
                }
            }
        }
    }).

Status      -> integer()
Headers     -> #{ apns_id := binary(), apns_unique_id := binary() | undefined}
Response    -> no_body | #{ reason := binary() }
```

Set `env` to `production` for App Store/TestFlight builds and use a production
device token. Sandbox and production device tokens are not interchangeable.
Only set `apns_expiration` when needed, and calculate it as a future Unix
timestamp; an expired value may be accepted by APNs but not delivered.

For `status` & `reason` definitions check [Apns Status Response Codes](https://developer.apple.com/documentation/usernotifications/handling-notification-responses-from-apns)

### Links

- [handling-notification-responses-from-apns](https://developer.apple.com/documentation/usernotifications/handling-notification-responses-from-apns)
