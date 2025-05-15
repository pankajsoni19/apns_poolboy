-module(apns_utils).
-author("Pankaj Soni <pankajsoni19@live.com>").

% API
-export([
          generate_token/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

generate_token(KeyId, TeamId, PrivKey) ->
  Iat = erlang:system_time(second),

  Header = base64url:encode(jsx:encode(#{alg => <<"ES256">>, kid => KeyId})),
  Claims = base64url:encode(jsx:encode(#{iss => TeamId, iat => Iat})),
  Payload = <<Header/binary, ".", Claims/binary>>,

  {ok, FileData} = file:read_file(PrivKey),

  ECPrivateKeyPem1 = case public_key:pem_decode(FileData) of
    [_, ECPrivateKeyPem] -> ECPrivateKeyPem;
    [ECPrivateKeyPem] -> ECPrivateKeyPem
  end,

  ECPrivateKey = public_key:pem_entry_decode(ECPrivateKeyPem1),
  
  Signature = base64url:encode(jwt_ecdsa:signature(Payload, sha256, ECPrivateKey)),

  <<Payload/binary, ".", Signature/binary>>.