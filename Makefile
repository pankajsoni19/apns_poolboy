PROJECT = apns_poolboy
PROJECT_VERSION = $(shell head -n 1 relx.config | awk '{split($$0, a, "\""); print a[2]}')

app:: rebar.config

LOCAL_DEPS = inets sasl
DEPS = gun lager uuid poolboy jwt
dep_lager = git https://github.com/erlang-lager/lager 3.9.2
dep_gun = git https://github.com/ninenines/gun.git 2.2.0
dep_uuid = git https://github.com/okeuday/uuid.git v2.0.2
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.2
dep_jwt = git https://github.com/artemeff/jwt.git 0.1.11

BUILD_DEPS += relx

include erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

