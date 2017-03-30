PROJECT = rabbitmq_message_timestamp_and_user_id_checker_in_cs_exchange
PROJECT_DESCRIPTION = RabbitMQ Message Timestamp And User Id Checker in cs- exchange

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, ["3.6.8"]}
endef

DEPS = rabbit_common rabbit
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
