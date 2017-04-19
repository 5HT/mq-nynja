VM       := vm.args
SYS      := sys.config
PLT_NAME := ~/.mq_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR  ?= .
LOG_DIR  ?= ./log
empty    :=
ROOTS    := deps apps
space    := $(empty) $(empty)
comma    := $(empty),$(empty)
VSN      := $(shell git rev-parse HEAD | head -c 6)
DATE     := $(shell date "+%Y%m%d-%H%M%S")
ERL_LIBS := $(subst $(space),:,$(ROOTS))
relx     := "{release,{$(RELEASE),\"$(VER)\"},[$(RELEASE)]}.\\n{include_erts,true}.\
\\n{extended_start_script,true}.\\n{generate_start_script,true}.\\n{sys_config,\"$(SYS)\"}.\
\\n{vm_args,\"$(VM)\"}.\\n{overlay,[{mkdir,\"log/sasl\"}]}."

test: eunit ct
clean:
	rm -f .applist
	mad $@
compile:
	mad $@
.applist:
	mad plan
$(RUN_DIR) $(LOG_DIR):
	mkdir -p $(RUN_DIR) & mkdir -p $(LOG_DIR)
console: .applist
	ERL_LIBS=$(ERL_LIBS) erl +pc unicode $(ERL_ARGS) -eval '[application:start(A) || A <- $(shell cat .applist)]'
start: $(RUN_DIR) $(LOG_DIR) .applist
	RUN_ERL_LOG_GENERATIONS=1000 RUN_ERL_LOG_MAXSIZE=20000000 \
	ERL_LIBS=$(ERL_LIBS) run_erl -daemon $(RUN_DIR)/ $(LOG_DIR)/ "exec $(MAKE) console"
attach:
	to_erl $(RUN_DIR)/
release:
	echo $(relx) > relx.config && relx
stop:
	@kill -9 $(shell ps ax -o pid= -o command=|grep $(RELEASE)|grep $(COOKIE)|awk '{print $$1}')
$(PLT_NAME):
	$(eval APPS := $(subst deps/,,$(subst apps/,,$(shell find apps deps -maxdepth 1 -mindepth 1 -type d))))
	ERL_LIBS=$(ERL_LIBS) dialyzer --build_plt --output_plt $(PLT_NAME) --apps $(APPS) || true

.PHONY: compile clean console start attach release dialyze
