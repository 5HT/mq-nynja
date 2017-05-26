VM       := vm.args
SYS      := sys.config
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

clean:
	rm -f .applist
	mad $@
compile:
	mad $@
.applist:
	mad plan
$(RUN_DIR) $(LOG_DIR):
	mkdir -p $(RUN_DIR) & mkdir -p $(LOG_DIR)
attach:
	to_erl $(RUN_DIR)/
console: .applist
	ERL_LIBS=$(ERL_LIBS) erl +pc unicode $(ERL_ARGS) -eval '[application:start(A) || A <- $(shell cat .applist)]'
start: $(RUN_DIR) $(LOG_DIR) .applist
	RUN_ERL_LOG_GENERATIONS=1000 RUN_ERL_LOG_MAXSIZE=20000000 \
	ERL_LIBS=$(ERL_LIBS) run_erl -daemon $(RUN_DIR)/ $(LOG_DIR)/ "exec $(MAKE) console"
stop:
	@kill -9 $(shell ps ax -o pid= -o command=|grep $(RELEASE)|grep $(COOKIE)|awk '{print $$1}')

.PHONY: compile clean console start
