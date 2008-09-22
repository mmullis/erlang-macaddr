
include ../vsn.mk

EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	macaddr_basic_test.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

