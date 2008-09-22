
include ../vsn.mk

EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

APP_TARGET=$(EBIN)/macaddr.app
APPUP_TARGET=$(EBIN)/macaddr.appup

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	macaddr.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(APP_TARGET): macaddr.app.src ../vsn.mk
	sed -e 's;%VSN%;$(MACADDR_VSN);' $< > $@

$(APPUP_TARGET): macaddr.appup.src ../vsn.mk
	sed -e 's;%VSN%;$(MACADDR_VSN);' $< > $@
