#PACKAGE := e3dtree
#PACKAGE := ekdarbo
#PACKAGE := etannenbaum
PACKAGE := yalinka

VERSION := `cat version`
RELEASE := `cat release`
REVISION := `git --no-pager log --max-count=1 --format=format:%H`

DESTDIR ?= install-dir

UID := `id -u`
ARCH := `uname -m`

#DEBUG := true

SONIF := priv/lib/$(PACKAGE).so

COBJ  := $(patsubst c_src/%.c, obj/%.o, $(wildcard c_src/*.c))

ERLS  := $(wildcard src/*.erl)
BEAMS := $(patsubst src/%.erl, ebin/%.beam, $(ERLS))

ERL_FLAGS := +warn_unused_function \
        +warn_bif_clash \
        +warn_deprecated_function \
        +warn_obsolete_guard \
        +warn_shadow_vars \
        +warn_export_vars \
        +warn_unused_records \
        +warn_unused_import \
        -Werror

ERL_LIBS := ${ERL_LIBS}:~/lib/erl

C_OPTS := -c -Wall -Wextra -pedantic

L_OPTS := -lm

ifdef DEBUG

C_OPTS += -DDEBUG
ERL_FLAGS += +debug_info

endif

.PHONY: clean

all: $(PACKAGE)


$(PACKAGE): $(SONIF) ebin/$(PACKAGE).app $(BEAMS)


#
# bake edoc, erlang documentation
#
doc: doc/overview.edoc doc/index.html

doc/index.html: doc/overview.edoc
	erl -noinput -eval "edoc:application($(PACKAGE), \".\", [])" -s erlang halt

doc/overview.edoc: version tools/parse.pl
	@mkdir -p doc
	perl tools/parse.pl README.md doc/overview.edoc $(VERSION)

doc-install: doc
	mkdir -p $(DESTDIR)/$(PACKAGE)
	install --mode=644 doc/*.html doc/*.css doc/*.png $(DESTDIR)/$(PACKAGE)


#
# main shared object file with NIF
#
$(SONIF): privlib obj $(COBJ)
	gcc -o $(SONIF) $(L_OPTS) $(COBJ)

obj:
	mkdir -p obj

obj/%.o: c_src/%.c
	gcc -o $@ $(C_OPTS) $<

privlib:
	mkdir -p priv/lib

#
# Erlang wrapper application and modules
#
ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl
	erlc -o ebin -I include $(ERL_FLAGS) $<

ebin/$(PACKAGE).app: ebin src/$(PACKAGE).app.in
	sed "s/{{VERSION}}/$(VERSION)/" \
		src/$(PACKAGE).app.in >ebin/$(PACKAGE).app

#
# testing framework. Using eunit and proper.
#
$(PACKAGE)_test: test/$(PACKAGE)_test.erl
	ERL_LIBS=$(ERL_LIBS) erlc -o ebin -I include test/$(PACKAGE)_test.erl

eunit:	$(PACKAGE) $(PACKAGE)_test
	erl -noinput -pa ebin \
		-eval 'ok = eunit:test('$(PACKAGE)'_test)' \
		-s $(PACKAGE) unload \
		-s erlang halt

proper: $(PACKAGE) $(PACKAGE)_test
	ERL_LIBS=$(ERL_LIBS):~/lib/erl erl -noinput -pa ebin \
		-s $(PACKAGE)_test start \
		-s $(PACKAGE) unload \
		-s erlang halt

oolong: $(PACKAGE) $(PACKAGE)_test
	ERL_LIBS=$(ERL_LIBS):~/lib/erl erl -noinput -pa ebin \
		-eval 'true = not is_tuple('$(PACKAGE)'_test:start([verbose, {numtests, 10000}]))' \
		-s $(PACKAGE) unload \
		-s erlang halt

measure: $(PACKAGE) $(PACKAGE)_test
	erl -noinput -pa ebin \
		-s $(PACKAGE)_test measure \
		-s $(PACKAGE) unload \
		-s erlang halt

test:	clean eunit proper

longtest: clean eunit oolong

#
# helpers, algo debug, auxiliary targets
#
obj/pure.o: tools/pure.c
	gcc -o $@ $(C_OPTS) $<

pure: obj obj/pure.o
	gcc -o pure $(L_OPTS) obj/pure.o

spec: opensuse.spec.in blob.spec.in
	@echo "Creating rpm spec files..."
	sed "s,{{VERSION}},$(VERSION),g; \
	     s,{{RELEASE}},$(RELEASE),g; \
	     s,{{REVISION}},$(REVISION),g" opensuse.spec.in >opensuse.spec

#
# cleanup target
#
clean:
	@rm -f pure *.o erl_crash.dump opensuse.spec $(SONIF)
	@rm -rf ebin doc .eunit priv/lib obj $(DESTDIR)
