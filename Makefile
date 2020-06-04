# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

APP=erl_playground

# usage: make TARGET REBAR_PROFILE=...
REBAR_PROFILE?=default
OVERLAY_CONFIG?=config/overlays/$(REBAR_PROFILE)/vars.config
GITREF?=$(shell git describe --tags --exact-match 2>/dev/null || git symbolic-ref -q --short HEAD)

# REBAR3=$(shell which rebar3)
REBAR3=./rebar3
ifeq ($(REBAR3),)
$(error "Rebar3 not available on this system")
endif

.PHONY: all deps rebar3 compile dialyze dialyze-full \
	test release tar clean distclean rebuild xref \
	rel/* log/* logclean refresh

# =============================================================================
# Rules to build the system
# =============================================================================

all: distclean deps release

deps:
	- $(REBAR3) compile

# usage:
# git checkout ${GITFROM}; make release ; git checkout ${GITTO} ; make release
# ./rebar3 appup generate ; cp .appup generated into src/<proj>.appup.src
# make relup % generates .relup
# make tar ; scp .tgz server:releases/<new_version>/<project>.tar.gz
# ./bin<project> upgrade <new_version>
appup-generate:
	- $(REBAR3) appup generate

relup:
	- $(REBAR3) relup

compile:
	- $(REBAR3) compile

dialyze:
	- $(REBAR3) dialyzer

dialyze-full:
	- $(REBAR3) as full_dialyze dialyzer

test:
	$(REBAR3) eunit

release: #gpg/secret-reveal
	- $(REBAR3) release

tar: gpg/secret-reveal
	- $(REBAR3) tar

clean:
	- $(REBAR3) clean

distclean: clean
	- rm -rf _build
	- $(REBAR3) clean --all # include deps

rebuild: distclean compile dialyze

xref: compile
	$(REBAR3) xref 2>&1 | grep -v -f .xrefignore

update-deps:
	$(REBAR3) unlock
	$(REBAR3) upgrade

# =============================================================================
# Misc
# =============================================================================

rel/%:
	cd _build/$(REBAR_PROFILE)/rel/$(APP) && bin/$(APP) $(@F)

log/%:
	tail -f _build/$(REBAR_PROFILE)/rel/$(APP)/log/$(@F)

logclean:
	- rm -rf _build/$(REBAR_PROFILE)/rel/$(APP)/log/*

$(REBAR3):
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

# usage: make local/mobile
# $(SERVER_LOCAL_RELEASES):
# 	- make release REBAR_PROFILE=$@ GITREF=$(GITREF)
local:
	- make release REBAR_PROFILE=$@ OVERLAY_CONFIG=config/overlays/$@/vars.config GITREF=$(GITREF)

refresh:
	git pull-force ; git submodule-update
	make compile $(REBAR_PROFILE)
	