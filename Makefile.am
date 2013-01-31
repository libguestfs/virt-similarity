# virt-similarity
# Copyright (C) 2013 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	COPYING \
	README \
	config.ml.in \
	.depend \
	virt-similarity.pod \
	virt-similarity.spec \
	virt-similarity.spec.in \
	$(SOURCES)

CLEANFILES = *~ *.cmi *.cmo *.cmx *.cma *.cmxa virt-similarity

OCAMLPACKAGES = -package unix,guestfs
OCAMLCFLAGS = -g -warn-error CDEFLMPSUVYZX $(OCAMLPACKAGES)
OCAMLOPTFLAGS = $(OCAMLCFLAGS)

# Sources in alphabetical order.
SOURCES = \
	cache.mli \
	cache.ml \
	cladogram.mli \
	cladogram.ml \
	config.ml \
	hash.ml \
	similarity.ml \
	utils.ml

# Objects in build order.
OBJECTS = \
	config.cmo \
	utils.cmo \
	hash.cmo \
	cache.cmo \
	cladogram.cmo \
	similarity.cmo

XOBJECTS = $(OBJECTS:.cmo=.cmx)

bin_SCRIPTS = virt-similarity

if HAVE_OCAMLOPT
virt-similarity: $(XOBJECTS)
	$(OCAMLFIND) opt $(OCAMLOPTFLAGS) \
	  -linkpkg $(XOBJECTS) -o $@
else
virt-similarity: $(OBJECTS)
	$(OCAMLFIND) c $(OCAMLCFLAGS) \
	  -linkpkg $(OBJECTS) -o $@
endif

# Rules for all OCaml files.
%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $< -o $@
%.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $< -o $@
%.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -c $< -o $@

SUFFIXES = .cmo .cmi .cmx .ml .mli .mll .mly

# OCaml dependencies.
depend: .depend

.depend: $(SOURCES)
	rm -f $@ $@-t
	$(OCAMLFIND) ocamldep $^ | \
	  $(SED) -e 's/ *$$//' | \
	  $(SED) -e :a -e '/ *\\$$/N; s/ *\\\n */ /; ta' | \
	  LANG=C sort > $@-t
	mv $@-t $@

-include .depend

# Manual page.
man_MANS = virt-similarity.1

virt-similarity.1: virt-similarity.pod
	pod2man \
	  -c "Virtualization Tools" \
	  --release "$(PACKAGE)-$(VERSION)" \
	  --section 1 \
	  --stderr --utf8 \
	  $< > $@

rpm: dist
	rpmbuild -ta $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar.gz

licensecheck:
	licensecheck $$(git ls-files)
